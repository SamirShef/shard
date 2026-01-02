#include "include/type_checker/type_checker.h"
#include "include/diagnostic/diagnostic.h"
#include "include/codegen/codegen.h"
#include "include/parser/parser.h"
#include "include/lexer/lexer.h"
#include "include/common.h"
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/MC/TargetRegistry.h>
#include <filesystem>
#include <iostream>
#include <sstream>
#include <fstream>
#include <cstdlib>

void print_errs_and_clear(Diagnostic &diag);

int main(int argc, char **argv) {
    if (argc != 2) {
        std::cerr << COLOR_RED << "Usage: shardc path/to/src.sd\n" << COLOR_RESET;
        return 1;
    }
    
    std::ifstream file(argv[1]);
    if (!file.is_open()) {
        std::cerr << COLOR_RED << "Unable to open file: " << argv[1] << "\n" << COLOR_RESET;
        return 1;
    }
    std::ostringstream content;
    content << file.rdbuf();

    std::filesystem::path file_path = std::filesystem::absolute(argv[1]);
    std::string executable_path = file_path;

    if (executable_path.find('.') != std::string::npos) {
        for (int i = executable_path.size() - 1; executable_path[i] != '.'; i--) {
            executable_path.pop_back();
        }
        executable_path.pop_back();
    }

    #if defined(_WIN32)
    const char *obj_ext = ".obj";
    const char *exe_ext = ".exe";
    #else
    const char *obj_ext = ".o";
    const char *exe_ext = "";
    #endif
    
    #if defined(_WIN32)
    executable_path += exe_ext;
    #endif
    const std::string object_path = executable_path + obj_ext;

    Diagnostic diag;
    std::string src = content.str();

    Lexer lex(diag, argv[1], src);
    std::vector<Token> tokens = lex.tokenize();
    for (const Token token : tokens) {
        std::cout << token.to_str() << '\n';
    }
    print_errs_and_clear(diag);

    Parser parser(diag, src, tokens);
    std::vector<NodeUPTR> stmts = parser.parse();
    for (const NodeUPTR &stmt : stmts) {
        std::cout << stmt->to_str() << '\n';
    }
    print_errs_and_clear(diag);

    TypeChecker type_checker(diag, stmts);
    type_checker.analyze();
    print_errs_and_clear(diag);

    CodeGenerator codegen(stmts, argv[1]);
    codegen.generate();
    std::cout << '\n';
    codegen.print_ir();

    std::unique_ptr<llvm::Module> module = codegen.get_module();

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    
    if (module->getFunction("main") == nullptr) {
        std::cerr << "\033[31mCompilation error: Program does not have entry point 'main'\033[0m" << '\n';
        return 1;
    }
    auto getTriple = []() -> std::string {
        const char *env_triple = std::getenv("SHARD_TRIPLE");
        if (env_triple && *env_triple) {
            return std::string(env_triple);
        }

        std::array<char, 256> buffer{};
        std::string result;
        #if defined(_WIN32)
        std::unique_ptr<FILE, int(*)(FILE*)> pipe(_popen("clang -dumpmachine", "r"), _pclose);
        #else
        std::unique_ptr<FILE, int(*)(FILE*)> pipe(popen("clang -dumpmachine", "r"), pclose);
        #endif
        if (!pipe) {
            return "";
        }
        while (fgets(buffer.data(), static_cast<int>(buffer.size()), pipe.get()) != nullptr) {
            result += buffer.data();
        }
        while (!result.empty() && (result.back() == '\n' || result.back() == '\r')) {
            result.pop_back();
        }
        return result;
    };
    auto defaultTripleForHost = []() -> std::string {
        #if defined(_WIN32)
            #if defined(__aarch64__) || defined(_M_ARM64)
                return "aarch64-pc-windows-msvc";
            #else
                return "x86_64-pc-windows-msvc";
            #endif
        #elif defined(__APPLE__)
            #if defined(__aarch64__)
                return "arm64-apple-darwin";
            #else
                return "x86_64-apple-darwin";
            #endif
        #else
            #if defined(__aarch64__)
                return "aarch64-unknown-linux-gnu";
            #else
                return "x86_64-pc-linux-gnu";
            #endif
        #endif
    };
    std::string target_triple = getTriple();
    if (target_triple.empty()) {
        target_triple = defaultTripleForHost();
    }
    module->setTargetTriple(llvm::Triple(target_triple));

    std::string error;
    const llvm::Target *target = llvm::TargetRegistry::lookupTarget(target_triple, error);
    if (!target) {
        std::cerr << error << '\n';
        return 1;
    }

    std::string CPU = "generic";
    std::string features = "";
    llvm::TargetOptions opt;
    auto reloc_model = std::optional<llvm::Reloc::Model>();
    std::unique_ptr<llvm::TargetMachine> target_machine(target->createTargetMachine(target_triple, CPU, features, opt, reloc_model));
    if (!target_machine) {
        std::cerr << "\033[31mCompilation error: Failed to create TargetMachine for triple '" << target_triple << "'\033[0m\n";
        return 1;
    }

    module->setDataLayout(target_machine->createDataLayout());

    std::error_code ec;
    llvm::raw_fd_ostream dest(object_path, ec, llvm::sys::fs::OF_None);
    if (ec) {
        std::cerr << "\033[31mCompilation error: Could not open file '" << object_path << "': " << ec.message() << "\033[0m\n";
        return 1;
    }

    llvm::legacy::PassManager pass;
    
    auto fileType = static_cast<llvm::CodeGenFileType>(1); // 1 = Object file
    if (target_machine->addPassesToEmitFile(pass, dest, nullptr, fileType)) {
        std::cerr << "\033[31mCompilation error: TargetMachine can't emit a file of this type\033[0m\n";
        return 1;
    }

    pass.run(*module);
    dest.flush();
    dest.close();

    const char *env_linker = std::getenv("SHARDC_LINKER");
    std::string linker = env_linker ? std::string(env_linker) : std::string("clang");
    #if defined(_WIN32)
    std::string link_cmd = linker + " \"" + object_path + "\" -o \"" + executable_path + "\" -fuse-ld=lld";
    #elif defined(__APPLE__)
    std::string link_cmd = linker + " \"" + object_path + "\" -o \"" + executable_path + "\"";
    #else
    std::string link_cmd = linker + " \"" + object_path + "\" -o \"" + executable_path + "\" -no-pie";
    #endif
    auto runAndCapture = [](const std::string& cmd) -> std::pair<int, std::string> {
        std::string output;
        #if defined(_WIN32)
        std::unique_ptr<FILE, int(*)(FILE*)> pipe(_popen(cmd.c_str(), "r"), _pclose);
        #else
        std::unique_ptr<FILE, int(*)(FILE*)> pipe(popen(cmd.c_str(), "r"), pclose);
        #endif
        if (!pipe) {
            return { -1, std::string("Failed to spawn: ") + cmd };
        }
        std::array<char, 512> buf{};
        while (fgets(buf.data(), static_cast<int>(buf.size()), pipe.get()) != nullptr) {
            output += buf.data();
        }
        int code = 0;
        #if defined(_WIN32)
        code = _pclose(pipe.release());
        #else
        code = pclose(pipe.release());
        #endif
        return { code, output };
    };

    auto [linkRes, link_out] = runAndCapture(link_cmd);
    if (linkRes != 0) {
        std::cerr << "\033[31mCompilation error: Link command: " << link_cmd << '\n';
        std::cerr << link_out << '\n';
        std::cerr << "Linking failed with code " << linkRes << "\033[0m\n";
        return 1;
    }

    std::cout << "\033[36;1mCOMPILING SUCCESS. Built executable:\033[0m " << executable_path << '\n';
    
    if (std::remove(object_path.c_str()) != 0) {
        std::cerr << "\033[31mCompilation error: Warning: Failed to remove object file: " << object_path << "\033[0m\n";
        return 1;
    }
    
    return 0;
}

void print_errs_and_clear(Diagnostic &diag) {
    if (diag.has_errs()) {
        diag.print_errs();
        diag.clear();
        exit(1);
    }
}