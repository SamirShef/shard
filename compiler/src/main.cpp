#include "include/diagnostic/diagnostic.h"
#include "include/lexer/lexer.h"
#include "include/common.h"
#include "include/parser/parser.h"
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <fstream>

void print_errs_and_clear(Diagnostic &diag);

int main(int argc, char **argv) {
    if (argc != 2) {
        std::cerr << RED << "Usage: shardc path/to/src.sd\n" << RESET;
        return 1;
    }
    
    std::ifstream file(argv[1]);
    if (!file.is_open()) {
        std::cerr << RED << "Unable to open file: " << argv[1] << "\n" << RESET;
        return 1;
    }
    std::ostringstream content;
    content << file.rdbuf();

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
    return 0;
}

void print_errs_and_clear(Diagnostic &diag) {
    if (diag.has_errs()) {
        diag.print_errs();
        diag.clear();
        exit(1);
    }
}