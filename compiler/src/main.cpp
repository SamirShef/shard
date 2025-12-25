#include "include/common.h"
#include "include/diagnostic/diagnostic.h"
#include "include/lexer/lexer.h"
#include <fstream>
#include <iostream>
#include <sstream>

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

    Lexer lex(diag, argv[1], content.str());
    std::vector<Token> tokens = lex.tokenize();
    for (Token token : tokens) {
        std::cout << token.to_str() << '\n';
    }

    if (diag.has_errs()) {
        diag.print_errs();
    }
    return 0;
}