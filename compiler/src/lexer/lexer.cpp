#include "../include/lexer/lexer.h"
#include <iostream>
#include <cstdlib>
#include <cctype>

std::vector<Token> Lexer::tokenize() {
    std::vector<Token> tokens;

    while (pos < src.length()) {
        const char c = peek();
        if (isspace(c)) {
            advanve();
        }
        else if (c == '/') {
            if (pos + 1 < src.length() && src[pos + 1] == '/') {
                skip_comments();
            }
        }
        else if (isalnum(c) || c == '_') {
            tokens.push_back(tokenize_id());
        }
        else {
            tokens.push_back({.kind = TokenKind::UNKNOWN, .val = "", .pos = {.file_name = file_name, .line = line, .column = column, .len = 0}});
        }
    }

    return tokens;
}

Token Lexer::tokenize_id() {
    u64 tmp_l = line;
    u64 tmp_c = column;
    std::string val;
    while (pos < src.length() && (isalnum(peek()) || peek() == '_')) {
        val += advanve();
    }
    return Token{TokenKind::ID, val, {file_name, tmp_l, tmp_c, val.length()}};
}

Token Lexer::tokenize_str_lit() {

}

Token Lexer::tokenize_char_lit() {

}

Token Lexer::tokenize_op() {

}

void Lexer::skip_comments() {

}

const char Lexer::peek(u64 rpos) const {
    if (pos + rpos >= src.length()) {
        std::cerr << RED << "The index passed to the lexer is out of bounds\n" << RESET;
        exit(1);
    }
    return src[pos + rpos];
}

const char Lexer::advanve() {
    auto c = peek();
    if (c == '\n') {
        column = 0;
        line++;
    }
    column++;
    pos++;
    return c;
}