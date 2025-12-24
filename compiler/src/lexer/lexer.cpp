#include "../include/lexer/lexer.h"
#include <stdexcept>

std::vector<Token> Lexer::tokenize() {
    std::vector<Token> tokens;

    for (const char c : file_name) {

    }

    return tokens;
}

Token Lexer::tokenize_id() {

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
    if (pos + rpos >= file_name.length()) {
        throw std::out_of_range("The index passed to the lexer is out of bounds");
    }
    return file_name[pos + rpos];
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