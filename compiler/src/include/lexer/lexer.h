#pragma once
#include "token.h"
#include <vector>

class Lexer {
    std::string file_name;
    std::string src;
    u64 pos;
    u64 line;
    u64 column;

public:
    Lexer(std::string fn, std::string s) : file_name(fn), src(s), pos(0), line(1), column(1) {}

    std::vector<Token> tokenize();

private:
    Token tokenize_id();
    Token tokenize_str_lit();
    Token tokenize_char_lit();
    Token tokenize_op();
    void skip_comments();
    const char peek(u64 rpos = 0) const;
    const char advanve();
};