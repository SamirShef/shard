#pragma once
#include "../diagnostic/diagnostic.h"
#include "token.h"
#include <vector>

class Lexer {
    Diagnostic &diag;
    std::string file_name;
    std::string src;
    u64 pos;
    u64 line;
    u64 column;

public:
    Lexer(Diagnostic &d, std::string fn, std::string s) : diag(d), file_name(fn), src(s), pos(0), line(1), column(1) {}

    std::vector<Token> tokenize();

private:
    Token tokenize_id();
    Token tokenize_num_lit();
    Token tokenize_str_lit();
    Token tokenize_char_lit();
    Token tokenize_op();
    void skip_comments();
    const char peek(u64 rpos = 0) const;
    const char advanve();
    const char get_escape_sequence(u64 tmp_l, u64 tmp_c, u64 tmp_p);
};