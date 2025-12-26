#include "../include/lexer/lexer.h"
#include "../include/common.h"
#include <iomanip>
#include <iostream>
#include <cstdlib>
#include <sstream>
#include <cctype>

static u64 start_line_pos = 0;

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
        else if (isalpha(c) || c == '_') {
            tokens.push_back(tokenize_id());
        }
        else if (isdigit(c) || c == '.') {
            tokens.push_back(tokenize_num());
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
    u64 tmp_p = pos;
    std::string val;
    while (pos < src.length() && (isalnum(peek()) || peek() == '_')) {
        val += advanve();
    }
    return Token{TokenKind::ID, val, {file_name, tmp_l, tmp_c, tmp_p, val.length()}};
}

Token Lexer::tokenize_num() {
    u64 tmp_l = line;
    u64 tmp_c = column;
    u64 tmp_p = pos;
    std::string val;
    bool has_dot = false;
    std::vector<DiagPart> errs;
    while (pos < src.length() && (isdigit(peek()) || peek() == '_' || peek() == '.')) {
        if (peek() == '.') {
            if (has_dot) {
                DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                             .level = DiagLevel::ERROR, .code = 0};
                errs.push_back(err);
            }
            if (pos < src.length() - 1 && !isdigit(peek(1)) || pos == src.length() - 1) {
                DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                             .level = DiagLevel::ERROR, .code = 1};
                errs.push_back(err);
            }
            if (errs.size() != 0) {
                if (pos < src.length()) {
                    advanve();
                }
                continue;
            }
            has_dot = true;
        }
        val += advanve();
    }
    char suffix = '\0';
    TokenKind type = TokenKind::ILIT;
    if (pos < src.length()) {
        suffix = advanve();
        switch (tolower(suffix)) {
            case ' ':
                break;
            case 's':
                type = TokenKind::SLIT;
                break;
            case 'l':
                type = TokenKind::LLIT;
                break;
            case 'f':
                type = TokenKind::FLIT;
                break;
            case 'd':
                type = TokenKind::DLIT;
                break;
            default: {
                DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                             .level = DiagLevel::ERROR, .code = 2};
                errs.push_back(err);
            }
        }
    }
    if (suffix == '\0' && has_dot) {
        type = TokenKind::DLIT;
    }
    for (auto err : errs) {
        err.line_len = pos - start_line_pos;
        err.pos.len = pos - tmp_p;
        std::ostringstream msg;
        switch (err.code) {
            case 0: {       // several points in a numeric literal
                msg << RED << "A numeric literal contains several dots.\n" << RESET;
                std::string line = ltrim(src.substr(err.start_line_pos, err.line_len));
                msg << std::setw(6) << err.pos.line << " | " << line << '\n';
                msg << "       | " << std::string(line.length() - err.pos.len, ' ') << RED << std::string(err.pos.len, '^') << RESET << " invalid literal";
                break;
            }
            case 1: {       // does not have digits after the decimal point
                msg << RED << "A numeric literal does not have digits after the decimal point.\n" << RESET;
                std::string line = ltrim(src.substr(err.start_line_pos, err.line_len));
                msg << std::setw(6) << err.pos.line << " | " << line << '\n';
                msg << "       | " << std::string(line.length() - err.pos.len, ' ') << RED << std::string(err.pos.len, '^') << RESET << " invalid literal";
                break;
            }
            case 2: {       // Unsupported suffix
                msg << RED << "Unsupported suffix of a numeric literal.\n" << RESET;
                std::string line = ltrim(src.substr(err.start_line_pos, err.line_len));
                msg << std::setw(6) << err.pos.line << " | " << line << '\n';
                msg << "       | " << std::string(line.length() - 1, ' ') << RED << '^' << RESET << " invalid suffix";
                break;
            }
        }
        err.msg = msg.str();
        diag.add_part(err);
    }
    return Token{type, val, {file_name, tmp_l, tmp_c, tmp_p, val.length()}};
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
        start_line_pos = pos;
    }
    column++;
    pos++;
    return c;
}