#include "../include/lexer/lexer.h"
#include "../include/common.h"
#include <iomanip>
#include <iostream>
#include <cstdlib>
#include <sstream>
#include <cctype>
#include <stdexcept>
#include <string>

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
        else if (c == '\'') {
            tokens.push_back(tokenize_char_lit());
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
            if (val.length() == 0) {
                val += '0';
            }
            has_dot = true;
        }
        else if (peek() == '_') {
            advanve();
            continue;
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
                try {
                    i32 ival = std::stoi(val);
                    if (std::abs(ival) > (1 << 15)) {
                        DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                                     .level = DiagLevel::ERROR, .code = 3};
                        errs.push_back(err);
                    }
                }
                catch (std::out_of_range ex) {
                    DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                                 .level = DiagLevel::ERROR, .code = 3};
                    errs.push_back(err);
                }
                type = TokenKind::SLIT;
                break;
            case 'l':
                try {
                    std::stol(val);
                }
                catch (std::out_of_range ex) {
                    DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                                 .level = DiagLevel::ERROR, .code = 3};
                    errs.push_back(err);
                }
                type = TokenKind::LLIT;
                break;
            case 'f':
                try {
                    std::stof(val);
                }
                catch (std::out_of_range ex) {
                    DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                                 .level = DiagLevel::ERROR, .code = 3};
                    errs.push_back(err);
                }
                type = TokenKind::FLIT;
                break;
            case 'd':
                try {
                    std::stod(val);
                }
                catch (std::out_of_range ex) {
                    DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                                 .level = DiagLevel::ERROR, .code = 3};
                    errs.push_back(err);
                }
                type = TokenKind::DLIT;
                break;
            default: {
                DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                             .level = DiagLevel::ERROR, .code = 2};
                errs.push_back(err);
            }
        }
    }
    if (suffix == '\0') {
        if (has_dot) {
            try {
                std::stod(val);
            }
            catch (std::out_of_range ex) {
                DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                             .level = DiagLevel::ERROR, .code = 3};
                errs.push_back(err);
            }
            type = TokenKind::DLIT;
        }
        else {
            i64 ival = 0;
            try {
                ival = std::stoll(val);
            }
            catch (std::out_of_range ex) {
                DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                             .level = DiagLevel::ERROR, .code = 3};
                errs.push_back(err);
            }
            if (std::abs(ival) < (1 << 15)) {
                type = TokenKind::SLIT;
            }
            else if (std::abs(ival) < ((u64)1 << 31)) {
                type = TokenKind::ILIT;
            }
            else if (std::abs(ival) < ((u64)1 << 63)) {
                type = TokenKind::LLIT;
            }
        }
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
            case 3: {       // Overflow number
                msg << RED << "Numeric literal cause overflow.\n" << RESET;
                std::string line = ltrim(src.substr(err.start_line_pos, err.line_len));
                msg << std::setw(6) << err.pos.line << " | " << line << '\n';
                msg << "       | " << std::string(line.length() - err.pos.len, ' ') << RED << std::string(err.pos.len, '^') << RESET << " invalid literal";
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
    u64 tmp_l = line;
    u64 tmp_c = column;
    u64 tmp_p = pos;
    std::string val;
    advanve();      // skip `'`
    while (pos < src.length() && peek() != '\'') {
        val += advanve();
    }
    if (pos == src.length()) {
        DiagPart err{.start_line_pos = start_line_pos, .line_len = pos - start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c,
                                                                                                 .pos = tmp_p, .len = pos - tmp_p},
                     .level = DiagLevel::ERROR, .code = 4};
        std::ostringstream msg;
        msg << RED << "Missing closing single quote `'` in character literal.\n" << RESET;
        std::string line = ltrim(src.substr(err.start_line_pos, err.line_len));
        msg << std::setw(6) << err.pos.line << " | " << line << '\n';
        msg << "       | " << std::string(line.length() - err.pos.len, ' ') << RED << std::string(err.pos.len, '^') << RESET << " invalid literal";
        err.msg = msg.str();
        diag.add_part(err);
    }
    else {
        advanve();  // skip `'`
    }
    if (val.length() != 1) {
        DiagPart err{.start_line_pos = start_line_pos, .line_len = pos - start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c,
                                                                                                 .pos = tmp_p, .len = pos - tmp_p},
                     .level = DiagLevel::ERROR, .code = 5};
        std::ostringstream msg;
        msg << RED << "The character constant must have a length of 1.\n" << RESET;
        std::string line = ltrim(src.substr(err.start_line_pos, err.line_len));
        msg << std::setw(6) << err.pos.line << " | " << line << '\n';
        msg << "       | " << std::string(line.length() - err.pos.len, ' ') << RED << std::string(err.pos.len, '^') << RESET << " invalid literal";
        err.msg = msg.str();
        diag.add_part(err);
    }
    return Token{TokenKind::CLIT, val, {file_name, tmp_l, tmp_c, tmp_p, val.length()}};
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