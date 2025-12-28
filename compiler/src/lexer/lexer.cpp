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
            if (pos + 1 < src.length() && (src[pos + 1] == '/' || src[pos + 1] == '*')) {
                skip_comments();
                continue;
            }
        }
        else if (isalpha(c) || c == '_') {
            tokens.push_back(tokenize_id());
        }
        else if (isdigit(c) || c == '.') {
            tokens.push_back(tokenize_num_lit());
        }
        else if (c == '\'') {
            tokens.push_back(tokenize_char_lit());
        }
        else if (c == '\"') {
            tokens.push_back(tokenize_str_lit());
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
    TokenKind kind = TokenKind::ID;
    if (val == "true" || val == "false") {
        kind = TokenKind::BLIT;
    }
    return Token{kind, val, {file_name, tmp_l, tmp_c, tmp_p, val.length()}};
}

Token Lexer::tokenize_num_lit() {
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
    TokenKind kind = TokenKind::ILIT;
    if (pos < src.length()) {
        suffix = advanve();
        switch (tolower(suffix)) {
            case ' ':
            case '\n':
                suffix = '\0';
                break;
            case 's':
                if (has_dot) {
                    DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                                 .level = DiagLevel::ERROR, .code = 6};
                    errs.push_back(err);
                }
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
                kind = TokenKind::SLIT;
                break;
            case 'l':
                if (has_dot) {
                    DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                                 .level = DiagLevel::ERROR, .code = 6};
                    errs.push_back(err);
                }
                try {
                    std::stol(val);
                }
                catch (std::out_of_range ex) {
                    DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                                 .level = DiagLevel::ERROR, .code = 3};
                    errs.push_back(err);
                }
                kind = TokenKind::LLIT;
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
                kind = TokenKind::FLIT;
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
                kind = TokenKind::DLIT;
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
            kind = TokenKind::DLIT;
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
                kind = TokenKind::SLIT;
            }
            else if (std::abs(ival) < ((u64)1 << 31)) {
                kind = TokenKind::ILIT;
            }
            else if (std::abs(ival) < ((u64)1 << 63)) {
                kind = TokenKind::LLIT;
            }
            else {
                DiagPart err{.start_line_pos = start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c, .pos = tmp_p},
                             .level = DiagLevel::ERROR, .code = 3};
                errs.push_back(err);
            }
        }
    }
    for (auto err : errs) {
        u64 end_line_pos = 0;
        for (end_line_pos = err.start_line_pos; end_line_pos < src.length() && src[end_line_pos] != '\n'; end_line_pos++);
        err.line_len = end_line_pos - err.start_line_pos;
        err.pos.len = pos - tmp_p;
        std::ostringstream msg;
        switch (err.code) {
            case 0: {       // several points in a numeric literal
                msg << RED << "A numeric literal contains several dots.\n" << RESET;
                std::string raw_line = src.substr(err.start_line_pos, err.line_len);
                std::string line = ltrim(raw_line);
                msg << std::setw(6) << err.pos.line << " | " << line << '\n';
                msg << "       | " << std::string(pos - start_line_pos - err.pos.len - raw_line.length() + line.length(), ' ') << RED
                    << std::string(err.pos.len - 1, '^') << RESET << " invalid literal";
                break;
            }
            case 1: {       // does not have digits after the decimal point
                msg << RED << "A numeric literal does not have digits after the decimal point.\n" << RESET;
                std::string raw_line = src.substr(err.start_line_pos, err.line_len);
                std::string line = ltrim(raw_line);
                msg << std::setw(6) << err.pos.line << " | " << line << '\n';
                msg << "       | " << std::string(pos - start_line_pos - err.pos.len - raw_line.length() + line.length(), ' ') << RED
                    << std::string(err.pos.len - 1, '^') << RESET << " invalid literal";
                break;
            }
            case 2: {       // Unsupported suffix
                msg << RED << "Unsupported suffix of a numeric literal.\n" << RESET;
                std::string raw_line = src.substr(err.start_line_pos, err.line_len);
                std::string line = ltrim(raw_line);
                msg << std::setw(6) << err.pos.line << " | " << line << '\n';
                msg << "       | " << std::string(pos - start_line_pos - 1 - raw_line.length() + line.length(), ' ') << RED << '^' << RESET << " invalid suffix";
                break;
            }
            case 3: {       // Overflow number
                msg << RED << "Numeric literal cause overflow.\n" << RESET;
                std::string raw_line = src.substr(err.start_line_pos, err.line_len);
                std::string line = ltrim(raw_line);
                msg << std::setw(6) << err.pos.line << " | " << line << '\n';
                msg << "       | " << std::string(pos - start_line_pos - err.pos.len - raw_line.length() + line.length(), ' ') << RED
                    << std::string(err.pos.len - 1, '^') << RESET << " invalid literal";
                break;
            }
            case 6: {       // Floating-point literal has integer suffix
                msg << RED << "A floating-point literal has an integer suffix.\n" << RESET;
                std::string raw_line = src.substr(err.start_line_pos, err.line_len);
                std::string line = ltrim(raw_line);
                msg << std::setw(6) << err.pos.line << " | " << line << '\n';
                msg << "       | " << std::string(pos - start_line_pos - 1 - raw_line.length() + line.length(), ' ') << RED << '^' << RESET << " invalid literal";
                break;
            }
        }
        err.msg = msg.str();
        diag.add_part(err);
    }
    return Token{kind, val, {file_name, tmp_l, tmp_c, tmp_p, val.length()}};
}

Token Lexer::tokenize_str_lit() {
    u64 tmp_l = line;
    u64 tmp_c = column;
    u64 tmp_p = pos;
    std::string val;
    advanve();      // skip `"`
    while (pos < src.length() && peek() != '\"') {
        char c;
        if (peek() == '\\') {
            advanve();
            c = get_escape_sequence(tmp_l, tmp_c, tmp_p);
        }
        else {
            c = advanve();
        }
        val += c;
    }
    if (pos == src.length()) {
        u64 end_line_pos = 0;
        for (end_line_pos = pos; end_line_pos < src.length() && src[end_line_pos] != '\n'; end_line_pos++);
        DiagPart err{.start_line_pos = start_line_pos, .line_len = end_line_pos - start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c,
                                                                                                          .pos = tmp_p, .len = pos - tmp_p},
                     .level = DiagLevel::ERROR, .code = 10};
        std::ostringstream msg;
        msg << RED << "Missing closing single quote `\"` in string literal.\n" << RESET;
        std::string line = ltrim(src.substr(err.start_line_pos, err.line_len));
        msg << std::setw(6) << err.pos.line << " | " << line << '\n';
        msg << "       | " << std::string(line.length() - err.pos.len, ' ') << RED << std::string(err.pos.len, '^') << RESET << " invalid literal";
        err.msg = msg.str();
        diag.add_part(err);
    }
    else {
        advanve();  // skip `"`
    }
    return Token{TokenKind::STRLIT, val, {file_name, tmp_l, tmp_c, tmp_p, val.length()}};
}

Token Lexer::tokenize_char_lit() {
    u64 tmp_l = line;
    u64 tmp_c = column;
    u64 tmp_p = pos;
    std::string val;
    advanve();      // skip `'`
    while (pos < src.length() && peek() != '\'') {
        char c;
        if (peek() == '\\') {
            advanve();
            c = get_escape_sequence(tmp_l, tmp_c, tmp_p);
        }
        else {
            c = advanve();
        }
        val += c;
    }
    if (pos == src.length()) {
        u64 end_line_pos = 0;
        for (end_line_pos = pos; end_line_pos < src.length() && src[end_line_pos] != '\n'; end_line_pos++);
        DiagPart err{.start_line_pos = start_line_pos, .line_len = end_line_pos - start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c,
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
        u64 end_line_pos = 0;
        for (end_line_pos = pos; end_line_pos < src.length() && src[end_line_pos] != '\n'; end_line_pos++);
        DiagPart err{.start_line_pos = start_line_pos, .line_len = end_line_pos - start_line_pos, .pos = {.file_name = file_name, .line = tmp_l, .column = tmp_c,
                                                                                                          .pos = tmp_p, .len = pos - tmp_p},
                     .level = DiagLevel::ERROR, .code = 5};
        std::ostringstream msg;
        msg << RED << "The character constant must have a length of 1.\n" << RESET;
        std::string raw_line = src.substr(err.start_line_pos, err.line_len);
        std::string line = ltrim(raw_line);
        msg << std::setw(6) << err.pos.line << " | " << line << '\n';
        msg << "       | " << std::string(pos - start_line_pos - err.pos.len - raw_line.length() + line.length(), ' ') << RED
            << std::string(err.pos.len, '^') << RESET << " invalid literal";
        err.msg = msg.str();
        diag.add_part(err);
    }
    return Token{TokenKind::CLIT, val, {file_name, tmp_l, tmp_c, tmp_p, val.length()}};
}

Token Lexer::tokenize_op() {

}

void Lexer::skip_comments() {
    if (peek(1) == '/') {
        while (pos < src.length() && peek() != '\n') {
            advanve();
        }
    }
    else {
        while (pos < src.length() - 1 && (peek() != '*' || peek(1) != '/')) {
            advanve();
        }
        advanve();
        advanve();
    }
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
        start_line_pos = pos + 1;
    }
    column++;
    pos++;
    return c;
}

const char Lexer::get_escape_sequence(u64 tmp_l, u64 tmp_c, u64 tmp_p) {
    switch (peek()) {
        case 'a':
            return '\a';
        case 'b':
            return '\b';
        case 'e':
            return '\e';
        case 'f':
            return '\f';
        case 'r':
            return '\r';
        case 'n':
            return '\n';
        case 't':
            return '\t';
        case 'v':
            return '\v';
        case '\\':
            return '\\';
        case '\'':
            return '\'';
        case '\"':
            return '\"';
        case '\?':
            return '\?';
        case 'o': {
            advanve();
            u16 val = 0;
            u8 digits_count = 0;
            for (int i = 0; pos < src.length() && i < 3; i++) {
                if (isdigit(peek()) && peek() < '8') {
                    val = val * 8 + peek() - '0';
                    digits_count++;
                    advanve();
                }
                else {
                    break;
                }
            }
            if (digits_count == 0) {
                u64 end_line_pos = 0;
                for (end_line_pos = pos; end_line_pos < src.length() && src[end_line_pos] != '\n'; end_line_pos++);
                DiagPart err{.start_line_pos = start_line_pos, .line_len = end_line_pos - start_line_pos, .pos = {.file_name = file_name, .line = tmp_l,
                                                                                                                  .column = tmp_c, .pos = tmp_p, .len = pos - tmp_p},
                             .level = DiagLevel::ERROR, .code = 8};
                std::ostringstream msg;
                msg << RED << "Empty escape sequence.\n" << RESET;
                std::string raw_line = src.substr(err.start_line_pos, err.line_len);
                std::string line = ltrim(raw_line);
                msg << std::setw(6) << err.pos.line << " | " << line << '\n';
                msg << "       | " << std::string(pos - start_line_pos - err.pos.len - raw_line.length() + line.length() + 1, ' ') << RED
                    << std::string(err.pos.len - 1, '^') << RESET << " invalid literal";
                err.msg = msg.str();
                diag.add_part(err);
            }
            if (val >= 128) {
                u64 end_line_pos = 0;
                for (end_line_pos = pos; end_line_pos < src.length() && src[end_line_pos] != '\n'; end_line_pos++);
                DiagPart err{.start_line_pos = start_line_pos, .line_len = end_line_pos - start_line_pos, .pos = {.file_name = file_name, .line = tmp_l,
                                                                                                                  .column = tmp_c, .pos = tmp_p, .len = pos - tmp_p},
                             .level = DiagLevel::ERROR, .code = 7};
                std::ostringstream msg;
                msg << RED << "The value of the escape sequence does not fit into the range `char`.\n" << RESET;
                std::string raw_line = src.substr(err.start_line_pos, err.line_len);
                std::string line = ltrim(raw_line);
                msg << std::setw(6) << err.pos.line << " | " << line << '\n';
                msg << "       | " << std::string(pos - start_line_pos - err.pos.len - raw_line.length() + line.length() + 1, ' ') << RED
                    << std::string(err.pos.len - 1, '^') << RESET << " invalid literal";
                err.msg = msg.str();
                diag.add_part(err);
            }
            return val;
        }
        case 'x': {
            advanve();
            u16 val = 0;
            u8 digits_count = 0;
            for (int i = 0; pos < src.length() && i < 2; i++) {
                if (isdigit(peek())) {
                    val = val * 16 + peek() - '0';
                    digits_count++;
                    advanve();
                }
                else if (isalpha(peek()) && tolower(peek()) < 'g') {
                    val = val * 16 + tolower(peek()) - 'a' + 10;
                    digits_count++;
                    advanve();
                }
                else {
                    break;
                }
            }
            std::cout << val << '\n';
            if (digits_count == 0) {
                u64 end_line_pos = 0;
                for (end_line_pos = pos; end_line_pos < src.length() && src[end_line_pos] != '\n'; end_line_pos++);
                DiagPart err{.start_line_pos = start_line_pos, .line_len = end_line_pos - start_line_pos, .pos = {.file_name = file_name, .line = tmp_l,
                                                                                                                  .column = tmp_c, .pos = tmp_p, .len = pos - tmp_p},
                             .level = DiagLevel::ERROR, .code = 8};
                std::ostringstream msg;
                msg << RED << "Empty escape sequence.\n" << RESET;
                std::string raw_line = src.substr(err.start_line_pos, err.line_len);
                std::string line = ltrim(raw_line);
                msg << std::setw(6) << err.pos.line << " | " << line << '\n';
                msg << "       | " << std::string(pos - start_line_pos - err.pos.len - raw_line.length() + line.length() + 1, ' ') << RED
                    << std::string(err.pos.len - 1, '^') << RESET << " invalid literal";
                err.msg = msg.str();
                diag.add_part(err);
            }
            if (val >= 128) {
                u64 end_line_pos = 0;
                for (end_line_pos = pos; end_line_pos < src.length() && src[end_line_pos] != '\n'; end_line_pos++);
                DiagPart err{.start_line_pos = start_line_pos, .line_len = end_line_pos - start_line_pos, .pos = {.file_name = file_name, .line = tmp_l,
                                                                                                                  .column = tmp_c, .pos = tmp_p, .len = pos - tmp_p},
                             .level = DiagLevel::ERROR, .code = 7};
                std::ostringstream msg;
                msg << RED << "The value of the escape sequence does not fit into the range `char`.\n" << RESET;
                std::string raw_line = src.substr(err.start_line_pos, err.line_len);
                std::string line = ltrim(raw_line);
                msg << std::setw(6) << err.pos.line << " | " << line << '\n';
                msg << "       | " << std::string(pos - start_line_pos - err.pos.len - raw_line.length() + line.length() + 1, ' ') << RED
                    << std::string(err.pos.len - 1, '^') << RESET << " invalid literal";
                err.msg = msg.str();
                diag.add_part(err);
            }
            return val;
        }
        default: {
            u64 end_line_pos = 0;
            for (end_line_pos = pos; end_line_pos < src.length() && src[end_line_pos] != '\n'; end_line_pos++);
            DiagPart err{.start_line_pos = start_line_pos, .line_len = end_line_pos - start_line_pos, .pos = {.file_name = file_name, .line = tmp_l,
                                                                                                              .column = tmp_c, .pos = tmp_p, .len = pos - tmp_p},
                         .level = DiagLevel::ERROR, .code = 9};
            std::ostringstream msg;
            msg << RED << "Unsupported escape sequence.\n" << RESET;
            std::string raw_line = src.substr(err.start_line_pos, err.line_len);
            std::string line = ltrim(raw_line);
            msg << std::setw(6) << err.pos.line << " | " << line << '\n';
            msg << "       | " << std::string(pos - start_line_pos - err.pos.len - raw_line.length() + line.length() + 1, ' ') << RED
                << std::string(err.pos.len, '^') << RESET << " invalid literal";
            err.msg = msg.str();
            diag.add_part(err);
            return '\0';
        }
    }
}