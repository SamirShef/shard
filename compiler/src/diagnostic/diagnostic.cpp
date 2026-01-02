#include "../include/diagnostic/codes_msg_table.h"
#include "../include/diagnostic/diagnostic.h"
#include <iostream>

void Diagnostic::add_part(DiagPart part) {
    errs.push_back(part);
}

bool Diagnostic::has_errs() const {
    return errs.size() != 0;
}

void Diagnostic::print_errs() const {
    for (auto part : errs) {
        std::cerr << part.to_str() << '\n';
    }
}

void Diagnostic::clear() {
    errs.clear();
}

std::string get_msg_by_code(u16 code) {
    return err_msgs.at(code);
}

void diag_part_create(Diagnostic &diag, DiagPart part, std::string &src, u64 space_len, u64 highlighter_len, std::string err_msg) {
    std::ostringstream msg;
    msg << COLOR_RED << get_msg_by_code(part.code) << '\n' << COLOR_RESET;
    std::string line = ltrim(src.substr(part.start_line_pos, part.line_len));
    msg << std::setw(6) << part.pos.line << " | " << line << '\n';
    msg << "       | " << std::string(space_len, ' ') << COLOR_RED << std::string(highlighter_len, '^') << COLOR_RESET << " " << err_msg;
    part.msg = msg.str();
    diag.add_part(part);
}

void diag_part_create(Diagnostic &diag, DiagPart part, std::string &src, std::string highlighter) {
    std::ostringstream msg;
    msg << COLOR_RED << get_msg_by_code(part.code) << '\n' << COLOR_RESET;
    std::string line = ltrim(src.substr(part.start_line_pos, part.line_len));
    msg << std::setw(6) << part.pos.line << " | " << line << '\n';
    msg << highlighter;
    part.msg = msg.str();
    diag.add_part(part);
}

void diag_part_create(Diagnostic &diag, u16 code, Position pos, DiagLevel level) {
    std::string msg = COLOR_RED + get_msg_by_code(code) + "\n" + COLOR_RESET;
    DiagPart err { .pos = pos, .level = level, .code = code, .msg = msg };
    diag.add_part(err);
}