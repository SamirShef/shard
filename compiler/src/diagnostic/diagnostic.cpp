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
    for (int i = 0; i < errs.size(); ++i) {
        std::cerr << errs[i].to_str() << '\n';
        if (i < errs.size() - 1) {
            std::cerr << '\n';
        }
    }
}

void Diagnostic::clear() {
    errs.clear();
}

std::string get_msg_by_code(u16 code) {
    return err_msgs.at(code);
}

void diag_part_create(Diagnostic &diag, DiagPart part, std::string err_msg) {
    std::ostringstream msg;
    msg << COLOR_RED << get_msg_by_code(part.code) << '\n' << COLOR_RESET;
    msg << err_msg;
    part.msg = msg.str();
    diag.add_part(part);
}

void diag_part_create(Diagnostic &diag, u16 code, Position pos, DiagLevel level, std::string err_msg) {
    DiagPart err { .pos = pos, .level = level, .code = code, };
    std::ostringstream msg;
    msg << COLOR_RED << get_msg_by_code(err.code) << '\n' << COLOR_RESET;
    msg << err_msg;
    err.msg = msg.str();
    diag.add_part(err);
}