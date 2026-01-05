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

std::string get_msg_by_code(u16 code, DiagLevel level) {
    switch (level) {
        case DiagLevel::ERROR:
            return err_msgs.at(code);
        case DiagLevel::WARNING:
            return war_msgs.at(code);
        case DiagLevel::NOTE:
            return notes_msgs.at(code);
    }
}

void diag_part_create(Diagnostic &diag, DiagPart part, std::string err_msg) {
    std::ostringstream msg;
    switch (part.level) {
        case DiagLevel::ERROR:
            msg << COLOR_RED;
            break;
        case DiagLevel::WARNING:
            msg << COLOR_YELLOW;
            break;
        case DiagLevel::NOTE:
            msg << COLOR_GREEN;
            break;
    }
    msg << get_msg_by_code(part.code, part.level) << '\n' << COLOR_RESET;
    msg << err_msg;
    part.msg = msg.str();
    diag.add_part(part);
}

void diag_part_create(Diagnostic &diag, u16 code, Position pos, DiagLevel level, std::string err_msg) {
    DiagPart err { .pos = pos, .level = level, .code = code, };
    std::ostringstream msg;
    switch (level) {
        case DiagLevel::ERROR:
            msg << COLOR_RED;
            break;
        case DiagLevel::WARNING:
            msg << COLOR_YELLOW;
            break;
        case DiagLevel::NOTE:
            msg << COLOR_GREEN;
            break;
    }
    msg << get_msg_by_code(err.code, level) << '\n' << COLOR_RESET;
    msg << err_msg;
    err.msg = msg.str();
    diag.add_part(err);
}