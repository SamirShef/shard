#pragma once
#include "../position.h"
#include "../common.h"
#include "level.h"
#include <iomanip>
#include <sstream>
#include <string>

struct DiagPart {
    u64 start_line_pos;
    u64 line_len;
    Position pos;
    DiagLevel level;
    u16 code;
    std::string msg;

    std::string to_str() const {
        std::ostringstream res;
        switch (level) {
            case DiagLevel::ERROR:
                res << RED << "error [E" << std::setfill('0') << std::setw(4) << code << "]";
                break;
            case DiagLevel::WARNING:
                res << BLUE << "warning [E" << std::setfill('0') << std::setw(4) << code << "]";
                break;
            case DiagLevel::NOTE:
                res << YELLOW << "note [E" << std::setfill('0') << std::setw(4) << code << "]";
                break;
        }
        res << " at: " << RESET << pos.file_name << ':' << pos.to_str() << '\n' << msg;
        return res.str();
    }
};