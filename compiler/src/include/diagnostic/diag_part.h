#pragma once
#include "../position.h"
#include "../common.h"
#include "level.h"
#include <iomanip>
#include <sstream>
#include <string>

struct DiagPart {
    Position pos;
    DiagLevel level;
    u16 code;
    std::string msg;

    std::string to_str() const {
        std::ostringstream res;
        switch (level) {
            case DiagLevel::ERROR:
                res << COLOR_RED << "error [E" << std::setfill('0') << std::setw(4) << code << "]";
                break;
            case DiagLevel::WARNING:
                res << COLOR_BLUE << "warning [E" << std::setfill('0') << std::setw(4) << code << "]";
                break;
            case DiagLevel::NOTE:
                res << COLOR_YELLOW << "note [E" << std::setfill('0') << std::setw(4) << code << "]";
                break;
        }
        res << " at: " << COLOR_RESET << pos.file_name << ':' << pos.to_str() << '\n' << msg;
        return res.str();
    }
};