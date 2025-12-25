#pragma once
#include "common.h"
#include <string>

struct Position {
    std::string file_name;
    u64 line;
    u64 column;
    u64 len;

    std::string to_str() const {
        return std::to_string(line) + ":" + std::to_string(column);
    }
};