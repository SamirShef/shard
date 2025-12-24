#pragma once
#include <string>
#include "common.h"

struct Position {
    std::string file_name;
    u64 line;
    u64 column;
    u64 len;
};