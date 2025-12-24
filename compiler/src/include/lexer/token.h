#pragma once
#include "token_kind.h"
#include "../position.h"

struct Token {
    TokenKind kind;
    std::string val;
    Position pos;
};