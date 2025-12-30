#pragma once
#include "../common.h"
#include <vector>

static std::vector<std::string> err_msgs {
    "A numeric literal contains several dots.",
    "A numeric literal does not have digits after the decimal point.",
    "Unsupported suffix of a numeric literal.",
    "Numeric literal cause overflow.",
    "Missing closing single quote `'` in character literal.",
    "The character constant must have a length of 1.",
    "A floating-point literal has an integer suffix.",
    "The value of the escape sequence does not fit into the range `char`.",
    "Empty escape sequence.",
    "Unsupported escape sequence.",
    "Missing closing double quote `\"` in string literal.",
    "Unsupported symbol.",
    "Expected identifier.",
    "Expected identifier, but got keyword.",
    "Expected semicolon.",
    "Unexpected symbol.",
    "Expected type.",
    "Expected expression."
};