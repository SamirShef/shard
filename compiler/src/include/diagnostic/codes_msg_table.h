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
    "Expected identifier, but got keyword or operator.",
    "Expected semicolon.",
    "Unexpected symbol.",
    "Expected type.",
    "Expected expression.",
    "Type mismatch.",
    "Unsupported expression.",
    "Type mismatch. Implicit casting is not possible.",
    "Unsupported statement.",
    "Variable does not declared.",
    "Invalid variable type.",
    "Function does not declared.",
    "The number of arguments in the function does not match.",
    "Not all paths return a value.",
    "Division by zero.",
    "Cannot be used in the global scope.",
    "A variable defined using the `var` keyword cannot have a constant type.",
    "Assigning a variable requires an expression.",
    "The value of a constant cannot be changed.",
    "`break` must be inside the loop.",
    "`continue` must be inside the loop.",
    "Unsupported member of the structure.",
    "Structure does not declared.",
    "The object is not a structure.",
    "Field does not declared.",
    "Access to private member.",
};

static std::vector<std::string> war_msgs {
    "The constant has the type with the modifier `const`."
};

static std::vector<std::string> notes_msgs {
    
};