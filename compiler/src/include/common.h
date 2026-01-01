#pragma once

using i8 = signed char;
using i16 = signed short;
using i32 = signed int;
using i64 = signed long;

using u8 = unsigned char;
using u16 = unsigned short;
using u32 = unsigned int;
using u64 = unsigned long;

using f32 = float;
using f64 = double;

#define RED "\033[31m"
#define GREEN "\033[32m"
#define YELLOW "\033[33m"
#define BLUE "\033[34m"
#define MAGENTA "\033[35m"
#define CYAN "\033[36m"
#define WHITE "\033[37m"
#define RESET "\033[0m"

#include <algorithm>
#include <locale>

inline std::string ltrim(std::string s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](u8 ch) {
        return !std::isspace(ch);
    }));
    return s;
}

inline std::string rtrim(std::string s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](u8 ch) {
        return !std::isspace(ch);
    }).base(), s.end());
    return s;
}