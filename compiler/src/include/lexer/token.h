#pragma once
#include "../position.h"
#include "token_kind.h"
#include <sstream>

struct Token {
    TokenKind kind;
    std::string val;
    Position pos;

    std::string to_str() const {
        std::ostringstream res;
        switch (kind) {
            case TokenKind::BLIT:
                res << "(bool) " << val;
                break;
            case TokenKind::CLIT:
                res << "(char) " << val;
                break;
            case TokenKind::SLIT:
                res << "(i16) " << val;
                break;
            case TokenKind::ILIT:
                res << "(i32) " << val;
                break;
            case TokenKind::LLIT:
                res << "(i64) " << val;
                break;
            case TokenKind::FLIT:
                res << "(f32) " << val;
                break;
            case TokenKind::DLIT:
                res << "(f64) " << val;
                break;
            case TokenKind::STRLIT:
                res << "(str) " << val;
                break;
            case TokenKind::UNKNOWN:
                res << "unknown";
                break;
            default:
                res << val;
                break;
        }
        res << " (" << pos.to_str() << ')';
        return res.str();
    }
};