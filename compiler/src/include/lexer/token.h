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
            case TokenKind::ID:
                res << val;
                break;
            case TokenKind::SEMI:
                res << ";";
                break;
            case TokenKind::COMMA:
                res << ",";
                break;
            case TokenKind::DOT:
                res << ".";
                break;
            case TokenKind::OPEN_PAREN:
                res << "(";
                break;
            case TokenKind::CLOSE_PAREN:
                res << ")";
                break;
            case TokenKind::OPEN_BRACE:
                res << "{";
                break;
            case TokenKind::CLOSE_BRACE:
                res << "}";
                break;
            case TokenKind::OPEN_BRACKET:
                res << "[";
                break;
            case TokenKind::CLOSE_BRACKET:
                res << "]";
                break;
            case TokenKind::AT:
                res << "@";
                break;
            case TokenKind::TILDE:
                res << "~";
                break;
            case TokenKind::QUESTION:
                res << "?";
                break;
            case TokenKind::COLON:
                res << ":";
                break;
            case TokenKind::DOLLAR:
                res << "$";
                break;
            case TokenKind::EQ:
                res << "=";
                break;
            case TokenKind::BANG:
                res << "!";
                break;
            case TokenKind::LT:
                res << "<";
                break;
            case TokenKind::GT:
                res << ">";
                break;
            case TokenKind::AND:
                res << "&";
                break;
            case TokenKind::OR:
                res << "|";
                break;
            case TokenKind::PLUS:
                res << "+";
                break;
            case TokenKind::MINUS:
                res << "-";
                break;
            case TokenKind::STAR:
                res << "*";
                break;
            case TokenKind::SLASH:
                res << "/";
                break;
            case TokenKind::CARET:
                res << "^";
                break;
            case TokenKind::PRECENT:
                res << "%";
                break;
            case TokenKind::UNKNOWN:
                res << "unknown";
                break;
        }
        res << " (" << pos.to_str() << ')';
        return res.str();
    }
};