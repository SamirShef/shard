#pragma once
#include "token_kind.h"
#include <unordered_map>
#include <string>

static std::unordered_map<std::string, TokenKind> keywords {
    { "var", TokenKind::VAR },
    { "const", TokenKind::CONST },
    { "char", TokenKind::CHAR },
    { "bool", TokenKind::BOOL },
    { "i16", TokenKind::I16 },
    { "i32", TokenKind::I32 },
    { "i64", TokenKind::I64 },
    { "f32", TokenKind::F32 },
    { "f64", TokenKind::F64 },
    { "noth", TokenKind::NOTH },
    { "fun", TokenKind::FUN },
    { "return", TokenKind::RET },
    { "if", TokenKind::IF },
    { "else", TokenKind::ELSE },
    { "for", TokenKind::FOR },
    { "break", TokenKind::BREAK },
    { "continue", TokenKind::CONTINUE },
};