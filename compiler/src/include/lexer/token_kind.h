#pragma once

enum class TokenKind {
    ID,             // identifier

    VAR,            // keyword `var`
    CONST,          // keyword `const`
    BOOL,           // keyword `bool`
    CHAR,           // keyword `char`
    I16,            // keyword `i16`
    I32,            // keyword `i32`
    I64,            // keyword `i64`
    F32,            // keyword `f32`
    F64,            // keyword `f64`
    FUN,            // keyword `fun`

    BLIT,           // bool literal
    CLIT,           // character literal
    SLIT,           // short literal
    ILIT,           // int literal
    LLIT,           // long literal
    FLIT,           // float literal
    DLIT,           // double literal
    STRLIT,         // string literal
    
    SEMI,           // `;`
    COMMA,          // `,`
    DOT,            // `.`
    OPEN_PAREN,     // `(`
    CLOSE_PAREN,    // `)`
    OPEN_BRACE,     // `}`
    CLOSE_BRACE,    // `{`
    OPEN_BRACKET,   // `[`
    CLOSE_BRACKET,  // `]`
    AT,             // `@`
    TILDE,          // `~`
    QUESTION,       // `?`
    COLON,          // `:`
    DOLLAR,         // `$`
    EQ,             // `=`
    EQ_EQ,          // `==`
    BANG,           // `!`
    BANG_EQ,        // `!=`
    LT,             // `<`
    GT,             // `>`
    LT_EQ,          // `<=`
    GT_EQ,          // `>=`
    AND,            // '&`
    OR,             // `|`
    LOG_AND,        // '&&`
    LOG_OR,         // `||`
    PLUS,           // `+`
    MINUS,          // `-`
    STAR,           // `*`
    SLASH,          // `/`
    PERCENT,        // `%`
    PLUS_EQ,        // `+=`
    MINUS_EQ,       // `-=`
    STAR_EQ,        // `*=`
    SLASH_EQ,       // `/=`
    PRECENT_EQ,     // `%=`
    CARET,          // `^`

    UNKNOWN,        // Unknown token, not expected by the lexer
};