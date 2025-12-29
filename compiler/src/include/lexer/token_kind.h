#pragma once

enum class TokenKind {
    ID,             // identifier or keyword

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
    PRECENT,        // `%`
    PLUS_EQ,        // `+=`
    MINUS_EQ,       // `-=`
    STAR_EQ,        // `*=`
    SLASH_EQ,       // `/=`
    PRECENT_EQ,     // `%=`
    CARET,          // `^`

    UNKNOWN,        // Unknown token, not expected by the lexer
};