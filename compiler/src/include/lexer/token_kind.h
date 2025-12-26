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
    BANG,           // `!`
    LT,             // `<`
    GT,             // `>`
    AND,            // '&`
    OR,             // `|`
    PLUS,           // `+`
    MINUS,          // `-`
    STAR,           // `*`
    SLASH,          // `/`
    CARET,          // `^`
    PRECENT,        // `%`

    UNKNOWN,        // Unknown token, not expected by the lexer
};