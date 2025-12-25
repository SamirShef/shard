#pragma once

enum class TokenKind {
    ID,             // identifier or keyword
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