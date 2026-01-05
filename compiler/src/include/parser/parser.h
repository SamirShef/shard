#pragma once
#include "../diagnostic/diagnostic.h"
#include "../lexer/token.h"
#include "ast.h"
#include <vector>

class Parser {
    Diagnostic &diag;
    std::string &src;
    std::vector<Token> tokens;
    u64 pos;

public:
    explicit Parser(Diagnostic &diag, std::string &src, std::vector<Token> tokens) : diag(diag), src(src), tokens(tokens), pos(0) {}

    std::vector<NodeUPTR> parse();

private:
    // statements
    NodeUPTR parse_stmt(bool skip_semi = true);
    NodeUPTR parse_var_def_stmt(AccessModifier access);
    NodeUPTR parse_var_asgn_stmt(AccessModifier access);
    NodeUPTR parse_fun_def_stmt(AccessModifier access);
    NodeUPTR parse_fun_call_stmt(AccessModifier access);
    NodeUPTR parse_return_stmt(AccessModifier access);
    NodeUPTR parse_if_else_stmt(AccessModifier access);
    NodeUPTR parse_for_stmt(AccessModifier access);
    NodeUPTR parse_struct_stmt(AccessModifier access);

    // expressions
    NodeUPTR parse_expr();
    NodeUPTR parse_log_and_expr();
    NodeUPTR parse_log_or_expr();
    NodeUPTR parse_equality_expr();
    NodeUPTR parse_comparation_expr();
    NodeUPTR parse_additive_expr();
    NodeUPTR parse_multiplicative_expr();
    NodeUPTR parse_unary_expr();
    NodeUPTR parse_primary_expr();

    Argument parse_arg();

    const Token peek(u64 rpos = 0) const;
    const Token advance();
    const bool is_type(const TokenKind kind) const;
    const bool match(TokenKind kind);
    const Token consume(TokenKind kind, u16 err_code);
    const Token consume(TokenKind kind, u16 err_code, std::string err_msg);
    const Token consume_semi();
    const Type consume_type();
    NodeUPTR create_compound_op(std::string var_name, Token op, NodeUPTR expr);
    u64 get_end_line_pos(Token start_token) const;
};