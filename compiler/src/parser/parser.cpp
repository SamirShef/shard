#include "../include/parser/parser.h"
#include <iostream>

static u64 start_line_pos = 0;

std::vector<NodeUPTR> Parser::parse() {
    std::vector<NodeUPTR> stmts;

    while (pos < tokens.size()) {
        stmts.push_back(parse_stmt());
    }

    return stmts;
}

NodeUPTR Parser::parse_stmt() {
    if (match(TokenKind::VAR) || match(TokenKind::CONST)) {
        return parse_var_def_stmt();
    }
    else if (match(TokenKind::ID)) {
        if (match(TokenKind::OPEN_PAREN)) {
            return parse_fun_call_stmt();
        }
        return parse_var_asgn_stmt();
    }
    else if (match(TokenKind::FUN)) {
        return parse_fun_def_stmt();
    }
    else if (match(TokenKind::RET)) {
        return parse_return_stmt();
    }
    else {
        DiagPart err { .pos = { .file_name = peek().pos.file_name, .line = peek().pos.line, .column = peek().pos.column, .pos = peek().pos.pos },
                       .level = DiagLevel::ERROR, .code = 15 };
        diag_part_create(diag, err, "unexpected symbol");
        advance();
    }
    return nullptr;
}

NodeUPTR Parser::parse_var_def_stmt() {
    const Token first_token = peek(-1);
    bool is_const = first_token.kind == TokenKind::CONST;
    const Token name_token = consume(TokenKind::ID, 12);
    if (name_token.kind != TokenKind::ID) {
        DiagPart err { .pos = { .file_name = peek(-1).pos.file_name, .line = peek(-1).pos.line, .column = peek(-1).pos.column,
                       .pos = peek(-1).pos.pos }, .level = DiagLevel::ERROR, .code = 13 };
        diag_part_create(diag, err, "keyword or operator");
    }
    consume(TokenKind::COLON, 15, "expected `:`");
    const Token type_start = peek();
    Type type = consume_type();
    if (!type.is_const) {
        type.is_const = is_const;
    }
    else {
        if (!is_const) {
            DiagPart err { .pos = { .file_name = type_start.pos.file_name, .line = type_start.pos.line, .column = type_start.pos.column,
                           .pos = type_start.pos.pos }, .level = DiagLevel::ERROR, .code = 29 };
            diag_part_create(diag, err, "");
        }
        else {
            DiagPart war { .pos = { .file_name = type_start.pos.file_name, .line = type_start.pos.line, .column = type_start.pos.column,
                           .pos = type_start.pos.pos }, .level = DiagLevel::WARNING, .code = 0 };
            diag_part_create(diag, war, "");
        }
    }

    NodeUPTR expr = nullptr;
    if (match(TokenKind::EQ)) {
        expr = parse_expr();
    }
    Position stmt_pos = first_token.pos;
    stmt_pos.len = consume_semi().pos.pos - stmt_pos.pos;
    return std::make_unique<VarDefStmt>(name_token.val, type, std::move(expr), stmt_pos);
}

NodeUPTR Parser::parse_var_asgn_stmt() {
    const Token name_token = peek(-1);
    if (match(TokenKind::PLUS_EQ) || match(TokenKind::MINUS_EQ) || match(TokenKind::STAR_EQ) || match(TokenKind::SLASH_EQ) ||
        match(TokenKind::PERCENT_EQ)) {
        Token op = peek(-1);
        NodeUPTR expr = parse_expr();
        consume_semi();
        return std::make_unique<VarAsgnStmt>(name_token.val, create_compound_op(name_token.val, op, std::move(expr)), name_token.pos);
    }
    else {
        consume(TokenKind::EQ, 15, "expected `=`");
        NodeUPTR expr = parse_expr();
        consume_semi();
        return std::make_unique<VarAsgnStmt>(name_token.val, std::move(expr), name_token.pos);
    }
    diag_part_create(diag, 15, name_token.pos, DiagLevel::ERROR, "");
    return nullptr;
}

NodeUPTR Parser::parse_fun_def_stmt() {
    const Token name_token = consume(TokenKind::ID, 12);
    if (name_token.kind != TokenKind::ID) {
        DiagPart err { .pos = { .file_name = peek(-1).pos.file_name, .line = peek(-1).pos.line, .column = peek(-1).pos.column,
                       .pos = peek(-1).pos.pos }, .level = DiagLevel::ERROR, .code = 13 };
        diag_part_create(diag, err, "keyword or operator");
    }
    consume(TokenKind::OPEN_PAREN, 15, "expected `(`");
    std::vector<Argument> args;
    while (!match(TokenKind::CLOSE_PAREN)) {
        args.push_back(parse_arg());
        if (peek().kind != TokenKind::CLOSE_PAREN) {
            consume(TokenKind::COMMA, 15, "expected `,`");
        }
    }

    Type ret_type = Type(TypeKind::NOTH);
    if (match(TokenKind::COLON)) {
        ret_type = consume_type();
    }
    
    consume(TokenKind::OPEN_BRACE, 15, "expected `{`");
    std::vector<NodeUPTR> block;
    while (!match(TokenKind::CLOSE_BRACE)) {
        block.push_back(parse_stmt());
    }
    return std::make_unique<FunDefStmt>(name_token.val, args, ret_type, std::move(block), name_token.pos);
}

NodeUPTR Parser::parse_fun_call_stmt() {
    const Token name_token = peek(-2);
    std::vector<NodeUPTR> args;
    while (!match(TokenKind::CLOSE_PAREN)) {
        args.push_back(parse_expr());
        if (peek().kind != TokenKind::CLOSE_PAREN) {
            consume(TokenKind::COMMA, 15, "expected `,`");
        }
    }
    consume_semi();
    return std::make_unique<FunCallStmt>(name_token.val, std::move(args), name_token.pos);
}

NodeUPTR Parser::parse_return_stmt() {
    Position pos = peek(-1).pos;
    NodeUPTR expr = nullptr;
    if (!match(TokenKind::SEMI)) {
        expr = parse_expr();
        consume_semi();
    }
    return std::make_unique<RetStmt>(std::move(expr), pos);
}

NodeUPTR Parser::parse_expr() {
    return parse_log_and_expr();
}

NodeUPTR Parser::parse_log_and_expr() {
    NodeUPTR LHS = parse_log_or_expr();
    while (match(TokenKind::LOG_AND)) {
        Token op = peek(-1);
        NodeUPTR RHS = parse_log_or_expr();
        Position pos = LHS->pos;
        pos.len = RHS->pos.pos + RHS->pos.len - LHS->pos.pos;
        return std::make_unique<BinaryExpr>(op, std::move(LHS), std::move(RHS), pos);
    }
    return LHS;
}

NodeUPTR Parser::parse_log_or_expr() {
    NodeUPTR LHS = parse_equality_expr();
    while (match(TokenKind::LOG_AND)) {
        Token op = peek(-1);
        NodeUPTR RHS = parse_equality_expr();
        Position pos = LHS->pos;
        pos.len = RHS->pos.pos + RHS->pos.len - LHS->pos.pos;
        return std::make_unique<BinaryExpr>(op, std::move(LHS), std::move(RHS), pos);
    }
    return LHS;
}

NodeUPTR Parser::parse_equality_expr() {
    NodeUPTR LHS = parse_comparation_expr();
    while (match(TokenKind::EQ_EQ) || match(TokenKind::BANG_EQ)) {
        const Token op = peek(-1);
        NodeUPTR RHS = parse_comparation_expr();
        Position pos = LHS->pos;
        pos.len = RHS->pos.pos + RHS->pos.len - LHS->pos.pos;
        return std::make_unique<BinaryExpr>(op, std::move(LHS), std::move(RHS), pos);
    }
    return LHS;
}

NodeUPTR Parser::parse_comparation_expr() {
    NodeUPTR LHS = parse_additive_expr();
    while (match(TokenKind::GT) || match(TokenKind::GT_EQ) || match(TokenKind::LT) || match(TokenKind::LT_EQ)) {
        const Token op = peek(-1);
        NodeUPTR RHS = parse_additive_expr();
        Position pos = LHS->pos;
        pos.len = RHS->pos.pos + RHS->pos.len - LHS->pos.pos;
        return std::make_unique<BinaryExpr>(op, std::move(LHS), std::move(RHS), pos);
    }
    return LHS;
}

NodeUPTR Parser::parse_additive_expr() {
    NodeUPTR LHS = parse_multiplicative_expr();
    while (match(TokenKind::PLUS) || match(TokenKind::MINUS)) {
        const Token op = peek(-1);
        NodeUPTR RHS = parse_multiplicative_expr();
        Position pos = LHS->pos;
        pos.len = RHS->pos.pos + RHS->pos.len - LHS->pos.pos;
        return std::make_unique<BinaryExpr>(op, std::move(LHS), std::move(RHS), pos);
    }
    return LHS;
}

NodeUPTR Parser::parse_multiplicative_expr() {
    NodeUPTR LHS = parse_unary_expr();
    while (match(TokenKind::STAR) || match(TokenKind::SLASH) || match(TokenKind::PERCENT)) {
        const Token op = peek(-1);
        NodeUPTR RHS = parse_unary_expr();
        Position pos = LHS->pos;
        pos.len = RHS->pos.pos + RHS->pos.len - LHS->pos.pos;
        return std::make_unique<BinaryExpr>(op, std::move(LHS), std::move(RHS), pos);
    }
    return LHS;
}

NodeUPTR Parser::parse_unary_expr() {
    while (match(TokenKind::BANG) || match(TokenKind::MINUS)) {
        const Token op = peek(-1);
        NodeUPTR RHS = parse_primary_expr();
        Position pos = RHS->pos;
        pos.len = RHS->pos.pos + RHS->pos.len - op.pos.pos;
        return std::make_unique<UnaryExpr>(op, std::move(RHS), pos);
    }
    return parse_primary_expr();
}

NodeUPTR Parser::parse_primary_expr() {
    Token tok = peek();
    switch (tok.kind) {
        case TokenKind::ID:
            advance();
            if (match(TokenKind::OPEN_PAREN)) {
                std::vector<NodeUPTR> args;
                while (!match(TokenKind::CLOSE_PAREN)) {
                    args.push_back(parse_expr());
                    if (peek().kind != TokenKind::CLOSE_PAREN) {
                        consume(TokenKind::COMMA, 15, "expected `,`");
                    }
                }
                return std::make_unique<FunCallExpr>(tok.val, std::move(args), tok.pos);
            }
            return std::make_unique<VarExpr>(tok.val, tok.pos);
        #define LIT(kind, field, val) std::make_unique<LiteralExpr>(Value(Type(TypeKind::kind, true), {.field = val}), tok.pos)
        case TokenKind::BLIT:
            advance();
            return LIT(BOOL, bool_val, tok.val == "true");
        case TokenKind::CLIT:
            advance();
            return LIT(CHAR, char_val, static_cast<char>(tok.val[0]));
        case TokenKind::SLIT:
            advance();
            return LIT(I16, i16_val, static_cast<i16>(std::stoi(tok.val)));
        case TokenKind::ILIT:
            advance();
            return LIT(I32, i32_val, std::stoi(tok.val));
        case TokenKind::LLIT:
            advance();
            return LIT(I64, i64_val, std::stol(tok.val));
        case TokenKind::FLIT:
            advance();
            return LIT(F32, f32_val, std::stof(tok.val));
        case TokenKind::DLIT:
            advance();
            return LIT(F64, f64_val, std::stod(tok.val));
        default: {
            DiagPart err { .pos = { .file_name = tok.pos.file_name, .line = tok.pos.line, .column = tok.pos.column, .pos = tok.pos.pos },
                           .level = DiagLevel::ERROR, .code = 17 };
            diag_part_create(diag, err, "unexpected symbol");
            return nullptr;
        }
        #undef LIT
    }
}

Argument Parser::parse_arg() {
    const Token name_token = consume(TokenKind::ID, 12);
    if (name_token.kind != TokenKind::ID) {
        DiagPart err { .pos = { .file_name = peek(-1).pos.file_name, .line = peek(-1).pos.line, .column = peek(-1).pos.column,
                       .pos = peek(-1).pos.pos }, .level = DiagLevel::ERROR, .code = 13 };
        diag_part_create(diag, err, "keyword or operator");
    }
    consume(TokenKind::COLON, 15, "expected `:`");
    Type type = consume_type();
    return Argument { .name = name_token.val, .type = type };
}

const Token Parser::peek(u64 rpos) const {
    if (pos + rpos >= tokens.size()) {
        std::cerr << COLOR_RED << "The index passed to the parser is out of bounds\n" << COLOR_RESET;
        exit(1);
    }
    return tokens[pos + rpos];
}

const Token Parser::advance() {
    Token tok = peek();
    pos++;
    if (pos < tokens.size() && tok.pos.line < peek().pos.line) {
        start_line_pos = peek().pos.pos;
    }
    return tok;
}

const bool Parser::is_type(const TokenKind kind) const {
    return (int)kind >= (int)TokenKind::BOOL && (int)kind <= (int)TokenKind::F64;
}

const bool Parser::match(TokenKind kind) {
    if (pos < tokens.size() && peek().kind == kind) {
        advance();
        return true;
    }
    return false;
}

const Token Parser::consume(TokenKind kind, u16 err_code) {
    return consume(kind, err_code, "unexpected symbol");
}

const Token Parser::consume(TokenKind kind, u16 err_code, std::string err_msg) {
    if (pos < tokens.size() && peek().kind == kind) {
        return advance();
    }
    if (pos < tokens.size()) {
        DiagPart err { .pos = { .file_name = peek().pos.file_name, .line = peek().pos.line, .column = peek().pos.column, .pos = peek().pos.pos },
                       .level = DiagLevel::ERROR, .code = err_code };
        diag_part_create(diag, err, err_msg);
    }
    else {
        DiagPart err { .pos = { .file_name = peek(-1).pos.file_name, .line = peek(-1).pos.line, .column = peek(-1).pos.column,
                       .pos = peek(-1).pos.pos }, .level = DiagLevel::ERROR, .code = err_code};
        diag_part_create(diag, err, err_msg);
    }
    return Token::null_token(pos < tokens.size() ? advance().pos : peek(-1).pos);
}

const Token Parser::consume_semi() {
    return consume(TokenKind::SEMI, 14);
}

const Type Parser::consume_type() {
    bool is_const = match(TokenKind::CONST);
    const Token type = advance();
    if (is_type(type.kind)) {
        switch (type.kind) {
            case TokenKind::BOOL:
                return Type(TypeKind::BOOL, is_const);
            case TokenKind::CHAR:
                return Type(TypeKind::CHAR, is_const);
            case TokenKind::I16:
                return Type(TypeKind::I16, is_const);
            case TokenKind::I32:
                return Type(TypeKind::I32, is_const);
            case TokenKind::I64:
                return Type(TypeKind::I64, is_const);
            case TokenKind::F32:
                return Type(TypeKind::F32, is_const);
            case TokenKind::F64:
                return Type(TypeKind::F64, is_const);
            case TokenKind::NOTH:
                return Type(TypeKind::NOTH, is_const);
        }
    }
    DiagPart err { .pos = { .file_name = type.pos.file_name, .line = type.pos.line, .column = type.pos.column, .pos = type.pos.pos },
                   .level = DiagLevel::ERROR, .code = 16 };
    diag_part_create(diag, err, "invalid type");
    return Type(TypeKind::I32);
}

NodeUPTR Parser::create_compound_op(std::string var_name, Token op, NodeUPTR expr) {
    Position pos = expr->pos;
    switch (op.kind) {
        #define EXPR(type, val) std::make_unique<BinaryExpr>(Token(TokenKind::type, val, op.pos), std::make_unique<VarExpr>(var_name, op.pos), std::move(expr), pos)
        case TokenKind::PLUS_EQ:
            return EXPR(PLUS, "+");
        case TokenKind::MINUS_EQ:
            return EXPR(MINUS, "-");
        case TokenKind::STAR_EQ:
            return EXPR(STAR, "*");
        case TokenKind::SLASH_EQ:
            return EXPR(SLASH, "/");
        case TokenKind::PERCENT_EQ:
            return EXPR(PERCENT, "%");
        default: {}
        #undef EXPR
    }
}

u64 Parser::get_end_line_pos(Token start_token) const {
    u64 i;
    for (i = pos; i < tokens.size() - 1 && tokens[i].pos.line == start_token.pos.line; ++i);
    if (i == tokens.size()) {
        return tokens[i - 1].pos.pos + tokens[i - 1].pos.len;
    }
    return tokens[i].pos.pos + tokens[i].pos.len;
}