#include "../include/parser/parser.h"
#include <iostream>

static u64 start_line_pos = 0;

std::vector<NodeUPTR> Parser::parse() {
    std::vector<NodeUPTR> stmts;

    while (pos < tokens.size()) {
        if (is_keyword_by_name(peek(), "var") ||
            is_keyword_by_name(peek(), "const")) {
            stmts.push_back(parse_var_def_stmt());
        }
        else {
            DiagPart err{.start_line_pos = start_line_pos, .line_len = get_end_line_pos(peek()) - start_line_pos,
                         .pos = {.file_name = peek().pos.file_name, .line = peek().pos.line, .column = peek().pos.column,
                         .pos = peek().pos.pos}, .level = DiagLevel::ERROR, .code = 15};
            diag_part_create(diag, err, src, peek().pos.pos - start_line_pos, peek().val.length(), "unexpected symbol");
            advance();
        }
    }

    return stmts;
}

NodeUPTR Parser::parse_var_def_stmt() {
    const Token first_token = advance();
    bool is_const = first_token.val == "const";
    Type type = consume_type();
    type.is_const = is_const;
    const std::string name = consume(TokenKind::ID, 12).val;
    if (is_keyword(peek(-1))) {
        DiagPart err{.start_line_pos = start_line_pos, .line_len = get_end_line_pos(peek(-1)) - start_line_pos,
                     .pos = {.file_name = peek(-1).pos.file_name, .line = peek(-1).pos.line, .column = peek(-1).pos.column,
                     .pos = peek(-1).pos.pos}, .level = DiagLevel::ERROR, .code = 13};
        diag_part_create(diag, err, src, peek(-1).pos.pos - start_line_pos, name.length(), "keyword");
    }
    NodeUPTR expr = nullptr;
    if (match(TokenKind::EQ)) {
        expr = parse_expr();
    }
    Position stmt_pos = first_token.pos;
    stmt_pos.len = consume_semi().pos.pos - stmt_pos.pos;
    return std::make_unique<VarDefStmt>(name, type, std::move(expr), stmt_pos);
}

NodeUPTR Parser::parse_expr() {
    return parse_log_and_expr();
}

NodeUPTR Parser::parse_log_and_expr() {
    NodeUPTR LHS = parse_log_or_expr();
    while (match(TokenKind::LOG_AND)) {
        NodeUPTR RHS = parse_log_or_expr();
        Position pos = LHS->pos;
        pos.len = RHS->pos.pos + RHS->pos.len - LHS->pos.pos;
        return std::make_unique<BinaryExpr>(TokenKind::LOG_AND, std::move(LHS), std::move(RHS), pos);
    }
    return LHS;
}

NodeUPTR Parser::parse_log_or_expr() {
    NodeUPTR LHS = parse_equality_expr();
    while (match(TokenKind::LOG_AND)) {
        NodeUPTR RHS = parse_equality_expr();
        Position pos = LHS->pos;
        pos.len = RHS->pos.pos + RHS->pos.len - LHS->pos.pos;
        return std::make_unique<BinaryExpr>(TokenKind::LOG_OR, std::move(LHS), std::move(RHS), pos);
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
        return std::make_unique<BinaryExpr>(op.kind, std::move(LHS), std::move(RHS), pos);
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
        return std::make_unique<BinaryExpr>(op.kind, std::move(LHS), std::move(RHS), pos);
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
        return std::make_unique<BinaryExpr>(op.kind, std::move(LHS), std::move(RHS), pos);
    }
    return LHS;
}

NodeUPTR Parser::parse_multiplicative_expr() {
    NodeUPTR LHS = parse_unary_expr();
    while (match(TokenKind::STAR) || match(TokenKind::SLASH) || match(TokenKind::PRECENT)) {
        const Token op = peek(-1);
        NodeUPTR RHS = parse_unary_expr();
        Position pos = LHS->pos;
        pos.len = RHS->pos.pos + RHS->pos.len - LHS->pos.pos;
        return std::make_unique<BinaryExpr>(op.kind, std::move(LHS), std::move(RHS), pos);
    }
    return LHS;
}

NodeUPTR Parser::parse_unary_expr() {
    while (match(TokenKind::BANG) || match(TokenKind::MINUS)) {
        const Token op = peek(-1);
        NodeUPTR RHS = parse_primary_expr();
        Position pos = RHS->pos;
        pos.len = RHS->pos.pos + RHS->pos.len - op.pos.pos;
        return std::make_unique<UnaryExpr>(op.kind, std::move(RHS), pos);
    }
    return parse_primary_expr();
}

NodeUPTR Parser::parse_primary_expr() {
    Token tok = peek();
    switch (tok.kind) {
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
            DiagPart err{.start_line_pos = start_line_pos, .line_len = get_end_line_pos(tok) - start_line_pos,
                         .pos = {.file_name = tok.pos.file_name, .line = tok.pos.line, .column = tok.pos.column,
                         .pos = tok.pos.pos}, .level = DiagLevel::ERROR, .code = 17};
            diag_part_create(diag, err, src, tok.pos.pos - start_line_pos, tok.val.length(), "unexpected symbol");
            return nullptr;
        }
        #undef LIT
    }
}

const Token Parser::peek(u64 rpos) const {
    if (pos + rpos >= tokens.size()) {
        std::cerr << RED << "The index passed to the parser is out of bounds\n" << RESET;
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

const bool Parser::is_keyword_by_name(const Token tok, const std::string val) const {
    return tok.kind == TokenKind::ID && tok.val == val;
}

const bool Parser::is_keyword(const Token tok) const {
    return std::find(keywords.begin(), keywords.end(), tok.val) != keywords.end();
}

const bool Parser::is_type(const std::string val) const {
    if (val == "bool" || val == "char" || val == "i16" || val == "i32" || val == "i64" || val == "f32" || val == "f64") {
        return true;
    }
    return false;
}

const bool Parser::match(TokenKind kind) {
    if (pos < tokens.size() && peek().kind == kind) {
        advance();
        return true;
    }
    return false;
}

const Token Parser::consume(TokenKind kind, u16 err_code) {
    if (pos < tokens.size() && peek().kind == kind) {
        return advance();
    }
    if (pos < tokens.size()) {
        DiagPart err{.start_line_pos = start_line_pos, .line_len = get_end_line_pos(peek()) - start_line_pos,
                     .pos = {.file_name = peek().pos.file_name, .line = peek().pos.line, .column = peek().pos.column,
                     .pos = peek().pos.pos}, .level = DiagLevel::ERROR, .code = err_code};
        diag_part_create(diag, err, src, peek().pos.pos - start_line_pos, peek().val.length(), "unexpected symbol");
    }
    else {
        DiagPart err{.start_line_pos = start_line_pos, .line_len = get_end_line_pos(peek(-1)) - start_line_pos,
                     .pos = {.file_name = peek(-1).pos.file_name, .line = peek(-1).pos.line, .column = peek(-1).pos.column,
                     .pos = peek(-1).pos.pos}, .level = DiagLevel::ERROR, .code = err_code};
        diag_part_create(diag, err, src, peek(-1).pos.pos - start_line_pos + peek(-1).pos.len, peek(-1).val.length(), "unexpected symbol");
    }
    return Token::null_token(pos < tokens.size() ? advance().pos : peek(-1).pos);
}

const Token Parser::consume_semi() {
    return consume(TokenKind::SEMI, 14);
}

const Type Parser::consume_type() {
    const Token type = consume(TokenKind::ID, 16);
    if (is_type(type.val)) {
        if (type.val == "bool") {
            return Type(TypeKind::BOOL);
        }
        else if (type.val == "char") {
            return Type(TypeKind::CHAR);
        }
        else if (type.val == "i16") {
            return Type(TypeKind::I16);
        }
        else if (type.val == "i32") {
            return Type(TypeKind::I32);
        }
        else if (type.val == "i64") {
            return Type(TypeKind::I64);
        }
        else if (type.val == "f32") {
            return Type(TypeKind::F32);
        }
        else if (type.val == "f64") {
            return Type(TypeKind::F64);
        }
    }
    DiagPart err{.start_line_pos = start_line_pos, .line_len = get_end_line_pos(type) - start_line_pos,
                 .pos = {.file_name = type.pos.file_name, .line = type.pos.line, .column = type.pos.column,
                 .pos = type.pos.pos}, .level = DiagLevel::ERROR, .code = 16};
    diag_part_create(diag, err, src, type.pos.pos - start_line_pos, type.val.length(), "invalid type");
    if (type.val == "") {
        advance();
    }
    return Type(TypeKind::I32);
}

u64 Parser::get_end_line_pos(Token start_token) const {
    u64 i;
    for (i = pos; i < tokens.size() - 1 && tokens[i].pos.line == start_token.pos.line; i++);
    if (i == tokens.size()) {
        return tokens[i - 1].pos.pos + tokens[i - 1].pos.len;
    }
    return tokens[i].pos.pos + tokens[i].pos.len;
}