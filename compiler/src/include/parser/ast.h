#pragma once
#include "../lexer/token.h"
#include "../position.h"
#include <iostream>
#include <sstream>
#include <memory>
#include <vector>

enum class NodeType {
    VAR_DEF_STMT,
    FUN_DEF_STMT,
    FUN_CALL_STMT,
    RET_STMT,
    LITERAL_EXPR,
    BINARY_EXPR,
    UNARY_EXPR,
    VAR_EXPR,
    FUN_CALL_EXPR
};

struct Node {
    NodeType type;
    Position pos;

    explicit Node(NodeType type, Position pos) : type(type), pos(pos) {}
    virtual ~Node() = default;
    
    virtual std::unique_ptr<Node> clone() const = 0;

    template<typename TNode>
    TNode *as() {
        if (TNode::get_type() == type) {
            return static_cast<TNode*>(this);
        }
        return nullptr;
    }

    template<typename TNode>
    const TNode *as() const {
        if (TNode::get_type() == type) {
            return static_cast<const TNode*>(this);
        }
        return nullptr;
    }

    virtual const std::string to_str() const = 0;
};

enum class TypeKind {
    BOOL,
    CHAR,
    I16,
    I32,
    I64,
    F32,
    F64,
    NOTH
};

struct Type {
    TypeKind kind;
    bool is_const;

    explicit Type(TypeKind kind) : kind(kind), is_const(false) {}
    explicit Type(TypeKind kind, bool is_const) : kind(kind), is_const(is_const) {}

    const bool operator==(Type &other) {
        return kind == other.kind && is_const == other.is_const;
    }

    const bool operator==(const Type &other) const {
        return kind == other.kind;
    }

    const std::string to_str() const {
        std::string res;
        switch (kind) {
            case TypeKind::BOOL:
                res = "bool";
                break;
            case TypeKind::CHAR:
                res = "char";
                break;
            case TypeKind::I16:
                res = "i16";
                break;
            case TypeKind::I32:
                res = "i32";
                break;
            case TypeKind::I64:
                res = "i64";
                break;
            case TypeKind::F32:
                res = "f32";
                break;
            case TypeKind::F64:
                res = "f64";
                break;
            case TypeKind::NOTH:
                res = "noth";
                break;
        }
        return is_const ? "const " + res : res;
    }
};

struct Value {
    Type type;
    union Data {
        bool bool_val;
        char char_val;
        i16 i16_val;
        i32 i32_val;
        i64 i64_val;
        f32 f32_val;
        f64 f64_val;
    } data;

    explicit Value(Type type, Data data) : type(type), data(data) {}

    const std::string to_str() const {
        std::string res;
        res = type.to_str() + " ";
        #define ADD_VAL(val) res += std::to_string(data.val)
        switch (type.kind) {
            case TypeKind::BOOL:
                ADD_VAL(bool_val);
                break;
            case TypeKind::CHAR:
                ADD_VAL(char_val);
                break;
            case TypeKind::I16:
                ADD_VAL(i16_val);
                break;
            case TypeKind::I32:
                ADD_VAL(i32_val);
                break;
            case TypeKind::I64:
                ADD_VAL(i64_val);
                break;
            case TypeKind::F32:
                ADD_VAL(f32_val);
                break;
            case TypeKind::F64:
                ADD_VAL(f64_val);
                break;
        }
        return res;
    }
};

struct Argument {
    std::string name;
    Type type;

    const std::string to_str() const {
        return "Argument: " + type.to_str() + ' ' + name;
    }
};

using NodeUPTR = std::unique_ptr<Node>;
using NodeSPTR = std::shared_ptr<Node>;

#define NODE Node(get_type(), pos)

struct VarDefStmt : Node {
    const std::string name;
    Type type;
    NodeUPTR expr;

    explicit VarDefStmt(const std::string name, Type type, NodeUPTR expr, Position pos) : name(name), type(type), expr(std::move(expr)), NODE {}

    NodeUPTR clone() const override {
        NodeUPTR cloned_expr = expr->clone();
        return std::make_unique<VarDefStmt>(name, type, std::move(cloned_expr), pos);
    }
    
    static NodeType get_type() {
        return NodeType::VAR_DEF_STMT;
    }

    const std::string to_str() const override {
        std::ostringstream res;
        res << "VarDefStmt: " << type.to_str() << ' ' << name;
        if (expr) {
            res << " = " << expr->to_str();
        }
        return res.str();
    }
};

struct FunDefStmt : Node {
    std::string name;
    std::vector<Argument> args;
    Type ret_type;
    std::vector<NodeUPTR> block;

    explicit FunDefStmt(std::string name, std::vector<Argument> args, Type ret_type, std::vector<NodeUPTR> block, Position pos)
                      : name(name), args(args), ret_type(ret_type), block(std::move(block)), NODE {}
    
    NodeUPTR clone() const override {
        std::vector<NodeUPTR> cloned_block(block.size());
        for (int i = 0; i < block.size(); ++i) {
            cloned_block[i] = block[i]->clone();
        }
        return std::make_unique<FunDefStmt>(name, args, ret_type, std::move(cloned_block), pos);
    }

    static NodeType get_type() {
        return NodeType::FUN_DEF_STMT;
    }

    const std::string to_str() const override {
        std::ostringstream res;
        res << "FunDefStmt: " << ret_type.to_str() << ' ' << name << " (";
        for (int i = 0; i < args.size(); ++i) {
            res << args[i].to_str();
            if (i < args.size() - 1) {
                res << ", ";
            }
        }
        res << ") {";
        if (!block.empty()) {
            res << '\n';
        }
        for (const auto &stmt : block) {
            res << "  " << stmt->to_str() << '\n';
        }
        res << '}';
        return res.str();
    }
};

struct FunCallStmt : Node {
    std::string fun_name;
    std::vector<NodeUPTR> args;

    explicit FunCallStmt(std::string fun_name, std::vector<NodeUPTR> args, Position pos) : fun_name(fun_name), args(std::move(args)), NODE {}

    NodeUPTR clone() const override {
        std::vector<NodeUPTR> cloned_args(args.size());
        for (int i = 0; i < args.size(); ++i) {
            cloned_args[i] = args[i]->clone();
        }
        return std::make_unique<FunCallStmt>(fun_name, std::move(cloned_args), pos);
    }
    
    static NodeType get_type() {
        return NodeType::FUN_CALL_STMT;
    }

    const std::string to_str() const override {
        std::ostringstream res;
        res << "FunCallStmt: " << fun_name << " (";
        for (int i = 0; i < args.size(); ++i) {
            res << args[i]->to_str();
            if (i < args.size() - 1) {
                res << ", ";
            }
        }
        res << ')';
        return res.str();
    }
};

struct RetStmt : Node {
    NodeUPTR expr;

    explicit RetStmt(NodeUPTR expr, Position pos) : expr(std::move(expr)), NODE {}

    NodeUPTR clone() const override {
        return std::make_unique<RetStmt>(expr ? expr->clone() : nullptr, pos);
    }

    static NodeType get_type() {
        return NodeType::RET_STMT;
    }

    const std::string to_str() const override {
        if (expr) {
            return "RetStmt: " + expr->to_str();
        }
        return "RetStmt: noth";
    }
};

struct LiteralExpr : Node {
    Value val;

    explicit LiteralExpr(Value val, Position pos) : val(val), NODE {}
    
    NodeUPTR clone() const override {
        return std::make_unique<LiteralExpr>(val, pos);
    }
    
    static NodeType get_type() {
        return NodeType::LITERAL_EXPR;
    }

    const std::string to_str() const override {
        return "LiteralExpr: " + val.to_str();
    }
};

struct BinaryExpr : Node {
    Token op;
    NodeUPTR LHS;
    NodeUPTR RHS;

    explicit BinaryExpr(Token op, NodeUPTR LHS, NodeUPTR RHS, Position pos) : op(op), LHS(std::move(LHS)), RHS(std::move(RHS)), NODE {}

    NodeUPTR clone() const override {
        return std::make_unique<BinaryExpr>(op, LHS->clone(), RHS->clone(), pos);
    }
    
    static NodeType get_type() {
        return NodeType::BINARY_EXPR;
    }

    const std::string to_str() const override {
        std::ostringstream res;
        res << "BinaryExpr: " << LHS->to_str() << ' ';
        switch (op.kind) {
            case TokenKind::LOG_AND:
                res << "&&";
                break;
            case TokenKind::LOG_OR:
                res << "||";
                break;
            case TokenKind::AND:
                res << "&";
                break;
            case TokenKind::OR:
                res << "|";
                break;
            case TokenKind::GT:
                res << ">";
                break;
            case TokenKind::GT_EQ:
                res << ">=";
                break;
            case TokenKind::LT:
                res << "<";
                break;
            case TokenKind::LT_EQ:
                res << "<=";
                break;
            case TokenKind::EQ_EQ:
                res << "==";
                break;
            case TokenKind::BANG:
                res << "!";
                break;
            case TokenKind::BANG_EQ:
                res << "!=";
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
            case TokenKind::PERCENT:
                res << "%";
                break;
            default:
                res << "<UNKNOWN>";
                break;
        }
        res << ' ' << RHS->to_str();
        return res.str();
    }
};

struct UnaryExpr : Node {
    Token op;
    NodeUPTR RHS;

    explicit UnaryExpr(Token op, NodeUPTR RHS, Position pos) : op(op), RHS(std::move(RHS)), NODE {}
    
    NodeUPTR clone() const override {
        return std::make_unique<UnaryExpr>(op, RHS->clone(), pos);
    }

    static NodeType get_type() {
        return NodeType::UNARY_EXPR;
    }

    const std::string to_str() const override {
        std::ostringstream res;
        res << "UnaryExpr: ";
        switch (op.kind) {
            case TokenKind::BANG:
                res << "!";
                break;
            case TokenKind::MINUS:
                res << "-";
                break;
            default:
                res << "<UNKNOWN>";
                break;
        }
        res << RHS->to_str();
        return res.str();
    }
};

struct VarExpr : Node {
    std::string var_name;

    explicit VarExpr(std::string var_name, Position pos) : var_name(var_name), NODE {}

    NodeUPTR clone() const override {
        return std::make_unique<VarExpr>(var_name, pos);
    }

    static NodeType get_type() {
        return NodeType::VAR_EXPR;
    }

    const std::string to_str() const override {
        return "VarExpr: " + var_name;
    }
};

struct FunCallExpr : Node {
    std::string fun_name;
    std::vector<NodeUPTR> args;

    explicit FunCallExpr(std::string fun_name, std::vector<NodeUPTR> args, Position pos) : fun_name(fun_name), args(std::move(args)), NODE {}

    NodeUPTR clone() const override {
        std::vector<NodeUPTR> cloned_args(args.size());
        for (int i = 0; i < args.size(); ++i) {
            cloned_args[i] = args[i]->clone();
        }
        return std::make_unique<FunCallExpr>(fun_name, std::move(cloned_args), pos);
    }

    static NodeType get_type() {
        return NodeType::FUN_CALL_EXPR;
    }

    const std::string to_str() const override {
        std::ostringstream res;
        res << "FunCallExpr: " << fun_name << " (";
        for (int i = 0; i < args.size(); ++i) {
            res << args[i]->to_str();
            if (i < args.size() - 1) {
                res << ", ";
            }
        }
        res << ')';
        return res.str();
    }
};

#undef NODE