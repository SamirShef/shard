#pragma once
#include "../lexer/token_kind.h"
#include "../position.h"
#include <sstream>
#include <memory>

enum class NodeType {
    VarDefStmt,
    LiteralExpr,
    BinaryExpr,
    UnaryExpr
};

struct Node {
    NodeType type;
    Position pos;

    explicit Node(NodeType type, Position pos) : type(type), pos(pos) {}
    virtual ~Node() = default;
    
    static NodeType get_type();

    template<typename TNode>
    Node *as(NodeType type) {
        if (TNode::get_type() == type) {
            return static_cast<TNode*>(this);
        }
        return nullptr;
    }

    template<typename TNode>
    const Node *as(NodeType type) const {
        if (TNode::get_type() == type) {
            return static_cast<TNode*>(this);
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
    F64
};

struct Type {
    TypeKind kind;
    bool is_const;

    explicit Type(TypeKind kind) : kind(kind), is_const(false) {}
    explicit Type(TypeKind kind, bool is_const) : kind(kind), is_const(is_const) {}

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

using NodeUPTR = std::unique_ptr<Node>;
using NodeSPTR = std::shared_ptr<Node>;

#define NODE Node(get_type(), pos)

struct VarDefStmt : Node {
    const std::string name;
    Type type;
    NodeUPTR expr;

    explicit VarDefStmt(const std::string name, Type type, NodeUPTR expr, Position pos) : name(name), type(type), expr(std::move(expr)), NODE {}

    static NodeType get_type() {
        return NodeType::VarDefStmt;
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

struct LiteralExpr : Node {
    Value val;

    explicit LiteralExpr(Value val, Position pos) : val(val), NODE {}
    
    static NodeType get_type() {
        return NodeType::LiteralExpr;
    }

    const std::string to_str() const override {
        return "LiteralExpr: " + val.to_str();
    }
};

struct BinaryExpr : Node {
    TokenKind op;
    NodeUPTR LHS;
    NodeUPTR RHS;

    explicit BinaryExpr(TokenKind op, NodeUPTR LHS, NodeUPTR RHS, Position pos) : op(op), LHS(std::move(LHS)), RHS(std::move(RHS)), NODE {}
    
    static NodeType get_type() {
        return NodeType::BinaryExpr;
    }

    const std::string to_str() const override {
        std::ostringstream res;
        res << "BinaryExpr: " << LHS->to_str() << ' ';
        switch (op) {
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
            case TokenKind::PRECENT:
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
    TokenKind op;
    NodeUPTR RHS;

    explicit UnaryExpr(TokenKind op, NodeUPTR RHS, Position pos) : op(op), RHS(std::move(RHS)), NODE {}

    static NodeType get_type() {
        return NodeType::UnaryExpr;
    }

    const std::string to_str() const override {
        std::ostringstream res;
        res << "UnaryExpr: ";
        switch (op) {
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

#undef NODE