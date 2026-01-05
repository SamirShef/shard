#pragma once
#include "../lexer/token.h"
#include "../position.h"
#include <iostream>
#include <sstream>
#include <memory>
#include <vector>

enum class NodeType {
    VAR_DEF_STMT,
    VAR_ASGN_STMT,
    FUN_DEF_STMT,
    FUN_CALL_STMT,
    RET_STMT,
    IF_ELSE_STMT,
    FOR_STMT,
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

    virtual const std::string to_str(i32 space) const = 0;
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

    const std::string to_str(i32 space) const override {
        std::ostringstream res;
        res << std::string(space, ' ') << "VarDefStmt: " << type.to_str() << ' ' << name;
        if (expr) {
            res << " = " << expr->to_str(0);
        }
        return res.str();
    }
};

struct VarAsgnStmt : Node {
    const std::string name;
    NodeUPTR expr;

    explicit VarAsgnStmt(const std::string name, NodeUPTR expr, Position pos) : name(name), expr(std::move(expr)), NODE {}

    NodeUPTR clone() const override {
        return std::make_unique<VarAsgnStmt>(name, expr ? expr->clone() : nullptr, pos);
    }
    
    static NodeType get_type() {
        return NodeType::VAR_ASGN_STMT;
    }

    const std::string to_str(i32 space) const override {
        return std::string(space, ' ') + "VarAsgnStmt: " + name + " = " + (expr ? expr->to_str(0) : "<nil>");
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

    const std::string to_str(i32 space) const override {
        std::ostringstream res;
        res << std::string(space, ' ') << "FunDefStmt: " << ret_type.to_str() << ' ' << name << " (";
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
            res << stmt->to_str(space + 2) << '\n';
        }
        res << (!block.empty() ? std::string(space, ' ') : "") << '}';
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

    const std::string to_str(i32 space) const override {
        std::ostringstream res;
        res << std::string(space, ' ') << "FunCallStmt: " << fun_name << " (";
        for (int i = 0; i < args.size(); ++i) {
            res << args[i]->to_str(0);
            if (i < args.size() - 1) {
                res << ", ";
            }
        }
        res << std::string(space, ' ') << ')';
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

    const std::string to_str(i32 space) const override {
        return std::string(space, ' ') + "RetStmt: " + (expr ? expr->to_str(0) : "<nil>");
    }
};

struct IfElseStmt : Node {
    NodeUPTR cond;
    std::vector<NodeUPTR> then_branch;
    std::vector<NodeUPTR> false_branch;

    explicit IfElseStmt(NodeUPTR cond, std::vector<NodeUPTR> then_branch, std::vector<NodeUPTR> false_branch, Position pos)
                      : cond(std::move(cond)), then_branch(std::move(then_branch)), false_branch(std::move(false_branch)), NODE {}

    NodeUPTR clone() const override {
        std::vector<NodeUPTR> cloned_then_branch(then_branch.size());
        for (int i = 0; i < then_branch.size(); ++i) {
            cloned_then_branch[i] = then_branch[i]->clone();
        }
        std::vector<NodeUPTR> cloned_false_branch(false_branch.size());
        for (int i = 0; i < false_branch.size(); ++i) {
            cloned_false_branch[i] = false_branch[i]->clone();
        }
        return std::make_unique<IfElseStmt>(cond ? cond->clone() : nullptr, std::move(cloned_then_branch), std::move(cloned_false_branch), pos);
    }

    static NodeType get_type() {
        return NodeType::IF_ELSE_STMT;
    }

    const std::string to_str(i32 space) const override {
        std::ostringstream res;
        res << std::string(space, ' ') << "IfElseStmt: (" << (cond ? cond->to_str(0) : "<nil>") << ") {";
        if (!then_branch.empty()) {
            res << '\n';
        }
        for (const auto &stmt : then_branch) {
            res << stmt->to_str(space + 2) << '\n';
        }
        res << (!then_branch.empty() ? std::string(space, ' ') : "") << '}';
        if (!false_branch.empty()) {
            res << " else {\n";
            for (const auto &stmt : false_branch) {
                res << stmt->to_str(space + 2) << '\n';
            }
            res << std::string(space, ' ') << '}';
        }
        return res.str();
    }
};

struct ForStmt : Node {
    NodeUPTR index;
    NodeUPTR cond;
    NodeUPTR change_index;
    std::vector<NodeUPTR> block;

    explicit ForStmt(NodeUPTR index, NodeUPTR cond, NodeUPTR change_index, std::vector<NodeUPTR> block, Position pos)
                   : index(std::move(index)), cond(std::move(cond)), change_index(std::move(change_index)), block(std::move(block)), NODE {}

    NodeUPTR clone() const override {
        std::vector<NodeUPTR> cloned_block(block.size());
        for (int i = 0; i < block.size(); ++i) {
            cloned_block[i] = block[i]->clone();
        }
        return std::make_unique<ForStmt>(index ? index->clone() : nullptr, cond ? cond->clone() : nullptr,
                                         change_index ? change_index->clone() : nullptr, std::move(cloned_block), pos);
    }

    static NodeType get_type() {
        return NodeType::FOR_STMT;
    }

    const std::string to_str(i32 space) const override {
        std::ostringstream res;
        res << std::string(space, ' ') << "ForStmt: (" << (index ? index->to_str(0) + ", " : "") << (cond ? cond->to_str(0) : "")
            << (change_index ?  + ", " + change_index->to_str(0) : "") << ") {";
        if (!block.empty()) {
            res << '\n';
        }
        for (const auto &stmt : block) {
            res << stmt->to_str(space + 2) << '\n';
        }
        res << (!block.empty() ? std::string(space, ' ') : "") << '}';
        return res.str();
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

    const std::string to_str(i32 space) const override {
        return std::string(space, ' ') + "LiteralExpr: " + val.to_str();
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

    const std::string to_str(i32 space) const override {
        std::ostringstream res;
        res << std::string(space, ' ') + "BinaryExpr: " << LHS->to_str(0) << ' ';
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
        res << ' ' << RHS->to_str(0);
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

    const std::string to_str(i32 space) const override {
        std::ostringstream res;
        res << std::string(space, ' ') + "UnaryExpr: ";
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
        res << RHS->to_str(0);
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

    const std::string to_str(i32 space) const override {
        return std::string(space, ' ') + "VarExpr: " + var_name;
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

    const std::string to_str(i32 space) const override {
        std::ostringstream res;
        res << std::string(space, ' ') << "FunCallExpr: " << fun_name << " (";
        for (int i = 0; i < args.size(); ++i) {
            res << args[i]->to_str(0);
            if (i < args.size() - 1) {
                res << ", ";
            }
        }
        res << ')';
        return res.str();
    }
};

#undef NODE