#pragma once
#include "../diagnostic/diagnostic.h"
#include "../parser/ast.h"
#include <unordered_map>
#include <stack>

class TypeChecker {
    Diagnostic &diag;
    std::vector<NodeUPTR> &stmts;
    std::stack<std::unordered_map<std::string, Type>> vars;

    struct Function {
        std::string name;
        std::vector<Type> args;
        Type ret_type;
    };
    std::unordered_map<std::string, Function> functions;
    std::stack<Type> fun_ret_types;
    static std::unordered_map<TypeKind, std::vector<TypeKind>> implicitly_cast_allowed;

public:
    TypeChecker(Diagnostic &diag, std::vector<NodeUPTR> &stmts) : diag(diag), stmts(stmts) {
        vars.push({});
    }
    
    void analyze();

private:
    void analyze_stmt(const Node &stmt);
    void analyze_var_def(const VarDefStmt &vds);
    void analyze_fun_def(const FunDefStmt &fds);
    void analyze_ret(const RetStmt &rs);

    Type analyze_expr(const Node &expr);
    Type analyze_literal_expr(const LiteralExpr &le);
    Type analyze_binary_expr(const BinaryExpr &be);
    Type analyze_unary_expr(const UnaryExpr &ue);
    Type analyze_var_expr(const VarExpr &ve);
    Type analyze_fun_call_expr(const FunCallExpr &fce);

    bool has_common_type(const Type LHS, const Type RHS);
    Type get_common_type(const Type LHS, const Type RHS, Position pos);
    bool can_implicitly_cast(const Type dest, const Type expected, Position pos);
    Type implicitly_cast(const Type dest, const Type expected, Position pos);
};