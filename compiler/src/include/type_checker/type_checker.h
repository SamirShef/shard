#pragma once
#include "../diagnostic/diagnostic.h"
#include "../parser/ast.h"
#include <stack>
#include <unordered_map>

class TypeChecker {
    Diagnostic &diag;
    std::vector<NodeUPTR> &stmts;
    std::stack<std::unordered_map<std::string, Type>> vars;
    static std::unordered_map<TypeKind, std::vector<TypeKind>> implicitly_cast_allowed;

public:
    TypeChecker(Diagnostic &diag, std::vector<NodeUPTR> &stmts) : diag(diag), stmts(stmts) {
        vars.push({});
    }
    
    void analyze();

private:
    void analyze_var_def(const VarDefStmt &vds);

    Type analyze_expr(const Node &expr);
    Type analyze_binary_expr(const BinaryExpr &be);
    Type analyze_unary_expr(const UnaryExpr &ue);
    Type analyze_literal_expr(const LiteralExpr &le);
    Type analyze_var_expr(const VarExpr &ve);

    bool has_common_type(const Type LHS, const Type RHS);
    Type get_common_type(const Type LHS, const Type RHS, Position pos);
    bool can_implicitly_cast(const Type dest, const Type expected, Position pos);
    Type implicitly_cast(const Type dest, const Type expected, Position pos);
};