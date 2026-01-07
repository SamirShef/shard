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

    struct Field {
        Type type;
        AccessModifier access;
    };
    
    struct Struct {
        std::string name;
        std::unordered_map<std::string, Field> fields;
    };
    std::unordered_map<std::string, Struct> structs;

    static std::unordered_map<TypeKind, std::vector<TypeKind>> implicitly_cast_allowed;

public:
    TypeChecker(Diagnostic &diag, std::vector<NodeUPTR> &stmts) : diag(diag), stmts(stmts) {
        vars.push({});
    }
    
    void analyze();

private:
    void analyze_stmt(const Node &stmt);
    void analyze_var_def(const VarDefStmt &vds);
    void analyze_var_asgn(const VarAsgnStmt &vas);
    void analyze_fun_def(const FunDefStmt &fds);
    void analyze_fun_call(const FunCallStmt &fcs);
    void analyze_ret(const RetStmt &rs);
    void analyze_if_else(const IfElseStmt &ies);
    void analyze_for(const ForStmt &fs);
    void analyze_struct(const StructStmt &ss);
    void analyze_field_asgn_stmt(const FieldAsgnStmt &fas);
    void analyze_method_call_stmt(const MethodCallStmt &mcs);

    Type analyze_expr(const Node &expr);
    Type analyze_literal_expr(const LiteralExpr &le);
    Type analyze_binary_expr(const BinaryExpr &be);
    Type analyze_unary_expr(const UnaryExpr &ue);
    Type analyze_var_expr(const VarExpr &ve);
    Type analyze_fun_call_expr(const FunCallExpr &fce);
    Type analyze_struct_expr(const StructExpr &se);
    Type analyze_field_expr(const FieldExpr &fe);
    Type analyze_method_call_expr(const MethodCallExpr &mce);

    bool has_common_type(const Type LHS, const Type RHS);
    Type get_common_type(const Type LHS, const Type RHS, Position pos);
    bool can_implicitly_cast(const Type dest, const Type expected, Position pos);
    Type implicitly_cast(const Type dest, const Type expected, Position pos);
};