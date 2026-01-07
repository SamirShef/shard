#pragma once
#include "../diagnostic/diagnostic.h"
#include "../parser/ast.h"
#include <stack>
#include <unordered_map>

class SemanticAnalyzer {
    Diagnostic &diag;
    std::vector<NodeUPTR> &stmts;
    
    enum class ExprValType {
        BOOL,
        CHAR,
        I16,
        I32,
        I64,
        F32,
        F64,
        NOTH,
        STRUCT,
        UNKNOWN
    };
    
    struct ExprVal {
        ExprValType type;
        union Data {
            bool bool_val;
            char char_val;
            i16 i16_val;
            i32 i32_val;
            i64 i64_val;
            f32 f32_val;
            f64 f64_val;
            u64 struct_index;
        } data;

        explicit ExprVal(ExprValType type, Data data) : type(type), data(data) {}

        f64 as_f64() const {
            switch (type) {
                #define AS_F64(field) static_cast<f64>(data.field)
                case ExprValType::BOOL:
                    return AS_F64(bool_val);
                case ExprValType::CHAR:
                    return AS_F64(char_val);
                case ExprValType::I16:
                    return AS_F64(i16_val);
                case ExprValType::I32:
                    return AS_F64(i32_val);
                case ExprValType::I64:
                    return AS_F64(i64_val);
                case ExprValType::F32:
                    return AS_F64(f32_val);
                case ExprValType::F64:
                    return AS_F64(f64_val);
                case ExprValType::NOTH:
                case ExprValType::STRUCT:
                case ExprValType::UNKNOWN:
                    return 0.0;
                #undef AS_F64
            }
        }

        ExprVal cast_to(ExprValType type) {
            if (type == this->type) {
                return *this;
            }
            switch (type) {
                #define AS(expr_type, type, field) ExprVal(ExprValType::expr_type, ExprVal::Data { .field = static_cast<type>(data.field) })
                case ExprValType::BOOL:
                    return AS(BOOL, bool, bool_val);
                case ExprValType::CHAR:
                    return AS(CHAR, char, char_val);
                case ExprValType::I16:
                    return AS(I16, i16, i16_val);
                case ExprValType::I32:
                    return AS(I32, i32, i32_val);
                case ExprValType::I64:
                    return AS(I64, i64, i64_val);
                case ExprValType::F32:
                    return AS(F32, f32, f32_val);
                case ExprValType::F64:
                    return AS(F64, f64, f64_val);
                case ExprValType::NOTH:
                case ExprValType::STRUCT:
                case ExprValType::UNKNOWN:
                    return get_default(type);
                #undef AS
            }
        }

        static ExprVal get_default(ExprValType type) {
            switch (type) {
                #define NULL_VAL(field) ExprVal(type, ExprVal::Data { .field = 0 })
                case ExprValType::BOOL:
                    return NULL_VAL(bool_val);
                case ExprValType::CHAR:
                    return NULL_VAL(char_val);
                case ExprValType::I16:
                    return NULL_VAL(i16_val);
                case ExprValType::I32:
                    return NULL_VAL(i32_val);
                case ExprValType::I64:
                    return NULL_VAL(i64_val);
                case ExprValType::F32:
                    return NULL_VAL(f32_val);
                case ExprValType::F64:
                    return NULL_VAL(f64_val);
                case ExprValType::STRUCT:
                case ExprValType::NOTH:
                case ExprValType::UNKNOWN:
                    return ExprVal(ExprValType::UNKNOWN, ExprVal::Data { .i32_val = 0 });
                #undef NULL_VAL
            }
        }
    };

    std::stack<std::unordered_map<std::string, ExprVal>> vars;

    struct Function {
        std::string name;
        std::vector<Argument> args;
        const std::vector<NodeUPTR> &block;
    };
    std::unordered_map<std::string, Function> functions;
    std::stack<Type> fun_ret_types;

    u32 loop_depth;

    struct Field {
        ExprVal val;
        Type type;
        bool is_manual_initialized;
        AccessModifier access;
    };
    
    struct Struct {
        std::string name;
        std::unordered_map<std::string, Field> fields;
    };
    std::unordered_map<std::string, Struct> structs;
    std::vector<Struct> structs_instances;
    
    static std::unordered_map<TypeKind, std::vector<TypeKind>> implicitly_cast_allowed;

public:
    SemanticAnalyzer(Diagnostic &diag, std::vector<NodeUPTR> &stmts) : diag(diag), stmts(stmts), loop_depth(0) {
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
    void analyze_break(const BreakStmt &bs);
    void analyze_continue(const ContinueStmt &cs);
    void analyze_struct(const StructStmt &ss);
    void analyze_field_asgn_stmt(const FieldAsgnStmt &fas);
    void analyze_method_call_stmt(const MethodCallStmt &mcs);

    ExprVal analyze_expr(const Node &expr);
    ExprVal analyze_literal_expr(const LiteralExpr &le);
    ExprVal analyze_binary_expr(const BinaryExpr &be);
    ExprVal analyze_unary_expr(const UnaryExpr &ue);
    ExprVal analyze_var_expr(const VarExpr &ve);
    ExprVal analyze_fun_call_expr(const FunCallExpr &fce);
    ExprVal analyze_struct_expr(const StructExpr &se);
    ExprVal analyze_field_expr(const FieldExpr &fe);
    ExprVal analyze_method_call_expr(const MethodCallExpr &mce);

    ExprValType get_common_type(ExprValType LHS, ExprValType RHS);
    ExprVal binary_two_values(ExprVal LHS, ExprVal RHS, TokenKind op, Position pos);
    ExprVal unary_value(ExprVal RHS, TokenKind op);

    ExprValType type_kind_to_expr_val_type(TypeKind type);
    TypeKind expr_val_type_to_type_kind(ExprValType type);
    bool can_implicitly_cast(const Type dest, const Type expected, Position pos);
    Type implicitly_cast(const Type dest, const Type expected, Position pos);
};