#include "../include/sema/sema.h"
#include <cmath>

std::unordered_map<TypeKind, std::vector<TypeKind>> SemanticAnalyzer::implicitly_cast_allowed {
    { TypeKind::CHAR, { TypeKind::I16, TypeKind::I32, TypeKind::I64 } },
    { TypeKind::I16, { TypeKind::I32, TypeKind::I64 } },
    { TypeKind::I32, { TypeKind::I64 } },
};

void SemanticAnalyzer::analyze() {
    for (auto &stmt : stmts) {
        analyze_stmt(*stmt);
    }
}

void SemanticAnalyzer::analyze_stmt(const Node &stmt) {
    switch (stmt.type) {
        case NodeType::VAR_DEF_STMT:
            analyze_var_def(*stmt.as<VarDefStmt>());
            break;
        case NodeType::VAR_ASGN_STMT:
            analyze_var_asgn(*stmt.as<VarAsgnStmt>());
            break;
        case NodeType::FUN_DEF_STMT:
            analyze_fun_def(*stmt.as<FunDefStmt>());
            break;
        case NodeType::FUN_CALL_STMT:
            analyze_fun_call(*stmt.as<FunCallStmt>());
            break;
        case NodeType::RET_STMT:
            analyze_ret(*stmt.as<RetStmt>());
            break;
        case NodeType::IF_ELSE_STMT:
            analyze_if_else(*stmt.as<IfElseStmt>());
            break;
        case NodeType::FOR_STMT:
            analyze_for(*stmt.as<ForStmt>());
            break;
        case NodeType::BREAK_STMT:
            analyze_break(*stmt.as<BreakStmt>());
            break;
        case NodeType::CONTINUE_STMT:
            analyze_continue(*stmt.as<ContinueStmt>());
            break;
        case NodeType::STRUCT_STMT:
            analyze_struct(*stmt.as<StructStmt>());
            break;
        case NodeType::FIELD_ASGN_STMT:
            analyze_field_asgn_stmt(*stmt.as<FieldAsgnStmt>());
            break;
        case NodeType::METHOD_CALL_STMT:
            analyze_method_call_stmt(*stmt.as<MethodCallStmt>());
            break;
        default:
            diag_part_create(diag, 21, stmt.pos, DiagLevel::ERROR, "");
            break;
    }
}

void SemanticAnalyzer::analyze_var_def(const VarDefStmt &vds) {
    ExprVal expr = vds.expr ? analyze_expr(*vds.expr).cast_to(type_kind_to_expr_val_type(vds.type.kind)) :
                   ExprVal::get_default(type_kind_to_expr_val_type(vds.type.kind));
    if (vds.type.kind == TypeKind::STRUCT) {
        Struct base = structs.at(vds.type.val);
        structs_instances.push_back(base);
        expr = ExprVal(ExprValType::STRUCT, ExprVal::Data { .struct_index = structs_instances.size() - 1 });
    }
    vars.top().emplace(vds.name, expr);
}

void SemanticAnalyzer::analyze_var_asgn(const VarAsgnStmt &vas) {
    if (fun_ret_types.empty()) {
        diag_part_create(diag, 28, vas.pos, DiagLevel::ERROR, "variable assignment");
    }
    if (!vas.expr) {
        diag_part_create(diag, 30, vas.pos, DiagLevel::ERROR, "");
        return;
    }
    std::stack<std::unordered_map<std::string, ExprVal>> vars_copy;
    while (!vars.empty()) {
        for (auto &var : vars.top()) {
            if (var.first == vas.name) {
                while (!vars_copy.empty()) {
                    vars.push(vars_copy.top());
                    vars_copy.pop();
                }
                var.second = analyze_expr(*vas.expr);
                return;
            }
        }
        vars_copy.push(vars.top());
        vars.pop();
    }
    diag_part_create(diag, 22, vas.pos, DiagLevel::ERROR, "Variable `" + vas.name + "` is undeclared in current space.");
}

void SemanticAnalyzer::analyze_fun_def(const FunDefStmt &fds) {
    Function fun { .name = fds.name, .args = fds.args, .block = fds.block };
    functions.emplace(fds.name, fun);
    fun_ret_types.push(fds.ret_type);
    bool has_ret_in_glob = false;
    vars.push({});
    for (int i = 0; i < fds.block.size(); ++i) {
        if (fds.block[i]->type == NodeType::RET_STMT) {
            has_ret_in_glob = true;
        }
        analyze_stmt(*fds.block[i]);
    }
    if (!has_ret_in_glob && fds.ret_type.kind != TypeKind::NOTH) {
        diag_part_create(diag, 26, fds.pos, DiagLevel::ERROR, "");
    }
    vars.pop();
    fun_ret_types.pop();
}

void SemanticAnalyzer::analyze_fun_call(const FunCallStmt &fcs) {
    if (fun_ret_types.empty()) {
        diag_part_create(diag, 28, fcs.pos, DiagLevel::ERROR, "function calling");
    }
    std::vector<NodeUPTR> args(fcs.args.size());
    for (int i = 0; i < args.size(); ++i) {
        args[i] = fcs.args[i]->clone();
    }
    FunCallExpr fce(fcs.fun_name, std::move(args), fcs.pos);
    analyze_fun_call_expr(fce);
}

void SemanticAnalyzer::analyze_ret(const RetStmt &rs) {
    if (fun_ret_types.empty()) {
        diag_part_create(diag, 28, rs.pos, DiagLevel::ERROR, "`return` statement");
    }
    ExprVal expr = rs.expr ? analyze_expr(*rs.expr) : ExprVal(ExprValType::NOTH, ExprVal::Data { .i32_val = 0 });
    implicitly_cast(Type(expr_val_type_to_type_kind(expr.type)), fun_ret_types.top(), rs.pos);
}

void SemanticAnalyzer::analyze_if_else(const IfElseStmt &ies) {
    if (fun_ret_types.empty()) {
        diag_part_create(diag, 28, ies.pos, DiagLevel::ERROR, "`if` statement");
    }
    ExprVal cond = analyze_expr(*ies.cond);
    vars.push({});
    for (auto &stmt : ies.then_branch) {
        analyze_stmt(*stmt);
    }
    vars.pop();
    vars.push({});
    for (auto &stmt : ies.false_branch) {
        analyze_stmt(*stmt);
    }
    vars.pop();
}

void SemanticAnalyzer::analyze_for(const ForStmt &fs) {
    if (fun_ret_types.empty()) {
        diag_part_create(diag, 28, fs.pos, DiagLevel::ERROR, "`for` statement");
    }
    vars.push({});
    ++loop_depth;
    if (fs.index) {
        analyze_stmt(*fs.index);
    }
    if (fs.change_index) {
        analyze_stmt(*fs.change_index);
    }
    for (auto &stmt : fs.block) {
        analyze_stmt(*stmt);
    }
    --loop_depth;
    vars.pop();
}

void SemanticAnalyzer::analyze_break(const BreakStmt &bs) {
    if (fun_ret_types.empty()) {
        diag_part_create(diag, 28, bs.pos, DiagLevel::ERROR, "`break` statement");
    }
    if (loop_depth == 0) {
        diag_part_create(diag, 32, bs.pos, DiagLevel::ERROR, "");
    }
}

void SemanticAnalyzer::analyze_continue(const ContinueStmt &cs) {
    if (fun_ret_types.empty()) {
        diag_part_create(diag, 28, cs.pos, DiagLevel::ERROR, "`continue` statement");
    }
    if (loop_depth == 0) {
        diag_part_create(diag, 33, cs.pos, DiagLevel::ERROR, "");
    }
}

void SemanticAnalyzer::analyze_struct(const StructStmt &ss) {
    Struct s { .name = ss.name };
    for (auto &field : ss.fields) {
        if (field->type == NodeType::VAR_DEF_STMT) {
            auto vds = field->as<VarDefStmt>();
            ExprVal val = ExprVal::get_default(type_kind_to_expr_val_type(vds->type.kind));
            s.fields.emplace(vds->name, Field { .val = val, .type = vds->type, .is_manual_initialized = false, .access = vds->access });
        }
        else {
            diag_part_create(diag, 42, ss.pos, DiagLevel::ERROR, "");
        }
    }
    structs.emplace(ss.name, s);
    
    for (auto &field : ss.fields) {
        if (field->type == NodeType::VAR_DEF_STMT) {
            auto vds = field->as<VarDefStmt>();
            ExprVal val = ExprVal(ExprValType::UNKNOWN, ExprVal::Data { .i32_val = 0 });
            if (vds->expr) {
                val = analyze_expr(*vds->expr);
            }
            else {
                if (vds->type.kind == TypeKind::STRUCT) {
                    structs_instances.push_back(s);
                    val = ExprVal(ExprValType::STRUCT, ExprVal::Data { .struct_index = structs_instances.size() - 1 });
                }
                else {
                    val = ExprVal::get_default(type_kind_to_expr_val_type(vds->type.kind));
                }
            }
            s.fields.emplace(vds->name, Field { .val = val, .type = vds->type, .is_manual_initialized = false, .access = vds->access });
        }
        else {
            diag_part_create(diag, 42, ss.pos, DiagLevel::ERROR, "");
        }
    }
}

void SemanticAnalyzer::analyze_field_asgn_stmt(const FieldAsgnStmt &fas) {
    ExprVal object_val = analyze_expr(*fas.object);
    Struct object = structs_instances[object_val.data.struct_index];
    if (auto field = object.fields.find(fas.name); field != object.fields.end()) {
        ExprVal val = analyze_expr(*fas.expr);
        Type dest = Type(expr_val_type_to_type_kind(val.type));
        if (val.type == ExprValType::STRUCT) {
            dest.val = structs_instances[val.data.struct_index].name;
        }
        implicitly_cast(dest, field->second.type, fas.expr->pos);
        object.fields.at(fas.name).val = val;
        return;
    }
    diag_part_create(diag, 37, fas.pos, DiagLevel::ERROR, "Field `" + fas.name + "` is undeclared in structure `" + object.name + "`.");
}

void SemanticAnalyzer::analyze_method_call_stmt(const MethodCallStmt &mcs) {

}

SemanticAnalyzer::ExprVal SemanticAnalyzer::analyze_expr(const Node &expr) {
    switch (expr.type) {
        case NodeType::LITERAL_EXPR:
            return analyze_literal_expr(*expr.as<LiteralExpr>());
        case NodeType::BINARY_EXPR:
            return analyze_binary_expr(*expr.as<BinaryExpr>());
        case NodeType::UNARY_EXPR:
            return analyze_unary_expr(*expr.as<UnaryExpr>());
        case NodeType::VAR_EXPR:
            return analyze_var_expr(*expr.as<VarExpr>());
        case NodeType::FUN_CALL_EXPR:
            return analyze_fun_call_expr(*expr.as<FunCallExpr>());
        case NodeType::STRUCT_EXPR:
            return analyze_struct_expr(*expr.as<StructExpr>());
        case NodeType::FIELD_EXPR:
            return analyze_field_expr(*expr.as<FieldExpr>());
        case NodeType::METHOD_CALL_EXPR:
            return analyze_method_call_expr(*expr.as<MethodCallExpr>());
        default:
            diag_part_create(diag, 19, expr.pos, DiagLevel::ERROR, "");
            return ExprVal(ExprValType::UNKNOWN, ExprVal::Data { .i32_val = 0 });
    }
}

SemanticAnalyzer::ExprVal SemanticAnalyzer::analyze_literal_expr(const LiteralExpr &le) {
    switch (le.val.type.kind) {
        #define VAL(type, field) ExprVal(ExprValType::type, ExprVal::Data { .field = le.val.data.field })
        case TypeKind::BOOL:
            return VAL(BOOL, bool_val);
        case TypeKind::CHAR:
            return VAL(CHAR, char_val);
        case TypeKind::I16:
            return VAL(I16, i16_val);
        case TypeKind::I32:
            return VAL(I32, i32_val);
        case TypeKind::I64:
            return VAL(I64, i64_val);
        case TypeKind::F32:
            return VAL(F32, f32_val);
        case TypeKind::F64:
            return VAL(F64, f64_val);
        default: {}
        #undef VAL
    }

    return ExprVal(ExprValType::UNKNOWN, ExprVal::Data { .i32_val = 0 });
}

SemanticAnalyzer::ExprVal SemanticAnalyzer::analyze_binary_expr(const BinaryExpr &be) {
    ExprVal LHS = analyze_expr(*be.LHS);
    ExprVal RHS = analyze_expr(*be.RHS);
    ExprValType common_type = get_common_type(LHS.type, RHS.type);

    switch (be.op.kind) {
        case TokenKind::PLUS:
        case TokenKind::MINUS:
        case TokenKind::STAR:
        case TokenKind::SLASH:
        case TokenKind::PERCENT:
        case TokenKind::EQ_EQ:
        case TokenKind::GT:
        case TokenKind::GT_EQ:
        case TokenKind::LT:
        case TokenKind::LT_EQ:
            return binary_two_values(LHS, RHS, be.op.kind, be.pos);
        case TokenKind::LOG_AND:
            return ExprVal(ExprValType::BOOL, ExprVal::Data { .bool_val = LHS.data.bool_val && RHS.data.bool_val });
        case TokenKind::LOG_OR:
            return ExprVal(ExprValType::BOOL, ExprVal::Data { .bool_val = LHS.data.bool_val || RHS.data.bool_val });
        case TokenKind::AND:
        case TokenKind::OR:
            return binary_two_values(LHS, RHS, be.op.kind, be.pos);
        default: {}
    }

    return ExprVal(ExprValType::UNKNOWN, ExprVal::Data { .i32_val = 0 });
}

SemanticAnalyzer::ExprVal SemanticAnalyzer::analyze_unary_expr(const UnaryExpr &ue) {
    ExprVal RHS = analyze_expr(*ue.RHS);

    switch (ue.op.kind) {
        case TokenKind::BANG:
        case TokenKind::MINUS:
            return unary_value(RHS, ue.op.kind);
        default: {}
    }

    return ExprVal(ExprValType::UNKNOWN, ExprVal::Data { .i32_val = 0 });
}

SemanticAnalyzer::ExprVal SemanticAnalyzer::analyze_var_expr(const VarExpr &ve) {
    auto vars_copy = vars;
    while (!vars_copy.empty()) {
        for (auto var : vars_copy.top()) {
            if (var.first == ve.name) {
                return var.second;
            }
        }
        vars_copy.pop();
    }
}

SemanticAnalyzer::ExprVal SemanticAnalyzer::analyze_fun_call_expr(const FunCallExpr &fce) {
    Function fun = functions.at(fce.name);
    vars.push({});
    for (auto &stmt : fun.block) {
        if (auto rs = stmt->as<RetStmt>()) {
            ExprVal expr = rs->expr ? analyze_expr(*rs->expr) : ExprVal(ExprValType::NOTH, ExprVal::Data { .i32_val = 0 });
            vars.pop();
            return expr;
        }
    }
    vars.pop();
    return ExprVal(ExprValType::NOTH, ExprVal::Data { .i32_val = 0 });
}

SemanticAnalyzer::ExprVal SemanticAnalyzer::analyze_struct_expr(const StructExpr &se) {
    Struct base = structs.at(se.name);
    for (int i = 0; i < se.fields.size(); ++i) {
        if (auto field = base.fields.find(se.fields[i].first); field != base.fields.end()) {
            if (field->second.is_manual_initialized) {
                diag_part_create(diag, 42, se.fields[i].second->pos, DiagLevel::ERROR, "Field `" + se.fields[i].first + "` has already been initialized.");
            }
            else {
                base.fields.at(se.fields[i].first).val = analyze_expr(*se.fields[i].second);
                base.fields.at(se.fields[i].first).is_manual_initialized = true;
            }
        }
        else {
            diag_part_create(diag, 37, se.fields[i].second->pos, DiagLevel::ERROR, "Field `" + se.fields[i].first + "` is undeclared in structure `" + se.name + "`.");
        }
    }
    structs_instances.push_back(base);
    return ExprVal(ExprValType::STRUCT, ExprVal::Data { .struct_index = structs_instances.size() - 1 });
}

SemanticAnalyzer::ExprVal SemanticAnalyzer::analyze_field_expr(const FieldExpr &fe) {
    ExprVal object_val = analyze_expr(*fe.object);
    Struct object = structs_instances[object_val.data.struct_index];
    if (auto field = object.fields.find(fe.name); field != object.fields.end()) {
        return object.fields.at(fe.name).val;
    }
    diag_part_create(diag, 37, fe.pos, DiagLevel::ERROR, "Field `" + fe.name + "` is undeclared in structure `" + object.name + "`.");
    return ExprVal(ExprValType::UNKNOWN, ExprVal::Data { .i32_val = 0 });
}

SemanticAnalyzer::ExprVal SemanticAnalyzer::analyze_method_call_expr(const MethodCallExpr &mce) {

}

SemanticAnalyzer::ExprValType SemanticAnalyzer::get_common_type(ExprValType LHS, ExprValType RHS) {
    if (LHS == RHS) {
        return LHS;
    }
    if ((int)LHS >= (int)ExprValType::CHAR && (int)LHS <= (int)ExprValType::F64 &&
        (int)RHS >= (int)ExprValType::CHAR && (int)RHS <= (int)ExprValType::F64) {
        return (int)LHS > (int)RHS ? LHS : RHS;
    }
    return ExprValType::UNKNOWN;
}

SemanticAnalyzer::ExprVal SemanticAnalyzer::binary_two_values(ExprVal LHS, ExprVal RHS, TokenKind op, Position pos) {
    ExprValType common_type = get_common_type(LHS.type, RHS.type);
    f64 lhs_val = LHS.as_f64();
    f64 rhs_val = RHS.as_f64();
    f64 res;
    switch (op) {
        #define BIN(op) lhs_val op rhs_val
        case TokenKind::PLUS:
            res = BIN(+);
            break;
        case TokenKind::MINUS:
            res = BIN(-);
            break;
        case TokenKind::STAR:
            res = BIN(*);
            break;
        case TokenKind::SLASH:
            if (rhs_val == 0) {
                diag_part_create(diag, 27, pos, DiagLevel::ERROR, "");
            }
            res = BIN(/);
            break;
        case TokenKind::PERCENT:
            res = std::fmod(lhs_val, rhs_val);
            break;
        case TokenKind::GT:
            res = BIN(>);
            break;
        case TokenKind::GT_EQ:
            res = BIN(>=);
            break;
        case TokenKind::LT:
            res = BIN(<);
            break;
        case TokenKind::LT_EQ:
            res = BIN(<=);
            break;
        case TokenKind::LOG_AND:
            res = BIN(&&);
            break;
        case TokenKind::LOG_OR:
            res = BIN(||);
            break;
        case TokenKind::AND:
            res = static_cast<i64>(lhs_val) & static_cast<i64>(rhs_val);
            break;
        case TokenKind::OR:
            res = static_cast<i64>(lhs_val) | static_cast<i64>(rhs_val);
            break;
        default: {}
        #undef BIN
    }

    ExprVal::Data result_data;
    switch (common_type) {
        #define AS(type) static_cast<type>(res)
        case ExprValType::BOOL:
            result_data.bool_val = AS(bool);
            break;
        case ExprValType::CHAR:
            result_data.char_val = AS(char);
            break;
        case ExprValType::I16:
            result_data.i16_val = AS(i16);
            break;
        case ExprValType::I32:
            result_data.i32_val = AS(i32);
            break;
        case ExprValType::I64:
            result_data.i64_val = AS(i64);
            break;
        case ExprValType::F32:
            result_data.f32_val = AS(f32);
            break;
        case ExprValType::F64:
            result_data.f64_val = res;
            break;
        default: {}
        #undef AS
    }

    return ExprVal(common_type, result_data);
}

SemanticAnalyzer::ExprVal SemanticAnalyzer::unary_value(ExprVal RHS, TokenKind op) {
    f64 rhs_val = RHS.as_f64();
    f64 res;
    switch (op) {
        #define UN(op) op rhs_val
        case TokenKind::BANG:
            res = UN(!);
            break;
        case TokenKind::MINUS:
            res = UN(-);
            break;
        default: {}
        #undef UN
    }

    ExprVal::Data result_data;
    switch (RHS.type) {
        #define AS(type) static_cast<type>(res)
        case ExprValType::BOOL:
            result_data.bool_val = AS(bool);
            break;
        case ExprValType::CHAR:
            result_data.char_val = AS(char);
            break;
        case ExprValType::I16:
            result_data.i16_val = AS(i16);
            break;
        case ExprValType::I32:
            result_data.i32_val = AS(i32);
            break;
        case ExprValType::I64:
            result_data.i64_val = AS(i64);
            break;
        case ExprValType::F32:
            result_data.f32_val = AS(f32);
            break;
        case ExprValType::F64:
            result_data.f64_val = res;
            break;
        default: {}
        #undef AS
    }

    return ExprVal(RHS.type, result_data);
}

SemanticAnalyzer::ExprValType SemanticAnalyzer::type_kind_to_expr_val_type(TypeKind type) {
    switch (type) {
        case TypeKind::BOOL:
            return ExprValType::BOOL;
        case TypeKind::CHAR:
            return ExprValType::CHAR;
        case TypeKind::I16:
            return ExprValType::I16;
        case TypeKind::I32:
            return ExprValType::I32;
        case TypeKind::I64:
            return ExprValType::I64;
        case TypeKind::F32:
            return ExprValType::F32;
        case TypeKind::F64:
            return ExprValType::F64;
        case TypeKind::NOTH:
            return ExprValType::NOTH;
        case TypeKind::STRUCT:
            return ExprValType::STRUCT;
        default: {}
    }
    return ExprValType::UNKNOWN;
}

TypeKind SemanticAnalyzer::expr_val_type_to_type_kind(ExprValType type) {
    switch (type) {
        case ExprValType::BOOL:
            return TypeKind::BOOL;
        case ExprValType::CHAR:
            return TypeKind::CHAR;
        case ExprValType::I16:
            return TypeKind::I16;
        case ExprValType::I32:
            return TypeKind::I32;
        case ExprValType::I64:
            return TypeKind::I64;
        case ExprValType::F32:
            return TypeKind::F32;
        case ExprValType::F64:
            return TypeKind::F64;
        case ExprValType::NOTH:
            return TypeKind::NOTH;
        case ExprValType::STRUCT:
            return TypeKind::STRUCT;
        case ExprValType::UNKNOWN:
            return TypeKind::I32;
        default: {}
    }
}

bool SemanticAnalyzer::can_implicitly_cast(const Type dest, const Type expected, Position pos) {
    if (dest == expected) {
        return true;
    }
    if (auto it = implicitly_cast_allowed.find(dest.kind); it != implicitly_cast_allowed.end() &&
        std::find(it->second.begin(), it->second.end(), expected.kind) != it->second.end()) {
        return true;
    }
    return false;
}

Type SemanticAnalyzer::implicitly_cast(const Type dest, const Type expected, Position pos) {
    if (dest == expected) {
        return expected;
    }
    if (auto it = implicitly_cast_allowed.find(dest.kind); it != implicitly_cast_allowed.end() &&
        std::find(it->second.begin(), it->second.end(), expected.kind) != it->second.end()) {
        return expected;
    }
    diag_part_create(diag, 20, pos, DiagLevel::ERROR, "Cannot cast `" + dest.to_str() + "` to `" + expected.to_str() + "`.");
    return Type(TypeKind::I32, true);
}