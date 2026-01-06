#include "../include/type_checker/type_checker.h"

std::unordered_map<TypeKind, std::vector<TypeKind>> TypeChecker::implicitly_cast_allowed {
    { TypeKind::CHAR, { TypeKind::I16, TypeKind::I32, TypeKind::I64 } },
    { TypeKind::I16, { TypeKind::I32, TypeKind::I64 } },
    { TypeKind::I32, { TypeKind::I64 } },
};

void TypeChecker::analyze() {
    for (const auto &stmt : stmts) {
        analyze_stmt(*stmt);
    }
}

void TypeChecker::analyze_stmt(const Node &stmt) {
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
        case NodeType::CONTINUE_STMT:
            break; // skip `break` or `continue`
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

void TypeChecker::analyze_var_def(const VarDefStmt &vds) {
    if (vds.type.kind == TypeKind::NOTH) {
        diag_part_create(diag, 23, vds.pos, DiagLevel::ERROR, "cannot use the `noth` type.");
        vars.top().emplace(vds.name, vds.type);
        return;
    }
    if (vds.expr) {
        implicitly_cast(analyze_expr(*vds.expr), vds.type, vds.expr->pos);
    }
    vars.top().emplace(vds.name, vds.type);
}

void TypeChecker::analyze_var_asgn(const VarAsgnStmt &vas) {
    auto vars_copy = vars;
    while (!vars_copy.empty()) {
        for (auto var : vars_copy.top()) {
            if (var.first == vas.name) {
                if (var.second.is_const) {
                    diag_part_create(diag, 31, vas.pos, DiagLevel::ERROR, "");
                    return;
                }
                implicitly_cast(analyze_expr(*vas.expr), var.second, vas.expr->pos);
                return;
            }
        }
        vars_copy.pop();
    }
    diag_part_create(diag, 22, vas.pos, DiagLevel::ERROR, "Variable `" + vas.name + "` is undeclared in current space.");
}

void TypeChecker::analyze_fun_def(const FunDefStmt &fds) {
    vars.push({});
    fun_ret_types.push(fds.ret_type);
    Function fun { .name = fds.name, .ret_type = fds.ret_type };
    for (auto arg : fds.args) {
        fun.args.push_back(arg.type);
    }
    functions.emplace(fds.name, fun);
    for (auto &stmt : fds.block) {
        analyze_stmt(*stmt);
    }
    fun_ret_types.pop();
    vars.pop();
}

void TypeChecker::analyze_fun_call(const FunCallStmt &fcs) {
    auto fun_it = functions.find(fcs.fun_name);
    if (fun_it == functions.end()) {
        diag_part_create(diag, 24, fcs.pos, DiagLevel::ERROR, "Function `" + fcs.fun_name + "` is undeclared in current space.");
        return;
    }
    Function fun = fun_it->second;
    if (fun.args.size() != fcs.args.size()) {
        diag_part_create(diag, 25, fcs.pos, DiagLevel::ERROR, "expected " + std::to_string(fun.args.size()) + ", but got " + std::to_string(fcs.args.size()) + '.');
        return;
    }
    for (int i = 0; i < fun.args.size(); ++i) {
        implicitly_cast(analyze_expr(*fcs.args[i]), fun.args[i], fcs.args[i]->pos);
    }
}

void TypeChecker::analyze_ret(const RetStmt &rs) {
    auto ret_type = fun_ret_types.top();
    if (rs.expr) {
        implicitly_cast(analyze_expr(*rs.expr), ret_type, rs.expr->pos);
    }
    else if (ret_type.kind != TypeKind::NOTH) {
        implicitly_cast(Type(TypeKind::NOTH), ret_type, rs.pos);
    }
}

void TypeChecker::analyze_if_else(const IfElseStmt &ies) {
    Type cond_type = analyze_expr(*ies.cond);
    implicitly_cast(cond_type, Type(TypeKind::BOOL), ies.cond->pos);
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

void TypeChecker::analyze_for(const ForStmt &fs) {
    vars.push({});
    if (fs.index) {
        analyze_stmt(*fs.index);
    }
    Type cond_type = analyze_expr(*fs.cond);
    implicitly_cast(cond_type, Type(TypeKind::BOOL), fs.cond->pos);
    if (fs.change_index) {
        analyze_stmt(*fs.change_index);
    }
    for (auto &stmt : fs.block) {
        analyze_stmt(*stmt);
    }
    vars.pop();
}

void TypeChecker::analyze_struct(const StructStmt &ss) {
    Struct s { .name = ss.name };
    for (auto &field : ss.fields) {
        if (field->type == NodeType::VAR_DEF_STMT) {
            auto vds = field->as<VarDefStmt>();
            s.fields.emplace(vds->name, vds->type);
        }
    }
    structs.emplace(ss.name, s);
}

void TypeChecker::analyze_field_asgn_stmt(const FieldAsgnStmt &fas) {

}

void TypeChecker::analyze_method_call_stmt(const MethodCallStmt &mcs) {

}

Type TypeChecker::analyze_expr(const Node &expr) {
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
            return Type(TypeKind::I32, true);
    }
}

Type TypeChecker::analyze_literal_expr(const LiteralExpr &le) {
    return le.val.type;
}

Type TypeChecker::analyze_binary_expr(const BinaryExpr &be) {
    const Type LHS = analyze_expr(*be.LHS);
    const Type RHS = analyze_expr(*be.RHS);

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
            if (!((int)LHS.kind >= (int)TypeKind::CHAR && (int)LHS.kind <= (int)TypeKind::F64) ||
                !((int)RHS.kind >= (int)TypeKind::CHAR && (int)RHS.kind <= (int)TypeKind::F64)) {
                diag_part_create(diag, 18, be.pos, DiagLevel::ERROR, "Cannot use `" + be.op.val + "` with `" + LHS.to_str() + "` and `" + RHS.to_str() + "` types.");
                break;
            }
            else {
                if (be.op.kind <= TokenKind::PERCENT) {
                    return get_common_type(LHS, RHS, be.op.pos);
                }
                else if (be.op.kind > TokenKind::PERCENT && be.op.kind <= TokenKind::LT_EQ) {
                    return Type(TypeKind::BOOL);
                }
                break;
            }
        case TokenKind::LOG_AND:
        case TokenKind::LOG_OR:
            if (LHS.kind != TypeKind::BOOL || RHS.kind != TypeKind::BOOL) {
                diag_part_create(diag, 18, be.pos, DiagLevel::ERROR, "Cannot use `" + be.op.val + "` with `" + LHS.to_str() + "` and `" + RHS.to_str() + "` types (expected `bool` type).");
                break;
            }
            else {
                return Type(TypeKind::BOOL);
            }
        case TokenKind::AND:
        case TokenKind::OR:
            if (!((int)LHS.kind >= (int)TypeKind::CHAR && (int)LHS.kind <= (int)TypeKind::I64) ||
                !((int)RHS.kind >= (int)TypeKind::CHAR && (int)RHS.kind <= (int)TypeKind::I64)) {
                diag_part_create(diag, 18, be.pos, DiagLevel::ERROR, "Cannot use `" + be.op.val + "` with `" + LHS.to_str() + "` and `" + RHS.to_str() + "` types (expected integer type).");
                break;
            }
            else {
                return get_common_type(LHS, RHS, be.op.pos);
            }
        default:
            return get_common_type(LHS, RHS, be.op.pos);
    }
}

Type TypeChecker::analyze_unary_expr(const UnaryExpr &ue) {
    Type type = analyze_expr(*ue.RHS);
    switch (ue.op.kind) {
        case TokenKind::BANG:
            if (type.kind != TypeKind::BOOL) {
                diag_part_create(diag, 18, ue.pos, DiagLevel::ERROR, "Cannot use `" + ue.op.val + "` with `" + type.to_str() + "` type (expected `bool` type).");
            }
            break;
        case TokenKind::MINUS:
            if (!((int)type.kind >= (int)TypeKind::CHAR && (int)type.kind >= (int)TypeKind::F64)) {
                diag_part_create(diag, 18, ue.pos, DiagLevel::ERROR, "Cannot use `" + ue.op.val + "` with `" + type.to_str() + "` type (expected integer or number type).");
            }
            break;
        default: {}
    }
    
    return type;
}

Type TypeChecker::analyze_var_expr(const VarExpr &ve) {
    auto vars_copy = vars;
    while (!vars_copy.empty()) {
        for (auto var : vars_copy.top()) {
            if (var.first == ve.name) {
                return var.second;
            }
        }
        vars_copy.pop();
    }
    diag_part_create(diag, 22, ve.pos, DiagLevel::ERROR, "Variable `" + ve.name + "` is undeclared in current space.");
    return Type(TypeKind::I32, true);
}

Type TypeChecker::analyze_fun_call_expr(const FunCallExpr &fce) {
    auto fun_it = functions.find(fce.name);
    if (fun_it == functions.end()) {
        diag_part_create(diag, 24, fce.pos, DiagLevel::ERROR, "Function `" + fce.name + "` is undeclared in current space.");
        return Type(TypeKind::I32, true);
    }
    Function fun = fun_it->second;
    if (fun.args.size() != fce.args.size()) {
        diag_part_create(diag, 25, fce.pos, DiagLevel::ERROR, "expected " + std::to_string(fun.args.size()) + ", but got " + std::to_string(fce.args.size()) + '.');
    }
    for (int i = 0; i < std::min(fun.args.size(), fce.args.size()); ++i) {
        implicitly_cast(analyze_expr(*fce.args[i]), fun.args[i], fce.args[i]->pos);
    }
    return fun.ret_type;
}

Type TypeChecker::analyze_struct_expr(const StructExpr &se) {
    if (auto it = structs.find(se.name); it != structs.end()) {
        return Type(TypeKind::STRUCT, true, it->first);
    }
    diag_part_create(diag, 35, se.pos, DiagLevel::ERROR, "Structure `" + se.name + "` is undeclared in current space.");
    return Type(TypeKind::I32, true);
}

Type TypeChecker::analyze_field_expr(const FieldExpr &fe) {
    Type object_type = analyze_expr(*fe.object);
    if (object_type.kind != TypeKind::STRUCT) {
        diag_part_create(diag, 36, fe.object->pos, DiagLevel::ERROR, "");
        return Type(TypeKind::I32, true);
    }
    if (auto it = structs.find(object_type.val); it != structs.end()) {
        if (auto field = it->second.fields.find(fe.name); field != it->second.fields.end()) {
            return field->second;
        }
        diag_part_create(diag, 37, fe.pos, DiagLevel::ERROR, "Field `" + fe.name + "` is undeclared in structure `" + object_type.val + "`.");
        return Type(TypeKind::I32, true);
    }
    diag_part_create(diag, 35, fe.object->pos, DiagLevel::ERROR, "Structure `" + object_type.val + "` is undeclared in current space.");
    return Type(TypeKind::I32, true);
}

Type TypeChecker::analyze_method_call_expr(const MethodCallExpr &mce) {
    return Type(TypeKind::I32, true);
}

bool TypeChecker::has_common_type(const Type LHS, const Type RHS) {
    if (LHS == RHS) {
        return true;
    }
    if ((int)LHS.kind >= (int)TypeKind::CHAR && (int)LHS.kind <= (int)TypeKind::F64 &&
        (int)RHS.kind >= (int)TypeKind::CHAR && (int)RHS.kind <= (int)TypeKind::F64) {
        return true;
    }
    return false;
}

Type TypeChecker::get_common_type(const Type LHS, const Type RHS, Position pos) {
    if (LHS == RHS) {
        return LHS;
    }
    if ((int)LHS.kind >= (int)TypeKind::CHAR && (int)LHS.kind <= (int)TypeKind::F64 &&
        (int)RHS.kind >= (int)TypeKind::CHAR && (int)RHS.kind <= (int)TypeKind::F64) {
        return (int)LHS.kind > (int)RHS.kind ? LHS : RHS;
    }
    diag_part_create(diag, 18, pos, DiagLevel::ERROR, "`" + LHS.to_str() + "` and `" + RHS.to_str() + "` types does not have a common type.");
    return Type(TypeKind::I32, true);
}

bool TypeChecker::can_implicitly_cast(const Type dest, const Type expected, Position pos) {
    if (dest == expected) {
        return true;
    }
    if (auto it = implicitly_cast_allowed.find(dest.kind); it != implicitly_cast_allowed.end() &&
        std::find(it->second.begin(), it->second.end(), expected.kind) != it->second.end()) {
        return true;
    }
    return false;
}

Type TypeChecker::implicitly_cast(const Type dest, const Type expected, Position pos) {
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