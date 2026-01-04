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
        case NodeType::FUN_DEF_STMT:
            analyze_fun_def(*stmt.as<FunDefStmt>());
            break;
        case NodeType::FUN_CALL_STMT:
            analyze_fun_call(*stmt.as<FunCallStmt>());
            break;
        case NodeType::RET_STMT:
            analyze_ret(*stmt.as<RetStmt>());
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
    implicitly_cast(analyze_expr(*vds.expr), vds.type, vds.expr->pos);
    vars.top().emplace(vds.name, vds.type);
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
        case TokenKind::GT:
        case TokenKind::GT_EQ:
        case TokenKind::LT:
        case TokenKind::LT_EQ:
            if (!((int)LHS.kind >= (int)TypeKind::CHAR && (int)LHS.kind <= (int)TypeKind::F64) ||
                !((int)RHS.kind >= (int)TypeKind::CHAR && (int)RHS.kind <= (int)TypeKind::F64)) {
                diag_part_create(diag, 18, be.pos, DiagLevel::ERROR, "Cannot use `" + be.op.val + "` with `" + LHS.to_str() + "` and `" + RHS.to_str() + "` types.");
            }
            break;
        case TokenKind::LOG_AND:
        case TokenKind::LOG_OR:
            if (LHS.kind != TypeKind::BOOL || RHS.kind != TypeKind::BOOL) {
                diag_part_create(diag, 18, be.pos, DiagLevel::ERROR, "Cannot use `" + be.op.val + "` with `" + LHS.to_str() + "` and `" + RHS.to_str() + "` types (expected `bool` type).");
            }
            break;
        case TokenKind::AND:
        case TokenKind::OR:
            if (!((int)LHS.kind >= (int)TypeKind::CHAR && (int)LHS.kind <= (int)TypeKind::I64) ||
                !((int)RHS.kind >= (int)TypeKind::CHAR && (int)RHS.kind <= (int)TypeKind::I64)) {
                diag_part_create(diag, 18, be.pos, DiagLevel::ERROR, "Cannot use `" + be.op.val + "` with `" + LHS.to_str() + "` and `" + RHS.to_str() + "` types (expected integer type).");
            }
            break;
        default: {}
    }

    return get_common_type(LHS, RHS, be.op.pos);
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
            if (var.first == ve.var_name) {
                return var.second;
            }
        }
        vars_copy.pop();
    }
    diag_part_create(diag, 22, ve.pos, DiagLevel::ERROR, "Variable `" + ve.var_name + "` is undeclared in current space.");
    return Type(TypeKind::I32, true);
}

Type TypeChecker::analyze_fun_call_expr(const FunCallExpr &fce) {
    auto fun_it = functions.find(fce.fun_name);
    if (fun_it == functions.end()) {
        diag_part_create(diag, 24, fce.pos, DiagLevel::ERROR, "Function `" + fce.fun_name + "` is undeclared in current space.");
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