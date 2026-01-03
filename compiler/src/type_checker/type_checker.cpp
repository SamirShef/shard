#include "../include/type_checker/type_checker.h"

std::unordered_map<TypeKind, std::vector<TypeKind>> TypeChecker::implicitly_cast_allowed {
    { TypeKind::CHAR, { TypeKind::I16, TypeKind::I32, TypeKind::I64 } },
    { TypeKind::I16, { TypeKind::I32, TypeKind::I64 } },
    { TypeKind::I32, { TypeKind::I64 } },
};

void TypeChecker::analyze() {
    for (const auto &stmt : stmts) {
        switch (stmt->type) {
            case NodeType::VAR_DEF_STMT:
                analyze_var_def(*stmt->as<VarDefStmt>());
                break;
            default:
                diag_part_create(diag, 21, stmt->pos, DiagLevel::ERROR, "");
                break;
        }
    }
}

void TypeChecker::analyze_var_def(const VarDefStmt &vds) {
    implicitly_cast(analyze_expr(*vds.expr), vds.type, vds.expr->pos);
    vars.top().emplace(vds.name, vds.type);
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
        default:
            diag_part_create(diag, 19, expr.pos, DiagLevel::ERROR, "");
            return Type(TypeKind::I32, true);
    }
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
            if (!((int)LHS.kind >= (int)TypeKind::CHAR && (int)LHS.kind >= (int)TypeKind::F64) ||
                !((int)RHS.kind >= (int)TypeKind::CHAR && (int)RHS.kind >= (int)TypeKind::F64)) {
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
            if (!((int)LHS.kind >= (int)TypeKind::CHAR && (int)LHS.kind >= (int)TypeKind::I64) ||
                !((int)RHS.kind >= (int)TypeKind::CHAR && (int)RHS.kind >= (int)TypeKind::I64)) {
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

Type TypeChecker::analyze_literal_expr(const LiteralExpr &le) {
    return le.val.type;
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

bool TypeChecker::has_common_type(const Type LHS, const Type RHS) {
    if (LHS == RHS) {
        return true;
    }
    if ((int)LHS.kind <= (int)TypeKind::F64 && (int)LHS.kind >= (int)TypeKind::CHAR &&
        (int)RHS.kind <= (int)TypeKind::F64 && (int)RHS.kind >= (int)TypeKind::CHAR) {
        return true;
    }
    return false;
}

Type TypeChecker::get_common_type(const Type LHS, const Type RHS, Position pos) {
    if (LHS == RHS) {
        return LHS;
    }
    if ((int)LHS.kind <= (int)TypeKind::F64 && (int)LHS.kind >= (int)TypeKind::CHAR &&
        (int)RHS.kind <= (int)TypeKind::F64 && (int)RHS.kind >= (int)TypeKind::CHAR) {
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