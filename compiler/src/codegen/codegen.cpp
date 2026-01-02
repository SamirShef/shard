#include "../include/codegen/codegen.h"

void CodeGenerator::generate() {
    for (const auto &stmt : stmts) {
        switch (stmt->type) {
            case NodeType::VAR_DEF_STMT:
                generate_var_def(*stmt->as<VarDefStmt>());
            default: {}
        }
    }
}

void CodeGenerator::generate_var_def(const VarDefStmt &vds) {
    llvm::Value *initializer = generate_expr(*vds.expr);
    llvm::Type *var_type = type_kind_to_llvm(vds.type.kind);
    if (initializer->getType() != var_type) {
        initializer = implicitly_cast(initializer, var_type);
    }
    auto var = new llvm::GlobalVariable(var_type, vds.type.is_const,
                                                                llvm::GlobalValue::ExternalLinkage,
                                                                llvm::dyn_cast_or_null<llvm::Constant>(initializer), vds.name);
    vars.top().emplace(vds.name, initializer);
}

llvm::Value *CodeGenerator::generate_expr(const Node &expr) {
    switch (expr.type) {
        case NodeType::LITERAL_EXPR:
            return generate_literal_expr(*expr.as<LiteralExpr>());
        case NodeType::BINARY_EXPR:
            return generate_binary_expr(*expr.as<BinaryExpr>());
        case NodeType::UNARY_EXPR:
            return generate_unary_expr(*expr.as<UnaryExpr>());
        case NodeType::VAR_EXPR:
            return generate_var_expr(*expr.as<VarExpr>());
    }
}

llvm::Value *CodeGenerator::generate_binary_expr(const BinaryExpr &be) {
    llvm::Value *LHS = generate_expr(*be.LHS);
    llvm::Value *RHS = generate_expr(*be.RHS);
    llvm::Type *lhs_type = LHS->getType();
    llvm::Type *rhs_type = RHS->getType();

    llvm::Type *common_type = get_common_type(lhs_type, rhs_type);
    if (lhs_type != common_type) {
        LHS = implicitly_cast(LHS, common_type);
        lhs_type = LHS->getType();
    }
    else if (rhs_type != common_type) {
        RHS = implicitly_cast(RHS, common_type);
        rhs_type = RHS->getType();
    }
    switch (be.op.kind) {
        case TokenKind::PLUS:
            if (lhs_type->isFloatingPointTy() || rhs_type->isFloatingPointTy()) {
                return builder.CreateFAdd(LHS, RHS, "fadd.tmp");
            }
            return builder.CreateAdd(LHS, RHS, "add.tmp");
        case TokenKind::MINUS:
            if (lhs_type->isFloatingPointTy() || rhs_type->isFloatingPointTy()) {
                return builder.CreateFSub(LHS, RHS, "fsub.tmp");
            }
            return builder.CreateSub(LHS, RHS, "sub.tmp");
        case TokenKind::STAR:
            if (lhs_type->isFloatingPointTy() || rhs_type->isFloatingPointTy()) {
                return builder.CreateFMul(LHS, RHS, "fmul.tmp");
            }
            return builder.CreateMul(LHS, RHS, "mul.tmp");
        case TokenKind::SLASH:
            if (lhs_type->isFloatingPointTy() || rhs_type->isFloatingPointTy()) {
                return builder.CreateFDiv(LHS, RHS, "fdiv.tmp");
            }
            return builder.CreateSDiv(LHS, RHS, "div.tmp");
        case TokenKind::PERCENT:
            if (lhs_type->isFloatingPointTy() || rhs_type->isFloatingPointTy()) {
                return builder.CreateFRem(LHS, RHS, "frem.tmp");
            }
            return builder.CreateSRem(LHS, RHS, "rem.tmp");
        case TokenKind::EQ_EQ:
            if (lhs_type->isFloatingPointTy() || rhs_type->isFloatingPointTy()) {
                return builder.CreateFCmpUEQ(LHS, RHS, "feq.tmp");
            }
            return builder.CreateICmpEQ(LHS, RHS, "eq.tmp");
        case TokenKind::BANG_EQ:
            if (lhs_type->isFloatingPointTy() || rhs_type->isFloatingPointTy()) {
                return builder.CreateNeg(builder.CreateFCmpUEQ(LHS, RHS, "feq.tmp"), "fnoteq.tmp");
            }
            return builder.CreateNeg(builder.CreateICmpEQ(LHS, RHS, "eq.tmp"), "noteq.tmp");
        case TokenKind::GT:
            if (lhs_type->isFloatingPointTy() || rhs_type->isFloatingPointTy()) {
                return builder.CreateFCmpUGT(LHS, RHS, "fgt.tmp");
            }
            return builder.CreateICmpSGT(LHS, RHS, "gt.tmp");
        case TokenKind::GT_EQ:
            if (lhs_type->isFloatingPointTy() || rhs_type->isFloatingPointTy()) {
                return builder.CreateFCmpUGE(LHS, RHS, "fge.tmp");
            }
            return builder.CreateICmpSGE(LHS, RHS, "ge.tmp");
        case TokenKind::LT:
            if (lhs_type->isFloatingPointTy() || rhs_type->isFloatingPointTy()) {
                return builder.CreateFCmpULT(LHS, RHS, "flt.tmp");
            }
            return builder.CreateICmpSLT(LHS, RHS, "lt.tmp");
        case TokenKind::LT_EQ:
            if (lhs_type->isFloatingPointTy() || rhs_type->isFloatingPointTy()) {
                return builder.CreateFCmpULE(LHS, RHS, "fle.tmp");
            }
            return builder.CreateICmpSLE(LHS, RHS, "le.tmp");
        case TokenKind::LOG_AND:
            return builder.CreateLogicalAnd(LHS, RHS, "land.tmp");
        case TokenKind::LOG_OR:
            return builder.CreateLogicalAnd(LHS, RHS, "lor.tmp");
        case TokenKind::AND:
            return builder.CreateAnd(LHS, RHS, "and.tmp");
        case TokenKind::OR:
            return builder.CreateOr(LHS, RHS, "or.tmp");
        default: {}
    }
}

llvm::Value *CodeGenerator::generate_unary_expr(const UnaryExpr &ue) {
    llvm::Value *value = generate_expr(*ue.RHS);
    
    switch (ue.op.kind) {
        case TokenKind::MINUS:
            if (value->getType()->isFloatingPointTy()) {
                return builder.CreateFNeg(value, "neg.tmp");
            }
            return builder.CreateNeg(value, "neg.tmp");
        case TokenKind::BANG:
            if (value->getType()->isFloatingPointTy()) {
                return builder.CreateFCmpOEQ(value, builder.getInt32(0), "lnot.tmp");
            }
            return builder.CreateICmpEQ(value, builder.getInt32(0), "lnot.tmp");
        default: {}
    }
}

llvm::Value *CodeGenerator::generate_literal_expr(const LiteralExpr &le) {
    switch (le.val.type.kind) {
        #define CONST_INT(func, val_field) llvm::ConstantInt::get(llvm::Type::func(context), le.val.data.val_field)
        #define CONST_FLOAT(func, val_field) llvm::ConstantFP::get(llvm::Type::func(context), le.val.data.val_field)
        case TypeKind::BOOL:
            return CONST_INT(getInt1Ty, bool_val);
        case TypeKind::CHAR:
            return CONST_INT(getInt8Ty, char_val);
        case TypeKind::I16:
            return CONST_INT(getInt16Ty, i16_val);
        case TypeKind::I32:
            return CONST_INT(getInt32Ty, i32_val);
        case TypeKind::I64:
            return CONST_INT(getInt64Ty, i64_val);
        case TypeKind::F32:
            return CONST_FLOAT(getFloatTy, f32_val);
        case TypeKind::F64:
            return CONST_FLOAT(getDoubleTy, f64_val);
        #undef CONST_FLOAT
        #undef CONST_INT
    }
}

llvm::Value *CodeGenerator::generate_var_expr(const VarExpr &ve) {
    auto vars_copy = vars;
    while (!vars_copy.empty()) {
        for (auto var : vars_copy.top()) {
            if (var.first == ve.var_name) {
                if (auto glob = llvm::dyn_cast<llvm::GlobalVariable>(var.second)) {
                    return builder.CreateLoad(glob->getValueType(), glob, var.first + ".load");
                }
                if (auto local = llvm::dyn_cast<llvm::AllocaInst>(var.second)) {
                    return builder.CreateLoad(local->getAllocatedType(), local, var.first + ".load");
                }
                return var.second;
            }
        }
        vars_copy.pop();
    }
}

llvm::Type *CodeGenerator::get_common_type(llvm::Type *LHS, llvm::Type *RHS) {
    if (LHS == RHS) {
        return LHS;
    }
    else if (LHS->isIntegerTy() || RHS->isIntegerTy()) {
        if (LHS->isIntegerTy() && RHS->isIntegerTy()) {
            unsigned left_width = LHS->getIntegerBitWidth();
            unsigned right_width = RHS->getIntegerBitWidth();

            return left_width > right_width ? LHS : RHS;
        }
        else if (LHS->isFloatingPointTy() || RHS->isFloatingPointTy()) {
            return LHS->isFloatingPointTy() ? LHS : RHS;
        }
    }
    else if (LHS->isFloatingPointTy() && RHS->isFloatingPointTy()) {
        return LHS->isDoubleTy() && RHS->isFloatTy() ? LHS : RHS;
    }
    else if (LHS->isDoubleTy() || RHS->isDoubleTy()) {
        return llvm::Type::getDoubleTy(context);
    }
    return nullptr;
}

llvm::Value *CodeGenerator::implicitly_cast(llvm::Value *dest, llvm::Type *expected) {
    llvm::Type *dest_type = dest->getType();
    if (dest_type == expected) {
        return dest;
    }
    else if (dest_type->isIntegerTy() && expected->isIntegerTy()) {
        unsigned long value_width = dest_type->getIntegerBitWidth();
        unsigned long expected_width = expected->getIntegerBitWidth();

        if (value_width > expected_width) {
            return builder.CreateTrunc(dest, expected, "trunc.tmp");
        }
        else {
            return builder.CreateSExt(dest, expected, "sext.tmp");
        }
    }
    else if (dest_type->isFloatingPointTy() && expected->isFloatingPointTy()) {
        if (dest_type->isFloatTy() && expected->isDoubleTy()) {
            return builder.CreateFPExt(dest, expected, "fpext.tmp");
        }
        else {
            return builder.CreateFPTrunc(dest, expected, "fptrunc.tmp");
        }
    }
    else if (dest_type->isIntegerTy() && expected->isFloatingPointTy()) {
        return builder.CreateSIToFP(dest, expected, "sitofp.tmp");
    }
    else if (dest_type->isPointerTy()) {
        return builder.CreatePointerCast(dest, expected, "ptrcast.tmp");
    }
    return nullptr;
}

llvm::Type *CodeGenerator::type_kind_to_llvm(TypeKind kind) {
    switch (kind) {
        #define TYPE(func) llvm::Type::func(context)
        case TypeKind::BOOL:
            return TYPE(getInt1Ty);
        case TypeKind::CHAR:
            return TYPE(getInt8Ty);
        case TypeKind::I16:
            return TYPE(getInt16Ty);
        case TypeKind::I32:
            return TYPE(getInt32Ty);
        case TypeKind::I64:
            return TYPE(getInt64Ty);
        case TypeKind::F32:
            return TYPE(getFloatTy);
        case TypeKind::F64:
            return TYPE(getDoubleTy);
        #undef TYPE
    }
}