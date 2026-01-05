#include "../include/codegen/codegen.h"

void CodeGenerator::generate() {
    for (const auto &stmt : stmts) {
        generate_stmt(*stmt);
    }
}

void CodeGenerator::generate_stmt(const Node &stmt) {
    switch (stmt.type) {
        case NodeType::VAR_DEF_STMT:
            generate_var_def(*stmt.as<VarDefStmt>());
            break;
        case NodeType::VAR_ASGN_STMT:
            generate_var_asgn(*stmt.as<VarAsgnStmt>());
            break;
        case NodeType::FUN_DEF_STMT:
            generate_fun_def(*stmt.as<FunDefStmt>());
            break;
        case NodeType::FUN_CALL_STMT:
            generate_fun_call(*stmt.as<FunCallStmt>());
            break;
        case NodeType::RET_STMT:
            generate_ret(*stmt.as<RetStmt>());
            break;
        case NodeType::IF_ELSE_STMT:
            generate_if_else(*stmt.as<IfElseStmt>());
            break;
        case NodeType::FOR_STMT:
            generate_for(*stmt.as<ForStmt>());
            break;
        default: {}
    }
}

void CodeGenerator::generate_var_def(const VarDefStmt &vds) {
    llvm::Value *initializer;
    if (vds.expr) {
        initializer = generate_expr(*vds.expr);
    }
    else {
        initializer = llvm::Constant::getNullValue(type_kind_to_llvm(vds.type.kind));
    }
    llvm::Type *var_type = type_kind_to_llvm(vds.type.kind);
    if (initializer->getType() != var_type) {
        initializer = implicitly_cast(initializer, var_type);
    }
    llvm::Value *var;
    if (fun_ret_types.empty()) {
        var = new llvm::GlobalVariable(*module, var_type, vds.type.is_const, llvm::GlobalValue::ExternalLinkage, llvm::dyn_cast_or_null<llvm::Constant>(initializer), vds.name);
    }
    else {
        var = builder.CreateAlloca(var_type, 0, vds.name);
        builder.CreateStore(initializer, var);
    }
    vars.top().emplace(vds.name, var);
}

void CodeGenerator::generate_var_asgn(const VarAsgnStmt &vas) {
    llvm::Value *val = generate_expr(*vas.expr);
    auto vars_copy = vars;
    while (!vars_copy.empty()) {
        for (auto var : vars_copy.top()) {
            if (var.first == vas.name) {
                if (auto glob = llvm::dyn_cast<llvm::GlobalVariable>(var.second)) {
                    val = implicitly_cast(val, glob->getValueType());
                }
                else if (auto local = llvm::dyn_cast<llvm::AllocaInst>(var.second)) {
                    val = implicitly_cast(val, local->getAllocatedType());
                }
                builder.CreateStore(val, var.second);
            }
        }
        vars_copy.pop();
    }
}

void CodeGenerator::generate_fun_def(const FunDefStmt &fds) {
    std::vector<llvm::Type*> args_types(fds.args.size());
    for (int i = 0; i < args_types.size(); ++i) {
        args_types[i] = type_kind_to_llvm(fds.args[i].type.kind);
    }
    llvm::FunctionType *ret_type = llvm::FunctionType::get(type_kind_to_llvm(fds.ret_type.kind), args_types, false);
    llvm::Function *fun = llvm::Function::Create(ret_type, llvm::GlobalValue::ExternalLinkage, fds.name, *module);
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entry", fun);
    builder.SetInsertPoint(entry);
    vars.push({});
    functions.emplace(fds.name, fun);
    u64 index = 0;
    for (llvm::Argument &arg : fun->args()) {
        arg.setName(fds.args[index].name);
        llvm::AllocaInst *arg_alloca = builder.CreateAlloca(arg.getType(), nullptr, fds.args[index].name);
        builder.CreateStore(&arg, arg_alloca);
        vars.top().emplace(fds.args[index].name, arg_alloca);
        ++index;
    }
    fun_ret_types.push(fun->getReturnType());
    for (auto &stmt : fds.block) {
        generate_stmt(*stmt);
    }
    if (fds.ret_type.kind == TypeKind::NOTH) {
        builder.CreateRetVoid();
    }
    fun_ret_types.pop();
}

void CodeGenerator::generate_fun_call(const FunCallStmt &fcs) {
    llvm::Function *fun = functions.at(fcs.fun_name);
    std::vector<llvm::Value*> args(fcs.args.size());
    for (int i = 0; i < args.size(); ++i) {
        args[i] = generate_expr(*fcs.args[i]);
    }
    builder.CreateCall(fun, args, fcs.fun_name + ".call");
}

void CodeGenerator::generate_ret(const RetStmt &rs) {
    if (!rs.expr) {
        builder.CreateRetVoid();
    }
    else {
        llvm::Value *val = generate_expr(*rs.expr);
        if (val->getType() != fun_ret_types.top()) {
            val = implicitly_cast(val, fun_ret_types.top());
        }
        builder.CreateRet(val);
    }
}

void CodeGenerator::generate_if_else(const IfElseStmt &ies) {
    llvm::Function *parent = builder.GetInsertBlock()->getParent();
    llvm::Value *cond = generate_expr(*ies.cond);
    llvm::BasicBlock *then_bb = llvm::BasicBlock::Create(context, "if.then.branch", parent);
    llvm::BasicBlock *else_bb = llvm::BasicBlock::Create(context, "if.else.branch", parent);
    llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(context, "if.merge.branch", parent);

    builder.CreateCondBr(cond, then_bb, else_bb);
    
    builder.SetInsertPoint(then_bb);
    vars.push({});
    for (auto &stmt : ies.then_branch) {
        generate_stmt(*stmt);
    }
    vars.pop();
    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(merge_bb);
    }

    builder.SetInsertPoint(else_bb);
    vars.push({});
    for (auto &stmt : ies.false_branch) {
        generate_stmt(*stmt);
    }
    vars.pop();
    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(merge_bb);
    }
    
    builder.SetInsertPoint(merge_bb);
}

void CodeGenerator::generate_for(const ForStmt &fs) {
    llvm::Function *parent = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *index_bb = llvm::BasicBlock::Create(context, "for.index", parent);
    llvm::BasicBlock *cond_bb = llvm::BasicBlock::Create(context, "for.cond", parent);
    llvm::BasicBlock *change_index_bb = llvm::BasicBlock::Create(context, "for.change_index", parent);
    llvm::BasicBlock *block_bb = llvm::BasicBlock::Create(context, "for.block", parent);
    llvm::BasicBlock *exit_bb = llvm::BasicBlock::Create(context, "for.exit", parent);

    builder.CreateBr(index_bb);
    builder.SetInsertPoint(index_bb);
    generate_stmt(*fs.index);

    builder.CreateBr(cond_bb);
    builder.SetInsertPoint(cond_bb);
    llvm::Value *condition_value = generate_expr(*fs.cond);

    builder.CreateCondBr(condition_value, block_bb, exit_bb);
    builder.SetInsertPoint(block_bb);
    vars.push({});
    for (auto& stmt : fs.block) {
        generate_stmt(*stmt);
    }
    vars.pop();

    builder.CreateBr(change_index_bb);
    builder.SetInsertPoint(change_index_bb);
    generate_stmt(*fs.change_index);

    builder.CreateBr(cond_bb);
    builder.SetInsertPoint(exit_bb);
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
        case NodeType::FUN_CALL_EXPR:
            return generate_fun_call_expr(*expr.as<FunCallExpr>());
        default: {}
    }
    return nullptr;
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
                    if (fun_ret_types.empty()) {
                        return glob->getInitializer();
                    }
                    return builder.CreateLoad(glob->getValueType(), glob, var.first + ".load");
                }
                else if (auto local = llvm::dyn_cast<llvm::AllocaInst>(var.second)) {
                    return builder.CreateLoad(local->getAllocatedType(), local, var.first + ".load");
                }
                return var.second;
            }
        }
        vars_copy.pop();
    }
}

llvm::Value *CodeGenerator::generate_fun_call_expr(const FunCallExpr &fce) {
    llvm::Function *fun = functions.at(fce.fun_name);
    std::vector<llvm::Value*> args(fce.args.size());
    for (int i = 0; i < args.size(); ++i) {
        args[i] = generate_expr(*fce.args[i]);
    }
    return builder.CreateCall(fun, args, fce.fun_name + ".call");
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
        case TypeKind::NOTH:
            return TYPE(getVoidTy);
        #undef TYPE
    }
}

llvm::Value *CodeGenerator::get_default_value(Type type) {
    switch (type.kind) {
        #define DEFAULT(func) llvm::ConstantInt::get(llvm::Type::func(context), 0)
        case TypeKind::BOOL:
            return DEFAULT(getInt1Ty);
        case TypeKind::CHAR:
            return DEFAULT(getInt8Ty);
        case TypeKind::I16:
            return DEFAULT(getInt16Ty);
        case TypeKind::I32:
            return DEFAULT(getInt32Ty);
        case TypeKind::I64:
            return DEFAULT(getInt64Ty);
        case TypeKind::F32:
            return llvm::ConstantFP::get(llvm::Type::getFloatTy(context), 0);
        case TypeKind::F64:
            return llvm::ConstantFP::get(llvm::Type::getDoubleTy(context), 0);
        default: {}
        #undef DEFAULT
    }
}