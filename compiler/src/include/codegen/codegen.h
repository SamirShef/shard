#pragma once
#include "../parser/ast.h"
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Type.h>
#include <stack>

class CodeGenerator {
    std::vector<NodeUPTR> &stmts;
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;
    std::stack<std::unordered_map<std::string, llvm::Value*>> vars;
    std::unordered_map<std::string, llvm::Function*> functions;
    std::stack<llvm::Type*> fun_ret_types;

    std::stack<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> loop_blocks;    // first - start of loop, second - end of loop

public:
    CodeGenerator(std::vector<NodeUPTR> &stmts, std::string file_name) : stmts(stmts), context(), builder(context),
                                                                         module(std::make_unique<llvm::Module>(file_name, context)) {
        vars.push({});
    }
    
    void generate();

    void print_ir() {
        module->print(llvm::outs(), nullptr);
    }

    std::unique_ptr<llvm::Module> get_module() {
        return std::move(module);
    }

private:
    void generate_stmt(const Node &stmt);
    void generate_var_def(const VarDefStmt &vds);
    void generate_var_asgn(const VarAsgnStmt &vas);
    void generate_fun_def(const FunDefStmt &fds);
    void generate_fun_call(const FunCallStmt &fcs);
    void generate_ret(const RetStmt &rs);
    void generate_if_else(const IfElseStmt &ies);
    void generate_for(const ForStmt &fs);
    void generate_break();
    void generate_continue();

    llvm::Value *generate_expr(const Node &expr);
    llvm::Value *generate_binary_expr(const BinaryExpr &be);
    llvm::Value *generate_unary_expr(const UnaryExpr &ue);
    llvm::Value *generate_literal_expr(const LiteralExpr &le);
    llvm::Value *generate_var_expr(const VarExpr &ve);
    llvm::Value *generate_fun_call_expr(const FunCallExpr &fce);

    llvm::Type *get_common_type(llvm::Type *LHS, llvm::Type *RHS);
    llvm::Value *implicitly_cast(llvm::Value *dest, llvm::Type *expected);
    llvm::Type *type_kind_to_llvm(TypeKind kind);
    llvm::Value *get_default_value(Type type);
};