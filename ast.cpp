#include "ast.hpp"

//Basic
llvm::LLVMContext AST::TheContext;
llvm::IRBuilder<> AST::Builder(TheContext);
std::unique_ptr<llvm::Module> AST::TheModule;
std::unique_ptr<llvm::legacy::FunctionPassManager> AST::TheFPM;
llvm::GlobalVariable *AST::TheVars;

// Types
llvm::Type *AST::i1;
llvm::Type *AST::i8;
llvm::Type *AST::i16;
llvm::Type *AST::i32;
llvm::Type *AST::i64;
llvm::PointerType *AST::TheListType;
//llvm::Type *AST::ConstantPointerNull;
// Global Variables
llvm::GlobalVariable *AST::TheNL;

//Functions
llvm::Function *AST::TheWriteInteger;
llvm::Function *AST::TheWriteString;
llvm::Function *AST::TheWriteCharacter;
llvm::Function *AST::TheWriteBoolean;

llvm::Function *AST::TheReadInteger;
llvm::Function *AST::TheReadString;
llvm::Function *AST::TheReadCharacter;
llvm::Function *AST::TheReadBoolean;

llvm::Function *AST::TheAbs;
llvm::Function *AST::TheOrd;
llvm::Function *AST::TheChr;
llvm::Function *AST::TheStrLen;
llvm::Function *AST::TheStrCmp;
llvm::Function *AST::TheStrCpy;
llvm::Function *AST::TheStrCat;

llvm::Function *AST::TheInit;
llvm::Function *AST::TheMalloc;
