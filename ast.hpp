#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <map>
#include <vector>
#include <cstring>
#include <stdio.h>
#include <algorithm>

#include "symbol.h"
#include "error.h"

/*------------------- LLVM ------------------- */
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Verifier.h>

/*------------ LLVM Optimizations -------------*/
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>

void yyerror(const char *msg);

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
static llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction,
                                          const std::string &VarName,
                                          llvm::Type *type) {
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                   TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(type, nullptr, VarName);
}


inline std::ostream& operator<<(std::ostream &out, Type t) {

  if (t == NULL) {
      out << "<undefined>";
      return out;
  }

  switch (t->kind) {
      case TYPE_VOID:
          out << "void";
          break;
      case TYPE_INTEGER:
          out << "integer";
          break;
      case TYPE_BOOLEAN:
          out << "boolean";
          break;
      case TYPE_CHAR:
          out << "char";
          break;
      case TYPE_ARRAY:
          out << "array [" << t->size << "] of ";
          out << t->refType;
          break;
      case TYPE_IARRAY:
          out << "array of ";
          out << t->refType;
          break;
      case TYPE_LIST:
          out << "list of ";
          out << t->refType;
          break;
      case TYPE_ANY:
          out << "any type";
          break;
      default:
          out << "not recognised type";
          break;
  }
  return out;
}

enum StmtType{
  SIMPLE_STMT,
  RETURN,
  EXIT,
  COND_RETURN
};

enum StringExpr{
  STRING,
  STRING_ITEM,
  OTHER
};


class AST {
  public:
    AST() {

    }
    virtual ~AST() {
      destroySymbolTable();
    }
    virtual void printOn(std::ostream &out) const = 0;
    virtual llvm::Value* compile() { return nullptr; } //const = 0;
    virtual void sem() {}
    void llvm_compile_and_dump(bool doOptimize=false) {
    // Initialize
    TheModule = std::make_unique<llvm::Module>("Tony program", TheContext);
    TheFPM = std::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());

    // Initialize types
    i1 = llvm::IntegerType::get(TheContext, 1);
    i8 = llvm::IntegerType::get(TheContext, 8);
    i16 = llvm::IntegerType::get(TheContext, 16);
    i32 = llvm::IntegerType::get(TheContext, 32);
    i64 = llvm::IntegerType::get(TheContext, 64);

    // Initialize global variables (e.g newline)
    llvm::ArrayType *nl_type = llvm::ArrayType::get(i8, 2); //create llvm array type of nl (it should have 2 items with 8 bits each (characters))
    TheNL = new llvm::GlobalVariable(
        *TheModule, nl_type, true, llvm::GlobalValue::PrivateLinkage,
        llvm::ConstantArray::get(nl_type, {c8('\n'), c8('\0')}), "nl"
    );
    TheNL->setAlignment(1);
    // Initialize Abgenetopoulos library functions
    llvm::FunctionType *writeInteger_type =
      llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i64}, false);
    TheWriteInteger =
      llvm::Function::Create(writeInteger_type, llvm::Function::ExternalLinkage,
                       "puti", TheModule.get());

   llvm::FunctionType *writeCharacter_type =
     llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i8}, false);
   TheWriteCharacter =
     llvm::Function::Create(writeCharacter_type, llvm::Function::ExternalLinkage,
                      "putc", TheModule.get());

    llvm::FunctionType *writeBoolean_type =
      llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i1}, false);
    TheWriteBoolean =
      llvm::Function::Create(writeBoolean_type, llvm::Function::ExternalLinkage,
                       "putb", TheModule.get());

    llvm::FunctionType *writeString_type =
      llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext),
                        {llvm::PointerType::get(i8, 0)}, false);
    TheWriteString =
      llvm::Function::Create(writeString_type, llvm::Function::ExternalLinkage,
                       "puts", TheModule.get());

      //---------------------READ functions---------------------
     std::vector<llvm::Type *> t;
     llvm::FunctionType *readInteger_type =
       llvm::FunctionType::get(i64, std::vector<llvm::Type *> {}, false);
     TheReadInteger =
       llvm::Function::Create(readInteger_type, llvm::Function::ExternalLinkage,
                        "geti", TheModule.get());

     llvm::FunctionType *readCharacter_type =
      llvm::FunctionType::get(i8, std::vector<llvm::Type *> {}, false);
     TheReadCharacter =
      llvm::Function::Create(readCharacter_type, llvm::Function::ExternalLinkage,
                       "getc", TheModule.get());

     llvm::FunctionType *readBoolean_type =
       llvm::FunctionType::get(i1, std::vector<llvm::Type *>{}, false);
     TheReadBoolean =
       llvm::Function::Create(readBoolean_type, llvm::Function::ExternalLinkage,
                        "getb", TheModule.get());

     llvm::FunctionType *readString_type =
       llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext),
                         {i64, llvm::PointerType::get(i8, 0)}, false);
     TheReadString =
       llvm::Function::Create(readString_type, llvm::Function::ExternalLinkage,
                        "gets", TheModule.get());

      //---------------------Conversion functions---------------------
      llvm::FunctionType *abs_type =
        llvm::FunctionType::get(i64, {i64}, false);
      TheAbs =
        llvm::Function::Create(abs_type, llvm::Function::ExternalLinkage,
                        "abs", TheModule.get());

      llvm::FunctionType *ord_type =
        llvm::FunctionType::get(i64, {i8}, false);
      TheOrd =
        llvm::Function::Create(ord_type, llvm::Function::ExternalLinkage,
                        "ord", TheModule.get());

      llvm::FunctionType *chr_type =
        llvm::FunctionType::get(i8, {i64}, false);
      TheChr =
        llvm::Function::Create(chr_type, llvm::Function::ExternalLinkage,
                        "chr", TheModule.get());


      llvm::FunctionType *strlen_type =
        llvm::FunctionType::get(i64, {llvm::PointerType::get(i8, 0)}, false);
      TheStrLen =
        llvm::Function::Create(strlen_type, llvm::Function::ExternalLinkage,
                        "strlen", TheModule.get());

      llvm::FunctionType *strcmp_type =
        llvm::FunctionType::get(i64,
          {llvm::PointerType::get(i8, 0), llvm::PointerType::get(i8, 0)}, false);
      TheStrCmp =
        llvm::Function::Create(strcmp_type, llvm::Function::ExternalLinkage,
                        "strcmp", TheModule.get());

      llvm::FunctionType *strcpy_type =
        llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext),
          {llvm::PointerType::get(i8, 0), llvm::PointerType::get(i8, 0)}, false);
      TheStrCpy =
        llvm::Function::Create(strcpy_type, llvm::Function::ExternalLinkage,
                        "strcpy", TheModule.get());
      TheStrCat =
        llvm::Function::Create(strcpy_type, llvm::Function::ExternalLinkage,
                        "strcat", TheModule.get());

    compile();
    bool bad = llvm::verifyModule(*TheModule, &llvm::errs());
    if (bad) {
      std::cerr << "The IR is bad!" << std::endl;
      TheModule->print(llvm::errs(), nullptr);
      std::exit(1);
    }
    // Initialize optimization Function Pass Manager
    if (doOptimize) {
      TheFPM->add(llvm::createPromoteMemoryToRegisterPass());
      TheFPM->add(llvm::createInstructionCombiningPass());
      TheFPM->add(llvm::createReassociatePass());
      TheFPM->add(llvm::createGVNPass());
      TheFPM->add(llvm::createCFGSimplificationPass());
    }
    TheFPM->doInitialization();


    TheModule->print(llvm::outs(), nullptr);
    }
    llvm::Type * getLLVMType(Type type) const{
      llvm::Type * retType;
      bool isReference;
      switch (type->kind) {
          case TYPE_VOID:
              retType = llvm::Type::getVoidTy(TheContext);
              break;
          case TYPE_INTEGER:
              retType = i64;
              break;
          case TYPE_BOOLEAN:
              retType = i1;
              break;
          case TYPE_CHAR:
              retType = i8;
              break;
          case TYPE_ARRAY:
              retType = llvm::PointerType::get(getLLVMType(type->refType), 0);
              break;
          case TYPE_IARRAY:
              retType = llvm::PointerType::get(getLLVMType(type->refType), 0);
              break;
          case TYPE_LIST:
              retType = llvm::PointerType::get(getLLVMType(type->refType), 0);
              break;
          case TYPE_ANY:
              retType = nullptr; //pithanon COnstantNullPointer.. ..
              break;
          default:
              retType = i64;
      }

      return retType;
    }

  protected:
    static llvm::LLVMContext TheContext;
    static llvm::IRBuilder<> Builder;
    static llvm::GlobalVariable *TheVars;
    static std::unique_ptr<llvm::Module> TheModule;
    static std::unique_ptr<llvm::legacy::FunctionPassManager> TheFPM;


    // Types
    static llvm::Type *i1;
    static llvm::Type *i8;
    static llvm::Type *i16;
    static llvm::Type *i32;
    static llvm::Type *i64;


    // Global Variables
    static llvm::GlobalVariable *TheNL;

    static llvm::ConstantInt* c64(int n) {
      return llvm::ConstantInt::get(TheContext, llvm::APInt(64, n, true));
    }
    //for integers (32 bits - complies with minimum 16 bit signed integers imposed by our programming language )
    static llvm::ConstantInt* c32(int n) {
      return llvm::ConstantInt::get(TheContext, llvm::APInt(32, n, true));
    }
    //for character (8 bit type)
    static llvm::ConstantInt* c8(char n) {
      return llvm::ConstantInt::get(TheContext, llvm::APInt(8, n, true));
    }
    //for boolean (1 bit type) - maybe not be necessary. Will see.
    static llvm::ConstantInt* c1(int n) {
      return llvm::ConstantInt::get(TheContext, llvm::APInt(1, n, true));
    }

    //Functions
    static llvm::Function *TheWriteInteger;
    static llvm::Function *TheWriteString;
    static llvm::Function *TheWriteCharacter;
    static llvm::Function *TheWriteBoolean;

    static llvm::Function *TheReadInteger;
    static llvm::Function *TheReadString;
    static llvm::Function *TheReadCharacter;
    static llvm::Function *TheReadBoolean;

    static llvm::Function *TheAbs;
    static llvm::Function *TheOrd;
    static llvm::Function *TheChr;
    static llvm::Function *TheStrLen;
    static llvm::Function *TheStrCmp;
    static llvm::Function *TheStrCpy;
    static llvm::Function *TheStrCat;

};

inline std::ostream& operator<< (std::ostream &out, const AST &t) {
  t.printOn(out);
  return out;
};

class VarList: public AST {
  public:
    VarList() : var_list(), type(NULL) {};
    ~VarList() {
      var_list.clear();
    }
    virtual void printOn(std::ostream &out) const override {
      out << "Var(s)(";
      for (int i = 0; i < var_list.size()-1; i++){
        std::string s = var_list[i];
        out << s << ", ";
      }
      out << var_list[var_list.size()-1];
      out << ")" << " with Type " << type;
    }
    void var_append(const char* d) { var_list.insert(var_list.begin(), d); }
    void var_type(Type t) { type = t; }
    std::vector<const char * > getVarList() { return var_list; }
    Type getType() { return type; }
    virtual void sem() override {
      for (int i = 0; i < var_list.size(); i++) {
        newVariable(var_list[i], type);
      }
    }
    virtual llvm::Value* compile() override {
      return nullptr;
    }
  private:
    std::vector<const char *> var_list;
    Type type;
};

class Arg: public AST {
  public:
    Arg(PassMode pass, VarList *v): passmode(pass)
    {
      var_list = v->getVarList();
      type = v->getType();
    };
    ~Arg() {
      var_list.clear();
    }
    virtual void printOn(std::ostream &out) const override {
      out << "Arg(s)(";
      for (int i = 0; i < var_list.size()-1; i++){
        std::string s = var_list[i];
        out << s << ", ";
      }
      out << var_list[var_list.size()-1];
      out << ")" << " with Type " << type << " and pass by " << passmode;
    }
    int getArgSize() { return var_list.size(); }
    Type getType() { return type; }
    void sem(SymbolEntry *p) {
      for (const char *name: var_list) newParameter(name, type, passmode, p);
    }
    virtual llvm::Value* compile() override {
      return nullptr;
    }
    std::vector<const char *> getArgListNames(){ return var_list; }
  private:
    PassMode passmode;
    std::vector<const char * > var_list;
    Type type;
};

typedef std::vector<Arg *> ArgList;

enum HeaderDef {
  DECL,
  DEF
};


class Header: public AST {
public:
  Header(Type t, const char * s, ArgList *a): type(t), name(s), arg_list(a) {}
  ~Header(){
    arg_list->clear();
  }

  Type getHeaderType() { return type; }
  const char * getHeaderName() { return name; }
  ArgList *getHeaderArgList() { return arg_list; }
  void setHeaderDef(HeaderDef h) { hdef = h; }
  HeaderDef getHeaderDef() { return hdef; }
  virtual void printOn(std::ostream &out) const override {
    out << "Header with name " << name << " and type " << type << "(";
    bool first = true;
    for (std::vector<Arg *>::iterator it = arg_list->begin(); it != arg_list->end(); ++it){
      if (!first) out << ", ";
      first = false;
      out << std::endl << **it;
    }
    out << std::endl << ")";
  }
  virtual void sem() override {
    SymbolEntry * p;
    p = newFunction(name);
    if (hdef == DECL) {
      forwardFunction(p);
    }
    openScope();
    for (Arg *a: *arg_list) { a->sem(p); }
    endFunctionHeader(p, type);
  }

  llvm::Function * compilef() {

      SymbolEntry * p;
      p = newFunction(name);
      if (hdef == DECL) {
        forwardFunction(p);
      }

      openScope();

      std::vector<llvm::Type *> argTypes;
      for (Arg *a: *arg_list) {
        a->sem(p);
        for (int i = 0; i < a->getArgSize(); i++){
          llvm::Type *tempType = this->getLLVMType(a->getType());
          argTypes.push_back(tempType);
        }
      }

      llvm::FunctionType * FT = llvm::FunctionType::get(getLLVMType(type), argTypes, false );
      llvm::Function * Function = llvm::Function::Create(FT, llvm::Function::ExternalLinkage,
                                                      name, TheModule.get());

      std::vector<const char *> names;

      for (Arg *a: *arg_list) {
        std::vector<const char *> tmp = a->getArgListNames();
        names.insert(names.end(), tmp.begin(), tmp.end());
      }

      int i = 0;
      for (auto &Arg : Function->args())
          Arg.setName(names[i++]);

      p->u.eFunction.llvmfun = Function;

      endFunctionHeader(p, type);

      return Function;
    }

private:
  Type type;
  const char * name;
  ArgList *arg_list;
  HeaderDef hdef;
};


class Stmt: public AST {
  public:
    virtual Type getReturnType() { return returntype; }
    virtual void setReturnType(Type t) { returntype = t; }
    virtual bool checkForExits() { return (this->getStmtType() == EXIT); }
    void setStmtType(StmtType s) { stmttype = s; }
    StmtType getStmtType() { return stmttype; }
    virtual bool checkForReturns() { return (this->getStmtType() == RETURN); }
    virtual void checkReturnType(Header *header) {
      if(!equalType(header->getHeaderType(), this->getReturnType()))
        fatal("Return type does not match function type");
    }
    virtual llvm::Value* compile() override {
      return nullptr;
    }
  private:
    StmtType stmttype;
    Type returntype;
};

typedef std::vector<Stmt *> StmtList;


enum DefType {
  FUNCTION,
  VARIABLE,
  DECLARATION
};

class Decl: public AST{
  public:
    Decl(Header *h): header(h) {}
    ~Decl() {}
    virtual void printOn(std::ostream &out) const override {
      out << "Decl with " << *header << std::endl;
    }

    virtual void sem() override {
      header->sem();
      closeScope();
    }
    virtual llvm::Value * compile() override {
      llvm::Function * f_ = header->compilef();
      closeScope();
      return nullptr;
    }
  private:
    Header *header;
};

class FuncBlock: public AST {
public:
  FuncBlock(): var_list(), sequence(), stmt_list(NULL), size(0), isMain(false) { }
  ~FuncBlock() {
    for (VarList *d : var_list) delete d;
  }

  void append_varlist(VarList *v) {
    var_list.insert(var_list.begin(), v);
    sequence.insert(sequence.begin(), VARIABLE);
  }
  void append_fun(FuncBlock *f){
    func_list.insert(func_list.begin(), f);
    sequence.insert(sequence.begin(), FUNCTION);
  }

  void append_decl(Decl *d) {
    decl_list.insert(decl_list.begin(), d);
    sequence.insert(sequence.begin(), DECLARATION);
  }

  void append_stmtlist(StmtList *stmtl) { stmt_list = stmtl; }

  void assignHeader(Header *h) {
    header = h;
  }

  void setMain() {
    isMain = true;
    checkHeader();
  }
  void checkHeader() {
    Type type = header->getHeaderType();
    if (!equalType(type, typeVoid)) {
      fatal("Main function should be of type void");
    }
    ArgList *arglist = header->getHeaderArgList();

    if (!arglist->empty()) {
      fatal("Main function should not have arguments");
    }
  }

  virtual void printOn(std::ostream &out) const override {
    out << "Block(" << std::endl;
    out << *header;
    bool first = true;
    for (VarList *d : var_list) {
      if (!first) out << ", ";
      first = false;
      out <<  std::endl << *d;
    }
    out << std::endl;
    if (stmt_list != NULL) {
      for (Stmt *stmt: *stmt_list) {
        out << *stmt;
      }
    }
    out << ")";
  }

  virtual void sem() override {

    if (!isMain) {
      header->sem();
    }

    if (false) printSymbolTable();;
    int v = 0, f = 0, d = 0;
    bool existsReturn = false;
    for (std::vector<DefType>::iterator it = sequence.begin(); it < sequence.end(); it++){
      if (*it == FUNCTION) {
        func_list[f]->sem();
        f++;
      }
      else if (*it == VARIABLE) {
        var_list[v]->sem();
        v++;
      }
      else if (*it == DECLARATION) {
        decl_list[d]->sem();
        d++;
      }
    }

    if (stmt_list != NULL) {
      for (Stmt *stmt : *stmt_list) {
        stmt->sem();

        if(stmt->checkForReturns()){

          if(stmt->checkForExits() && header->getHeaderType() != typeVoid)
            fatal("Exit can only be used inside void function blocks");
          //std::cout << "statement " << *stmt << " has checkReturn() " << std::endl;
          existsReturn = true;
          //std::cout << "header type = " << header->getHeaderType() << " return type = " << stmt->getReturnType() <<std::endl;
          stmt->checkReturnType(header);
        }
      }
    }

    if(header->getHeaderDef() == DEF && !existsReturn && !equalType(header->getHeaderType(), typeVoid)) {
      fatal("Non void function must have a return statement.");
    }

    if (false) printSymbolTable();;
    closeScope();
  }

  virtual llvm::Value* compile() override {

    if (!isMain) {
        SymbolEntry * e = lookupEntry(header->getHeaderName(), LOOKUP_ALL_SCOPES, false);
        if (!e){
          thisFunction = header->compilef();
        }
        else {
          thisFunction = e->u.eFunction.llvmfun;
        }
    }
    else{
      llvm::FunctionType *FT = llvm::FunctionType::get(i32, {}, false );
      thisFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage,
                                                        "main", TheModule.get());

    }

    llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", thisFunction);
    Builder.SetInsertPoint(BB);


    //Builder.CreateCall(TheInit, {});

    int v = 0, f = 0, d = 0;
    int count = 1;
    for (std::vector<DefType>::iterator it = sequence.begin(); it < sequence.end(); it++){
      if (*it == FUNCTION) {
        llvm::Value * _t = func_list[f]->compile();
        f++;
        Builder.SetInsertPoint(BB);
      }
      else if (*it == VARIABLE) {
        var_list[v]->sem();
        llvm::Type * t = getLLVMType(var_list[v]->getType());
        for (const char * s : var_list[v]->getVarList()) {
            llvm::AllocaInst *alloca_temp = CreateEntryBlockAlloca(thisFunction, s, t);
            SymbolEntry * e = lookupEntry(s, LOOKUP_ALL_SCOPES, false);
            e->u.eVariable.allocainst = alloca_temp;
        }
        v++;
      }
      else if (*it == DECLARATION) {
        decl_list[d]->compile();
        d++;
        Builder.SetInsertPoint(BB);
      }
    }

    if (stmt_list != NULL) {
      for (Stmt *stmt : *stmt_list) {
        stmt->compile();

        if(stmt->checkForReturns()){
          bool existsReturn = true;
        }
      }
    }

    if (false) printSymbolTable();;
    closeScope();
    // Emit the program code.
    //compile();
    if (isMain) {
      Builder.CreateRet(c32(0));
    }
    else {
      if(equalType(header->getHeaderType(), typeVoid)) Builder.CreateRetVoid();
    }
    llvm::verifyFunction(*thisFunction);
    bool bad = llvm::verifyFunction(*thisFunction, &llvm::errs());
    if (bad) {
      std::cerr << "The function " << header->getHeaderName() << " is bad!" << std::endl;
      thisFunction->print(llvm::errs(), nullptr);
      std::exit(1);
    }
    if (isMain) {
    TheFPM->run(*thisFunction);
    }
    return nullptr;
  }

private:
  Header *header;
  std::vector<VarList *> var_list;
  std::vector<FuncBlock *> func_list;
  std::vector<Decl *> decl_list;
  StmtList *stmt_list;
  std::vector<DefType> sequence;
  int size;
  llvm::Function * thisFunction;
  bool isMain;
};

// EXPRESSIONS (e.g atoms, constants, operations applied to expressions )

class Expr : public AST {
  public:
    //virtual void compile() const = 0; UNCOMMENT WHEN COMPILE IS IMPLEMENTED ON other classes
    void type_check(Type t) {
      sem();
      if (!equalType(type, t))
        fatal("Type mismatch");
    }
    bool isBasicType(Type t) {
      return (equalType(t, typeChar) || equalType(t, typeBoolean) || equalType(t, typeInteger));
    }
    void setLeft() { isLeft = true; }
    Type getType() { return type; }
    bool isLValue() { return lval; }
    StringExpr getStringExpr() { return stringExpr; }
    virtual llvm::Value* compile() override {
      return nullptr;
    }
  protected:
    Type type;
    bool lval;
    bool isLeft = false;
     // enumeration of valid types for a expression.
     // used to throw error on cases like "test"[0] := 4
    StringExpr stringExpr;
};

typedef std::vector<Expr *> ExprList;

class IntConst : public Expr {
  public:
    IntConst(int n) : num(n) {}
    virtual void printOn(std::ostream &out) const override {
      out << "IntConst(" << num << ")";
    }
    ~IntConst() {}
    /*
    virtual void compile() const override {
      std::cout << "  pushl $" << num << "\n";
    }
    */
    virtual void sem() override { lval = false; stringExpr = OTHER; type = typeInteger; }
    virtual llvm::Value* compile()override { return c64(num); }
  private:
    int num;
};

class CharConst : public Expr {
  public:
    CharConst(char c) : character(c) {}
    virtual void printOn(std::ostream &out) const override {
      out << "CharConst(" << character << ")";
    }
    ~CharConst() {}
    /*
    virtual void compile() const override {
      std::cout << "  pushl $" << num << "\n";
    }
    */
    virtual void sem() override { lval = false; stringExpr = OTHER; type = typeChar; }
    virtual llvm::Value* compile() override { return c8(character); }

  private:
    char character;
};

class BoolConst : public Expr {
  public:
    BoolConst(bool b): logic(b) {}
    ~BoolConst() {}
    virtual void printOn(std::ostream &out) const override {
      out << "Logic(" << logic << ")";
    }
    /*
    virtual void compile() const override {
      std::cout << "  pushl $" << num << "\n";
    }
    */
    virtual void sem() override { lval = false; stringExpr = OTHER; type = typeBoolean; }
    virtual llvm::Value* compile() override { return c1(int(logic)); }

  private:
    bool logic;
};

class Id : public Expr {
public:
  Id(const char * v) : var(v) {}
  virtual void printOn(std::ostream &out) const override {
    out << "Id(" << var << "@" << ")";
  }
  const char *getIdName() { return var; }
  EntryType getEntryType() {
    return entry;
  }
  virtual void sem() override {
    lval = true;
    SymbolEntry *e = lookupEntry(var,LOOKUP_ALL_SCOPES, false);
    if (e==NULL) { fatal("Id has not been declared"); }
    entry = e->entryType;
    if (entry == ENTRY_VARIABLE) {
      type = e->u.eVariable.type;
    }
    else if (entry == ENTRY_FUNCTION) {
      type = e->u.eFunction.resultType;
    }
    else if (entry == ENTRY_PARAMETER) {
      type = e->u.eParameter.type;
    }
    stringExpr = OTHER;
  }
  virtual llvm::Value* compile() override {
    SymbolEntry *e = lookupEntry(var,LOOKUP_ALL_SCOPES, false);
    if (e==NULL) { fatal("Id has not been declared"); }
    entry = e->entryType;
    if (entry == ENTRY_VARIABLE) {
      if (!isLeft)
        return Builder.CreateLoad(e->u.eVariable.allocainst, var);
      else return e->u.eVariable.allocainst;
    }
    else if (entry == ENTRY_FUNCTION) {
      return e->u.eFunction.llvmfun;
    }
    else if (entry == ENTRY_PARAMETER) {
      type = e->u.eParameter.type;
      if (e->u.eParameter.mode == PASS_BY_REFERENCE)
    }
    return nullptr;
    //lookup the entry

    // llvm::Value *v = Builder.CreateGEP(TheVars, {c32(0), c32(hashTheVars[e])} , name_ptr );
    // return Builder.CreateLoad(v, name);

  }


private:
  const char * var;
  EntryType entry;
};

class BinOp : public Expr {
public:
  BinOp(Expr *l, const char *o, Expr *r) : left(l), op(o), right(r) {}
  ~BinOp() {
    delete left;
    delete right;
  }
  virtual void printOn(std::ostream &out) const override {
    out << "a" <<  op << "(" << *left << ", " << *right << ")";
  }

  virtual llvm::Value* compile() override {
    llvm::Value* l = left->compile();
    llvm::Value* r = right->compile();
    if(!strcmp(op, "+")) return Builder.CreateAdd(l, r, "addtmp");
    else if(!strcmp(op, "-")) return Builder.CreateSub(l, r, "subtmp");
    else if(!strcmp(op, "*")) return Builder.CreateMul(l, r, "multmp");
    else if(!strcmp(op, "/")) return Builder.CreateSDiv(l, r, "divtmp");
    else if(!strcmp(op, "mod")) return Builder.CreateSRem(l, r, "modtmp");
    else if(!strcmp(op, "and")) return Builder.CreateAnd(l, r, "andtmp"); // not sure yet if CreateAnd short circuits the logical expression
    else if(!strcmp(op, "or")) return Builder.CreateOr(l, r, "ortmp");
    else if(!strcmp(op, "=")) return Builder.CreateICmpEQ(l, r, "eqtmp");
    else if(!strcmp(op, "<=")) return Builder.CreateICmpSLE(l, r, "addtmp");
    else if(!strcmp(op, "<")) return Builder.CreateICmpSLT(l, r, "addtmp");
    else if(!strcmp(op, ">=")) return Builder.CreateICmpSGE(l, r, "addtmp");
    else if(!strcmp(op, ">")) return Builder.CreateICmpSGT(l, r, "addtmp");
    else if(!strcmp(op, "<>")) return Builder.CreateICmpNE(l, r, "neqtmp");
    else return nullptr;

  }

  virtual void sem() override {
    lval = false;
    if (!strcmp(op, "+") || !strcmp(op, "-") || !strcmp(op, "*") || !strcmp(op, "/") || !strcmp(op, "mod")){
      left->type_check(typeInteger);
      right->type_check(typeInteger);
      type = typeInteger;
    }
    else if (!strcmp(op, "=") || !strcmp(op, "<>") || !strcmp(op, "<") || !strcmp(op, ">") || !strcmp(op, "<=") || !strcmp(op, ">=")){
      left->sem();
      right->sem();
      if (!equalType(left->getType() , right->getType()) || !isBasicType(left->getType()) || !isBasicType(right->getType()))
        fatal("Operands must be an instance of same basic types");
      type = typeBoolean;
    }
    else if (!strcmp(op, "and") || !strcmp(op, "or")){
      left->type_check(typeBoolean);
      right->type_check(typeBoolean);
      type = typeBoolean;
    }
  }

private:
  Expr *left;
  const char *op;
  Expr *right;
};

class UnOp : public Expr {
public:
  UnOp(const char *o, Expr *e) : op(o), expr(e) {}
  ~UnOp() {
    delete expr;
  }
  virtual void printOn(std::ostream &out) const override {
    out << op << "(" << *expr << ")";
  }
  //virtual void compile() const override {};

  virtual void sem() override {
    lval = false;
    if ((strcmp(op, "+") == 0) || (strcmp(op, "-") == 0)) {
      expr->type_check(typeInteger);
      type = typeInteger;
    }
    else if ((strcmp(op, "not") == 0)) {
      expr->type_check(typeBoolean);
      type = typeBoolean;
    }
    else std::cout << "Aliens." << std::endl;
    stringExpr = OTHER;
  }

  virtual llvm::Value* compile() override {
    llvm::Value* operand = expr->compile();
    if(!strcmp(op, "+")) return operand;
    else if(!strcmp(op, "-")) return Builder.CreateMul(operand, c64(-1), "minus_sign_tmp");
    else return nullptr;

  }

private:
  const char *op; //operators can be multi-character
  Expr *expr;
};

class ListUnOp: public Expr {
public:
  ListUnOp(const char *o, Expr *e) : expr(e), op(o) {}
  ~ListUnOp() {}
  virtual void printOn(std::ostream &out) const override {
    out << "Unary list operator(" << op << "(" << *expr << "))" << std::endl;
  }
  virtual void sem() override {
    lval = false;
    expr->sem();
    if(expr->getType()->kind != TYPE_LIST) fatal("Operand is not a list");
    if(strcmp(op, "nil?") == 0) type = typeBoolean;
    else if(strcmp(op, "head") == 0) {
      if (isTypeAny(expr->getType()->refType)) fatal("Cannot apply head operator on empty list.");
      type = expr->getType()->refType;
     }
    else if(strcmp(op, "tail") == 0) {
      if (isTypeAny(expr->getType()->refType)) fatal("Cannot apply tail operator on empty list.");
      type = expr->getType();
    }
    else std::cout << " Aliens." << std::endl;
    stringExpr = OTHER;
  }
  virtual llvm::Value* compile() override {
    return nullptr;
  }
private:
  const char *op;
  Expr *expr;
};

class ListBinOp: public Expr {
public:
  ListBinOp(const char *o, Expr *e1, Expr *e2): op(o), expr1(e1), expr2(e2) {}
  ~ListBinOp() {}
  virtual void printOn(std::ostream &out) const override {
    out << "Binary list operator " << op << "(" << *expr1 << "," << *expr2 << ")" << std::endl;
  }
  virtual void sem() override {
    lval = false;
    expr1->sem();
    expr2->sem();
    if(expr2->getType()->kind != TYPE_LIST)  fatal("Operand 2 must be of type list");

    if(!equalType(expr1->getType(), expr2->getType()->refType)) fatal("Operands must be of same type");

    if (strcmp(op, "#") == 0) type = typeList(expr1->getType());
    else std::cout << "Aliens." << std::endl;
    stringExpr = OTHER;
  }
  virtual llvm::Value* compile() override {
    return nullptr;
  }
private:
  const char *op;
  Expr *expr1, *expr2;
};

class Nil: public Expr {
  public:
    Nil() {}
    ~Nil() {}
    virtual void printOn(std::ostream &out) const override {
      out << "list of any type" << std::endl;
    }
    virtual void sem() override {
      lval = false;
      type = typeList(typeAny);
      stringExpr = OTHER;
    }
    virtual llvm::Value* compile() override {
      return nullptr;
    }
  private:
};

class StringConst: public Expr {
  public:
    StringConst(const char * s) : stringconst(s) {}
    ~StringConst() {}
    virtual void printOn(std::ostream &out) const override {
      out << "Const string" << "(" << stringconst << ")";
    }
    //virtual void compile() const override {};
    virtual void sem() override {
      lval = false;
      size_t size = strlen(stringconst);
      type = typeIArray(typeChar); //Warning: may come back
      stringExpr = STRING;
    }
    virtual llvm::Value* compile() override {
      return nullptr;
    }
  private:
    const char * stringconst;
};

class Array: public Expr {
  public:
    Array(Type t, Expr *e) : arrayType(t), sizeExpr(e), arraySize(10) {}
    ~Array() {}

    virtual void printOn(std::ostream &out) const override {
      out << "Array of type [" << arrayType << "] with size " << arraySize << " induced by expression " << *sizeExpr;
    }

    virtual void sem() override {
      lval = false;
      sizeExpr->type_check(typeInteger);
      type = typeIArray(arrayType);
    }
    virtual llvm::Value* compile() override {
      return nullptr;
    }
  private:
    Type arrayType;
    Expr *sizeExpr;
    int arraySize;
};

class ArrayItem: public Expr{
  public:
    ArrayItem(Expr *a, Expr *e): atom(a), expr(e) {}
    ~ArrayItem() {}
    virtual void printOn(std::ostream &out) const override {
      out << "Item Array of type " << type << std::endl;
    }

    virtual void sem() override {
      expr->type_check(typeInteger);

      atom->sem();
      if (!isTypeArray(atom->getType())) fatal("Non array type is not subscritable");
      if (atom->getStringExpr() == STRING) stringExpr = STRING_ITEM;
      else stringExpr = OTHER;
      //std::cout << "Type of atom " << atom->getType() << std::endl;
      //std::cout << "ref type of " << atom->getType()->refType << std::endl;
      type = atom->getType()->refType;
      //std :: cout << type << std::endl;
      lval = true;
    }
    virtual llvm::Value* compile() override {
      return nullptr;
    }
  private:
    Expr *atom, *expr;
};

class CallExpr : public Expr {
public:
  CallExpr(Id *v): id(v), exprlist(NULL) {}
  CallExpr(Id *v, ExprList *e): id(v), exprlist(e) {}
  ~CallExpr() {
    exprlist->clear();
  }
  virtual void printOn(std::ostream &out) const override {
    out << "Call(" << *id << "with expressions";
    bool first = true;
    for (std::vector<Expr *>::iterator it = exprlist->begin(); it != exprlist->end(); ++it){
      if (!first) out << ", ";
      first = false;
      out << std::endl << **it;
    }
    out << std::endl << ")";
  }
  virtual void sem() override {
    lval = false;

    id->sem();
    EntryType entry = id->getEntryType();
    if (entry != ENTRY_FUNCTION) {
      fatal("Object %s is not callable", id->getIdName());
    }

    SymbolEntry *p = lookupEntry(id->getIdName(), LOOKUP_ALL_SCOPES, false);
    if (p->u.eFunction.isForward) fatal("Function needs to be defined before calling it.");
    type = p->u.eFunction.resultType;
    if (equalType(p->u.eFunction.resultType, typeVoid)) fatal("Call expression should not be of type Void.");

    SymbolEntry *args = p->u.eFunction.firstArgument;

    int exprsize;
    if (exprlist == NULL) exprsize = 0;
    else exprsize = exprlist->size();
    int argsize = 0;

    while (args != NULL) {
      argsize++;
      args = args->u.eParameter.next;
    }

    if (argsize != exprsize) {
      fatal("Expected %d arguments, but %d were given.", argsize, exprsize);
    }

    if (exprsize){
      int i = 0;
      ExprList reversed = *exprlist;
      std::reverse(reversed.begin(), reversed.end());
      args = p->u.eFunction.firstArgument;
      for (Expr *expr: reversed) {
        expr->sem();
        if (!equalType(expr->getType(), args->u.eParameter.type)){
          fatal("Wrong parameter type at position %d", i);
        }
        args = args->u.eParameter.next;
        i++;
      }
    }
  }
  virtual llvm::Value* compile() override {
    SymbolEntry *p = lookupEntry(id->getIdName(), LOOKUP_ALL_SCOPES, false);
    llvm::Function *calledFun = p->u.eFunction.llvmfun;
    std::vector<llvm::Value*> argv;
    argv.clear();
    if (exprlist == NULL) return Builder.CreateCall(calledFun, std::vector<llvm::Value*> {});
    ExprList reversed = *exprlist;
    std::reverse(reversed.begin(), reversed.end());
    for (Expr *expr: reversed) {
      argv.push_back(expr->compile());
    }
    return Builder.CreateCall(calledFun, argv);
  }
private:
  Id *id;
  ExprList *exprlist;
  Type t;
};

// STATEMENTS (e.g Let, If.. else ...)

class CallStmt: public Stmt{
  public:
    CallStmt(Id *v): id(v), exprlist(NULL) {}
    CallStmt(Id *v, ExprList *e): id(v), exprlist(e) {}
    ~CallStmt() {
      exprlist->clear();
    }
    virtual void printOn(std::ostream &out) const override {
      out << "CallStmt(" << *id << "with expressions";
      bool first = true;
      for (std::vector<Expr *>::iterator it = exprlist->begin(); it != exprlist->end(); ++it){
        if (!first) out << ", ";
        first = false;
        out << std::endl << **it;
      }
      out << std::endl << ")";
    }
    virtual void sem() override {
      setStmtType(SIMPLE_STMT);
      id->sem();
      EntryType entry = id->getEntryType();
      if (entry != ENTRY_FUNCTION) {
        fatal("Object %s is not callable", id->getIdName());
      }

      SymbolEntry *p = lookupEntry(id->getIdName(), LOOKUP_ALL_SCOPES, false);
      if (p->u.eFunction.isForward) fatal("Function needs to be defined before calling it.");
      if (!equalType(p->u.eFunction.resultType, typeVoid)) fatal("Call expression must of type Void.");

      SymbolEntry *args = p->u.eFunction.firstArgument;
      int argsize = 0 ;
      int exprsize;
      if (exprlist == NULL) exprsize = 0;
      else exprsize = exprlist->size();

      while (args != NULL) {
        argsize++;
        args = args->u.eParameter.next;
      }

      if (argsize != exprsize) {
        fatal("Expected %d arguments, but %d were given.", argsize, exprsize);
      }

      if (exprsize) {
        int i = 0;
        ExprList reversed = *exprlist;
        std::reverse(reversed.begin(), reversed.end());

        args = p->u.eFunction.firstArgument;
        for (Expr *expr: reversed) {
          expr->sem();
          if (!equalType(expr->getType(), args->u.eParameter.type)){
            fatal("Wrong parameter type at position %d.", i);
          }
          args = args->u.eParameter.next;
          i++;
        }
      }
    }

    virtual llvm::Value* compile() override {
      SymbolEntry *p = lookupEntry(id->getIdName(), LOOKUP_ALL_SCOPES, false);
      llvm::Function *calledFun = p->u.eFunction.llvmfun;
      std::vector<llvm::Value*> argv;
      argv.clear();
      if (exprlist == NULL) return Builder.CreateCall(calledFun, std::vector<llvm::Value*> {});
      ExprList reversed = *exprlist;
      std::reverse(reversed.begin(), reversed.end());
      for (Expr *expr: reversed) {
        argv.push_back(expr->compile());
      }
      Builder.CreateCall(calledFun, argv);
      return nullptr;
    }
  private:
    Id *id;
    ExprList *exprlist;
};

class Skip : public Stmt {
  public:
    Skip() {}
    ~Skip() {}
    virtual void printOn(std::ostream &out) const override {
      out << "Empty instruction" << std::endl;
    }
    virtual void sem() override { setStmtType(SIMPLE_STMT); }
    virtual llvm::Value* compile() override { return nullptr; }
};

class For : public Stmt {
public:
  For(StmtList *sl1, Expr *e, StmtList *sl2, StmtList *sl3 ):
  init_list(sl1), terminate_expr(e), next_list(sl2),  stmt_list(sl3) {}
  ~For() {
    init_list->clear();
    next_list->clear();
    stmt_list->clear();
  }
  virtual void printOn(std::ostream &out) const override {
    out << "For( " << std::endl << "Init statement(s) :" << std::endl;
    for (Stmt *stmt: *init_list) {
      out << *stmt;
    }
    out << "Loop condition: " << *terminate_expr;
    out << std::endl;
    out << "Loop update statement(s): ";
    for (Stmt *stmt: *next_list) {
      out << *stmt;
    }
    out << "Loop statement(s): ";
    for (Stmt *stmt: *stmt_list) {
      out << *stmt;
    }
    out << ")";
  }

  bool checkForExits() override {

    bool exitExists = false;
    for (Stmt *s: *stmt_list) {
      if(s->checkForExits()) {
        exitExists = true;
       }
    }

    return exitExists;
  }

  bool checkForReturns() override  {

     bool oneList = false;
     Type temp = typeVoid;

     for (Stmt *s: *stmt_list) {
       if(s->checkForReturns()) {
          setReturnType(s->getReturnType());
          if (!oneList) temp = s->getReturnType();
          else { if( !equalType(temp, s->getReturnType()) ) fatal("For statements cannot have returns of different types."); }
          oneList = true;
        }
     }

     return oneList;
  }

  virtual void sem() override {
      setStmtType(SIMPLE_STMT);
      for (Stmt *s: *init_list) s->sem();
      terminate_expr->type_check(typeBoolean);
      for (Stmt *s: *next_list) s->sem();
      for (Stmt *s: *stmt_list) {
        s->sem();
      }
  }

  virtual llvm::Value* compile() override {
    //emit initialization code
    /*unordered_map<Value *> init;
    Value *Init [init_list.size()]
    for (Stmt *s: *init_list) {
      Value *i = s->compile();
      if (i != nullptr)
        init.insert(pair<,>);
    }*/
    llvm::BasicBlock *PrevBB = Builder.GetInsertBlock();
    llvm::Function *TheFunction = PrevBB->getParent();
    llvm::BasicBlock *LoopBB =
      llvm::BasicBlock::Create(TheContext, "loop", TheFunction);
    llvm::BasicBlock *BodyBB =
      llvm::BasicBlock::Create(TheContext, "body", TheFunction);
    llvm::BasicBlock *AfterBB =
      llvm::BasicBlock::Create(TheContext, "endfor", TheFunction);
    Builder.CreateBr(LoopBB);
    Builder.SetInsertPoint(LoopBB);
    /*for (auto const& pair: init) {
      llvm::Type * a = getType(init.first);
      init.second;
      PHINode *phi_iter = Builder.CreatePHI();
    }*/
    // PHINode *phi_iter = Builder.CreatePHI(i64, 2, "iter");
    // phi_iter->addIncoming(n, PrevBB);
    llvm::Value *cond_value = terminate_expr->compile();
    llvm::Value *loop_cond =
      Builder.CreateICmpNE(cond_value, c1(1), "loop_cond");
    Builder.CreateCondBr(loop_cond, BodyBB, AfterBB);
    Builder.SetInsertPoint(BodyBB);
    // Value *remaining =
    //   Builder.CreateSub(phi_iter, c64(1), "remaining");
    for (Stmt *s: *stmt_list) s->compile();
    for (Stmt *s: *next_list) s->compile();
    // phi_iter->addIncoming(remaining, Builder.GetInsertBlock());
    Builder.CreateBr(LoopBB);
    Builder.SetInsertPoint(AfterBB);
    return nullptr;
  }

private:
  StmtList *init_list, *next_list, *stmt_list;
  Expr *terminate_expr;
};

typedef std::pair<Expr*, StmtList *> IfPair;
typedef std::vector<IfPair> IfPairList;

class If : public Stmt {
public:
  If(Expr *c1, StmtList* s1, IfPairList *elseIf , StmtList *Last ):
  elif(elseIf), s_last(Last)
  {
    full_list = new IfPairList();
    full_list = elif;

    IfPair  temp = std::make_pair(c1,s1);
    full_list->insert(full_list->begin(), temp);
  }
  ~If() {
    full_list->clear();
  }

  bool checkForExits() override {

    bool exitExists = false;
    for (IfPair cond_st: *full_list) {
      for (Stmt *s: *cond_st.second) {
        if(s->checkForExits()) {
           exitExists = true;
         }
      }
    }
    for (Stmt *s: *s_last) {
      if(s->checkForExits()) {
        exitExists = true;
       }
    }

    return exitExists;
  }

  bool checkForReturns() override  {

     bool atLeastOneInList = false, everyOne = true, oneList = false;
     Type temp = typeVoid;
     for (IfPair cond_st: *full_list) {
       atLeastOneInList = false;
       for (Stmt *s: *cond_st.second) {
         if(s->checkForReturns()) {
            setReturnType(s->getReturnType());
            if (!oneList) temp = s->getReturnType();
            else { if(!equalType(temp, s->getReturnType())) fatal("If statements cannot have returns of different types."); }
            oneList = true;
            atLeastOneInList = true;
          }
       }
       if(!atLeastOneInList) everyOne = false;
     }
     for (Stmt *s: *s_last) {
       atLeastOneInList = false;
       if(s->checkForReturns()) {
          atLeastOneInList = true;
          setReturnType(s->getReturnType());
          if (!oneList) temp = s->getReturnType();
          else { if( !equalType(temp, s->getReturnType()) ) fatal("If statements cannot have returns of different types."); }
          oneList = true;
        }
     }
     if(!atLeastOneInList) everyOne = false;

     if(everyOne) return true;
     else{
       if(oneList) {
         warning("There are condition-statements pairs that do not have a return");
         return true;
       }
       else{
         return false;
       }
     }
   }

  virtual void printOn(std::ostream &out) const override {
    out << "If(" << std::endl;
    bool isFirst = true;
    for (IfPair cond_st: *full_list) {
        if(isFirst) {
          out << "If with conditional: "<< *cond_st.first<< " statement(s): " << std::endl;
          for (Stmt *stmt: *cond_st.second) {
            out << *stmt;
          }
          isFirst = false;
        }
        else{
          out << "ElseIf conditional: " << *cond_st.first << " with statement(s): " << std::endl;
          for (Stmt *stmt: *cond_st.second) {
            out << *stmt;
          }
        }
    }
    out <<  ")";

  }
  virtual void sem() override {
    setStmtType(SIMPLE_STMT);
    for(IfPair a: *full_list) {
      Expr *e = a.first;
      e->sem();
      if (!equalType(e->getType(), typeBoolean)) fatal("Conditional should be of type bool");

      for (Stmt *s: *a.second) {
        s->sem();
      }
    }
    for (Stmt *s: *s_last) {
      s->sem();
    }
  }

  virtual llvm::Value* compile()  override {

    // number of if then pairs
    int n = full_list->size(), counter=0;

    // create after block. Should be inserted at the end of If_then_else block
    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(TheContext, "endif", TheFunction);
    llvm::BasicBlock *ElseIfBB;
    if (n > 1) {
      ElseIfBB = llvm::BasicBlock::Create(TheContext, "elseif", TheFunction);
    }
    llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(TheContext, "else", TheFunction);

    llvm::BasicBlock *CurrentBB;

    for(IfPair a: *full_list) {
      if (counter > 0)
        Builder.SetInsertPoint(CurrentBB);

      Expr *e = a.first;
      StmtList *s_list = a.second;
      llvm::Value *v = e->compile();
      llvm::Value *cond = Builder.CreateICmpNE(v, c1(0), "if_cond");
      llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

      llvm::BasicBlock *ThenBB =
        llvm::BasicBlock::Create(TheContext, "then", TheFunction);
      if (counter == n-1){
        Builder.CreateCondBr(cond, ThenBB, ElseBB);
      }
      else {
        Builder.CreateCondBr(cond, ThenBB, ElseIfBB);
        CurrentBB = ElseIfBB;
        if(counter < n-2)
        ElseIfBB = llvm::BasicBlock::Create(TheContext, "elseif", TheFunction);
      }

      Builder.SetInsertPoint(ThenBB);

      for(Stmt *s: *s_list) {
        if(s!= nullptr) s->compile();
      }
      Builder.CreateBr(AfterBB);
      counter++;
    }

    Builder.SetInsertPoint(ElseBB);
    for(Stmt *s: *s_last) {
      if(s!= nullptr) s->compile();
    }
    Builder.CreateBr(AfterBB);
    Builder.SetInsertPoint(AfterBB);

    return nullptr;

  }

private:
  IfPairList *elif, *full_list;
  StmtList *s_last;
};

class Let: public Stmt {
public:
  Let(Expr *v, Expr *e): var(v),  expr(e) {}
  ~Let() { delete expr; }
  virtual void printOn(std::ostream &out) const override {
    out << "Let(" << *var << " = " << *expr << ")" << std::endl;
  }
  virtual void sem() override {
    setStmtType(SIMPLE_STMT);
    var->sem();
    if (!var->isLValue()) fatal("Can't assign value to non lvalue");
    if (var->getStringExpr() == STRING_ITEM) fatal("Can't assign value to item of a constant string type object");
    expr->type_check(var->getType());
  }
  virtual llvm::Value* compile() override {
    var->setLeft();
    llvm::Value * alloc_tmp = var->compile();
    llvm::Value * val_tmp = expr->compile();
    Builder.CreateStore(val_tmp, alloc_tmp);
    return nullptr;
  }
private:
  Expr *var;
  Expr *expr;
};

class Return: public Stmt {
public:
  Return(Expr *expr) : returnExpr(expr) {}
  ~Return() {}
  virtual void printOn(std::ostream &out) const override {
    out << "Return expression " << *returnExpr << std::endl;
  }
  virtual void sem() override {
    setStmtType(RETURN);
    returnExpr->sem();
    setReturnType(returnExpr->getType());
  }
  virtual llvm::Value* compile() override {
    llvm::Value *RetValue = returnExpr->compile();
    Builder.CreateRet(RetValue);
    return nullptr;
   }


private:
  Expr *returnExpr;
};

class Exit: public Stmt {
  public:
    Exit() {};
    ~Exit() {};
    virtual void printOn(std::ostream &out) const override {
      out << "Exit" << std::endl;
    }
    virtual void sem() override {
      setStmtType(EXIT);
    }
    virtual llvm::Value* compile() override {
      Builder.CreateRetVoid();
      return nullptr;
    }
};

#endif
