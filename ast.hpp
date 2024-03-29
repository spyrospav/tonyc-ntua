#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <map>
#include <vector>
#include <cstring>
#include <stdio.h>
#include <algorithm>
#include <set>
#include <utility>
#include <string.h>

#include "symbol.h"
#include "error.h"

/*------------------- LLVM ------------------- */
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>

#include <llvm/Support/Alignment.h>

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

/*
 * Statements types :
 *  Simple (For, If, Assignment)
 *  Return
 *  Exit
 */
enum StmtType{
  SIMPLE_STMT,
  RETURN,
  EXIT
  //COND_RETURN
};

/*
 *
 */
enum StringExpr{
  STRING,
  STRING_ITEM,
  OTHER
};


class AST {
  public:
    AST() { }
    virtual ~AST() {
      destroySymbolTable();
    }
    virtual void printOn(std::ostream &out) const = 0;
    virtual llvm::Value* compile() { return nullptr; }
    virtual void sem() {}
    void llvm_compile_and_dump(bool doOptimize=false) {

      // Initialize TheModule
      TheModule = std::make_unique<llvm::Module>("Tony program", TheContext);

      // Initialize Function Pass Manager
      TheFPM = std::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());

      // add optimization Functions to FPM
      // doOptimize = True -> Run the following optimization passess
      if (doOptimize) {
        TheFPM->add(llvm::createPromoteMemoryToRegisterPass());
        TheFPM->add(llvm::createInstructionCombiningPass());
        TheFPM->add(llvm::createReassociatePass());
        TheFPM->add(llvm::createGVNPass());
        TheFPM->add(llvm::createCFGSimplificationPass());
      }
      TheFPM->doInitialization();

      // Initialize types
      // i1: Boolean, i8: Char, i32: Main return, i64: Integer
      i1 = llvm::IntegerType::get(TheContext, 1);
      i8 = llvm::IntegerType::get(TheContext, 8);
      i16 = llvm::IntegerType::get(TheContext, 16);
      i32 = llvm::IntegerType::get(TheContext, 32);
      i64 = llvm::IntegerType::get(TheContext, 64);

      // Node type : {i64, *i64}
      llvm::StructType *NodeType = llvm::StructType::create(TheContext, "nodetype");
      NodeType->setBody({i64, llvm::PointerType::get(NodeType, 0)});
      TheListType = llvm::PointerType::get(NodeType, 0);

      // Initialize global variables (e.g newline)
      llvm::ArrayType *nl_type = llvm::ArrayType::get(i8, 2); //create llvm array type of nl (it should have 2 items with 8 bits each (characters))
      TheNL = new llvm::GlobalVariable(
          *TheModule, nl_type, true, llvm::GlobalValue::PrivateLinkage,
          llvm::ConstantArray::get(nl_type, {c8('\n'), c8('\0')}), "nl"
      );
      TheNL->setAlignment(llvm::MaybeAlign(1));

      // Initialize abenetopoulos library functions (https://github.com/abenetopoulos/edsger_lib)

      /*---------------- puti ----------------*/
      llvm::FunctionType *writeInteger_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(TheContext),
        {i64},
        false
      );

      TheWriteInteger = llvm::Function::Create(
        writeInteger_type,
        llvm::Function::ExternalLinkage,
        "puti",
        TheModule.get()
      );

      SymbolEntry *p = lookupEntry("puti", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheWriteInteger;

      /*---------------- putc ----------------*/
      llvm::FunctionType *writeCharacter_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(TheContext),
        {i8},
        false
      );

      TheWriteCharacter = llvm::Function::Create(
        writeCharacter_type,
        llvm::Function::ExternalLinkage,
        "putc",
        TheModule.get()
      );

      p = lookupEntry("putc", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheWriteCharacter;

      /*---------------- putb ----------------*/
      llvm::FunctionType *writeBoolean_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(TheContext),
        {i1},
        false
      );

      TheWriteBoolean = llvm::Function::Create(
        writeBoolean_type,
        llvm::Function::ExternalLinkage,
        "putb",
        TheModule.get()
      );

      p = lookupEntry("putb", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheWriteBoolean;

      /*---------------- puts ----------------*/
      llvm::FunctionType *writeString_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(TheContext),
        {llvm::PointerType::get(i8, 0)},
        false
      );

      TheWriteString = llvm::Function::Create(
        writeString_type,
        llvm::Function::ExternalLinkage,
        "puts",
        TheModule.get());

      p = lookupEntry("puts", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheWriteString;

      /*---------------- geti ----------------*/
      std::vector<llvm::Type *> t;

      llvm::FunctionType *readInteger_type = llvm::FunctionType::get(
        i64,
        std::vector<llvm::Type *> {},
        false
      );

      TheReadInteger = llvm::Function::Create(
        readInteger_type,
        llvm::Function::ExternalLinkage,
        "geti",
        TheModule.get()
      );

      p = lookupEntry("geti", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheReadInteger;

      /*---------------- getc ----------------*/
      llvm::FunctionType *readCharacter_type = llvm::FunctionType::get(
        i8,
        std::vector<llvm::Type *> {},
        false
      );

      TheReadCharacter = llvm::Function::Create(
        readCharacter_type,
        llvm::Function::ExternalLinkage,
        "getc",
        TheModule.get()
      );

      p = lookupEntry("getc", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheReadCharacter;

      /*---------------- getb ----------------*/
      llvm::FunctionType *readBoolean_type = llvm::FunctionType::get(
        i1,
        std::vector<llvm::Type *>{},
        false
      );

      TheReadBoolean = llvm::Function::Create(
        readBoolean_type,
        llvm::Function::ExternalLinkage,
        "getb",
        TheModule.get()
      );

      p = lookupEntry("getb", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheReadBoolean;

      /*---------------- gets ----------------*/
      llvm::FunctionType *readString_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(TheContext),
        {i64, llvm::PointerType::get(i8, 0)},
        false
      );

      TheReadString = llvm::Function::Create(
        readString_type,
        llvm::Function::ExternalLinkage,
        "gets",
        TheModule.get()
      );

      p = lookupEntry("gets", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheReadString;

      /*---------------- abs ----------------*/
      llvm::FunctionType *abs_type = llvm::FunctionType::get(
        i64,
        {i64},
        false
      );

      TheAbs = llvm::Function::Create(
        abs_type,
        llvm::Function::ExternalLinkage,
        "abs",
        TheModule.get()
      );

      p = lookupEntry("abs", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheAbs;

      /*---------------- ord ----------------*/
      llvm::FunctionType *ord_type = llvm::FunctionType::get(
        i64,
        {i8},
        false
      );

      TheOrd = llvm::Function::Create(
        ord_type,
        llvm::Function::ExternalLinkage,
        "ord",
        TheModule.get()
      );

      p = lookupEntry("ord", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheOrd;

      /*---------------- chr ----------------*/
      llvm::FunctionType *chr_type = llvm::FunctionType::get(
        i8,
        {i64},
        false
      );

      TheChr = llvm::Function::Create(
        chr_type,
        llvm::Function::ExternalLinkage,
        "chr",
        TheModule.get()
      );

      p = lookupEntry("chr", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheChr;

      /*---------------- strlen ----------------*/
      llvm::FunctionType *strlen_type = llvm::FunctionType::get(
        i64,
        {llvm::PointerType::get(i8, 0)},
        false
      );

      TheStrLen = llvm::Function::Create(
        strlen_type,
        llvm::Function::ExternalLinkage,
        "strlen",
        TheModule.get()
      );

      p = lookupEntry("strlen", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheStrLen;

      /*---------------- strcmp ----------------*/
      llvm::FunctionType *strcmp_type = llvm::FunctionType::get(
        i64,
        {llvm::PointerType::get(i8, 0),
        llvm::PointerType::get(i8, 0)},
        false
      );

      TheStrCmp =
        llvm::Function::Create(
        strcmp_type,
        llvm::Function::ExternalLinkage,
        "strcmp",
        TheModule.get()
      );

      p = lookupEntry("strcmp", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheStrCmp;

      /*---------------- strcpy ----------------*/
      llvm::FunctionType *strcpy_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(TheContext),
        {llvm::PointerType::get(i8, 0),
        llvm::PointerType::get(i8, 0)},
        false
      );

      TheStrCpy = llvm::Function::Create(
        strcpy_type,
        llvm::Function::ExternalLinkage,
        "strcpy",
        TheModule.get()
      );

      p = lookupEntry("strcpy", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheStrCpy;

      /*---------------- strcat ----------------*/
      TheStrCat = llvm::Function::Create(
        strcpy_type,
        llvm::Function::ExternalLinkage,
        "strcat",
        TheModule.get()
      );

      p = lookupEntry("strcat", LOOKUP_ALL_SCOPES, false);
      p->u.eFunction.llvmfun = TheStrCat;

      /*---------------- GC_malloc ----------------*/
      llvm::FunctionType *malloc_type = llvm::FunctionType::get(
        llvm::PointerType::get(i8, 0),
        {i64},
        false
      );

      TheMalloc = llvm::Function::Create(
        malloc_type,
        llvm::Function::ExternalLinkage,
        "GC_malloc",
        TheModule.get()
      );

      /*---------------- GC_init ----------------*/
      llvm::FunctionType *init_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(TheContext),
        {},
        false
      );

      TheInit = llvm::Function::Create(
        init_type,
        llvm::Function::ExternalLinkage,
        "GC_init",
        TheModule.get()
      );

      // Compile code
      compile();

      // Verify Module
      bool bad = llvm::verifyModule(*TheModule, &llvm::errs());
      if (bad) {
        std::cerr << "The IR is bad!" << std::endl;
        TheModule->print(llvm::errs(), nullptr);
        std::exit(1);
      }

      // Emit IR code
      TheModule->print(llvm::outs(), nullptr);
    }

    // Converts Type to llvm::Type
    llvm::Type * getLLVMType(Type type) const{
      llvm::Type * retType, * tmp;
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
          case TYPE_IARRAY:
              retType = llvm::PointerType::get(getLLVMType(type->refType), 0);
              break;
          case TYPE_LIST:
              retType = TheListType;
              break;
          case TYPE_ANY:
              retType = nullptr;
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
    static llvm::PointerType *TheListType;


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

    static llvm::Function *TheInit;
    static llvm::Function *TheMalloc;
};

inline std::ostream& operator<< (std::ostream &out, const AST &t) {
  t.printOn(out);
  return out;
};

// Local variable definitions
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

// Functions' arguments
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
    PassMode getPassMode() { return passmode; }
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

// Needed for expressions like (int a,b; char c; bool d,e,f)
typedef std::vector<Arg *> ArgList;

enum HeaderDef {
  DECL, // declaration
  DEF // definitions
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
    std::reverse(arg_list->begin(), arg_list->end());
    SymbolEntry * p;
    if (strcmp(name, "malloc") == 0)
      fatal("Cannot declare a function with name \"malloc\".");
    p = newFunction(name);
    if (hdef == DECL) {
      forwardFunction(p);
    }
    openScope();
    for (Arg *a: *arg_list) { a->sem(p); }
    endFunctionHeader(p, type);
  }

  void setLiveVariables(std::vector< std::pair <std::string, Type> > s) {
    live_vars = s;
    // for (int i = 0; i < live_vars.size(); i++) {
    //   std::cout << live_vars[i].first << std::endl;
    // }
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
        if (a->getPassMode() == PASS_BY_REFERENCE) tempType = llvm::PointerType::get(tempType, 0);
        argTypes.push_back(tempType);
      }
    }
    // std::cout << "in function: " << this->name << std::endl;
    for (int i = 0; i < live_vars.size(); i++) {
      // std::cout << live_vars[i].first << ", " << live_vars[i].second << std::endl;
      newParameter(live_vars[i].first.c_str(), live_vars[i].second, PASS_BY_REFERENCE, p);
      // std::cout << "after newparameter" << std::endl;
      llvm::Type *tempType = this->getLLVMType(live_vars[i].second);
      argTypes.push_back(llvm::PointerType::get(tempType, 0));
    }


    if (p->u.eFunction.llvmfun != nullptr) {
      endFunctionHeader(p, type);
      return p->u.eFunction.llvmfun;
    }


    llvm::FunctionType * FT = llvm::FunctionType::get(
      getLLVMType(type),
      argTypes,
      false
    );

    llvm::Function * Function = llvm::Function::Create(
      FT,
      llvm::Function::ExternalLinkage,
      name,
      TheModule.get()
    );

    std::vector<const char *> names;

    for (Arg *a: *arg_list) {
      std::vector<const char *> tmp = a->getArgListNames();
      names.insert(names.end(), tmp.begin(), tmp.end());
    }

    for (int i = 0; i < live_vars.size(); i++) {
      names.insert(names.end(), live_vars[i].first.c_str());
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
  std::vector< std::pair <std::string, Type> > live_vars;
};

// Abstract definition of statements
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
    else {
      openScope();
    }

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
    int counter = 1;
    if (stmt_list != NULL) {
      for (Stmt *stmt : *stmt_list) {
        stmt->sem();
        if(stmt->checkForReturns()){
          if(stmt->checkForExits() && header->getHeaderType() != typeVoid)
            fatal("Exit can only be used inside void function blocks");
          existsReturn = true;
          stmt->checkReturnType(header);
        }
      }
    }

    if(header->getHeaderDef() == DEF && !existsReturn && !equalType(header->getHeaderType(), typeVoid)) {
      fatal("Non void function must have a return statement.");
    }

    for (auto it = liveVariables.begin(); it != liveVariables.end(); ++it) {
      std::string s {(*it)->id};
      if ((*it)->entryType == ENTRY_VARIABLE) live_vars.push_back(std::make_pair(s, (*it)->u.eVariable.type));
      else if ((*it)->entryType == ENTRY_PARAMETER) live_vars.push_back(std::make_pair(s, (*it)->u.eParameter.type));
      else std::cout << "aliens" << std::endl;
    }

    //printSymbolTable();
    closeScope();

    header->setLiveVariables(live_vars);

  }

  virtual llvm::Value* compile() override {

    if (!isMain) {
      thisFunction = header->compilef();
    }
    else {

      openScope();
      llvm::FunctionType *FT = llvm::FunctionType::get(i32, {}, false );
      thisFunction = llvm::Function::Create(
        FT,
        llvm::Function::ExternalLinkage,
        "main",
        TheModule.get()
      );
    }

    llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", thisFunction);
    Builder.SetInsertPoint(BB);

    if (isMain) {
      Builder.CreateCall(TheInit, {});
    }

    for (auto &arg : thisFunction->args()) {
      std::string name = arg.getName().str();
      llvm::AllocaInst *alloca = Builder.CreateAlloca(arg.getType(), nullptr, name);
      Builder.CreateStore(&arg, alloca);
      SymbolEntry * e = lookupEntry(name.c_str(), LOOKUP_CURRENT_SCOPE, false);
      e->allocainst = alloca;
    }

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
            e->allocainst = alloca_temp;
        }
        v++;
      }
      else if (*it == DECLARATION) {
        decl_list[d]->compile();
        d++;
        // Builder.SetInsertPoint(BB);
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
    closeScope();

    if (isMain) {
      Builder.CreateRet(c32(0));
    }
    else {
      // Return statement produce by class Return for non-main function
      if(equalType(header->getHeaderType(), typeVoid)) {
        // function returns void
        if (Builder.GetInsertBlock()->getTerminator()) {
          Builder.SetInsertPoint(
            llvm::BasicBlock::Create(TheContext, "lone_exit", Builder.GetInsertBlock()->getParent())
          );
        }
        Builder.CreateRetVoid();
      }
      else {
        if (!Builder.GetInsertBlock()->getTerminator()) {
          //function is non void, thus this is not reachable
          Builder.CreateUnreachable();
        }
      }
    }

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
  std::vector< std::pair <std::string, Type> > live_vars;
};

// Expressions (e.g atoms, constants, operations applied to expressions )

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
    void setLeft() { realLeft = true; }
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
    bool realLeft = false;
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
    virtual void sem() override { lval = false; stringExpr = OTHER; type = typeChar; }
    virtual llvm::Value* compile() override {
      return c8(character); }

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
    if (e==NULL) { fatal("Id \"%s\" has not been declared", var); }
    entry = e->entryType;
    if (entry == ENTRY_VARIABLE) {
      addLiveVariable(e);
      type = e->u.eVariable.type;
    }
    else if (entry == ENTRY_FUNCTION) {
      type = e->u.eFunction.resultType;
      lval = false;
    }
    else if (entry == ENTRY_PARAMETER) {
      addLiveVariable(e);
      type = e->u.eParameter.type;
    }
    stringExpr = OTHER;
  }
  virtual llvm::Value* compile() override {
    SymbolEntry *e = lookupEntry(var,LOOKUP_ALL_SCOPES, false);
    entry = e->entryType;
    if (entry == ENTRY_VARIABLE) {
      if (!realLeft) return Builder.CreateLoad(e->allocainst, var);
      else return e->allocainst;
    }
    else if (entry == ENTRY_FUNCTION) {
      return e->u.eFunction.llvmfun;
    }
    else if (entry == ENTRY_PARAMETER) {

      if (e->u.eParameter.mode == PASS_BY_REFERENCE) {
        if (!realLeft)
          return Builder.CreateLoad(Builder.CreateLoad(e->allocainst, var), var);
        else return Builder.CreateLoad(e->allocainst, var);
      }
      if (!realLeft)
        return Builder.CreateLoad(e->allocainst, var);
      else return e->allocainst;
    }

    return nullptr;
  }


private:
  const char * var;
  EntryType entry;
};

class BinOp : public Expr {
public:
  BinOp(Expr *l, const char *o, Expr *r) : left(l), op(o), right(r) {
    canBeShortCircuited = !strcmp(op, "and") || !strcmp(op, "or");
  }
  ~BinOp() {
    delete left;
    delete right;
  }
  virtual void printOn(std::ostream &out) const override {
    out << op << "(" << *left << ", " << *right << ")";
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

  virtual llvm::Value* compile() override {
    if(!canBeShortCircuited){
      llvm::Value* l = left->compile();
      llvm::Value* r = right->compile();
      if(!strcmp(op, "+")) return Builder.CreateAdd(l, r, "addtmp");
      else if(!strcmp(op, "-")) return Builder.CreateSub(l, r, "subtmp");
      else if(!strcmp(op, "*")) return Builder.CreateMul(l, r, "multmp");
      else if(!strcmp(op, "/")) return Builder.CreateSDiv(l, r, "divtmp");
      else if(!strcmp(op, "mod")) return Builder.CreateSRem(l, r, "modtmp");
      else if(!strcmp(op, "=")) return Builder.CreateICmpEQ(l, r, "eqtmp");
      else if(!strcmp(op, "<=")) return Builder.CreateICmpSLE(l, r, "letmp");
      else if(!strcmp(op, "<")) return Builder.CreateICmpSLT(l, r, "ltmp");
      else if(!strcmp(op, ">=")) return Builder.CreateICmpSGE(l, r, "getmp");
      else if(!strcmp(op, ">")) return Builder.CreateICmpSGT(l, r, "gtmp");
      else if(!strcmp(op, "<>")) return Builder.CreateICmpNE(l, r, "neqtmp");
    }

    else {

      if(!strcmp(op, "or")) {

        llvm::Value* l = left->compile();

        llvm::BasicBlock *L_Calc_BB = Builder.GetInsertBlock();
        llvm::Function *TheFunction = L_Calc_BB->getParent();
        llvm::BasicBlock *Or_Calc_BB =
          llvm::BasicBlock::Create(TheContext, "or_calc", TheFunction);
        llvm::BasicBlock *R_Calc_BB =
          llvm::BasicBlock::Create(TheContext, "r_calc", TheFunction);

        llvm::Value* tru_val = c1(int(true));
        llvm::Value* doShortCirc = Builder.CreateICmpEQ(l, tru_val, "ortmp");
        Builder.CreateCondBr(doShortCirc, Or_Calc_BB, R_Calc_BB);

        Builder.SetInsertPoint(R_Calc_BB);

        llvm::Value* r = right->compile();
        llvm::Value* or_res =  r;
        R_Calc_BB = Builder.GetInsertBlock();
        Builder.CreateBr(Or_Calc_BB);

        Builder.SetInsertPoint(Or_Calc_BB);
        llvm::PHINode *phi_iter = Builder.CreatePHI(i1, 2, "iter");
        phi_iter->addIncoming(or_res, R_Calc_BB);
        phi_iter->addIncoming(tru_val, L_Calc_BB);
        return phi_iter;

      }
      else if(!strcmp(op, "and")) {

        llvm::Value* l = left->compile();

        llvm::BasicBlock *L_Calc_BB = Builder.GetInsertBlock();
        llvm::Function *TheFunction = L_Calc_BB->getParent();
        llvm::BasicBlock *And_Calc_BB =
          llvm::BasicBlock::Create(TheContext, "and_calc", TheFunction);
        llvm::BasicBlock *R_Calc_BB =
          llvm::BasicBlock::Create(TheContext, "r_calc", TheFunction);

        llvm::Value* fls_val = c1(int(false));
        llvm::Value* doShortCirc = Builder.CreateICmpEQ(l, fls_val, "andtmp");
        Builder.CreateCondBr(doShortCirc, And_Calc_BB, R_Calc_BB);

        Builder.SetInsertPoint(R_Calc_BB);

        llvm::Value* r = right->compile();
        llvm::Value* and_res =  r;
        R_Calc_BB = Builder.GetInsertBlock();
        Builder.CreateBr(And_Calc_BB);

        Builder.SetInsertPoint(And_Calc_BB);
        llvm::PHINode *phi_iter = Builder.CreatePHI(i1, 2, "iter");
        phi_iter->addIncoming(and_res, R_Calc_BB);
        phi_iter->addIncoming(fls_val, L_Calc_BB);
        return phi_iter;

      }
    }

    return nullptr;

  }

private:
  Expr *left;
  const char *op;
  Expr *right;
  bool canBeShortCircuited;
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
    else if(!strcmp(op, "not")) return Builder.CreateNot(operand, "not_value");
    return nullptr;
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
    if(strcmp(op, "nil?") == 0){ type = typeBoolean; }
    else if(strcmp(op, "head") == 0) {
      if (isTypeAny(expr->getType()->refType)) fatal("Cannot apply head operator on empty list.");
      type = expr->getType()->refType;
     }
    else if(strcmp(op, "tail") == 0) {
      if (isTypeAny(expr->getType()->refType)) fatal("Cannot apply tail operator on empty list.");
      type = expr->getType();
    }
    stringExpr = OTHER;
  }
  virtual llvm::Value* compile() override {
    llvm::Value *v = expr->compile();
    if(strcmp(op, "nil?") == 0) {
      return Builder.CreateICmpEQ(v, llvm::ConstantPointerNull::get(TheListType), "eqtmp");
    }
    else if(strcmp(op, "head") == 0) {
      llvm::Value *h = Builder.CreateGEP(v, {c32(0), c32(0)}, "headptr");
      if ((type->kind != TYPE_LIST) && (type->kind != TYPE_IARRAY)) {
        return  Builder.CreateTrunc(Builder.CreateLoad(h, "headtmp"), getLLVMType(type), "head");
      }
      else {
        return Builder.CreateIntToPtr(Builder.CreateLoad(h, "head"), getLLVMType(type));
      }
    }
    else if(strcmp(op, "tail") == 0) {
      llvm::Value *t = Builder.CreateGEP(v, {c32(0), c32(1)}, "tailptr");
      return Builder.CreateLoad(t, "tail");
    }
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
    llvm::Value *l = expr1->compile();
    llvm::Value *r = expr2->compile();
    llvm::Value *alloca = Builder.CreateCall(TheMalloc, {c64(16)}, "l");
    llvm::Value *n = Builder.CreateBitCast(alloca, TheListType, "nodetmp");
    llvm::Value *h = Builder.CreateGEP(n, {c32(0), c32(0)}, "headptr");
    if ((expr1->getType()->kind != TYPE_LIST) && (expr1->getType()->kind != TYPE_IARRAY)) {
      llvm::Value *l_cast = Builder.CreateZExt(l, i64, "headcast");
      Builder.CreateStore(l_cast, h);
    }
    else {
      llvm::Value *l_cast = Builder.CreatePtrToInt(l, i64, "headcast");
      Builder.CreateStore(l_cast, h);
    }
    llvm::Value *t = Builder.CreateGEP(n, {c32(0), c32(1)}, "tailptr");
    Builder.CreateStore(r, t);
    return n;

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
      return llvm::ConstantPointerNull::get(TheListType);
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
    virtual void sem() override {
      lval = false;
      size_t size = strlen(stringconst);
      type = typeIArray(typeChar); //Warning: may come back
      stringExpr = STRING;
    }
    virtual llvm::Value* compile() override {
      llvm::Value *temp = Builder.CreateGlobalStringPtr(stringconst);
      return temp;
    }
  private:
    const char * stringconst;
};

class Array: public Expr {
  public:
    Array(Type t, Expr *e) : arrayType(t), sizeExpr(e), arraySize(-1) {}
    ~Array() {}

    virtual void printOn(std::ostream &out) const override {
      out << "Array of type [" << arrayType << "] with size " << arraySize << " induced by expression " << *sizeExpr;
    }

    virtual void sem() override {
      lval = false;
      sizeExpr->type_check(typeInteger);
      type = typeIArray(arrayType);
      // Check if sizeExpr can be evaluated -> mini interpreter for that
      // If we can't, throw fatal/error
    }

    virtual llvm::Value* compile() override {
      llvm::Value *r = sizeExpr->compile();
      llvm::Value *rsize = Builder.CreateMul(r, c64(sizeOfType(arrayType)), "arraysize");
      llvm::Value *alloca = Builder.CreateCall(TheMalloc, {rsize}, "arraytmp");
      llvm::Value *n = Builder.CreateBitCast(alloca, llvm::PointerType::get(getLLVMType(arrayType), 0), "arrayptr");
      return n;
    }

  private:
    Type arrayType;
    Expr *sizeExpr;
    int arraySize;
};

class ArrayItem: public Expr{
  public:
    ArrayItem(Expr *a, Expr *e): arr(a), expr(e) {}
    ~ArrayItem() {}

    virtual void printOn(std::ostream &out) const override {
      out << "Item Array of type " << type << std::endl;
    }

    virtual void sem() override {
      expr->type_check(typeInteger);
      arr->sem();
      if (!isTypeArray(arr->getType())) fatal("Non array type is not subscritable");
      if (arr->getStringExpr() == STRING) stringExpr = STRING_ITEM;
      else stringExpr = OTHER;
      type = arr->getType()->refType;
      lval = true;
    }
    virtual llvm::Value* compile() override {
      llvm::Value *arrptr = arr->compile();
      llvm::Value *index = expr->compile();
      llvm::Value* n = Builder.CreateInBoundsGEP(arrptr, index, "idx");
      if (realLeft) return n;
      else return Builder.CreateLoad(n, "arrayitem");
    }
  private:
    Expr *arr, *expr;
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
    stringExpr = OTHER;
    id->sem();
    EntryType entry = id->getEntryType();
    if (entry != ENTRY_FUNCTION) {
      fatal("Object %s is not callable", id->getIdName());
    }

    SymbolEntry *p = lookupEntry(id->getIdName(), LOOKUP_ALL_SCOPES, false);
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
      args = p->u.eFunction.firstArgument;
      for (Expr *expr: reversed) {
        expr->sem();
        if (args->u.eParameter.mode == PASS_BY_REFERENCE && !expr->isLValue()) {
          fatal("Cannot pass by reference a non lvalue");
        }
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
    SymbolEntry *args = p->u.eFunction.firstArgument;
    if (exprlist == NULL && p->u.eFunction.firstArgument == NULL) return Builder.CreateCall(calledFun, std::vector<llvm::Value*> {});
    if (exprlist != NULL) {
      ExprList reversed = *exprlist;
      args = p->u.eFunction.firstArgument;
      for (Expr *expr: reversed) {
        if (args->u.eParameter.mode == PASS_BY_REFERENCE) {
          expr->setLeft();
        }
        llvm::Value * v = expr->compile();
        argv.push_back(v);
        args = args->u.eParameter.next;
      }
    }


    //std::cout << id->getIdName() << std::endl;
    while (1) {
      if (args != NULL) {
        SymbolEntry *pp = lookupEntry(args->id, LOOKUP_ALL_SCOPES, false);
        llvm::Value * v;
        if (pp->entryType == ENTRY_VARIABLE) {
          v = pp->allocainst;
        }
        else if (pp->entryType == ENTRY_PARAMETER) {
          if (pp->u.eParameter.mode == PASS_BY_REFERENCE)
            v = Builder.CreateLoad(pp->allocainst, pp->id);
          else v = pp->allocainst;
        }
        argv.push_back(v);
        args = args->u.eParameter.next;
      }
      else break;
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
        args = p->u.eFunction.firstArgument;
        for (Expr *expr: reversed) {
          expr->sem();
          if (args->u.eParameter.mode == PASS_BY_REFERENCE && !expr->isLValue()) {
            fatal("Cannot pass by reference a non lvalue");
          }
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
      SymbolEntry *args = p->u.eFunction.firstArgument;
      if (exprlist == NULL && p->u.eFunction.firstArgument == NULL) return Builder.CreateCall(calledFun, std::vector<llvm::Value*> {});
      if (exprlist != NULL) {
        ExprList reversed = *exprlist;
        args = p->u.eFunction.firstArgument;
        for (Expr *expr: reversed) {
          if (args->u.eParameter.mode == PASS_BY_REFERENCE) {
            expr->setLeft();
          }
          llvm::Value * v = expr->compile();
          argv.push_back(v);
          args = args->u.eParameter.next;
        }
      }

      while (1) {
        if (args != NULL) {
          SymbolEntry *pp = lookupEntry(args->id, LOOKUP_ALL_SCOPES, false);
          llvm::Value * v;
          if (pp->entryType == ENTRY_VARIABLE) {
            v = pp->allocainst;
          }
          else if (pp->entryType == ENTRY_PARAMETER) {
            if (pp->u.eParameter.mode == PASS_BY_REFERENCE)
              v = Builder.CreateLoad(pp->allocainst, pp->id);
            else v = pp->allocainst;
          }
          argv.push_back(v);
          args = args->u.eParameter.next;
        }
        else break;
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

    llvm::BasicBlock *PrevBB = Builder.GetInsertBlock();
    Builder.SetInsertPoint(PrevBB);
    for (Stmt *s: *init_list) s->compile();
    llvm::Function *TheFunction = PrevBB->getParent();
    llvm::BasicBlock *LoopBB =
      llvm::BasicBlock::Create(TheContext, "loop", TheFunction);
    llvm::BasicBlock *BodyBB =
      llvm::BasicBlock::Create(TheContext, "body", TheFunction);
    llvm::BasicBlock *AfterBB =
      llvm::BasicBlock::Create(TheContext, "endfor", TheFunction);
    Builder.CreateBr(LoopBB);
    Builder.SetInsertPoint(LoopBB);

    llvm::Value *cond_value = terminate_expr->compile();
    Builder.CreateCondBr(cond_value, BodyBB, AfterBB);
    Builder.SetInsertPoint(BodyBB);
    for (Stmt *s: *stmt_list) s->compile();
    for (Stmt *s: *next_list) s->compile();
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

    int n = full_list->size(), counter=0;

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
      if (!Builder.GetInsertBlock()->getTerminator()) {
        Builder.CreateBr(AfterBB);
      }
      counter++;
    }

    Builder.SetInsertPoint(ElseBB);
    for(Stmt *s: *s_last) {
      if(s!= nullptr) s->compile();
    }
    if (!Builder.GetInsertBlock()->getTerminator()) {
      Builder.CreateBr(AfterBB);
    }
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
    var->setLeft();
  }
  virtual llvm::Value* compile() override {
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
    if (Builder.GetInsertBlock()->getTerminator()) {
      Builder.SetInsertPoint(
        llvm::BasicBlock::Create(TheContext, "lone_ret", Builder.GetInsertBlock()->getParent())
      );
    }
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
      if (Builder.GetInsertBlock()->getTerminator()) {
        Builder.SetInsertPoint(
          llvm::BasicBlock::Create(TheContext, "lone_exit", Builder.GetInsertBlock()->getParent())
        );
      }
      Builder.CreateRetVoid();
      return nullptr;
    }
};

#endif
