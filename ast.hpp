#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <map>
#include <vector>
#include <cstring>
#include <stdio.h>

#include "symbol.h"
#include "error.h"

void yyerror(const char *msg);

inline std::ostream& operator<<(std::ostream &out, Type t) {
  int a;
  if (t == typeVoid) {
    a = 0;
  }
  else if (t == typeInteger){
    a = 1;
  }
  else if (t == typeBoolean) {
    a = 2;
  }
  else if (t == typeChar){
    a = 3;
  }
  else if (t->kind == TYPE_IARRAY) {
    a = 4;
  }
  switch (a) {
    case 0: out << "void"; break;
    case 1: out << "int";  break;
    case 2: out << "bool"; break;
    case 3: out << "char"; break;
    case 4: out << "array of type " << t->refType; break;
  }
  return out;
}

class AST {
  public:
    AST() {

    }
    virtual ~AST() {
      destroySymbolTable();
    }
    virtual void printOn(std::ostream &out) const = 0;
    virtual void sem() {}
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
    Type getType() {return type;}
    virtual void sem() override {
      for (int i = 0; i < var_list.size(); i++) {
        //std::cout << "trying to insert to symbol table! variable " << var_list[i] << std::endl;
        newVariable(var_list[i], type);
      }
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
    void sem_(SymbolEntry *p) {
      for (const char *name: var_list) newParameter(name, type, passmode, p);
    }
  private:
    PassMode passmode;
    std::vector<const char * > var_list;
    Type type;
};

typedef std::vector<Arg *> ArgList;

class Stmt: public AST {
};

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
    SymbolEntry * p = lookupEntry(name, LOOKUP_CURRENT_SCOPE, false);
    if (p == NULL) {
      p = newFunction(name);
      if (hdef == DECL) {
        forwardFunction(p);
      }
      openScope();
      printSymbolTable();
      for (Arg *a: *arg_list) { a->sem_(p); }
      endFunctionHeader(p, type);
      printSymbolTable();
      //closeScope();
    }
  }
private:
  Type type;
  const char * name;
  ArgList *arg_list;
  HeaderDef hdef;
};

enum DefType {
  FUNCTION,
  VARIABLE,
  DECLARATION
};

class FuncBlock: public Stmt {
public:
  FuncBlock(): var_list(), size(0), isMain(false) {}
  ~FuncBlock() {
    for (VarList *d : var_list) delete d;
  }

  void append_varlist(VarList *d) {
    std::cout << "Add var in block!" << std::endl;
    var_list.insert(var_list.begin(), d);
    sequence.insert(sequence.begin(), VARIABLE);
  }
  void append_fun(FuncBlock *f){
    std::cout << "Add func in block!" << std::endl;
    func_list.insert(func_list.begin(), f);
    sequence.insert(sequence.begin(), FUNCTION);
  }
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
    out << std::endl << ")";
  }

  virtual void sem() override {
    if (!isMain) header->sem();
    int v = 0, f = 0;
    for (std::vector<DefType>::iterator it = sequence.begin(); it < sequence.end(); it++){
      if (*it == FUNCTION) {
        func_list[f]->sem();
        f++;
      }
      else if (*it == VARIABLE) {
        var_list[v]->sem();
        v++;
      }
    }
    //for (Stmt *stmt : stmt_list) stmt->sem();
    printSymbolTable();
    closeScope();
  }

private:
  Header *header;
  std::vector<VarList *> var_list;
  std::vector<FuncBlock *> func_list;
  std::vector<Stmt *> stmt_list;
  std::vector<DefType> sequence;
  int size;
  bool isMain;
};

// EXPRESSIONS (e.g atoms, constants, operations applied to expressions )

class Expr : public AST {
  public:
    //virtual void compile() const = 0; UNCOMMENT WHEN COMPILE IS IMPLEMENTED ON other classes
    void type_check(Type t) {
      sem();
      if (!equalType(type, t))
        yyerror("Type mismatch");
    }
    bool isBasicType(Type t) {
      return (equalType(t, typeChar) || equalType(t, typeBoolean) || equalType(t, typeInteger));
    }
    Type getType() { return type; }
  protected:
    Type type;
};

class Atom : public Expr {
  public:


};

class IntConst : public Expr {
  IntConst(int n) : num(n) {}
  virtual void printOn(std::ostream &out) const override {
    out << "IntConst(" << num << ")";
  }
  /*
  virtual void compile() const override {
    std::cout << "  pushl $" << num << "\n";
  }
  */
  virtual void sem() override { type = typeInteger; }

  private:
    int num;
};

class CharConst : public Expr {
  CharConst(char c) : character(c) {}
  virtual void printOn(std::ostream &out) const override {
    out << "CharConst(" << character << ")";
  }
  /*
  virtual void compile() const override {
    std::cout << "  pushl $" << num << "\n";
  }
  */
  virtual void sem() override { type = typeChar; }

  private:
    char character;
};

class BoolConst : public Expr {
  BoolConst(const char * s) {
    if (s == "false") logic = false;
    else if (s == "true") logic = true;
  }
  virtual void printOn(std::ostream &out) const override {
    out << "Logic(" << logic << ")";
  }
  /*
  virtual void compile() const override {
    std::cout << "  pushl $" << num << "\n";
  }
  */
  virtual void sem() override { type = typeBoolean; }

  private:
    bool logic;
};

class Id : public Expr {
public:
  Id(const char * v) : var(v) {}
  virtual void printOn(std::ostream &out) const override {
    out << "Id(" << var << "@" << ")";
  }
  /*
  virtual void compile() const override {
    if (nestingDiff == 0)
      // Local variable.
      std::cout << "  pushl " << 4 * offset << "(%ebp)\n";
    else {
      // Non-local variable; follow nestingDiff access links.
      std::cout << "  movl 0(%ebp), %esi\n";
      for (int i = 1; i < nestingDiff; ++i)
        std::cout << "  movl 0(%esi), %esi\n";
      std::cout << "  pushl " << 4 * offset << "(%esi)\n";
    }
  }
  */
  virtual void sem() override {
    SymbolEntry *e = lookupEntry(var,LOOKUP_CURRENT_SCOPE, false);
    type = e->u.eVariable.type;
  }

private:
  const char * var;
};

class BinOp : public Expr {
public:
  BinOp(Expr *l, const char *o, Expr *r) : left(l), op(o), right(r) {}
  ~BinOp() {
    delete left;
    delete right;
  }
  virtual void printOn(std::ostream &out) const override {
    out << op << "(" << *left << ", " << *right << ")";
  }
  /*
  virtual void compile() const override {
    left->compile();
    right->compile();
    std::cout << "  popl %ebx\n"  // right
              << "  popl %eax\n"; // left
    switch (op) {
    case '+':
      std::cout << "  addl %ebx, %eax\n"
                << "  pushl %eax\n";
      break;
    case '-':
      std::cout << "  subl %ebx, %eax\n"
                << "  pushl %eax\n";
      break;
    case '*':
      std::cout << "  mull %ebx\n"
                << "  pushl %eax\n";
      break;
    case '/':
      std::cout << "  cdq\n"
                << "  divl %ebx\n"
                << "  pushl %eax\n";
      break;
    case '%':
      std::cout << "  cdq\n"
                << "  divl %ebx\n"
                << "  pushl %edx\n";
      break;
    }
  }
  */
  virtual void sem() override {
    if (strcmp(op, "+") || strcmp(op, "-") || strcmp(op, "*") || strcmp(op, "/") || strcmp(op, "mod")){
      left->type_check(typeInteger);
      right->type_check(typeInteger);
      type = typeInteger;
    }
    else if (strcmp(op, "=") || strcmp(op, "<>") || strcmp(op, "<") || strcmp(op, ">") || strcmp(op, "<=") || strcmp(op, ">=")){
      if (!equalType(left->getType(), right->getType()) || !isBasicType(left->getType()) || isBasicType(right->getType()))
        yyerror("Operands must be an instance of same basic types");
      type = typeBoolean;
    }
    else if (strcmp(op, "and") || strcmp(op, "or")){
      left->type_check(typeBoolean);
      right->type_check(typeBoolean);
      type = typeBoolean;
    }
  }

private:
  Expr *left;
  const char *op; //operators can be multi-character
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
    if (strcmp(op, "=") || strcmp(op, "-")) {
      expr->type_check(typeInteger);
      type = typeInteger;
    }
    else if (strcmp(op, "not")) {
      expr->type_check(typeBoolean);
      type=typeBoolean;
    }
  }

private:
  const char *op; //operators can be multi-character
  Expr *expr;
  Type type;
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
      size_t size = strlen(stringconst);
      type = typeArray(size, typeChar);
    }
  private:
    const char * stringconst;
};

// STATEMENTS (e.g Let, If.. else ...)

class Let: public Stmt {
public:
  Let(char v, Expr *e): var(v), offset(-1), expr(e) {}
  ~Let() { delete expr; }
  virtual void printOn(std::ostream &out) const override {
    out << "Let(" << var << " = " << *expr << ")";
  }
  virtual void run() const override {
    rt_stack[offset] = expr->eval();
  }
  virtual void sem() override {
    SymbolEntry *lhs = st.lookup(var);
    expr->type_check(lhs->type);
    offset = lhs->offset;
  }
private:
  char var;
  int offset;
  Expr *expr;
};


#endif
