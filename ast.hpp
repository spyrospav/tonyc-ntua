#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <map>
#include <vector>

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
  switch (a) {
    case 0: out << "void"; break;
    case 1: out << "int";  break;
    case 2: out << "bool"; break;
    case 3: out << "char"; break;
  }
  return out;
}

class AST {
  public:
    AST() {
      initSymbolTable(1024);
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

class Expr: public AST {
public:
  virtual int eval() const = 0;
  void type_check(Type t) {
    sem();
    if (type != t) yyerror("Type mismatch");
  }
protected:
  Type type;
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
        std::cout << "trying to insert to symbol table! variable " << var_list[i] << std::endl;
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
    SymbolEntry * p = newFunction(name);
    if (hdef == DECL) {
      forwardFunction(p);
    }
    openScope();
    for (Arg *a: *arg_list) { a->sem_(p); }
    endFunctionHeader(p, type);
    closeScope();
  }

private:
  Type type;
  const char * name;
  ArgList *arg_list;
  HeaderDef hdef;
};

class FuncBlock: public Stmt {
public:
  FuncBlock(): var_list(), size(0) {}
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
    std::cout << "open in block" << std::endl;
    openScope();
    //sem for current function block
    //header->sem();

    //sem for variables in block and also for children function definition
    int v = 0, f = 0;
    std::cout << "Seq size " << sequence.size() << std::endl;
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
    closeScope();
  }
private:
  Header *header;
  std::vector<Stmt *> def_list;
  std::vector<VarList *> var_list;
  std::vector<FuncBlock *> func_list;
  std::vector<Stmt *> stmt_list;

  enum DefType {
    FUNCTION,
    VARIABLE,
    DECLARATION
  };
  std::vector<DefType> sequence;
  int size;
};

#endif
