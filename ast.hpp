#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <map>
#include <vector>

#include "symbol.h"

void yyerror(const char *msg);

inline std::ostream& operator<<(std::ostream &out, Type t) {
  int a;
  if (t == typeInteger) {
    a = 1;
  }
  else if (t == typeBoolean) {
    a = 2;
  }
  else {
    a = 3;
  }
  switch (a) {
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

class Var: public AST {
  public:
    Var() : var_list(), type(NULL) {};
    ~Var() {
      var_list.clear();
    }
    virtual void printOn(std::ostream &out) const override {
      out << "Var(s)(";
      for (int i = 0; i < var_list.size()-1; i++){
        std::string s = var_list[i];
        out << s << ", ";
      }
      out << var_list[var_list.size()-1];
      out << ")" << " with Type " << type << std::endl;;
    }
    void var_append(const char* d) { var_list.push_back(d); }
    void var_type(Type t) { type = t; }
    std::vector<const char * > getVarList() {return var_list;}
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
    Arg(PassMode pass, Var *v): passmode(pass)
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
      out << ")" << " with Type " << type << " and pass by " << passmode << std::endl;;
    }
  private:
    PassMode passmode;
    std::vector<const char * > var_list;
    Type type;
};

typedef std::vector<Arg *> Arg_List;

class Stmt: public AST {
};

class Block: public Stmt {
public:
  Block(): var_list(), size(0) {}
  ~Block() {
    for (Var *d : var_list) delete d;
  }
  void append_var(Var *d) { std::cout << "Add var in block!" << std::endl; var_list.push_back(d); }

  virtual void printOn(std::ostream &out) const override {
    out << "Block(";
    bool first = true;
    for (Var *d : var_list) {
      if (!first) out << ", ";
      first = false;
      out << *d;
    }
    out << ")";
  }

  virtual void sem() override {
    std::cout << "open in block" << std::endl;
    openScope();
    for (Var *v : var_list) v->sem();
    closeScope();
  }
private:
  std::vector<Var *> var_list;
  int size;
};

#endif
