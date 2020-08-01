#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <map>
#include <vector>

#include "symbol.hpp"

void yyerror(const char *msg);

inline std::ostream& operator<<(std::ostream &out, Type t) {
  switch (t->kind) {
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
    virtual void printOn(std::ostream &out) const override {
      out << "Var(s)(";
      for (int i = 0; i < var_list.size()-1; i++)
        out << var_list[i] << ", ";
      out << var_list[var_list.size()-1];
      out << " with Type " << type;
    }
    void var_append(std::string d) { var_list.push_back(d); }
    void var_type(Type t) { type = t; }
    virtual void sem() override {
      for (int i = 0; i < var_list.size(); i++)
        newVariable(var_list[i].c_str(), type);
    }
    private:
      std::vector<std::string> var_list;
      Type type;
};

class Stmt: public AST {
};

class Block: public Stmt {
public:
  Block(): var_list(), size(0) {}
  ~Block() {
    for (Var *d : var_list) delete d;
  }
  void append_var(Var *d) { var_list.push_back(d); }


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
    openScope();
    for (Var *v : var_list) v->sem();
    closeScope();
  }
private:
  std::vector<Var *> var_list;
  int size;
};

#endif
