#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <map>
#include <vector>

#include "symbol.hpp"

void yyerror(const char *msg);

inline std::ostream& operator<<(std::ostream &out, Type t) {
  switch (t) {
    case typeInteger: out << "int";  break;
    case typeBoolean: out << "bool"; break;
    case typeChar:    out << "char"; break;
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
}

class Var: public AST {
  public:
    Var() : var_list(), type(NULL);
    virtual void printOn(std::ostream &out) const override {
      for (int i = 0; i < var_list.size(); i++)
        out << "Var(" << var_list[i] << " : " << type << ")";
    }
    void var_append(Decl *d) { var_list.push_back(d); }
    void var_type(Type t) { type = t; }
    virtual void sem() override {
      for (int i = 0; i < var_list.size(); i++)
        newVariable(var_list[i], type);
    }
    private:
      std::vector<std::string> var_list;
      Type type;
}

#endif
