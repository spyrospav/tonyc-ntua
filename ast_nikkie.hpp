#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <map>
#include <vector>

#include "symbol.hpp"

void yyerror(const char *msg);

inline std::ostream& operator<<(std::ostream &out, Type t) {
  switch (t) {
    case TYPE_int: out << "int"; break;
    case TYPE_bool: out << "bool"; break;
  }
  return out;
}

class AST {
public:
  virtual ~AST() {}
  virtual void printOn(std::ostream &out) const = 0;
  virtual void sem() {}
};

inline std::ostream& operator<< (std::ostream &out, const AST &t) {
  t.printOn(out);
  return out;
}

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

class Stmt: public AST {
public:
  virtual void run() const = 0;
};

extern std::vector<int> rt_stack;

class Id: public Expr {
public:
  Id(char v): var(v), offset(-1) {}
  virtual void printOn(std::ostream &out) const override {
    out << "Id(" << var << "@" << offset << ")";
  }
  virtual int eval() const override {
    return rt_stack[offset];
  }
  virtual void sem() override {
    SymbolEntry *e = st.lookup(var);
    type = e->type;
    offset = e->offset;
  }
private:
  char var;
  int offset;
};

class Const: public Expr {
public:
  Const(int n): num(n) {}
  virtual void printOn(std::ostream &out) const override {
    out << "Const(" << num << ")";
  }
  virtual int eval() const override {
    return num;
  }
  virtual void sem() override {
    type = TYPE_int;
  }
private:
  int num;
};

class BinOp: public Expr {
public:
  BinOp(Expr *l, char o, Expr *r): left(l), op(o), right(r) {}
  ~BinOp() { delete left; delete right; }
  virtual void printOn(std::ostream &out) const override {
    out << op << "(" << *left << ", " << *right << ")";
  }
  virtual int eval() const override {
    switch (op) {
      case '+': return left->eval() + right->eval();
      case '-': return left->eval() - right->eval();
      case '*': return left->eval() * right->eval();
      case '/': return left->eval() / right->eval();
      case '%': return left->eval() % right->eval();
      case '=': return left->eval() == right->eval();
      case '<': return left->eval() < right->eval();
      case '>': return left->eval() > right->eval();
    }
    return 0;  // this will never be reached.
  }
  virtual void sem() override {
    left->type_check(TYPE_int);
    right->type_check(TYPE_int);
    switch (op) {
      case '+': case '-': case '*': case '/': case '%':
        type = TYPE_int; break;
      case '=': case '<': case '>':
        type = TYPE_bool; break;
    }
  }
private:
  Expr *left;
  char op;
  Expr *right;
};

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

class Print: public Stmt {
public:
  Print(Expr *e): expr(e) {}
  ~Print() { delete expr; }
  virtual void printOn(std::ostream &out) const override {
    out << "Print(" << *expr << ")";
  }
  virtual void run() const override {
    std::cout << expr->eval() << std::endl;
  }
  virtual void sem() override {
    expr->type_check(TYPE_int);
  }
private:
  Expr *expr;
};

class If: public Stmt {
public:
  If(Expr *c, Stmt *s1, Stmt *s2 = nullptr):
    cond(c), stmt1(s1), stmt2(s2) {}
  ~If() { delete cond; delete stmt1; delete stmt2; }
  virtual void printOn(std::ostream &out) const override {
    out << "If(" << *cond << ", " << *stmt1;
    if (stmt2 != nullptr) out << ", " << *stmt2;
    out << ")";
  }
  virtual void run() const override {
    if (cond->eval())
      stmt1->run();
    else if (stmt2 != nullptr)
      stmt2->run();
  }
  virtual void sem() override {
    cond->type_check(TYPE_bool);
    stmt1->sem();
    if (stmt2 != nullptr) stmt2->sem();
  }
private:
  Expr *cond;
  Stmt *stmt1;
  Stmt *stmt2;
};

class For: public Stmt {
public:
  For(Expr *e, Stmt *s): expr(e), stmt(s) {}
  ~For() { delete expr; delete stmt; }
  virtual void printOn(std::ostream &out) const override {
    out << "For(" << *expr << ", " << *stmt << ")";
  }
  virtual void run() const override {
    for (int times = expr->eval(), i = 0; i < times; ++i)
      stmt->run();
  }
  virtual void sem() override {
    expr->type_check(TYPE_int);
    stmt->sem();
  }
private:
  Expr *expr;
  Stmt *stmt;
};

class Decl: public AST {
public:
  Decl(char c, Type t): var(c), type(t) {}
  virtual void printOn(std::ostream &out) const override {
    out << "Decl(" << var << " : " << type << ")";
  }
  virtual void sem() override {
    st.insert(var, type);
  }
private:
  char var;
  Type type;
};

class Block: public Stmt {
public:
  Block(): decl_list(), stmt_list(), size(0) {}
  ~Block() {
    for (Decl *d : decl_list) delete d;
    for (Stmt *s : stmt_list) delete s;
  }
  void append_decl(Decl *d) { decl_list.push_back(d); }
  void append_stmt(Stmt *s) { stmt_list.push_back(s); }
  void merge(Block *b) {
    stmt_list = b->stmt_list;
    b->stmt_list.clear();
    delete b;
  }
  virtual void printOn(std::ostream &out) const override {
    out << "Block(";
    bool first = true;
    for (Decl *d : decl_list) {
      if (!first) out << ", ";
      first = false;
      out << *d;
    }
    for (Stmt *s : stmt_list) {
      if (!first) out << ", ";
      first = false;
      out << *s;
    }
    out << ")";
  }
  virtual void run() const override {
    for (int i = 0; i < size; ++i) rt_stack.push_back(0);
    for (Stmt *s : stmt_list) s->run();
    for (int i = 0; i < size; ++i) rt_stack.pop_back();
  }
  virtual void sem() override {
    st.openScope();
    for (Decl *d : decl_list) d->sem();
    for (Stmt *s : stmt_list) s->sem();
    size = st.getSizeOfCurrentScope();
    st.closeScope();
  }
private:
  std::vector<Decl *> decl_list;
  std::vector<Stmt *> stmt_list;
  int size;
};

#endif
