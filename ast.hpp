#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <map>
#include <vector>
#include <cstring>
#include <stdio.h>
#include <algorithm>
//#include <utility>

#include "symbol.h"
#include "error.h"

void yyerror(const char *msg);

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
  }
  return out;
}

enum StmtType{
  SIMPLE_STMT,
  RETURN,
  EXIT
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
    void sem(SymbolEntry *p) {
      for (const char *name: var_list) newParameter(name, type, passmode, p);
    }
  private:
    PassMode passmode;
    std::vector<const char * > var_list;
    Type type;
};

typedef std::vector<Arg *> ArgList;

class Stmt: public AST {
  public:
    virtual Type getReturnType() { return typeVoid; }
    void setStmtType(StmtType s) { stmttype = s; }
    StmtType getStmtType() { return stmttype; }
  private:
    StmtType stmttype;
};

typedef std::vector<Stmt *> StmtList;

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
    printSymbolTable();
    for (Arg *a: *arg_list) { a->sem(p); }
    endFunctionHeader(p, type);
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
  void append_stmtlist(StmtList *stmtl) { stmt_list = stmtl; }

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
    std::cout << *header;
    printSymbolTable();
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
        if(stmt->getStmtType() == EXIT && header->getHeaderType() != typeVoid)
          fatal("Exit can only be used inside void function blocks");
        if(stmt->getStmtType() == RETURN){
          existsReturn = true;
          if(!equalType(header->getHeaderType(), stmt->getReturnType()))
            fatal("Return type does not match function type");
        }
      }
    }
    if(header->getHeaderDef() == DEF && !existsReturn && !equalType(header->getHeaderType(), typeVoid)) {
      fatal("Non void function must have a return statement.");
    }
    printSymbolTable();
    closeScope();
  }

private:
  Header *header;
  std::vector<VarList *> var_list;
  std::vector<FuncBlock *> func_list;
  std::vector<Decl *> decl_list;
  StmtList *stmt_list;
  std::vector<DefType> sequence;
  int size;
  bool isMain;
};

// EXPRESSIONS (e.g atoms, constants, operations applied to expressions )

class Expr : public AST {
  public:
    //virtual void compile() const = 0; UNCOMMENT WHEN COMPILE IS IMPLEMENTED ON other classes
    void type_check(Type t) { //
      sem();
      if (!equalType(type, t))
        fatal("Type mismatch");
    }
    bool isBasicType(Type t) {
      return (equalType(t, typeChar) || equalType(t, typeBoolean) || equalType(t, typeInteger));
    }
    Type getType() { return type; }
    bool isLValue() { return lval; }
    StringExpr getStringExpr() { return stringExpr; }
  protected:
    Type type;
    bool lval;
     // enumeration of valid types for a expression.
     // used to throw error on cases like "test"[0] := 4
    StringExpr stringExpr;
};

typedef std::vector<Expr *> ExprList;

class Atom : public Expr {
  public:
};

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
  const char *getIdName() { return var; }
  EntryType getEntryType() {
    return entry;
  }
  virtual void sem() override {
    lval = true;
    printSymbolTable();
    SymbolEntry *e = lookupEntry(var,LOOKUP_ALL_SCOPES, false);
    if (e==NULL) { fatal("Id has not been declared"); }
    entry = e->entryType;
    if (entry == ENTRY_VARIABLE ) {
      type = e->u.eVariable.type;
    }
    else if (entry == ENTRY_FUNCTION ) {
      type = e->u.eFunction.resultType;
    }
    else if (entry == ENTRY_PARAMETER ) {
      type = e->u.eParameter.type;
    }
    stringExpr = OTHER;
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
    lval = false;
    if (!strcmp(op, "+") || !strcmp(op, "-") || !strcmp(op, "*") || !strcmp(op, "/") || !strcmp(op, "mod")){
      left->sem();
      right->sem();
      std::cout << " left operand " << *left << " operator " << op << " right operand " << *right << std::endl;
      left->type_check(typeInteger);
      right->type_check(typeInteger);
      type = typeInteger;
    }
    else if (!strcmp(op, "=") || !strcmp(op, "<>") || !strcmp(op, "<") || !strcmp(op, ">") || !strcmp(op, "<=") || !strcmp(op, ">=")){
      left->sem();
      right->sem();
      std::cout << " left operand " << *left << " operator " << op << " right operand " << *right << std::endl;
      if (!equalType(left->getType() , right->getType()) || !isBasicType(left->getType()) || !isBasicType(right->getType()))
        fatal("Operands must be an instance of same basic types");
      type = typeBoolean;
    }
    else if (!strcmp(op, "and") || !strcmp(op, "or")){
      left->sem();
      right->sem();
      std::cout << " left operand " << *left << " operator " << op << " right operand " << *right << std::endl;
      std::cout << " with types " << left->getType() << ", " << right->getType() << std::endl;
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
    if (!strcmp(op, "=") || !strcmp(op, "-")) {
      expr->type_check(typeInteger);
      type = typeInteger;
    }
    else if (!strcmp(op, "not")) {
      expr->type_check(typeBoolean);
      type=typeBoolean;
    }
    stringExpr = OTHER;
  }

private:
  const char *op; //operators can be multi-character
  Expr *expr;
  Type type;
};

class ListUnOp: public Expr {
public:
  ListUnOp(const char *o, Expr *e) : expr(e), op(o) {}
  ~ListUnOp() {}
  virtual void printOn(std::ostream &out) const override {
    out << "Unary list operator( " << op << "( " << *expr << " ) " << std::endl;
  }
  virtual void sem() override {
    lval = false;
    expr->sem();
    if(expr->getType()->kind != TYPE_LIST) fatal("Operand is not a list");
    if(strcmp(op, "nil?") == 0) type = typeBoolean;
    else if(strcmp(op, "head") == 0) {
      //std::cout << expr->getType() << std::endl;
      //std::cout << typeList(typeAny) << std::endl;
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
    int exprsize = exprlist->size();
    int argsize = 0 ;

    while (args != NULL) {
      argsize++;
      args = args->u.eParameter.next;
    }

    if (argsize != exprsize) {
      fatal("Expected %d arguments, but %d were given.", argsize, exprsize);
    }

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
      int exprsize = exprlist->size();

      while (args != NULL) {
        argsize++;
        args = args->u.eParameter.next;
      }

      if (argsize != exprsize) {
        fatal("Expected %d arguments, but %d were given.", argsize, exprsize);
      }

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

  virtual void sem() override {
      setStmtType(SIMPLE_STMT);
      for (Stmt *s: *init_list) s->sem();
      terminate_expr->sem();
      for (Stmt *s: *next_list) s->sem();
      for (Stmt *s: *stmt_list) s->sem();

      if (!equalType(terminate_expr->getType(),typeBoolean)) {
        fatal("Terminate expression in for loop should be of type Bool");
      }
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

private:
  IfPairList *elif, *full_list;
  StmtList *s_last;
};

class Let: public Stmt {
public:
  Let(Expr *v, Expr *e): var(v),  expr(e) {}
  /*
  Let(Call *call, Expr *e) {
    fatal("Cannot assign to non l-value");
  }
  Let(StringConst *s, Expr *e) {
    fatal("Cannot assign to non l-value");
  }
  */
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
private:
  Expr *var;
  Expr *expr;
};

class Return: public Stmt {
public:
  Return(Expr *expr) : returnExpr(expr) {}
  ~Return() {}
  Type getReturnType() override { return returnType; }
  virtual void printOn(std::ostream &out) const override {
    out << "Return expression " << *returnExpr << std::endl;
  }
  virtual void sem() override {
    std::cout << "We returning" << std::endl;
    setStmtType(RETURN);
    returnExpr->sem();
    returnType = returnExpr->getType();
  }
private:
  Expr *returnExpr;
  Type returnType;
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
};

#endif
