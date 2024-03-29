%{
#include <stdio.h>
#include "lexer.hpp"
#include <string>
#include "ast.hpp"
#include "ast.cpp"
#include "symbol.h"
#include "general.h"

%}


%token T_bool       "bool"
%token T_char       "char"
%token T_decl       "decl"
%token T_def        "def"
%token T_else       "else"
%token T_elsif      "elsif"
%token T_end        "end"
%token T_exit       "exit"
%token T_for        "for"
%token T_head       "head"
%token T_if         "if"
%token T_int        "int"
%token T_list       "list"
%token T_new        "new"
%token T_nil        "nil"
%token T_nil_quest  "nil?"
%token T_ref        "ref"
%token T_return     "return"
%token T_skip       "skip"
%token T_tail       "tail"
%token T_not        "not"
%token T_and        "and"
%token T_or         "or"
%token T_assign     ":="
%token T_neq        "<>"
%token T_leq        "<="
%token T_geq        ">="

%token<logic> T_false      "false"
%token<logic> T_true       "true"
%token<var> T_id
%token<num> T_int_const
%token<char_const> T_char_const
%token<string_const> T_string_const

%token T_plus_sign   "+"
%token T_minus_sign  "-"

%left<op>               T_or
%left<op>               T_and
%nonassoc<op>           T_not
%nonassoc<op>           T_head T_nil_quest T_tail
%nonassoc<op>           T_geq T_leq '>' '<' '=' T_neq
%right<op>              '#'
%left<op>               '+' '-'
%left<op>               '*' '/' T_mod
%nonassoc<op>           T_plus_sign T_minus_sign

%union {
  FuncBlock *funcblock;
  Stmt *stmt;
  Expr *expr;
  ExprList *exprlist;
  VarList *varlist;
  StmtList *stmtlist;
  const char *var;
  char *string_const;
  char char_const;
  bool logic;
  int num;
  const char *op;
  bool b;
  Type type;
  Arg *arg;
  ArgList *arglist;
  Header *header;
  CallStmt *callstmt;
  CallExpr *callexpr;
  IfPairList *ifpairlist;
  Decl *decl;
}

%type<funcblock> program func-def-list func-def
%type<decl> func-decl
%type<stmt>  stmt simple if-stmt
%type<expr>  expr atom
%type<type>  type
%type<varlist> id-list var-def
%type<arglist> formal-list formal-list-plus
%type<arg> formal
%type<header> header
//%type<op> math-op comp-op logic-op sign
%type<exprlist> expr-list-plus expr-list
%type<callstmt> call-stmt
%type<callexpr> call-expr
%type<stmtlist> stmt-list-plus simple-list simple-list-plus else-stmt
%type<ifpairlist> elif-stmt


%%


program:
  { initSymbolTable(2048); openScope(); StandardLibraryInit(); }
    func-def {
      $2->setMain(); $2->sem();
      /* codegen  */
      destroySymbolTable();
      #ifndef SEM
      initSymbolTable(2048);
      openScope();
      StandardLibraryInit();
      /* std::cout << "Starting codegen " << std::endl; */
      $2->llvm_compile_and_dump(doOptimize);
      closeScope();
      destroySymbolTable();
      #endif
  }
;

func-def:
  T_def header ':' func-def-list stmt-list-plus T_end {
    $$ = $4; $2->setHeaderDef(DEF); $$->assignHeader($2); $$->append_stmtlist($5);
    //std::cout << *$$ << std::endl;
  }
;

func-def-list:
    /* nothing */ { $$ = new FuncBlock(); }
  | func-def func-def-list { $2->append_fun($1); $$ = $2; }
  | func-decl func-def-list { $2->append_decl($1); $$ = $2; }
  | var-def func-def-list { $2->append_varlist($1);  $$ = $2; }
;

header:
    type T_id '(' formal-list ')' { $$ = new Header($1, $2, $4); }
  | T_id '(' formal-list ')' { $$ = new Header(typeVoid, $1, $3); }
;

formal-list:
    /* nothing */ { $$ = new ArgList(); }
  | formal formal-list-plus { $2->push_back($1); $$ = $2; }
;

formal:
    "ref" var-def { $$ = new Arg(PASS_BY_REFERENCE, $2); }
  | var-def { $$ = new Arg(PASS_BY_VALUE, $1); }
;

formal-list-plus:
    /* nothing */ { $$ = new ArgList(); }
  | ';' formal formal-list-plus { $3->push_back($2); $$ = $3; }
;

type: "int" { $$ = typeInteger; }
  | "bool"  { $$ = typeBoolean; }
  | "char" { $$ = typeChar; }
  | type '[' ']' {$$ = typeIArray($1); }
  | "list" '[' type ']' { $$ = typeList($3); }
;

func-decl: "decl" header { $2->setHeaderDef(DECL); $$ = new Decl($2); }//std::cout << *$$ <<std::endl; std::cout << "aliens" << std::endl; }
;

var-def: type T_id id-list { $3->var_append($2); $3->var_type($1); $$ = $3; }
;

id-list:
    /* nothing */ { $$ = new VarList();}
  | ',' T_id id-list { $3->var_append($2); $$ = $3; }
;

stmt-list-plus:
    stmt { $$ = new StmtList(); $$->insert($$->begin(), $1); }
  | stmt stmt-list-plus { $2->insert($2->begin(), $1); $$ = $2; }
;

stmt:
    simple { $$ = $1;}
  | "exit" { $$ = new Exit(); }
  | "return" expr { $$ = new Return($2); }
  | if-stmt { $$ = $1;}
  | "for" simple-list ';' expr ';' simple-list ':' stmt-list-plus "end" { $$ = new For($2, $4, $6, $8); }
  ;

if-stmt:
    "if" expr ':' stmt-list-plus elif-stmt else-stmt "end" { $$ = new If($2, $4, $5, $6); }
;

elif-stmt:
    /* nothing */ {$$ = new IfPairList();}
  | "elsif" expr ':' stmt-list-plus elif-stmt { $5->insert($5->begin(), std::make_pair($2, $4)); $$ = $5; }
;

else-stmt:
    /* nothing */ { $$ = new StmtList(); }
  | "else" ':' stmt-list-plus { $$ = $3; }
;

simple:
    atom ":=" expr { $$ = new Let($1, $3); }
  | "skip" { $$ = new Skip(); }
  | call-stmt { $$ = $1; }
;

simple-list:
    simple simple-list-plus { $2->insert($2->begin(), $1); $$ = $2; }
;

simple-list-plus:
    /* nothing */ { $$ = new StmtList(); }
  | ',' simple-list { $$ = $2; }
;

call-stmt:
    T_id '(' expr-list ')' { $$ = new CallStmt(new Id($1), $3); }
  | T_id '('')' { $$ = new CallStmt(new Id($1)); }
;

call-expr:
    T_id '(' expr-list ')' { $$ = new CallExpr(new Id($1), $3); }
  | T_id '('')' { $$ = new CallExpr(new Id($1)); }
;

expr-list:
    expr expr-list-plus { $2->insert($2->begin(), $1); $$ = $2; }
;

expr-list-plus:
    /* nothing */{ $$ = new ExprList();}
  | ',' expr expr-list-plus { $3->insert($3->begin(), $2); $$ = $3;}
;

atom:
    T_id { $$ = new Id($1); }
  | atom '[' expr ']' { $$ = new ArrayItem($1, $3); }
  | T_string_const { $$ = new StringConst($1); }
  | call-expr { $$ = $1; }
;

expr:
    atom { $$ = $1; }
  | T_int_const { $$ = new IntConst($1); }
  | T_char_const { $$ = new CharConst($1); }
  | '(' expr ')' { $$ = $2; }
  | '+' expr %prec T_plus_sign { $$ = new UnOp($1, $2); }
  | '-' expr %prec T_minus_sign { $$ = new UnOp($1, $2); }
  | expr '+' expr { $$ = new BinOp($1, $2, $3); }
  | expr '-' expr { $$ = new BinOp($1, $2, $3); }
  | expr '*' expr { $$ = new BinOp($1, $2, $3); }
  | expr '/' expr { $$ = new BinOp($1, $2, $3); }
  | expr T_mod expr { $$ = new BinOp($1, $2, $3); }
  | "true" { $$ = new BoolConst(true); }
  | "false" { $$ = new BoolConst(false); }
  | T_not expr {$$ = new UnOp("not", $2); }
  | expr "and" expr { $$ = new BinOp($1, $2, $3); }
  | expr "or" expr { $$ = new BinOp($1, $2, $3); }
  | expr '=' expr { $$ = new BinOp($1, $2, $3); }
  | expr ">=" expr { $$ = new BinOp($1, $2, $3); }
  | expr "<=" expr { $$ = new BinOp($1, $2, $3); }
  | expr '>' expr { $$ = new BinOp($1, $2, $3); }
  | expr '<' expr { $$ = new BinOp($1, $2, $3); }
  | expr "<>" expr { $$ = new BinOp($1, $2, $3); }
  | "new" type '[' expr ']' { $$ = new Array($2, $4); }
  | T_nil_quest '(' expr ')' {  $$ = new ListUnOp("nil?", $3); }
  | "nil" { $$ = new Nil(); }
  | expr '#' expr { $$ = new ListBinOp($2, $1, $3); }
  | "head" '(' expr ')' {$$ = new ListUnOp("head", $3); }
  | "tail" '(' expr ')' { $$ = new ListUnOp("tail", $3); }
;

%%

int main(int argc, char *argv[]){

  //std::cout << argc << std::endl;
  if(argc>1) {
    if (!(strcmp(argv[1], "-O")))
      doOptimize = true;
    else {
      std::cout << "Wrong arguments given" << std::endl;
      exit(1);
    }
  }
  else
    doOptimize = false;

  int result = yyparse();

  //if (result == 0) printf("Success.\n");
  return result;
}
