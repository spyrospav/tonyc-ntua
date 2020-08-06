%{
#include <stdio.h>
#include "lexer.hpp"
#include <string>
#include "ast.hpp"
#include "symbol.h"

bool first = true;
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

%left<op>               T_and
%nonassoc<op>           T_not
%left<op>               T_or

%left<op>               '*'
%left<op>               '/'
%left<op>               '+'
%left<op>               '-'
%right<op>              '#'
%left<op> T_mod         "mod"

%nonassoc<op>           T_assign
%nonassoc<op>           T_neq
%nonassoc<op>           T_leq
%nonassoc<op>           T_geq

%union {
  FuncBlock *funcblock;
  Stmt *stmt;
  Expr *expr;
  VarList *varlist;
  const char *var;
  char *string_const;
  char char_const;
  bool logic;
  int num;
  char *op;
  bool b;
  Type type;
  Arg *arg;
  ArgList *arglist;
  Header *header;

}

%type<funcblock> program func-def-list func-def func-decl
%type<stmt>  stmt
%type<expr>  expr
%type<type>  type
%type<varlist> id-list var-def
%type<arglist> formal-list formal-list-plus
%type<arg> formal
%type<header> header
%type<op> math-op comp-op logic-op sign
%%


program:
  { initSymbolTable(1024); openScope(); printSymbolTable(); }
    func-def { $2->setMain(); std::cout << *$2; $2->sem(); }
;

func-def:
  //T_def header ':' func-def-list stmt-list-plus T_end { }
  T_def header ':' func-def-list T_end {
    $$ = $4; $2->setHeaderDef(DEF); $$->assignHeader($2);
    std::cout << *$$ << std::endl;
  }
;

func-def-list:
    /* nothing */ { $$ = new FuncBlock(); }
  | func-def func-def-list { $2->append_fun($1); $$=$2; }
  | func-decl func-def-list { $2->append_fun($1); $$=$2; }
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

func-decl: "decl" header { $2->setHeaderDef(DECL); $$ = new FuncBlock(); $$->assignHeader($2); std::cout << *$$ <<std::endl;}
;

var-def: type T_id id-list { $3->var_append($2); $3->var_type($1); $$ = $3; }
;

id-list:
    /* nothing */ { $$ = new VarList();}
  | ',' T_id id-list { $3->var_append($2); $$ =$3; }
;

stmt-list-plus:
    stmt { }//$$ = new Block(); $$->append_stmt($1); }
  | stmt stmt-list-plus { }//$2->append($1); $$ = $2; }
;

stmt:
    simple { }//$$ = $1;}
  | "exit" { }//$$ = new Exit(); }
  | "return" expr { }//$$ = new Return($2); }
  | if-stmt { }//$$ = $1;}
  | "for" simple-list ';' expr ';' simple-list ':' stmt-list-plus "end" { }//$$ = new For($2, $4, $6, $8); }
  ;

if-stmt:
    "if" expr ':' stmt-list-plus elif-stmt else-stmt "end" { }//$$ = new If($2, $4, $5, $6); }
;

elif-stmt:
    /* nothing */
  | "elsif" expr ':' stmt-list-plus elif-stmt { }//$$ = new Elsif($2, $4, $5, $6); }
;

else-stmt:
    /* nothing */
  | "else" ':' stmt-list-plus { }//$$ = new Else($3); }
;

simple:
    "skip" {}//$$ = new Skip();}
  | atom ":=" expr { $$ = new Let($1, $3);}
  | call {}//$$ = new Call();}
;

simple-list:
    simple simple-list-plus { }//$2->append_simple($1); $$ = $2; }
;

simple-list-plus:
    /* nothing */ { }//$$ = new Simple(); }
  | ',' simple-list { }//$$ = $2; }
;

call:
    T_id '(' expr-list ')' { }//$$ = new Call($1, $3); }
  | T_id '('')' { }//$$ = new Call($1); }
;

expr-list:
    expr expr-list-plus {}// $2->append_expr($1); $$ = $2; }
;

expr-list-plus:
    /* nothing */{ }//$$ = new Expr();}
  | ',' expr expr-list-plus {}// $2->append_expr($1); $$ = $2;}
;

atom:
    T_id {}// $$ = new Id($1); }
  | T_string_const { } //$$ = new StringConst($1); }
  | atom '[' expr ']' {}// $$ = new Apply_atom_expr($1, $3); }
  | call { }//$$ = $1;}
;

expr:
    atom {}// $$ = $1; }
  | T_int_const { $$ = new IntConst($1); }
  | T_char_const { $$ = new CharConst($1); }
  | '(' expr ')' { $$ = $2; }
  | sign expr { $$ = new UnOp($1, $2); }
  | expr math-op expr { $$ = new BinOp($1, $2, $3); }
  | expr comp-op expr { $$ = new BinOp($1, $2, $3); }
  | "true" { $$ = new BoolConst($1); }
  | "false" { $$ = new BoolConst($1); }
  | "not" expr {$$ = new UnOp($2); }
  | expr logic-op expr { $$ = new BinOp($1, $2, $3); }
  | "new" type '[' expr ']' { $4->type_check(typeInteger);  $$ = new NewArray($2, $4->); }
  | "nil" { }//$$ = new Nil(); }
  | "nil?" '(' expr ')'  {}//$$ = new ListOp($1, $3);}
  | expr '#' expr       {}//$$ = new ListOp($1, $2, $3); }
  | "head" '(' expr ')' {}//$$ = new ListOp($1, $3);}
  | "tail" '(' expr ')' {}//$$ = new ListOp($1, $3);}
;

sign:
    '+' { }
  | '-' { }
;

math-op:
    '+' {}
  | '-' {}
  | '*' { }
  | '/' { }
  | T_mod { }
;

comp-op:
    '='   { }
  | "<>"  { }
  | '>'   { }
  | '<'   { }
  | ">="  { }
  | "<="  { }
;

logic-op:
    "and" { }//$$ = $1; }
  | "or"  { }//$$ = $1; }
;

%%

int main(){
  int result = yyparse();
  if (result == 0) printf("Success.\n");
  return result;
}
