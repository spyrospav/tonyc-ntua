%{
#include <stdio.h>
#include "lexer.hpp"
#include <string>
%}


%token T_bool       "bool"
%token T_char       "char"
%token T_decl       "decl"
%token T_def        "def"
%token T_else       "else"
%token T_elsif      "elsif"
%token T_end        "end"
%token T_exit       "exit"
%token T_false      "false"
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
%token T_true       "true"
%token T_not        "not"
%token T_and        "and"
%token T_or         "or"
%token T_assign     ":="
%token T_neq        "<>"
%token T_leq        "<="
%token T_geq        ">="

%token<var> T_id
%token<num> T_int_const
%token T_char_const
%token T_string_const

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
  Block *block;
  Stmt *stmt;
  Expr *expr;
  std::string var;
  std::string string_const;
  char char_const;
  int num;
  char[] op;
  bool b;
}

%type<block> program stmt_list
%type<stmt>  stmt
%type<expr>  expr


%%

program:
  func-def //may need to create one more block
;


func-def:
  "def" header ':' func-def-list stmt-list-plus "end"
;

func-def-list:
    /* nothing */ {$$ = new Block();}
  | func-def func-def-list { $1->append($2); $$ = $1; } //append mallon anapoda
  | func-decl func-def-list { $1->append($2); $$ = $1; }
  | var-def func-def-list { $1->append($2); $$ = $1; }
;

header:
    type T_id '(' formal-list ')' {$$ = new Func($1, $2, $4);}
  | T_id '(' formal-list ')' {$$ = new Func(NULL, $2, $4 )}
;

formal-list:
    /* nothing */
  | formal formal-list-plus
;

formal:
    "ref" type T_id id-list
  | type T_id id-list
;

formal-list-plus:
    /* nothing */
  | ';' formal formal-list-plus //h edw h sto formal-list 8a prepei na ftiaxnoume to list object
;

type: "int" {$$ = typeInteger;}
  | "bool"  {$$ = typeBoolean;}
  | "char" {$$ = typeChar;}
  | type '[' ']' { $$ = {TYPE_IARRAY, $1, 0, 0};}
  | "list" '[' type ']' { $$ = {TYPE_LIST, $3}}
;

func-decl: "decl" header
;

var-def: type T_id id-list { $3->var_append($2); $1->var_append($1); $$  = $3; }
; //var def object has variables' type as first elemnt of list

id-list:
    /* nothing */ {$$  = new Var();}
  | ',' T_id id-list {$3->var_append($2); $$ =$3; }
;

stmt-list-plus:
    stmt {$$ = new Block(); $$->append_stmt($1);}
  | stmt stmt-list-plus {$2->append($1); $$ = $2;}
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
    /* nothing */
  | "elsif" expr ':' stmt-list-plus elif-stmt { $$ = new Elsif($2, $4, $5, $6); }
;

else-stmt:
    /* nothing */
  | "else" ':' stmt-list-plus { $$ = new Else($3); }
;

simple:
    "skip" {$$ = new Skip();}
  | atom ":=" expr {$$ = new Let();}
  | call {$$ = new Call();}
;

simple-list:
    simple simple-list-plus { $2->append_simple($1); $$ = $2; }
;

simple-list-plus:
    /* nothing */ { $$ = new Simple(); }
  | ',' simple-list { $$ = $2; }
;

call:
    T_id '(' expr-list ')' { $$ = new Call($1, $3); }
  | T_id '('')' { $$ = new Call($1); }
;

expr-list:
    expr expr-list-plus ( $2->append_expr($1); $$ = $2; }
;

expr-list-plus:
    /* nothing */{ $$ = new Expr();}
  | ',' expr expr-list-plus { $2->append_expr($1); $$ = $2;}
;

atom:
    T_id { $$ = new Id($1); }
  | T_string_const { $$ = new String_const($1); }
  | atom '[' expr ']' { $$ = apply_atom-expr($1, $3); }
  | call { $$ = $1;}
;

expr:
    atom { $$ = $1; }
  | T_int_const { $$ = new Int_const($1); }
  | T_char_const { $$ = new Char_const($1); }
  | '(' expr ')' { $$ = $2; }
  | sign expr {$$ = new Sign($1, $2);}
  | expr math-op expr {$$ = new Math-op($1, $2, $3);   }
  | expr comp-op expr {$$ = new Comp-op($1, $2, $3);   }
  | "true" { $$ = new Logic($1); }
  | "false" { $$ = new Logic($1); }
  | "not" expr      {$$ = new Logic-op($2); }
  | expr logic-op expr { $$ = new Logic-op($1, $2, $3); }
  | "new" type '[' expr ']' { $$ = new New-expr($2, $4); } //periergo
  | "nil"
  | "nil?" '(' expr ')'  {$$ = new List-op($1, $3);}
  | expr '#' expr       {$$ = new List-op($1, $2, $3); }
  | "head" '(' expr ')' {$$ = new List-op($1, $3);}
  | "tail" '(' expr ')' {$$ = new List-op($1, $3);}
;


sign:
    '+' { $$ = $1; }
  | '-' { $$ = $1; }
;

math-op:
    '+' { $$ = $1; }
  | '-' { $$ = $1; }
  | '*' { $$ = $1; }
  | '/' { $$ = $1; }
  | T_mod { $$ = $1; }
;

comp-op:
    '='   { $$ = $1; }
  | "<>"  { $$ = $1; }
  | '>'   { $$ = $1; }
  | '<'   { $$ = $1; }
  | ">="  { $$ = $1; }
  | "<="  { $$ = $1; }
;

logic-op:
    "and" { $$ = $1; }
  | "or"  { $$ = $1; }
;

%%

int main(){
  int result = yyparse();
  if (result == 0) printf("Success.\n");
  return result;
}
