%{
#include <stdio.h>
#include "lexer.hpp"
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

%token T_id
%token T_int_const
%token T_char_const
%token T_string_const

%left               T_and
%nonassoc           T_not
%left               T_or

%left               '*'
%left               '/'
%left               '+'
%left               '-'
%right              '#'
%left T_mod         "mod"

%nonassoc           T_assign
%nonassoc           T_neq
%nonassoc           T_leq
%nonassoc           T_geq

%{
  typedef int YYSTYPE;
%}

%%

program:
  func-def
;


func-def:
  "def" header ':' func-def-list stmt-list-plus "end"
;

func-def-list:
    /* nothing */
  | func-def func-def-list
  | func-decl func-def-list
  | var-def func-def-list
;

header:
    type T_id '(' formal-list ')'
  | T_id '(' formal-list ')'
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
  | ';' formal formal-list-plus
;

type: "int" | "bool" | "char" | type '[' ']' | "list" '[' type ']'
;

func-decl: "decl" header
;

var-def: type T_id id-list
;

id-list:
    /* nothing */
  | ',' T_id id-list
;

stmt-list-plus:
    stmt
  | stmt stmt-list-plus
;

stmt:
    simple
  | "exit"
  | "return" expr
  | if-stmt
  | "for" simple-list ';' expr ';' simple-list ':' stmt-list-plus "end"
;

if-stmt:
    "if" expr ':' stmt-list-plus elif-stmt else-stmt "end"
;

elif-stmt:
    /* nothing */
  | "elsif" expr ':' stmt-list-plus elif-stmt
;

else-stmt:
    /* nothing */
  | "else" ':' stmt-list-plus
;

simple:
    "skip"
  | atom ":=" expr
  | call
;
simple-list:
    simple simple-list-plus
;

simple-list-plus:
    /* nothing */
  | ',' simple-list
;

call:
    T_id '(' expr-list ')'
  | T_id '('')'
;

expr-list:
    expr expr-list-plus
;

expr-list-plus:
    /* nothing */
  | ',' expr expr-list-plus
;

atom:
    T_id
  | T_string_const
  | atom '[' expr ']'
  | call
;

expr:
    atom
  | T_int_const
  | T_char_const
  | '(' expr ')'
  | sign expr
  | expr math-op expr
  | expr comp-op expr
  | "true"
  | "false"
  | "not" expr
  | expr logic-op expr
  | "new" type '[' expr ']'
  | "nil"
  | "nil?" '(' expr ')'
  | expr '#' expr
  | "head" '(' expr ')'
  | "tail" '(' expr ')'
;

sign:
    '+'
  | '-'
;

math-op:
    '+'
  | '-'
  | '*'
  | '/'
  | T_mod
;

comp-op:
    '='
  | "<>"
  | '>'
  | '<'
  | ">="
  | "<="
;

logic-op:
    "and"
  | "or"
;

%%

int main(){
  int result = yyparse();
  if (result == 0) printf("Success.\n");
  return result;
}
