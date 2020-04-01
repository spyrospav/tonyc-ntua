%{
#include <stdio.h>
#include "lexer.hpp"
%}

%token T_program    "program"
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
%left               T_mod

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
| hellotest
;

hellotest: "hellotest"
;

func-def:
  "def" header ':' func-def-list stmt-plus-list "end"
;


func-def-list:
    /* nothing */
  | func-def func-def-list
  | func-decl func-def-list
  | var-def func-def-list
;

stmt-plus-list:
    stmt
  | stmt stmt-plus-list
;

var-def: "vd"
;

stmt: "stmt"
;

func-decl: "fdl"
;

header: "hd"
;

%%

int main(){
  int result = yyparse();
  if (result == 0) printf("Success.\n");
  return result;
}
