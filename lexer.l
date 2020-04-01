%option noyywrap

%{
#include <stdio.h>
#include "parser.hpp"
#include "lexer.hpp"

int lineno = 1;
int comment_level = 0;

#define T_eof 0

%}

L [A-Za-z]
D [0-9]
H [0-9A-Fa-f]
W [ \t\r]
E \\(n|t|r|0|\'|\"|\\|x{H}{H})

%x COMMENT

%%

"and"    { return T_and; }
"bool"   { return T_bool; }
"char"   { return T_char; }
"decl"   { return T_decl; }
"def"    { return T_def; }
"else"   { return T_else; }
"elsif"  { return T_elsif; }
"end"    { return T_end; }
"exit"   { return T_exit; }
"false"  { return T_false; }
"for"    { return T_for; }
"head"   { return T_head; }
"if"     { return T_if; }
"int"    { return T_int; }
"list"   { return T_list; }
"mod"    { return T_mod; }
"new"    { return T_new; }
"nil"    { return T_nil; }
"nil?"   { return T_nil_quest; }
"not"    { return T_not; }
"or"     { return T_or; }
"ref"    { return T_ref; }
"return" { return T_return; }
"skip"   { return T_skip; }
"tail"   { return T_tail; }
"true"   { return T_true; }
":="     { return T_assign; }
"<>"     { return T_neq; }
"<="     { return T_leq; }
">="     { return T_geq; }

{L}({L}|{D}|_|\?)* { return T_id; }
{D}+    { return T_int_const; }

\'([^\'\"\\]|{E})\' { return T_char_const; }
\"([^\n\'\"\\]|{E})*\" { return T_string_const; }

[\,\;\:] { return yytext[0]; }
[+\-\*/#=<>]
[()\[\]+\-/%*=]   { return yytext[0]; }

{W}+    { /* nothing */ }

\%.*\n  { lineno++; }

"<*" { BEGIN(COMMENT); }
<COMMENT>"<*" { ++comment_level; fprintf(stdout, "Im in line %d with comment_level %d \n", lineno, comment_level); }
<COMMENT>"*"+">" { if(comment_level) --comment_level;
                   else BEGIN(INITIAL); }
<COMMENT>\n { lineno++; }
<COMMENT>"*"+ { /* nothing */ }
<COMMENT>[^*\n]+ { /* nothing */ }

\n      { lineno++; }

.       { yyerror("a"); }

%%

void yyerror(const char *msg) {
  fprintf(stderr, "Illegal character with code %d in line %d \n",
                    yytext[0], lineno, msg);
      exit(1);
}
/*
#ifdef yyFlexLexer
int main () {
  yyFlexLexer lexer;
  int token;
  do {
    token = lexer.yylex();
    printf("token=%d, lexeme=\"%s\"\n", token, lexer.YYText());
  } while (token != T_eof);
}
#else

int main () {
  int token;
  do {
    token = yylex();
    printf("token=%d, lexeme=\"%s\"\n", token, yytext);
  } while (token != T_eof);
}
#endif
*/
