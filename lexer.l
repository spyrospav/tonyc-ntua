%option noyywrap

%{
#define T_eof 0
#define T_and 1001
#define T_bool 1002
#define T_char 1003
#define T_decl 1004
#define T_def 1005
#define T_else 1006
#define T_elsif 1007
#define T_end 1008
#define T_exit 1009
#define T_false 1010
#define T_for 1011
#define T_head 1012
#define T_if 1013
#define T_int 1014
#define T_list 1015
#define T_mod 1016
#define T_new 1017
#define T_nil 1018
#define T_nil_quest 1019
#define T_not 1020
#define T_or 1021
#define T_ref 1022
#define T_return 1023
#define T_skip 1024
#define T_tail 1025
#define T_true 1026
#define T_neq 1027
#define T_leq 1028
#define T_geq 1029
#define T_assign 1030
#define T_id 1031
#define T_int_const 1032
#define T_char_const 1033
#define T_string_const 1034

int lineno = 1;
int comment_level = 0;

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
[+\-\*/#=<>] { return yytext[0]; }

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

.       { fprintf(stderr, "Illegal character with code %d in line %d \n",
                  yytext[0], lineno);
          exit(1);
        }

%%

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
