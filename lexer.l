%option noyywrap

%{
#include "ast.hpp"
#include <stdio.h>
#include "parser.hpp"
#include "lexer.hpp"
#include "general.h"

//int linecount = 1;
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

"and"    { yylval.op = strdup(yytext); return T_and; }
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
"mod"    { yylval.op = strdup(yytext); return T_mod; }
"new"    { return T_new; }
"nil"    { return T_nil; }
"nil?"   { return T_nil_quest; }
"not"    { yylval.op = strdup(yytext); return T_not; }
"or"     { yylval.op = strdup(yytext); return T_or; }
"ref"    { return T_ref; }
"return" { return T_return; }
"skip"   { return T_skip; }
"tail"   { return T_tail; }
"true"   { return T_true; }
":="     { return T_assign; }
"<>"     { yylval.op = strdup(yytext); return T_neq; }
"<="     { yylval.op = strdup(yytext); return T_leq; }
">="     { yylval.op = strdup(yytext); return T_geq; }

{L}({L}|{D}|_|\?)* {  yylval.var = strdup(yytext); return T_id; }
{D}+    { yylval.num = atoi(yytext); return T_int_const; }

\'([^\'\"\\]|{E})\' {
                        std::string temp = yytext;
                        temp = temp.substr(1, temp.length()-1);
                        if(temp[0] == '\\' && temp[1] == 'x') {
                          //for hexadecimals
                          char* yytext_end;
                          yylval.char_const = (char) std::strtol(&(yytext[3]), &yytext_end, 16);
                        }
                        else if(temp[0] == '\\') {
                          //for escape chars
                          char t;
                          switch(temp[1]) {
                            case 'n': t = '\n'; break;
                            case 't': t = '\t'; break;
                            case 'r': t = 'r'; break;
                            case '0': t = '\0'; break;
                            case '\\': t = 92; break;
                            case '\'': t = 39; break;
                            case '"': t = 34; break;
                          }
                          yylval.char_const = t;
                        }
                        else {
                          //for simple chars
                          yylval.char_const = yytext[1];
                        }
                       return T_char_const;
                       }

\"([^\n\'\"\\]|{E})*\" {  std::string res_string = fixSequence(yytext);
                          char *cstr = new char[res_string.length() + 1];
                          strcpy(cstr, res_string.c_str());
                          yylval.string_const = cstr;
                          return T_string_const;
                       }

[\,\;\:] { return yytext[0]; }
[+\-\*/#=<>] { yylval.op = strdup(yytext); return yytext[0]; }
[()\[\]+\-/%*=]   { return yytext[0]; }

\%.*\n  { linecount++; }

"<*" { BEGIN(COMMENT); }
<COMMENT>"<*" { ++comment_level; fprintf(stdout, "Im in line %d with comment_level %d \n", linecount, comment_level); }
<COMMENT>"*"+">" { if(comment_level) --comment_level;
                   else BEGIN(INITIAL); }
<COMMENT>\n { linecount++; }
<COMMENT>"*"+ { /* nothing */ }
<COMMENT>[^*\n]+ { /* nothing */ }

{W}+    { /* nothing */ }

\n      { linecount++; }

.       { yyerror("a"); }

%%

void yyerror(const char *msg) {
  fprintf(stderr, "Illegal character with code %d in line %d \n %s",
                    yytext[0], linecount, msg);
      exit(1);
}


bool inHexBounds (char c) {
  return (((65 <= c) && (c <= 70)) || ((97 <= c) and (c <= 102)) || ((c >= 48) && (c <= 57)));
}

std::string fixSequence (char * s) {
  std::string s_string = s;
  std::vector<char> res;
  char previous = s[0];
  for (int i = 1; i < s_string.size()-1; i++) {
    if(previous == '\\' && s[i] == 'x') {
      if (s_string.size() - 1 - i < 2) {
        res.push_back('\\');
        res.push_back('x');
        previous = 'x';
        continue;
      }
      char c_1 = s[i+1];
      char c_2 = s[i+2];
      if (inHexBounds(c_1) && inHexBounds(c_2)) {
        char heximal[3];
        heximal[0] = c_1;
        heximal[1] = c_2;
        heximal[2] = '\0';
        const char *hex = heximal;
        res.push_back((char) std::strtol(hex, NULL, 16));
      }
      else {
        res.push_back('\\');
        res.push_back('x');
        res.push_back(c_1);
        res.push_back(c_2);
      }
      i = i + 2;
      previous = c_2;
      continue;
    }
    else if(previous == '\\') {
      //for escape chars
      char t;
      switch(s[i]) {
        case 'n': t = '\n'; break;
        case 't': t = '\t'; break;
        case 'r': t = 'r'; break;
        case '0': t = '\0'; break;
        case '\\': t = 92; break;
        case '\'': t = 39; break;
        case '"': t = 34; break;
        default : t = s[i]; res.push_back('\\'); break;
      }
      res.push_back(t);
    }
    else {
      if(s[i] != '\\')
        res.push_back(s[i]);
    }
    previous = s[i];
  }

  return std::string(res.begin(), res.end());

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
