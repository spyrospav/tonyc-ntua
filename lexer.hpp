#ifndef __LEXER_HPP__
#define __LEXER_HPP__

#include <string>

int yylex();
void yyerror(const char *msg);
std::string fixSequence (char * s);

#endif
