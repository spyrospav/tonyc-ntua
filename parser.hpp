/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_PARSER_HPP_INCLUDED
# define YY_YY_PARSER_HPP_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    T_bool = 258,
    T_char = 259,
    T_decl = 260,
    T_def = 261,
    T_else = 262,
    T_elsif = 263,
    T_end = 264,
    T_exit = 265,
    T_false = 266,
    T_for = 267,
    T_head = 268,
    T_if = 269,
    T_int = 270,
    T_list = 271,
    T_new = 272,
    T_nil = 273,
    T_nil_quest = 274,
    T_ref = 275,
    T_return = 276,
    T_skip = 277,
    T_tail = 278,
    T_true = 279,
    T_not = 280,
    T_and = 281,
    T_or = 282,
    T_assign = 283,
    T_neq = 284,
    T_leq = 285,
    T_geq = 286,
    T_id = 287,
    T_int_const = 288,
    T_char_const = 289,
    T_string_const = 290,
    T_mod = 291
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 61 "parser.y" /* yacc.c:1909  */

  Block *block;
  Stmt *stmt;
  Expr *expr;
  Var *var_list;
  const char *var;
  char *string_const;
  char char_const;
  int num;
  char *op;
  bool b;
  Type type;

#line 105 "parser.hpp" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_HPP_INCLUDED  */
