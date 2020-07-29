/******************************************************************************
 *  CVS version:
 *     $Id: error.c,v 1.2 2004/05/05 22:00:08 nickie Exp $
 ******************************************************************************
 *
 *  C code file : error.c
 *  Project     : PCL Compiler
 *  Version     : 1.0 alpha
 *  Written by  : Nikolaos S. Papaspyrou (nickie@softlab.ntua.gr)
 *  Date        : May 14, 2003
 *  Description : Generic symbol table in C, simple error handler
 *
 *  Comments: (in Greek iso-8859-7)
 *  ---------
 *  Εθνικό Μετσόβιο Πολυτεχνείο.
 *  Σχολή Ηλεκτρολόγων Μηχανικών και Μηχανικών Υπολογιστών.
 *  Τομέας Τεχνολογίας Πληροφορικής και Υπολογιστών.
 *  Εργαστήριο Τεχνολογίας Λογισμικού
 */


/* ---------------------------------------------------------------------
   ---------------------------- Header files ---------------------------
   --------------------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "general.h"
#include "error.h"


/* ---------------------------------------------------------------------
   --------- Υλοποίηση των συναρτήσεων του χειριστή σφαλμάτων ----------
   --------------------------------------------------------------------- */

void internal (const char * fmt, ...)
{
   va_list ap;

   va_start(ap, fmt);
   if (fmt[0] == '\r')
      fmt++;
   else
      fprintf(stderr, "%s:%d: ", filename, linecount);
   fprintf(stderr, "Internal error, ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");
   va_end(ap);
   exit(1);
}

void fatal (const char * fmt, ...)
{
   va_list ap;

   va_start(ap, fmt);
   if (fmt[0] == '\r')
      fmt++;
   else
      fprintf(stderr, "%s:%d: ", filename, linecount);
   fprintf(stderr, "Fatal error, ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");
   va_end(ap);
   exit(1);
}

void error (const char * fmt, ...)
{
   va_list ap;

   va_start(ap, fmt);
   if (fmt[0] == '\r')
      fmt++;
   else
      fprintf(stderr, "%s:%d: ", filename, linecount);
   fprintf(stderr, "Error, ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");
   va_end(ap);
}

void warning (const char * fmt, ...)
{
   va_list ap;

   va_start(ap, fmt);
   if (fmt[0] == '\r')
      fmt++;
   else
      fprintf(stderr, "%s:%d: ", filename, linecount);
   fprintf(stderr, "Warning, ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");
   va_end(ap);
}
