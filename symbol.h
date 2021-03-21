/******************************************************************************
 *  CVS version:
 *     $Id: symbol.h,v 1.1 2003/05/13 22:21:01 nickie Exp $
 ******************************************************************************
 *
 *  C header file : symbol.h
 *  Project       : PCL Compiler
 *  Version       : 1.0 alpha
 *  Written by    : Nikolaos S. Papaspyrou (nickie@softlab.ntua.gr)
 *  Date          : May 14, 2003
 *  Description   : Generic symbol table in C
 *
 *  Comments: (in Greek iso-8859-7)
 *  ---------
 *  ������ �������� �����������.
 *  ����� ������������ ��������� ��� ��������� �����������.
 *  ������ ����������� ������������ ��� �����������.
 *  ���������� ����������� ����������
 */


#ifndef __SYMBOL_H__
#define __SYMBOL_H__


/* ---------------------------------------------------------------------
   -------------------------- ����� bool -------------------------------
   --------------------------------------------------------------------- */

#include <stdbool.h>
#include <map>
#include <set>
#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>

#if 0
typedef enum { false=0, true=1 } bool;
#endif


/* ---------------------------------------------------------------------
   ------------ ������� �������� ��� ������ �������� -------------------
   --------------------------------------------------------------------- */

#define START_POSITIVE_OFFSET 8     /* ������ ������ offset ��� �.�.   */
#define START_NEGATIVE_OFFSET 0     /* ������ �������� offset ��� �.�. */


/* ---------------------------------------------------------------------
   --------------- ������� ����� ��� ������ �������� -------------------
   --------------------------------------------------------------------- */

/* ����� ��������� ��� ��� ��������� ��� �������� */

typedef int           RepInteger;         /* ��������                  */
typedef unsigned char RepBoolean;         /* ������� �����             */
typedef char          RepChar;            /* ����������                */
typedef long double   RepReal;            /* �����������               */
typedef const char *  RepString;          /* �������������             */


/* ����� ��������� ��� ������������� ����������� */

enum Type_ {
      TYPE_VOID,                        /* ����� ����� ������������� */
      TYPE_INTEGER,                     /* ��������                  */
      TYPE_BOOLEAN,                     /* ������� �����             */
      TYPE_CHAR,                        /* ����������                */
      TYPE_REAL,                        /* �����������               */
      TYPE_ARRAY,                       /* ������� ������� ��������  */
      TYPE_IARRAY,                      /* ������� �������� �������� */
      TYPE_POINTER,
      TYPE_LIST,
      TYPE_ANY
};

enum Pardef{                               /* ��������� ����������  */
    PARDEF_COMPLETE,                    /* ������ �������     */
    PARDEF_DEFINE,                      /* �� ���� �������    */
    PARDEF_CHECK                        /* �� ���� �������    */
};

typedef struct Type_tag * Type;

struct Type_tag {
    Type_ kind;
    Type           refType;              /* ����� ��������            */
    RepInteger     size;                 /* �������, �� ����� ������� */
    unsigned int   refCount;             /* �������� ��������         */
};


/* ����� �������� ��� ������ �������� */

typedef enum {
   ENTRY_VARIABLE,                       /* ����������                 */
   ENTRY_CONSTANT,                       /* ��������                   */
   ENTRY_FUNCTION,                       /* �����������                */
   ENTRY_PARAMETER,                      /* ���������� �����������     */
   ENTRY_TEMPORARY                       /* ���������� ����������      */
} EntryType;


/* ����� ���������� ���������� */

typedef enum {
   PASS_BY_VALUE,                        /* ���' ����                  */
   PASS_BY_REFERENCE                     /* ���' �������               */
} PassMode;


/* ����� �������� ���� ������ �������� */

typedef struct SymbolEntry_tag SymbolEntry;

struct SymbolEntry_tag {
   const char   * id;                 /* ����� ��������������          */
   EntryType      entryType;          /* ����� ��� ��������            */
   unsigned int   nestingLevel;       /* ����� �����������             */
   unsigned int   hashValue;          /* ���� ���������������          */
   SymbolEntry  * nextHash;           /* ������� ������� ���� �.�.     */
   SymbolEntry  * nextInScope;        /* ������� ������� ���� �������� */

   union {                            /* ������� �� ��� ���� ��������: */

      struct {                                /******* ��������� *******/
         Type          type;                  /* �����                 */
         int           offset;                /* Offset ��� �.�.       */
         llvm::AllocaInst * allocainst;
      } eVariable;

      struct {                                /******** ������� ********/
         Type          type;                  /* �����                 */
         union {                              /* ����                  */
            RepInteger vInteger;              /*    �������            */
            RepBoolean vBoolean;              /*    ������             */
            RepChar    vChar;                 /*    ����������         */
            RepReal    vReal;                 /*    ����������         */
            RepString  vString;               /*    ������������       */
         } value;
      } eConstant;

      struct {                                /******* ��������� *******/
         bool          isForward;             /* ������ forward        */
         SymbolEntry * firstArgument;         /* ����� ����������      */
         SymbolEntry * lastArgument;          /* ��������� ����������  */
         Type          resultType;            /* ����� �������������   */
         Pardef        pardef;
         int           firstQuad;             /* ������ �������        */
         llvm::Function * llvmfun;
      } eFunction;

      struct {                                /****** ���������� *******/
         Type          type;                  /* �����                 */
         int           offset;                /* Offset ��� �.�.       */
         PassMode      mode;                  /* ������ ����������     */
         SymbolEntry * next;                  /* ������� ����������    */
         llvm::AllocaInst * llvmpar;
      } eParameter;

      struct {                                /** ��������� ��������� **/
         Type          type;                  /* �����                 */
         int           offset;                /* Offset ��� �.�.       */
         int           number;
      } eTemporary;

   } u;                               /* ����� ��� union               */
};


/* ����� ������� �������� ��� ���������� ���� ���� �������� */

typedef struct Scope_tag Scope;

struct Scope_tag {
    unsigned int   nestingLevel;             /* ����� �����������      */
    unsigned int   negOffset;                /* ������ �������� offset */
    Scope        * parent;                   /* ������������ ��������  */
    SymbolEntry  * entries;                  /* ������� ��� ���������  */
    std::set<SymbolEntry *>  live_variables;
};


/* ����� ���������� ���� ������ �������� */

typedef enum {
    LOOKUP_CURRENT_SCOPE,
    LOOKUP_ALL_SCOPES
} LookupType;


/* ---------------------------------------------------------------------
   ------------- ��������� ���������� ��� ������ �������� --------------
   --------------------------------------------------------------------- */

extern Scope        * currentScope;       /* �������� ��������         */
extern unsigned int   quadNext;           /* ������� �������� �������� */
extern unsigned int   tempNumber;         /* �������� ��� temporaries  */

extern const Type typeVoid;
extern const Type typeInteger;
extern const Type typeBoolean;
extern const Type typeChar;
extern const Type typeReal;
extern const Type typeAny;


/* ---------------------------------------------------------------------
   ------ ��������� ��� ����������� ��������� ��� ������ �������� ------
   --------------------------------------------------------------------- */

void          initSymbolTable    (unsigned int size);
void          destroySymbolTable (void);

void          openScope          (void);
void          closeScope         (void);

SymbolEntry * newVariable        (const char * name, Type type, llvm::AllocaInst *a = nullptr);
SymbolEntry * newConstant        (const char * name, Type type, ...);
SymbolEntry * newFunction        (const char * name, llvm::Function * f = nullptr);
SymbolEntry * newParameter       (const char * name, Type type,
                                  PassMode mode, SymbolEntry * f);
SymbolEntry * newTemporary       (Type type);

void          forwardFunction    (SymbolEntry * f);
void          endFunctionHeader  (SymbolEntry * f, Type type);
void          destroyEntry       (SymbolEntry * e);
SymbolEntry * lookupEntry        (const char * name, LookupType type,
                                  bool err);

Type          typeArray          (RepInteger size, Type refType);
Type          typeIArray         (Type refType);
Type          typePointer        (Type refType);
Type          typeList           (Type refType);
void          destroyType        (Type type);
unsigned int  sizeOfType         (Type type);
bool          equalType          (Type type1, Type type2);
bool          isTypeAny          (Type type);
bool          isTypeArray        (Type type);
void          printType          (Type type);
void          printMode          (PassMode mode);

void          printSymbolTable   ();
void          StandardLibraryInit();
void          addLiveVariable    (SymbolEntry * e);
#endif
