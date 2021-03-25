/******************************************************************************
 *  CVS version:
 *     $Id: symbol.c,v 1.3 2004/05/05 22:00:08 nickie Exp $
 ******************************************************************************
 *
 *  C code file : symbol.c
 *  Project     : PCL Compiler
 *  Version     : 1.0 alpha
 *  Written by  : Nikolaos S. Papaspyrou (nickie@softlab.ntua.gr)
 *  Date        : May 14, 2003
 *  Description : Generic symbol table in C
 *
 *  Comments: (in Greek iso-8859-7)
 *  ---------
 *  ������ �������� �����������.
 *  ����� ������������ ��������� ��� ��������� �����������.
 *  ������ ����������� ������������ ��� �����������.
 *  ���������� ����������� ����������
 */


/* ---------------------------------------------------------------------
   ---------------------------- Header files ---------------------------
   --------------------------------------------------------------------- */

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <iostream>
#include "general.h"
#include "error.h"
#include "symbol.h"
#include <map>
#include <set>

/* ---------------------------------------------------------------------
   ------------- ��������� ���������� ��� ������ �������� --------------
   --------------------------------------------------------------------- */

Scope        * currentScope;           /* �������� ��������              */
std::set<SymbolEntry *> live_variables;
unsigned int   quadNext;               /* ������� �������� ��������      */
unsigned int   tempNumber;             /* �������� ��� temporaries       */

static unsigned int   hashTableSize;   /* ������� ������ ��������������� */
static SymbolEntry ** hashTable;       /* ������� ���������������        */

std::map<SymbolEntry *, int> hashTheVars;  /* The order in which each variable (found by its symbol entry) is placed in the hash table */

unsigned int numInserted = 0;                /* Number of variales inserted to symbol table */

static struct Type_tag typeConst [] = {
    { TYPE_VOID,    NULL, 0, 0 },
    { TYPE_INTEGER, NULL, 0, 0 },
    { TYPE_BOOLEAN, NULL, 0, 0 },
    { TYPE_CHAR,    NULL, 0, 0 },
    { TYPE_REAL,    NULL, 0, 0 },
    { TYPE_ANY,     NULL, 0, 0 }
};

const Type typeVoid    = &(typeConst[0]);
const Type typeInteger = &(typeConst[1]);
const Type typeBoolean = &(typeConst[2]);
const Type typeChar    = &(typeConst[3]);
const Type typeReal    = &(typeConst[4]);
const Type typeAny     = &(typeConst[5]);

/* ---------------------------------------------------------------------
   ------- ��������� ���������� ����������� ��� ������ �������� --------
   --------------------------------------------------------------------- */

typedef unsigned long int HashType;

static HashType PJW_hash (const char * key)
{
    /*
     *  P.J. Weinberger's hashing function. See also:
     *  Aho A.V., Sethi R. & Ullman J.D, "Compilers: Principles,
     *  Techniques and Tools", Addison Wesley, 1986, pp. 433-437.
     */

    const HashType PJW_OVERFLOW =
        (((HashType) 0xf) << (8 * sizeof(HashType) - 4));
    const int PJW_SHIFT = (8 * (sizeof(HashType) - 1));

    HashType h, g;

    for (h = 0; *key != '\0'; key++) {
        h = (h << 4) + (*key);
        if ((g = h & PJW_OVERFLOW) != 0) {
            h ^= g >> PJW_SHIFT;
            h ^= g;
        }
    }
    return h;
}

void strAppendChar (char * buffer, RepChar c)
{
    switch (c) {
        case '\n':
            strcat(buffer, "\\n");
            break;
        case '\t':
            strcat(buffer, "\\t");
            break;
        case '\r':
            strcat(buffer, "\\r");
            break;
        case '\0':
            strcat(buffer, "\\0");
            break;
        case '\\':
            strcat(buffer, "\\\\");
            break;
        case '\'':
            strcat(buffer, "\\'");
            break;
        case '\"':
            strcat(buffer, "\\\"");
            break;
        default: {
            char s[] = { '\0', '\0' };

            *s = c;
            strcat(buffer, s);
        }
    }
}

void strAppendString (char * buffer, RepString str)
{
    const char * s;

    for (s = str; *s != '\0'; s++)
        strAppendChar(buffer, *s);
}


/* ---------------------------------------------------------------------
   ------ ��������� ��� ����������� ��������� ��� ������ �������� ------
   --------------------------------------------------------------------- */

void initSymbolTable (unsigned int size)
{
    unsigned int i;

    /* �������� �������������� */

    currentScope = NULL;
    live_variables = std::set<SymbolEntry *>();
    quadNext     = 1;
    tempNumber   = 1;
    numInserted = 0;

    /* ������������ ��� ������ ��������������� */

    hashTableSize = size;
    hashTable = (SymbolEntry **) my_new(size * sizeof(SymbolEntry *));

    for (i = 0; i < size; i++)
        hashTable[i] = NULL;
}

void destroySymbolTable ()
{
    unsigned int i;

    /* ���������� ��� ������ ��������������� */

    for (i = 0; i < hashTableSize; i++)
        if (hashTable[i] != NULL)
            destroyEntry(hashTable[i]);

    my_delete(hashTable);
}

void openScope ()
{
    Scope * newScope = (Scope *) my_new(sizeof(Scope));

    newScope->negOffset = START_NEGATIVE_OFFSET;
    newScope->parent    = currentScope;
    newScope->entries   = NULL;

    if (currentScope == NULL)
        newScope->nestingLevel = 1;
    else
        newScope->nestingLevel = currentScope->nestingLevel + 1;

    currentScope = newScope;

}

void closeScope ()
{
    SymbolEntry * e = currentScope->entries;
    Scope       * t = currentScope;

    while (e != NULL) {
        SymbolEntry * next = e->nextInScope;

        hashTable[e->hashValue] = e->nextHash;
        destroyEntry(e);
        e = next;
    }

    if (currentScope->nestingLevel > 2) {
      for (auto it = live_variables.begin(); it != live_variables.end(); ++it) {
        if ((*it)->nestingLevel < currentScope->parent->nestingLevel)
          live_variables.insert((*it));
      }
    }
    else {
      live_variables.clear();
    }
    currentScope = currentScope->parent;
    my_delete(t);

}

static void insertEntry (SymbolEntry * e)
{
    e->nextHash             = hashTable[e->hashValue];
    hashTable[e->hashValue] = e;
    e->nextInScope          = currentScope->entries;
    currentScope->entries   = e;
    numInserted++;
}

static SymbolEntry * newEntry (const char * name)
{
    SymbolEntry * e;

    /* ������� �� ������� ��� */

    for (e = currentScope->entries; e != NULL; e = e->nextInScope)
        if (strcmp(name, e->id) == 0) {
            error("Duplicate identifier: %s", name);
            return NULL;
        }

    /* ������������ ���� �����: entryType ��� u */

    e = (SymbolEntry *) my_new(sizeof(SymbolEntry));
    e->id = (const char *) my_new(strlen(name) + 1);
    strcpy((char *) (e->id), name);
    e->hashValue    = PJW_hash(name) % hashTableSize;
    e->nestingLevel = currentScope->nestingLevel;
    insertEntry(e);
    return e;
}

SymbolEntry * newVariable (const char * name, Type type, llvm::AllocaInst * a)
{
    SymbolEntry * e = newEntry(name);

    if (e != NULL) {
        e->entryType = ENTRY_VARIABLE;
        e->u.eVariable.type = type;
        e->u.eVariable.allocainst = a;
        type->refCount++;
        currentScope->negOffset -= sizeOfType(type);
        e->u.eVariable.offset = currentScope->negOffset;
    }
    return e;
}

SymbolEntry * newConstant (const char * name, Type type, ...)
{
    SymbolEntry * e;
    va_list ap;

    union {
        RepInteger vInteger;
        RepBoolean vBoolean;
        RepChar    vChar;
        RepReal    vReal;
        RepString  vString;
    } value;

    va_start(ap, type);
    switch (type->kind) {
        case TYPE_INTEGER:
            value.vInteger = va_arg(ap, RepInteger);
            break;
        case TYPE_BOOLEAN:
            value.vBoolean = va_arg(ap, int);     /* RepBool is promoted */
            break;
        case TYPE_CHAR:
            value.vChar = va_arg(ap, int);        /* RepChar is promoted */
            break;
        case TYPE_REAL:
            value.vReal = va_arg(ap, RepReal);
            break;
        case TYPE_ARRAY:
            if (equalType(type->refType, typeChar)) {
                RepString str = va_arg(ap, RepString);

                value.vString = (const char *) my_new(strlen(str) + 1);
                strcpy((char *) (value.vString), str);
                break;
            }
        default:
            internal("Invalid type for constant");
    }
    va_end(ap);

    if (name == NULL) {
        char buffer[256];

        switch (type->kind) {
            case TYPE_INTEGER:
                sprintf(buffer, "%d", value.vInteger);
                break;
            case TYPE_BOOLEAN:
                if (value.vBoolean)
                    sprintf(buffer, "true");
                else
                    sprintf(buffer, "false");
                break;
            case TYPE_CHAR:
                strcpy(buffer, "'");
                strAppendChar(buffer, value.vChar);
                strcat(buffer, "'");
                break;
            case TYPE_REAL:
                sprintf(buffer, "%Lg", value.vReal);
                break;
            case TYPE_ARRAY:
                strcpy(buffer, "\"");
                strAppendString(buffer, value.vString);
                strcat(buffer, "\"");
        }
        e = newEntry(buffer);
    }
    else
        e = newEntry(name);

    if (e != NULL) {
        e->entryType = ENTRY_CONSTANT;
        e->u.eConstant.type = type;
        type->refCount++;
        switch (type->kind) {
            case TYPE_INTEGER:
                e->u.eConstant.value.vInteger = value.vInteger;
                break;
            case TYPE_BOOLEAN:
                e->u.eConstant.value.vBoolean = value.vBoolean;
                break;
            case TYPE_CHAR:
                e->u.eConstant.value.vChar = value.vChar;
                break;
            case TYPE_REAL:
                e->u.eConstant.value.vReal = value.vReal;
                break;
            case TYPE_ARRAY:
                e->u.eConstant.value.vString = value.vString;
        }
    }
    return e;
}

SymbolEntry * newFunction (const char * name, llvm::Function * f)
{
    SymbolEntry * e = lookupEntry(name, LOOKUP_CURRENT_SCOPE, false);
    if (e == NULL) {
        e = newEntry(name);
        if (e != NULL) {
          e->entryType = ENTRY_FUNCTION;
          e->u.eFunction.isForward = false;
          e->u.eFunction.pardef = PARDEF_DEFINE;
          e->u.eFunction.firstArgument = e->u.eFunction.lastArgument = NULL;
          e->u.eFunction.resultType = NULL;
          e->u.eFunction.llvmfun = f;
        }
        return e;
    }
    else if (e->entryType == ENTRY_FUNCTION && e->u.eFunction.isForward) {
        e->u.eFunction.isForward = false;
        e->u.eFunction.pardef = PARDEF_CHECK;
        e->u.eFunction.lastArgument = NULL;
        return e;
    }
    else {
       error("Duplicate identifier: %s", name);
       return NULL;
    }
}

SymbolEntry * newParameter (const char * name, Type type,
                            PassMode mode, SymbolEntry * f)
{
    SymbolEntry * e;

    if (f->entryType != ENTRY_FUNCTION)
        internal("Cannot add a parameter to a non-function");
    switch (f->u.eFunction.pardef) {
        case PARDEF_DEFINE:
            e = newEntry(name);
            if (e != NULL) {
                e->entryType = ENTRY_PARAMETER;
                e->u.eParameter.type = type;
                type->refCount++;
                e->u.eParameter.mode = mode;
                e->u.eParameter.next = NULL;
            }
            if (f->u.eFunction.lastArgument == NULL)
                f->u.eFunction.firstArgument = f->u.eFunction.lastArgument = e;
            else {
                f->u.eFunction.lastArgument->u.eParameter.next = e;
                f->u.eFunction.lastArgument = e;
            }
            return e;
        case PARDEF_CHECK:
            e = f->u.eFunction.lastArgument;
            if (e == NULL)
                e = f->u.eFunction.firstArgument;
            else
                e = e->u.eParameter.next;
            if (e == NULL)
                error("More parameters than expected in redeclaration "
                      "of function %s", f->id);
            else if (!equalType(e->u.eParameter.type, type))
                error("Parameter type mismatch in redeclaration "
                      "of function %s", f->id);
            else if (e->u.eParameter.mode != mode)
                error("Parameter passing mode mismatch in redeclaration "
                      "of function %s", f->id);
            else if (strcmp(e->id, name) != 0)
                error("Parameter name mismatch in redeclaration "
                      "of function %s", f->id);
            else
                insertEntry(e);
            f->u.eFunction.lastArgument = e;
            return e;
        case PARDEF_COMPLETE:
            fatal("Cannot add a parameter to an already defined function");
    }
    return NULL;
}

static unsigned int fixOffset (SymbolEntry * args)
{
    if (args == NULL)
        return 0;
    else {
        unsigned int rest = fixOffset(args->u.eParameter.next);

        args->u.eParameter.offset = START_POSITIVE_OFFSET + rest;
        if (args->u.eParameter.mode == PASS_BY_REFERENCE)
            return rest + 2;
        else
            return rest + sizeOfType(args->u.eParameter.type);
    }
}

void forwardFunction (SymbolEntry * f)
{
    if (f->entryType != ENTRY_FUNCTION)
        internal("Cannot make a non-function forward");
    f->u.eFunction.isForward = true;
}

void endFunctionHeader (SymbolEntry * f, Type type)
{
    if (f->entryType != ENTRY_FUNCTION)
        internal("Cannot end parameters in a non-function");
    switch (f->u.eFunction.pardef) {
        case PARDEF_COMPLETE:
            internal("Cannot end parameters in an already defined function");
            break;
        case PARDEF_DEFINE:
            fixOffset(f->u.eFunction.firstArgument);
            f->u.eFunction.resultType = type;
            type->refCount++;
            break;
        case PARDEF_CHECK:
            if ((f->u.eFunction.lastArgument != NULL &&
                 f->u.eFunction.lastArgument->u.eParameter.next != NULL) ||
                (f->u.eFunction.lastArgument == NULL &&
                 f->u.eFunction.firstArgument != NULL))
                error("Fewer parameters than expected in redeclaration "
                      "of function %s", f->id);
            if (!equalType(f->u.eFunction.resultType, type))
                error("Result type mismatch in redeclaration of function %s",
                      f->id);
            break;
    }
    f->u.eFunction.pardef = PARDEF_COMPLETE;
}

SymbolEntry * newTemporary (Type type)
{
    char buffer[10];
    SymbolEntry * e;

    sprintf(buffer, "$%d", tempNumber);
    e = newEntry(buffer);

    if (e != NULL) {
        e->entryType = ENTRY_TEMPORARY;
        e->u.eVariable.type = type;
        type->refCount++;
        currentScope->negOffset -= sizeOfType(type);
        e->u.eTemporary.offset = currentScope->negOffset;
        e->u.eTemporary.number = tempNumber++;
    }
    return e;
}

void destroyEntry (SymbolEntry * e)
{
    SymbolEntry * args;

    switch (e->entryType) {
        case ENTRY_VARIABLE:
            destroyType(e->u.eVariable.type);
            break;
        case ENTRY_CONSTANT:
            if (e->u.eConstant.type->kind == TYPE_ARRAY)
                delete((char *) (e->u.eConstant.value.vString));
            destroyType(e->u.eConstant.type);
            break;
        case ENTRY_FUNCTION:
            args = e->u.eFunction.firstArgument;
            while (args != NULL) {
                SymbolEntry * p = args;

                destroyType(args->u.eParameter.type);
                delete((char *) (args->id));
                args = args->u.eParameter.next;
                delete(p);
            }
            destroyType(e->u.eFunction.resultType);
            break;
        case ENTRY_PARAMETER:
            /* �� ���������� �������������� ���� �� �� ��������� */
            return;
        case ENTRY_TEMPORARY:
            destroyType(e->u.eTemporary.type);
            break;
    }
    delete((char *) (e->id));
    delete(e);
}

SymbolEntry * lookupEntry (const char * name, LookupType type, bool err)
{
    unsigned int  hashValue = PJW_hash(name) % hashTableSize;
    SymbolEntry * e         = hashTable[hashValue];

    switch (type) {
        case LOOKUP_CURRENT_SCOPE:
            while (e != NULL && e->nestingLevel == currentScope->nestingLevel)
                if (strcmp(e->id, name) == 0)
                    return e;
                else
                    e = e->nextHash;
            break;
        case LOOKUP_ALL_SCOPES:
            while (e != NULL)
                if (strcmp(e->id, name) == 0)
                    return e;
                else
                    e = e->nextHash;
            break;
    }

    if (err)
        error("Unknown identifier: %s", name);
    return NULL;
}

Type typeArray (RepInteger size, Type refType)
{
    Type n = (Type) my_new(sizeof(struct Type_tag));

    n->kind     = TYPE_ARRAY;
    n->refType  = refType;
    n->size     = size;
    n->refCount = 1;

    refType->refCount++;

    return n;
}

Type typeIArray (Type refType)
{
    Type n = (Type) my_new(sizeof(struct Type_tag));

    n->kind     = TYPE_IARRAY;
    n->refType  = refType;
    n->refCount = 1;

    refType->refCount++;

    return n;
}

Type typePointer (Type refType)
{
    Type n = (Type) my_new(sizeof(struct Type_tag));

    n->kind     = TYPE_POINTER;
    n->refType  = refType;
    n->refCount = 1;

    refType->refCount++;

    return n;
}

Type typeList (Type refType)
{
    Type n = (Type) my_new(sizeof(struct Type_tag));

    n->kind     = TYPE_LIST;
    n->refType  = refType;
    n->refCount = 1;

    refType->refCount++;

    return n;
}

void destroyType (Type type)
{
    switch (type->kind) {
        case TYPE_ARRAY:
        case TYPE_IARRAY:
        case TYPE_LIST:
        case TYPE_POINTER:
            if (--(type->refCount) == 0) {
                destroyType(type->refType);
                delete(type);
            }
    }
}

unsigned int sizeOfType (Type type)
{
    switch (type->kind) {
        case TYPE_VOID:
            internal("Type void has no size");
            break;
        case TYPE_INTEGER:
        case TYPE_IARRAY:
        case TYPE_POINTER:
          return 8;
        case TYPE_LIST:
            return 16;
        case TYPE_BOOLEAN:
        case TYPE_CHAR:
            return 1;
        case TYPE_REAL:
            return 10;
        case TYPE_ARRAY:
            return type->size * sizeOfType(type->refType);
    }
    return 0;
}

bool equalType (Type type1, Type type2)
{
    if (type1->kind != type2->kind) {
      if (type1->kind == TYPE_ANY || type2->kind == TYPE_ANY)
        return true;
      else
        return false;
    }
    switch (type1->kind) {
        case TYPE_ARRAY:
            if (type1->size != type2->size)
                return false;
        case TYPE_IARRAY:
        case TYPE_POINTER:
            return equalType(type1->refType, type2->refType);
            break;
        case TYPE_LIST:
            return equalType(type1->refType, type2->refType);
        case TYPE_ANY:
            return false;
    }
    return true;
}

bool isTypeAny (Type type) {
  if (type->kind == TYPE_ANY)
    return true;
  else
    return false;
}

bool isTypeArray (Type type) {
  if (type->kind == TYPE_IARRAY || type->kind == TYPE_ARRAY)
    return true;
  else
    return false;
}

void printType (Type type)
{
    if (type == NULL) {
        printf("<undefined>");
        return;
    }

    switch (type->kind) {
        case TYPE_VOID:
            printf("void");
            break;
        case TYPE_INTEGER:
            printf("integer");
            break;
        case TYPE_BOOLEAN:
            printf("boolean");
            break;
        case TYPE_CHAR:
            printf("char");
            break;
        case TYPE_REAL:
            printf("real");
            break;
        case TYPE_ARRAY:
            printf("array [%d] of ", type->size);
            printType(type->refType);
            break;
        case TYPE_IARRAY:
            printf("array of ");
            printType(type->refType);
            break;
        case TYPE_POINTER:
            printf("^");
            printType(type->refType);
            break;
        case TYPE_LIST:
              printf("list of ");
              printType(type->refType);
        case TYPE_ANY:
              printf("any type");
    }
}

void printMode (PassMode mode)
{
    if (mode == PASS_BY_REFERENCE)
        printf("var ");
}

#define SHOW_OFFSETS

void printSymbolTable ()
{
    Scope       * scp;
    SymbolEntry * e;
    SymbolEntry * args;

    scp = currentScope;
    if (scp == NULL)
        printf("no scope\n");
    else
        while (scp != NULL) {
            printf("scope: ");
            e = scp->entries;
            while (e != NULL) {
                if (e->entryType == ENTRY_TEMPORARY)
                    printf("$%d", e->u.eTemporary.number);
                else
                    printf("%s", e->id);
                switch (e->entryType) {
                    case ENTRY_FUNCTION:
                        printf("(");
                        args = e->u.eFunction.firstArgument;
                        while (args != NULL) {
                            printMode(args->u.eParameter.mode);
                            printf("%s : ", args->id);
                            printType(args->u.eParameter.type);
                            args = args->u.eParameter.next;
                            if (args != NULL)
                                printf("; ");
                        }
                        printf(") : ");
                        printType(e->u.eFunction.resultType);
                        break;
#ifdef SHOW_OFFSETS
                    case ENTRY_VARIABLE:
                        printf("[%d]", e->u.eVariable.offset);
                        break;
                    case ENTRY_PARAMETER:
                        printf("[%d]", e->u.eParameter.offset);
                        break;
                    case ENTRY_TEMPORARY:
                        printf("[%d]", e->u.eTemporary.offset);
                        break;
#endif
                }
                //printf(" Nesting level: %d\n", e->nestingLevel);
                e = e->nextInScope;
                if (e != NULL)
                    printf(", ");
            }
            scp = scp->parent;
            printf("\n");
        }
    printf("----------------------------------------\n");
}

void StandardLibraryInit() {

  SymbolEntry *p;

  //decl puti (int n)
  p = newFunction("puti");
  openScope();
  newParameter("n", typeInteger, PASS_BY_VALUE, p);
  endFunctionHeader(p, typeVoid);
  closeScope();

  //decl putb (bool b)
  p = newFunction("putb");
  openScope();
  newParameter("c", typeBoolean, PASS_BY_VALUE, p);
  endFunctionHeader(p, typeVoid);
  closeScope();

  //decl putc (char c)
  p = newFunction("putc");
  openScope();
  newParameter("c", typeChar, PASS_BY_VALUE, p);
  endFunctionHeader(p, typeVoid);
  closeScope();

  //decl puts (char[] s)
  p = newFunction("puts");
  openScope();
  newParameter("s", typeIArray(typeChar), PASS_BY_VALUE, p);
  endFunctionHeader(p, typeVoid);
  closeScope();


  //decl int geti (int n)
  p = newFunction("geti");
  openScope();
  endFunctionHeader(p, typeInteger);
  closeScope();

  //decl bool getb (bool b)
  p = newFunction("getb");
  openScope();
  endFunctionHeader(p, typeBoolean);
  closeScope();

  //decl char getc (char c)
  p = newFunction("getc");
  openScope();
  endFunctionHeader(p, typeChar);
  closeScope();

  //decl gets (int n, char[] s)
  p = newFunction("gets");
  openScope();
  newParameter("n", typeInteger, PASS_BY_VALUE, p);
  newParameter("s", typeIArray(typeChar), PASS_BY_VALUE, p);
  endFunctionHeader(p, typeVoid);
  closeScope();

  //decl int abs (int n)
  p = newFunction("abs");
  openScope();
  newParameter("n", typeInteger, PASS_BY_VALUE, p);
  endFunctionHeader(p, typeInteger);
  closeScope();

  //decl int ord (char c)
  p = newFunction("ord");
  openScope();
  newParameter("c", typeChar, PASS_BY_VALUE, p);
  endFunctionHeader(p, typeInteger);
  closeScope();

  //decl char chr (int n)
  p = newFunction("chr");
  openScope();
  newParameter("n", typeInteger, PASS_BY_VALUE, p);
  endFunctionHeader(p, typeChar);
  closeScope();

  //decl int strlen (char[] s)
  p = newFunction("strlen");
  openScope();
  newParameter("s", typeIArray(typeChar), PASS_BY_VALUE, p);
  endFunctionHeader(p, typeInteger);
  closeScope();

  //decl int strcmp (char[] s1, s2)
  p = newFunction("strcmp");
  openScope();
  newParameter("s1", typeIArray(typeChar), PASS_BY_VALUE, p);
  newParameter("s2", typeIArray(typeChar), PASS_BY_VALUE, p);
  endFunctionHeader(p, typeInteger);
  closeScope();

  //decl strcpy (char[] trg, src)
  p = newFunction("strcpy");
  openScope();
  newParameter("trg", typeIArray(typeChar), PASS_BY_VALUE, p);
  newParameter("src", typeIArray(typeChar), PASS_BY_VALUE, p);
  endFunctionHeader(p, typeVoid);
  closeScope();

  //decl strcat (char[] trg, src)
  p = newFunction("strcat");
  openScope();
  newParameter("trg", typeIArray(typeChar), PASS_BY_VALUE, p);
  newParameter("src", typeIArray(typeChar), PASS_BY_VALUE, p);
  endFunctionHeader(p, typeVoid);
  closeScope();

}

void addLiveVariable(SymbolEntry *e) {
  live_variables.insert(e);
}
