
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 1 "cutter.c"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lex.yy.c"

//omit yytokentype enum
//fixes bison/flex/etc compliation issues
#define YYTOKENTYPE

#define YYSTYPE pointer

//verbose error reporting for debugging
#define YYERROR_VERBOSE 1

#define TEMP 999

int Q_count;
int T_count;
char conv[10];
char word[MAX_LENGTH];

struct quad
{ int serial; //counter
  int opcode; //operation
  pointer op1;
  pointer op2;
  union
    { pointer op3;
      struct quad *target;
    } either;
  struct quad *next;
};

typedef struct quad *item;

item first_quad;
item last_quad;
item quad;

item add_quad(int operator, pointer first, pointer secnd)
{ Q_count++;
  last_quad -> next = (item) malloc(sizeof(struct quad));
  last_quad = last_quad -> next;
  last_quad -> serial = Q_count;
  last_quad -> opcode = operator;
  last_quad -> op1 = first;
  last_quad -> op2 = secnd;
  last_quad -> either.op3 = NULL;
  last_quad -> next = NULL;
  return(last_quad);
}

pointer get_temp()
{
  sprintf(word, "temp_%d", ++T_count);

  return(make_entry(TEMP, word));
}

void print_quad(struct quad *q)
{
  
  printf("\t(%d)\t", q->serial);

  if(q->opcode == ASSIGN)
    printf(" := ");
  else
    printf(" %c ", q->opcode);

  if(q->op1 != NULL)
    printf(" %s ", q->op1->lexeme);
  else
    printf(" _ ");

  if(q->op2 != NULL)
    printf(" %s ",  q->op2->lexeme);
  else
    printf(" _ ");

  if(q->either.op3 != NULL)
    printf(" %s ", q->either.op3->lexeme);
  else
    printf(" _ ");
}

void print_quad_sequence()
{
  struct quad *current;
  printf("\n\t Here is the quad sequence\n");

  current = first_quad->next;

  while(current != NULL)
  {
    printf("\n");
    print_quad(current);
    current = current->next;
  }
}



/* Line 189 of yacc.c  */
#line 178 "y.tab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     INTEGER = 257,
     REAL = 258,
     IDENTIFIER = 259,
     PROGRAM = 260,
     VAR = 261,
     FUNCTION = 262,
     RESULT = 263,
     PROCEDURE = 264,
     BEGKEY = 265,
     END = 266,
     IF = 267,
     THEN = 268,
     ELSE = 269,
     INTKEY = 270,
     REALKEY = 271,
     INPUT = 272,
     OUTPUT = 273,
     READ = 274,
     WRITE = 275,
     ASSIGN = 276,
     RELOP = 277,
     endmarker = 278
   };
#endif
/* Tokens.  */
#define INTEGER 257
#define REAL 258
#define IDENTIFIER 259
#define PROGRAM 260
#define VAR 261
#define FUNCTION 262
#define RESULT 263
#define PROCEDURE 264
#define BEGKEY 265
#define END 266
#define IF 267
#define THEN 268
#define ELSE 269
#define INTKEY 270
#define REALKEY 271
#define INPUT 272
#define OUTPUT 273
#define READ 274
#define WRITE 275
#define ASSIGN 276
#define RELOP 277
#define endmarker 278




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 268 "y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  5
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   99

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  35
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  22
/* YYNRULES -- Number of rules.  */
#define YYNRULES  47
/* YYNRULES -- Number of states.  */
#define YYNSTATES  109

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   279

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      26,    27,    33,    31,    29,    32,    25,    34,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    30,    28,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,     2
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     9,    14,    21,    23,    25,    27,    29,
      33,    37,    41,    45,    49,    51,    53,    55,    58,    62,
      72,    74,    78,    82,    86,    90,    92,    96,   100,   107,
     112,   117,   122,   124,   128,   130,   134,   138,   142,   144,
     148,   152,   154,   156,   158,   163,   167,   169
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      36,     0,    -1,    37,    40,    43,    48,    25,    -1,    37,
      40,    48,    25,    -1,     6,     5,    26,    39,    27,    28,
      -1,    18,    -1,    19,    -1,     5,    -1,    38,    -1,    38,
      29,    39,    -1,     7,    41,    28,    -1,    40,    41,    28,
      -1,     5,    30,    42,    -1,     5,    29,    41,    -1,    16,
      -1,    17,    -1,    44,    -1,    43,    44,    -1,    45,    40,
      48,    -1,     8,     5,    26,    46,    27,    30,     9,    42,
      28,    -1,    47,    -1,    46,    28,    47,    -1,     5,    30,
      42,    -1,     5,    29,    47,    -1,    11,    49,    12,    -1,
      50,    -1,    49,    28,    50,    -1,     5,    22,    53,    -1,
      13,    51,    14,    50,    15,    50,    -1,    20,    26,    52,
      27,    -1,    21,    26,    52,    27,    -1,     5,    26,    52,
      27,    -1,    48,    -1,    53,    23,    53,    -1,    53,    -1,
      52,    29,    53,    -1,    53,    31,    54,    -1,    53,    32,
      54,    -1,    54,    -1,    54,    33,    55,    -1,    54,    34,
      55,    -1,    55,    -1,    56,    -1,     5,    -1,     5,    26,
      52,    27,    -1,    26,    53,    27,    -1,     3,    -1,     4,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   136,   136,   137,   142,   148,   149,   150,   155,   156,
     161,   162,   167,   180,   196,   197,   202,   203,   209,   215,
     221,   222,   228,   231,   237,   242,   243,   248,   259,   260,
     261,   262,   263,   268,   273,   279,   287,   304,   321,   326,
     343,   360,   365,   366,   367,   371,   376,   377
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INTEGER", "REAL", "IDENTIFIER",
  "PROGRAM", "VAR", "FUNCTION", "RESULT", "PROCEDURE", "BEGKEY", "END",
  "IF", "THEN", "ELSE", "INTKEY", "REALKEY", "INPUT", "OUTPUT", "READ",
  "WRITE", "ASSIGN", "RELOP", "endmarker", "'.'", "'('", "')'", "';'",
  "','", "':'", "'+'", "'-'", "'*'", "'/'", "$accept", "program",
  "opening", "io_id", "id_list", "var_lst", "var_dec", "type_ex",
  "fun_lst", "fun_dec", "header", "arg_list", "arg_spec", "block",
  "st_list", "statement", "rel_exp", "ex_list", "expresion", "term",
  "factor", "constant", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   279,   257,   258,   259,   260,   261,   262,   263,
     264,   265,   266,   267,   268,   269,   270,   271,   272,   273,
     274,   275,   276,   277,   278,    46,    40,    41,    59,    44,
      58,    43,    45,    42,    47
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    35,    36,    36,    37,    38,    38,    38,    39,    39,
      40,    40,    41,    41,    42,    42,    43,    43,    44,    45,
      46,    46,    47,    47,    48,    49,    49,    50,    50,    50,
      50,    50,    50,    51,    52,    52,    53,    53,    53,    54,
      54,    54,    55,    55,    55,    55,    56,    56
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     5,     4,     6,     1,     1,     1,     1,     3,
       3,     3,     3,     3,     1,     1,     1,     2,     3,     9,
       1,     3,     3,     3,     3,     1,     3,     3,     6,     4,
       4,     4,     1,     3,     1,     3,     3,     3,     1,     3,
       3,     1,     1,     1,     4,     3,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     0,     1,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    16,     0,     0,     7,     5,
       6,     8,     0,     0,     0,    10,     0,     0,     0,     0,
       0,    32,     0,    25,    11,    17,     0,     0,     3,     0,
       0,    13,    14,    15,    12,     0,     0,     0,    46,    47,
      43,     0,     0,     0,    38,    41,    42,     0,     0,    24,
       0,     2,    18,     9,     4,     0,     0,    20,    27,     0,
      34,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    26,     0,     0,     0,     0,    31,     0,     0,    45,
       0,    33,    36,    37,    39,    40,    29,    30,    23,    22,
       0,    21,    35,    44,     0,     0,    28,     0,    19
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     2,     3,    21,    22,     7,    13,    44,    14,    15,
      16,    66,    67,    31,    32,    33,    52,    69,    70,    54,
      55,    56
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -78
static const yytype_int8 yypact[] =
{
      45,    17,     8,    26,    51,   -78,    55,    33,    11,    35,
      12,    57,    14,    40,     2,   -78,    26,    53,   -78,   -78,
     -78,    52,    58,    55,    50,   -78,    60,    27,     0,    61,
      62,   -78,     3,   -78,   -78,   -78,    59,     6,   -78,    11,
      63,   -78,   -78,   -78,   -78,    84,     0,     0,   -78,   -78,
      64,     0,    78,   -11,    36,   -78,   -78,     0,     0,   -78,
      14,   -78,   -78,   -78,   -78,    42,    46,   -78,    44,    25,
      44,     0,    16,    14,     0,     0,     0,     0,     0,    28,
      29,   -78,    84,    50,    65,    84,   -78,     0,    34,   -78,
      79,    44,    36,    36,   -78,   -78,   -78,   -78,   -78,   -78,
      87,   -78,    44,   -78,    14,    50,   -78,    69,   -78
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -78,   -78,   -78,   -78,    54,    82,     1,   -77,   -78,    85,
     -78,   -78,   -43,    -5,   -78,   -59,   -78,   -21,   -28,     4,
       5,   -78
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      53,    81,    17,    48,    49,    50,    99,    10,     5,    36,
      11,     9,    74,    12,    90,    59,    18,    12,    68,    27,
      75,    76,     4,    72,    41,    12,    51,    28,   107,    19,
      20,    60,    62,     6,    29,    30,    79,    80,     9,    98,
      25,    11,   101,    89,    12,   106,    91,    75,    76,    46,
      88,     1,    86,    47,    87,    96,    97,    87,    87,   102,
       9,   103,    26,    87,    23,    24,    42,    43,    34,    77,
      78,    82,    83,    84,    85,    75,    76,     8,    38,    92,
      93,    39,    94,    95,    61,    40,    45,    57,    58,    65,
      71,    64,    73,    63,   104,   100,   105,   108,    37,    35
};

static const yytype_uint8 yycheck[] =
{
      28,    60,     7,     3,     4,     5,    83,     6,     0,    14,
       8,     5,    23,    11,    73,    12,     5,    11,    46,     5,
      31,    32,     5,    51,    23,    11,    26,    13,   105,    18,
      19,    28,    37,     7,    20,    21,    57,    58,     5,    82,
      28,     8,    85,    27,    11,   104,    74,    31,    32,    22,
      71,     6,    27,    26,    29,    27,    27,    29,    29,    87,
       5,    27,     5,    29,    29,    30,    16,    17,    28,    33,
      34,    29,    30,    27,    28,    31,    32,    26,    25,    75,
      76,    29,    77,    78,    25,    27,    26,    26,    26,     5,
      26,    28,    14,    39,    15,    30,     9,    28,    16,    14
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     6,    36,    37,     5,     0,     7,    40,    26,     5,
      41,     8,    11,    41,    43,    44,    45,    48,     5,    18,
      19,    38,    39,    29,    30,    28,     5,     5,    13,    20,
      21,    48,    49,    50,    28,    44,    48,    40,    25,    29,
      27,    41,    16,    17,    42,    26,    22,    26,     3,     4,
       5,    26,    51,    53,    54,    55,    56,    26,    26,    12,
      28,    25,    48,    39,    28,     5,    46,    47,    53,    52,
      53,    26,    53,    14,    23,    31,    32,    33,    34,    52,
      52,    50,    29,    30,    27,    28,    27,    29,    52,    27,
      50,    53,    54,    54,    55,    55,    27,    27,    47,    42,
      30,    47,    53,    27,    15,     9,    50,    42,    28
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

/* Line 1455 of yacc.c  */
#line 136 "cutter.c"
    { print_next_and_rule("P -> QVZB."); return 0;}
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 137 "cutter.c"
    { print_next_and_rule("P -> QVB.");  return 0;}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 143 "cutter.c"
    { print_next_and_rule("Q -> program "); printf("%s(L)", (yyvsp[(2) - (6)]) -> lexeme);}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 148 "cutter.c"
    { print_next_and_rule("I -> input");      }
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 149 "cutter.c"
    { print_next_and_rule("I -> output");     }
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 150 "cutter.c"
    { print_next_and_rule("I -> "); printf("%s", (yyvsp[(1) - (1)]) -> lexeme); }
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 155 "cutter.c"
    { print_next_and_rule("L -> I");   }
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 156 "cutter.c"
    { print_next_and_rule("L -> I,L"); }
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 161 "cutter.c"
    { print_next_and_rule("V -> var D;"); }
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 162 "cutter.c"
    { print_next_and_rule("V -> VD;"); }
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 167 "cutter.c"
    {
    print_next_and_rule("D -> "); printf("%s:Y", (yyvsp[(1) - (3)])->lexeme);
    (yyval) = (yyvsp[(3) - (3)]); 
    (yyvsp[(1) - (3)]) -> type_info = (yyvsp[(3) - (3)]) -> lex_value;

		if ((yyvsp[(3) - (3)]) -> lex_value == 270) 
      printf(" int");
		else if ((yyvsp[(3) - (3)]) -> lex_value == 271) 
      printf(" double");
		
    printf(" %s;", (yyvsp[(1) - (3)]) -> lexeme); 
  }
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 180 "cutter.c"
    { 
    print_next_and_rule("D -> "); printf("%s,D", (yyvsp[(1) - (3)])->lexeme);
    (yyval) = (yyvsp[(3) - (3)]); 
    (yyvsp[(1) - (3)]) -> type_info = (yyvsp[(3) - (3)]) -> lex_value;
                
    if ((yyvsp[(3) - (3)]) -> lex_value == 270) 
      printf(" int");
		else if ((yyvsp[(3) - (3)]) -> lex_value == 271) 
      printf(" double");  
    
    printf(" %s;", (yyvsp[(1) - (3)]) -> lexeme); 
  }
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 196 "cutter.c"
    { print_next_and_rule("Y -> integer");}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 197 "cutter.c"
    { print_next_and_rule("Y -> real"); }
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 202 "cutter.c"
    { print_next_and_rule("Z -> M"); }
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 203 "cutter.c"
    { print_next_and_rule("Z -> ZM");}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 209 "cutter.c"
    { print_next_and_rule("M -> HVB"); }
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 216 "cutter.c"
    { print_next_and_rule("H -> function "); printf("%s(U):result Y;", (yyvsp[(2) - (9)])->lexeme); }
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 221 "cutter.c"
    { print_next_and_rule("U -> A"); }
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 222 "cutter.c"
    { print_next_and_rule("U -> U;A"); }
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 228 "cutter.c"
    { print_next_and_rule("A -> "); printf("%s:Y", (yyvsp[(1) - (3)])->lexeme); 
      (yyval) = (yyvsp[(3) - (3)]);
    }
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 231 "cutter.c"
    { print_next_and_rule("A -> "); printf("%s:Y", (yyvsp[(1) - (3)])->lexeme); 
      (yyval) = (yyvsp[(3) - (3)]);
  }
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 237 "cutter.c"
    { print_next_and_rule("B -> begin K end"); }
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 242 "cutter.c"
    { print_next_and_rule("K -> S"); }
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 243 "cutter.c"
    { print_next_and_rule("K -> K;S"); }
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 248 "cutter.c"
    { print_next_and_rule("S -> "); printf("%s:=E", (yyvsp[(1) - (3)])->lexeme); 
      
      if((yyvsp[(3) - (3)])->lex_value == TEMP)
        T_count--;

      quad = add_quad(ASSIGN, (yyvsp[(3) - (3)]), NULL);
      quad->either.op3 = (yyvsp[(1) - (3)]);
      print_quad(quad);

    }
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 259 "cutter.c"
    { print_next_and_rule("S -> if R then S else S"); }
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 260 "cutter.c"
    { print_next_and_rule("S -> read(X)"); }
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 261 "cutter.c"
    { print_next_and_rule("S -> write(X)"); }
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 262 "cutter.c"
    { print_next_and_rule("S -> id(X)"); printf(" %s", (yyvsp[(1) - (4)])->lexeme);}
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 263 "cutter.c"
    { print_next_and_rule("S -> B"); }
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 268 "cutter.c"
    { print_next_and_rule("R -> E "); printf("%s E", (yyvsp[(2) - (3)])->lexeme); }
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 273 "cutter.c"
    { print_next_and_rule("X -> E"); 
      quad = add_quad((int)'(', (yyvsp[(1) - (1)]), NULL);
      print_quad(quad);
    
    }
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 279 "cutter.c"
    { print_next_and_rule("X -> X,E"); 
      quad = add_quad((int)',', (yyvsp[(3) - (3)]), NULL);
      print_quad(quad);
  }
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 287 "cutter.c"
    { print_next_and_rule("E -> E+T"); 
     
      if((yyvsp[(1) - (3)])->lex_value == TEMP) {
        (yyval) = (yyvsp[(1) - (3)]);
        if((yyvsp[(3) - (3)])->lex_value == TEMP)
          T_count--;
      } else if((yyvsp[(3) - (3)])->lex_value == TEMP) {
          (yyval) = (yyvsp[(3) - (3)]);
      } else {
          (yyval) = get_temp();
      }

      quad = add_quad((int) '+', (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
      quad->either.op3 = (yyval);
      print_quad(quad);
  }
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 304 "cutter.c"
    { print_next_and_rule("E -> E-T"); 

      if((yyvsp[(1) - (3)])->lex_value == TEMP) {
        (yyval) = (yyvsp[(1) - (3)]);
        if((yyvsp[(3) - (3)])->lex_value == TEMP)
          T_count--;
      } else if((yyvsp[(3) - (3)])->lex_value == TEMP) {
          (yyval) = (yyvsp[(3) - (3)]);
      } else {
          (yyval) = get_temp();
      }

      quad = add_quad((int) '-', (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
      quad->either.op3 = (yyval);
      print_quad(quad);
  }
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 321 "cutter.c"
    { print_next_and_rule("E -> T"); }
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 326 "cutter.c"
    { print_next_and_rule("T -> T*F"); 
      
      if((yyvsp[(1) - (3)])->lex_value == TEMP) {
        (yyval) = (yyvsp[(1) - (3)]);
        if((yyvsp[(3) - (3)])->lex_value == TEMP)
          T_count--;
      } else if((yyvsp[(3) - (3)])->lex_value == TEMP) {
          (yyval) = (yyvsp[(3) - (3)]);
      } else {
          (yyval) = get_temp();
      }

      quad = add_quad((int) '*', (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
      quad->either.op3 = (yyval);
      print_quad(quad);
    }
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 343 "cutter.c"
    { print_next_and_rule("T -> T/F"); 
  
      if((yyvsp[(1) - (3)])->lex_value == TEMP) {
        (yyval) = (yyvsp[(1) - (3)]);
        if((yyvsp[(3) - (3)])->lex_value == TEMP)
          T_count--;
      } else if((yyvsp[(3) - (3)])->lex_value == TEMP) {
          (yyval) = (yyvsp[(3) - (3)]);
      } else {
          (yyval) = get_temp();
      }

      quad = add_quad((int) '/', (yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]));
      quad->either.op3 = (yyval);
      print_quad(quad);
  }
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 360 "cutter.c"
    { print_next_and_rule("T -> F");   }
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 365 "cutter.c"
    { print_next_and_rule("F -> C"); }
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 366 "cutter.c"
    { print_next_and_rule("F -> "); printf("%s", (yyvsp[(1) - (1)])->lexeme);}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 367 "cutter.c"
    { print_next_and_rule("F -> "); printf("%s(X)", (yyvsp[(1) - (4)])->lexeme);
      quad = add_quad((int) ')', (yyvsp[(1) - (4)]), NULL);
      print_quad(quad);
  }
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 371 "cutter.c"
    { print_next_and_rule("F -> (E)"); (yyval) = (yyvsp[(2) - (3)]);}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 376 "cutter.c"
    { print_next_and_rule("C -> "); printf("%s", (yyvsp[(1) - (1)])->lexeme);}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 377 "cutter.c"
    { print_next_and_rule("C -> "); printf("%s", (yyvsp[(1) - (1)])->lexeme);}
    break;



/* Line 1455 of yacc.c  */
#line 1969 "y.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 1675 of yacc.c  */
#line 380 "cutter.c"


int yylex()
{ int answer;
  last_token = last_token -> link;
  if (last_token != NULL)
    { answer = last_token -> code;
      if ((answer < 256) || (answer > 300)) yylval = NULL;
      else yylval = last_token -> alias.reference;  
      return(answer);
    }
  else return (endmarker);
}

void print_next_token()
{ address next_token;
  if (last_token == NULL)
	{ printf("\n$\t   \t"); return; }
  else
	{ next_token = last_token -> link;
	  if (next_token == NULL)
		{ printf("\n$\t   \t"); return; }
        else
		{ if (next_token -> code < 256) 
           	    printf("\n%c\t   \t", next_token -> code); 
	 	  else 
                printf("\n%s\t\t", next_token -> alias.reference -> lexeme);
	      }
	}     
}

void print_next_and_rule(char *rule)
{
  print_next_token();
  printf("\t\t %s", rule);
}


int yyerror(char *msg)
{ printf("%s\n", msg);
  exit(1);
}

int main()
{ preload();
  first_token = (address) malloc(sizeof(struct token));
  first_token -> link = NULL;
  last_token = first_token;
  line_count = 0;
  printf("\n\n ECHOING THE INPUT STREAM: \n\n"); 
  printf("\n Line %2d:  ", ++line_count);
  tokenize();
  first_quad = (item) malloc(sizeof(struct quad));
  first_quad -> next = NULL;
  last_quad = first_quad;
  strcpy(conv, "0123456789");
  strcpy(word, "temp");
  Q_count = 0;
  T_count = 0;
  printf("\n\n PARSING BEGINS HERE ");
  printf("\n\nNext Token\tTop of the Stack\n_____________________________________\n");
  last_token = first_token;
  yyparse();
  print_table();
  print_quad_sequence();
  printf("\n\n THE PARSER IS FINISHED\n---------------------------------------\n");
  return 0;
}
