%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_LENGTH 30

#define INTEGER 257
#define REAL 258
#define IDENTIFIER 259

#define ASSIGN 276
#define RELOP 277

#define NEWLINE 301
#define BLANK 302
#define TABKEY 303
#define WRONG 304

struct member
	{ int lex_value;
	  char lexeme[MAX_LENGTH];
	  int integer_const;
	  double real_const;
	  struct member *next;
// added
          int type_info;
	  int temp_flag;
	  void *anchor; 
	  int function_flag;
	};

typedef struct member entry;
typedef entry *pointer;

pointer hash_group[97];

struct token
  { int code;
    struct token *link;
    union
      { pointer reference;
        int supp_info;
      } alias;
   };

typedef struct token element;
typedef element *address; 

address first_token;
address last_token;

int line_count=0;

FILE *key_list;

int hash (char *ident)
{ int k;
  char any[MAX_LENGTH];
  for (k = 0; k < MAX_LENGTH - 1; k++) any[k] = ' ';
  any[MAX_LENGTH] = '\0';
  strcpy(any, ident);
  return((73*any[0] + 5*any[1] + 18*any[2] + 7*any[4]+ 31*any[5]) % 97);
}

pointer look_up(char *ident)
{ int h;
  pointer group_h;
  h = hash(ident);
  group_h = hash_group[h];
  while (group_h != NULL)
     { if (strcmp(group_h -> lexeme, ident) == 0)
           return(group_h);
       else
           group_h = group_h -> next;
     }
  return(NULL);
}

pointer make_entry(int code, char *ident)
{ int h;
  pointer new_loc;
  new_loc = (pointer) malloc(sizeof(struct member));
  new_loc -> lex_value = code;
  new_loc->temp_flag=0;
  new_loc->function_flag=0;
  new_loc->anchor=NULL;
  strcpy(new_loc -> lexeme, ident);
  if (code == INTEGER)
     new_loc -> integer_const = atoi(ident);
  else if (code == REAL)
     new_loc -> real_const = atof(ident);
  h = hash(ident);
  new_loc -> next = hash_group[h];
  hash_group[h] = new_loc;
  return(new_loc);
}

pointer install(int code, char *ident)
{ pointer locus;
  locus = look_up(ident);
  if (locus == NULL)
      locus = make_entry(code, ident);
   /*   printf("\n (%3d) ", locus -> lex_value);
      printf("%s ", locus -> lexeme);
      if (locus -> lex_value == INTEGER)
          printf(" = %d", locus -> integer_const);
      else if (locus -> lex_value == REAL)
          printf(" = %5.7e", locus -> real_const);
   */
      return (locus);
}

void print_table()
{ int h;
  pointer i;
  printf("\n\n THE CONTENTS OF THE SYMBOL TABLE:\n\n");
  for (h = 0; h < 97; h++)
      { i = hash_group[h];
        while (i != NULL)
	  { printf("group[%2d] -> % 3d ", h, i -> lex_value);
	    printf("%s ", i -> lexeme);
	    if ((i -> lex_value == IDENTIFIER) && (i -> type_info != 0))
	       printf("  (type = %d)", i -> type_info); 
	    else if (i -> lex_value == INTEGER)
               printf(" = %d", i -> integer_const);
            else if (i-> lex_value == REAL)
               printf(" = %5.7e", i -> real_const);
	    printf("\n");
	    i = i -> next;
	  }
       }
}

void create_token(int code, pointer symbol)
{ last_token -> link = (address) malloc (sizeof(element));
  last_token = last_token -> link;
  last_token -> link = NULL;
  if (symbol == NULL)
     { last_token -> code = code;
       if (code == NEWLINE)
           last_token -> alias.supp_info = line_count;
     }
  else
     { last_token -> code = symbol -> lex_value;
       last_token -> alias.reference = symbol;
     }
}

pointer do_both(int code, char *ident)
{ 
	pointer t=install(code,ident);
	create_token(code,t);
	return t;
}

void print_tokens()
{ address p;
  p = first_token -> link;
  printf("\n\n HERE IS THE TOKEN SEQUENCE :\n\n");
  while (p != NULL)
     { printf(" %3d ", p -> code);
       if (p -> code < INTEGER)
           printf("  %c ", p -> code);
       else if (p -> code < NEWLINE)
           printf("  %s ", p -> alias.reference -> lexeme);
       else if (p -> code == NEWLINE)
           printf(" line# %3d", p -> alias.supp_info);
       printf("\n");
       p = p -> link;
     }
  printf("\n");
}


void read_keys()
{ int key_code;
  char key_word[MAX_LENGTH];
  pointer locus;
  while ((fscanf(key_list, "%i %s", &key_code, &key_word))!= EOF)
      { locus = look_up(key_word);
        if (locus == NULL)
	    locus = make_entry(key_code, key_word);
        else
	   printf("Dupplicate Key Word '%s' rejected\n", key_word);
      }
}

preload()
{ int h;
  for (h = 0; h < 97; h++)
       hash_group[h] = NULL;
  if ((key_list=fopen("keywords", "r")) == NULL)
     { printf("Can't read 'keywords' file"); exit(1); }
  read_keys();
}

%}

letter	[A-Za-z]
digit	[0-9]
ident	{letter}({letter}|{digit})*
op	"+"|"-"|"*"|"/"
deli	"("|")"|";"|","|":"|"."
relop	"<"|"<="|"="|">="|">"|"<>"|"!="
ws	[ \t]+
nl	[\n]
poz	[1-9]
int	{poz}{digit}*
exp	[Ee][-+]?{int}
frac	{digit}*({exp})?
other	.

%%

{ws}			ECHO;
{nl}			printf("\n Line %2d:  ", ++line_count);
{ident}			{ printf("%s", yytext); do_both(IDENTIFIER, yytext); }
:=			{ printf("%s", yytext); do_both(ASSIGN,yytext); }
{relop}			{ printf("%s", yytext); do_both(RELOP,yytext);
				  last_token -> code = RELOP; }
({op}|{deli})		{ ECHO; create_token(yytext[0], NULL); }
(0|{int})		{ printf("%s", yytext); do_both(INTEGER,yytext); }
{digit}+		{ printf("%s", yytext); do_both(INTEGER,yytext); }
{int}({exp})?		{ printf("%s", yytext); do_both(REAL,yytext); }
(0|{int})"."{frac}	{ printf("%s", yytext); do_both(REAL,yytext); }
"."{digit}+({exp})?	{ printf("%s", yytext); do_both(REAL,yytext); }		
{other}		    printf("\n ERROR: Illegal character %c\n\t\t",yytext[0]);

%%

int yywrap()
{ print_tokens();
  printf("\n THE TOKENIZER IS FINISHED \n");
  print_table();
  return(1);
}

/*
int main()
{ preload();
  print_table();
  first_token = (address) malloc (sizeof(element));
  first_token -> link = NULL;
  last_token = first_token;
  printf("\n\n ECHOING THE INPUT STREAM: \n\n");
  line_count = 0;
  printf("\n Line %2d:  ", ++line_count);
  yylex();
  return(0);
}

*/



