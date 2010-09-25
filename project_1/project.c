/** Benjamin Rhodes **/

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

#define NEWLINE 278
#define BLANK 279
#define TABKEY 280
#define WRONG 281

struct member
{
  int lex_value;
	char lexeme[MAX_LENGTH];
	int integer_const;
	double real_const;
	struct member *next;
};

typedef struct member entry;
typedef entry *pointer;

pointer hash_group[97];

struct token
{ 
  int code;
  struct token *link;
  union
  { 
    pointer reference;
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
  return((13*any[0] + 5*any[1] + 18*any[2] + 7*any[3]) % 97);
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
{ 
  int h;
  pointer new_loc;
  new_loc = (pointer) malloc(sizeof(struct member));
  new_loc -> lex_value = code;
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
 
void print_table()
{ 
  int h;
  pointer i;
  printf("\n\n THE CONTENTS OF THE SYMBOL TABLE:\n\n");
  for (h = 0; h < 97; h++)
  { 
    i = hash_group[h];
    while (i != NULL)
	  { 
	    printf("group[%2d] -> % 3d ", h, i -> lex_value);
	    printf("%s ", i -> lexeme);
	    
	    if (i -> lex_value == INTEGER)
        printf(" = %d", i -> integer_const);
      else if (i-> lex_value == REAL)
        printf(" = %5.7e", i -> real_const);
	    
	    printf("\n");
	    i = i -> next;
	  }
  }
}


void read_keys()
{ 
  int key_code;
  char key_word[MAX_LENGTH];
  pointer locus;
  while ((fscanf(key_list, "%i %s", &key_code, &key_word))!= EOF)
  { 
    locus = look_up(key_word);
    
    if (locus == NULL) 
      locus = make_entry(key_code, key_word);
    else
      printf("Duplicate Key Word '%s' rejected\n", key_word);
  }
}

void preload()
{ 
  int h;
  for (h = 0; h < 97; h++)
       hash_group[h] = NULL;
       
  if ((key_list=fopen("keywords", "r")) == NULL) { 
    printf("Can't read 'keywords' file"); 
    exit(1); 
  }
  
  read_keys();
}

void add_token_with_id(int id, char *t) 
{
  printf("%s", t);
  pointer locus = look_up(t);
  if (locus == NULL) 
  {
    locus = make_entry(id, t);
  }
  
  element *new_token = (element *) malloc(sizeof(element));

  new_token->code = id;
  new_token->alias.reference = locus->lexeme;
    
  last_token->link = new_token;
  last_token = last_token->link;
}


int key_code_count = 282;
void handle_other(char *text)
{
  pointer locus = look_up(text);
  int id = 0;
  if (locus == NULL) 
  {
   
    if(strlen(text) == 1)
      id = (int) text[0];
    else 
      id = ++key_code_count;
      
    locus = make_entry(id, text);
    
	}  else
    id = locus->lex_value;
    
	add_token_with_id(id, text);
}

%}

letter	  [A-Za-z]
digit	    [0-9]
ident	    {letter}({letter}|{digit})*
relop	    "<"|"<="|"="|">="|">"|"<>"|"!="
ws	      [ \t]+
nl	      [\n]
poz	      [1-9]
int	      {poz}{digit}*
exp	      (0\.)?[0-9]*[Ee][-+]?{int}
float     ([0-9]*)?\.[0-9]+
other	    .
%%

{ws}		  ECHO;
{nl}		  printf("\n Line %2d:  ", ++line_count);
{ident}		add_token_with_id(IDENTIFIER, yytext);
{exp}     add_token_with_id(REAL, yytext);
{float}   add_token_with_id(REAL, yytext);
{int}     add_token_with_id(INTEGER, yytext);
:=		    add_token_with_id(ASSIGN, yytext);
{relop}	  add_token_with_id(RELOP, yytext);
{other}   handle_other(yytext);

%%

void print_tokens()
{
  printf("\n\nHERE IS THE TOKEN SEQUENCE :\n\n");
  
  struct token *t = first_token;
  while(t) {
    printf("%d\t%s\n", t->code, t->alias.reference);
    t = t->link;
  }
}


int yywrap()
{ 
  print_table();
  print_tokens(); 
  return(1);
}

main()
{ preload();
  first_token = (address) malloc (sizeof(element));
  first_token -> link = NULL;
  last_token = first_token;
  printf("\n\n ECHOING THE INPUT STREAM: \n\n");
  line_count = 0;
  printf("\n Line %2d:  ", ++line_count);
  yylex();
  return(0);
}