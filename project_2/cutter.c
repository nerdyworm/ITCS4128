%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lex.yy.c"

//omit yytokentype enum
#define YYTOKENTYPE
#define YYSTYPE pointer

int Q_count;
int T_count;
char conv[10];
char word[MAX_LENGTH];

struct quad
{ int serial;
  int opcode;
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
{ int a, i, k;
  char back[MAX_LENGTH];
  k = 0;
  a = ++T_count;
  while (a > 0)
      { back[k++] = conv[a % 10];
	a = a / 10;
      }
  k = k -1;
  for (i = k; i >= 0; i--) word[4+k-i] = back[i];
  return(make_entry(IDENTIFIER, word));
}
 
 
%}

//explicit values for the token identifiers
%start program

%token INTEGER 257
%token REAL 258
%token IDENTIFIER 259
%token PROGRAM 260
%token VAR 261
%token FUNCTION 262 
%token RESULT 263
%token PROCEDURE 264
%token BEGKEY 265
%token END 266
%token IF 267
%token THEN 268
%token ELSE 269
%token INTKEY 270
%token REALKEY 271
%token INPUT 272
%token OUTPUT 273
%token READ 274
%token WRITE 275
%token ASSIGN 276
%token RELOP 277
%token endmarker 278

%%

program : opening var_lst '.'	{ print_next_token(); printf("\t\t QV.");
				  printf("\n\n SUCCESSFUL PARSING \n\n");
				  return 0; }
	;

opening	: PROGRAM IDENTIFIER '(' id_list ')' ';'
	;

io_id	: INPUT	{ print_next_token(); printf("\t\t input"); }
	| OUTPUT	{ print_next_token(); printf("\t\t output"); }
	| IDENTIFIER	{ print_next_token(); printf("\t\t %s", $1 -> lexeme); }
	;

id_list	: io_id	{ print_next_token(); printf("\t\t I"); }
	| io_id ',' id_list	{ print_next_token(); printf("\t\t I,L"); }
	;

var_lst	: VAR var_dec ';'		
	| var_lst var_dec ';'
	;
	
var_dec	: IDENTIFIER ':' type_ex  { $$ = $3; $1 -> type_info = $3 -> lex_value;
		if ($3 -> lex_value == 270) printf("\n int");
		else if ($3 -> lex_value == 271) printf("\n double");
		printf(" %s ;", $1 -> lexeme); }
	| IDENTIFIER ',' var_dec  { $$ = $3; $1 -> type_info = $3 -> lex_value;
                if ($3 -> lex_value == 270) printf("\n int");
		else if ($3 -> lex_value == 271) printf("\n double");  
                printf(" %s ;", $1 -> lexeme); }
	;

 
type_ex	: INTKEY		{ $$ = $1; }
	| REALKEY
	;

%%

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
  printf("\n\n PARSING BEGINS HERE\n---------------------------------------\n");
  printf("\n\nNext Token\tTop of the Stack\n_____________________________________\n");
  last_token = first_token;
  yyparse();
  print_table();
  printf("\n\n THE PARSER IS FINISHED\n---------------------------------------\n");
  return 0;
}
