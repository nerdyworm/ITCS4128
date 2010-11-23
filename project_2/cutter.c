%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lex.yy.c"

//omit yytokentype enum
#define YYTOKENTYPE
#define YYSTYPE pointer

#define YYERROR_VERBOSE 1

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
// P -> QVZB. | QVB.
program : opening var_lst fun_lst block '.' { print_next_token(); printf("\t\t QVZB."); return 0;}
  | opening var_lst block '.'	{ print_next_token(); printf("\t\t QV.");
				  printf("\n\n SUCCESSFUL PARSING \n\n");
				  return 0; }

	;
// Q -> program id(L);
opening	: PROGRAM IDENTIFIER '(' id_list ')' ';'
	;

// I -> input | output | id
io_id	: INPUT	{ print_next_token(); printf("\t\t input"); }
	| OUTPUT	{ print_next_token(); printf("\t\t output"); }
	| IDENTIFIER	{ print_next_token(); printf("\t\t %s", $1 -> lexeme); }
	;
// L -> I | I, L
id_list	: io_id	{ print_next_token(); printf("\t\t I"); }
	| io_id ',' id_list	{ print_next_token(); printf("\t\t I,L"); }
	;

// V -> var D; | VD;
var_lst	: VAR var_dec ';'		
	| var_lst var_dec ';'
	;
	
// D -> id:Y | id,D
var_dec	: IDENTIFIER ':' type_ex  { $$ = $3; $1 -> type_info = $3 -> lex_value;
		if ($3 -> lex_value == 270) printf("\n int");
		else if ($3 -> lex_value == 271) printf("\n double");
		printf(" %s ;", $1 -> lexeme); }
	| IDENTIFIER ',' var_dec  { $$ = $3; $1 -> type_info = $3 -> lex_value;
                if ($3 -> lex_value == 270) printf("\n int");
		else if ($3 -> lex_value == 271) printf("\n double");  
                printf(" %s ;", $1 -> lexeme); }
	;

// Y -> integer | real
type_ex	: INTKEY		{ $$ = $1; }
	| REALKEY { $$ =  $1; }
	;

// Z -> M | ZM
fun_lst : fun_dec   { print_next_token(); printf("\t\t Z");}
  | fun_lst fun_dec { print_next_token(); printf("\t\t ZM"); }
  
  ;

// M -> HVB
fun_dec : header var_lst block { print_next_token(); printf("\t\t HVB"); }

  ;
// H -> function id(U):result Y;
header : FUNCTION IDENTIFIER '(' arg_list ')' ':' type_ex';' {
          print_next_token(); printf("\t\t function id(U):result Y;"); }

  ;
// U -> A | U;A
arg_list :  arg_spec { print_next_token(); printf("\t\t A"); }
  | arg_list ';' arg_spec { print_next_token(); printf("\t\t U;A"); }

  ;
// A -> id:Y | id,A
arg_spec : IDENTIFIER ':' type_ex { print_next_token(); printf("\t\t id:Y"); }
  | IDENTIFIER ',' arg_spec { print_next_token(); printf("\t\t id,Y"); }


  ;
// B -> begin K end
block : BEGKEY st_list END {print_next_token(); printf("\t\t begin K end"); }

 ;
// K -> S | K;S
st_list : statement       { print_next_token(); printf("\t\t S"); }
  | st_list ';' statement { print_next_token(); printf("\t\t K;S"); }
  ; 
// S -> id:=E | if R then S else S | read(X) | write(X) | id(X) | B
statement : IDENTIFIER ':' '=' { print_next_token(); printf("\t\t id:=E"); }

  ;
// R -> E relop E
// X -> E | X,E
// E -> E+T | E-T | T
// T -> T*F | T/F | F
// F -> C | id | id(X) | (E)
// C -> whole_number | real_number

/* 
	P	program
	Q	opening
	V	var_lst
	Z	fun_lst
	B	block
	I	io_id
	L	id_list
	D	var_dec
	Y	type_ex
	M	fun_dec
	H 	header
	U	arg_lst
	A	arg_spec
	K	st_list
	S	statement
	E	expression
  R	rel_exp
	X	ex_list
	T	term
	F	factor
	C	constant
*/
%%

int yylex()
{ int answer;
  last_token = last_token -> link;
  if (last_token != NULL)
    { answer = last_token -> code;
      if ((answer < 256) || (answer > 300)) yylval = NULL;
      else yylval = last_token -> alias.reference;  

      //printf("answer :%d\n", answer);
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
