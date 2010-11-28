%{
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

//easy type comaparions for quads
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
item quad; //temp used in rules

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

%}

//explicit values for the token identifiers
//fixes bison/flex/etc compilation issues
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
program : 
    opening var_lst fun_lst block '.' { print_next_and_rule("P -> QVZB."); return 0;}
  | opening var_lst block '.'         { print_next_and_rule("P -> QVB.");  return 0;}
	;

// Q -> program id(L);
opening	: 
  PROGRAM IDENTIFIER '(' id_list ')' ';' 
                              { print_next_and_rule("Q -> program "); printf("%s(L)", $2 -> lexeme);}
	;

// I -> input | output | id
io_id	: 
    INPUT                     { print_next_and_rule("I -> input");      }
  | OUTPUT                    { print_next_and_rule("I -> output");     }
  | IDENTIFIER                { print_next_and_rule("I -> "); printf("%s", $1 -> lexeme); }
	;

// L -> I | I, L
id_list	: 
    io_id                     { print_next_and_rule("L -> I");   }
  | io_id ',' id_list         { print_next_and_rule("L -> I,L"); }
	;

// V -> var D; | VD;
var_lst	: 
    VAR var_dec ';'	          { print_next_and_rule("V -> var D;"); }
  | var_lst var_dec ';'       { print_next_and_rule("V -> VD;"); }
	;
	
// D -> id:Y | id,D
var_dec	: 
  IDENTIFIER ':' type_ex  {
    print_next_and_rule("D -> "); printf("%s:Y", $1->lexeme);
    $$ = $3; 

		if ($3 -> lex_value == 270) 
      printf(" int");
		else if ($3 -> lex_value == 271) 
      printf(" double");
		
    printf(" %s;", $1 -> lexeme); 
  }
	
  | IDENTIFIER ',' var_dec  { 
    print_next_and_rule("D -> "); printf("%s,D", $1->lexeme);
    $$ = $3; 
                
    if ($3 -> lex_value == 270) 
      printf(" int");
		else if ($3 -> lex_value == 271) 
      printf(" double");  
    
    printf(" %s;", $1 -> lexeme); 
  }
	;

// Y -> integer | real
type_ex	: 
    INTKEY	                  { print_next_and_rule("Y -> integer");}
	| REALKEY                   { print_next_and_rule("Y -> real"); }
	;

// Z -> M | ZM
fun_lst : 
    fun_dec                   { print_next_and_rule("Z -> M"); }
  | fun_lst fun_dec           { print_next_and_rule("Z -> ZM");}
  
  ;

// M -> HVB
fun_dec : 
  header var_lst block        { print_next_and_rule("M -> HVB"); }

  ;

// H -> function id(U):result Y;
header : 
  FUNCTION IDENTIFIER '(' arg_list ')' ':' RESULT type_ex';' 
                              { print_next_and_rule("H -> function "); printf("%s(U):result Y;", $2->lexeme); }
  ;

// U -> A | U;A
arg_list :  
    arg_spec                  { print_next_and_rule("U -> A"); }
  | arg_list ';' arg_spec     { print_next_and_rule("U -> U;A"); }

  ;

// A -> id:Y | id,A
arg_spec : 
    IDENTIFIER ':' type_ex    { print_next_and_rule("A -> "); printf("%s:Y", $1->lexeme); 
      $$ = $3;
    }
  | IDENTIFIER ',' arg_spec   { print_next_and_rule("A -> "); printf("%s:Y", $1->lexeme); 
      $$ = $3;
  }
  ;

// B -> begin K end
block : BEGKEY st_list END    { print_next_and_rule("B -> begin K end"); }
 ;

// K -> S | K;S
st_list :
    statement                 { print_next_and_rule("K -> S"); }
  | st_list ';' statement     { print_next_and_rule("K -> K;S"); }
  ; 

// S -> id:=E | if R then S else S | read(X) | write(X) | id(X) | B
statement : 
    IDENTIFIER ASSIGN expresion               { print_next_and_rule("S -> "); printf("%s:=E", $1->lexeme); 
      
      if($3->lex_value == TEMP)
          T_count--;

      quad = add_quad(ASSIGN, $3, NULL);
      quad->either.op3 = $1;
      print_quad(quad);
    }

  | IF rel_exp THEN statement ELSE statement  { print_next_and_rule("S -> if R then S else S"); }
  | READ '(' ex_list ')'                      { print_next_and_rule("S -> read(X)"); }
  | WRITE '(' ex_list ')'                     { print_next_and_rule("S -> write(X)"); }
  | IDENTIFIER '(' ex_list ')'                { print_next_and_rule("S -> id(X)"); printf(" %s", $1->lexeme);}
  | block                                     { print_next_and_rule("S -> B"); }
  ;

// R -> E relop E
rel_exp: 
  expresion RELOP expresion   { print_next_and_rule("R -> E "); printf("%s E", $2->lexeme); }
  ;

// X -> E | X,E
ex_list: 
    expresion                 { print_next_and_rule("X -> E");   }
  | ex_list ',' expresion     { print_next_and_rule("X -> X,E"); }
  ;

// E -> E+T | E-T | T
expresion: 
    expresion '+' term        { print_next_and_rule("E -> E+T"); 
     
      if($1->lex_value == TEMP) {
        $$ = $1;
        if($3->lex_value == TEMP)
          T_count--;
      } else if($3->lex_value == TEMP) {
          $$ = $3;
      } else {
          $$ = get_temp();
      }

      quad = add_quad((int) '+', $1, $3);
      quad->either.op3 = $$;
      print_quad(quad);
  }

  | expresion '-' term        { print_next_and_rule("E -> E-T"); 

      if($1->lex_value == TEMP) {
        $$ = $1;
        if($3->lex_value == TEMP)
          T_count--;
      } else if($3->lex_value == TEMP) {
          $$ = $3;
      } else {
          $$ = get_temp();
      }

      quad = add_quad((int) '-', $1, $3);
      quad->either.op3 = $$;
      print_quad(quad);
  }

  | term                      { print_next_and_rule("E -> T"); } 
  ;

// T -> T*F | T/F | F
term: 
    term '*' factor           { print_next_and_rule("T -> T*F"); 
      
      if($1->lex_value == TEMP) {
        $$ = $1;
        if($3->lex_value == TEMP)
          T_count--;
      } else if($3->lex_value == TEMP) {
          $$ = $3;
      } else {
          $$ = get_temp();
      }

      quad = add_quad((int) '*', $1, $3);
      quad->either.op3 = $$;
      print_quad(quad);
    }

  | term '/' factor           { print_next_and_rule("T -> T/F"); 
  
      if($1->lex_value == TEMP) {
        $$ = $1;
        if($3->lex_value == TEMP)
          T_count--;
      } else if($3->lex_value == TEMP) {
          $$ = $3;
      } else {
          $$ = get_temp();
      }

      quad = add_quad((int) '/', $1, $3);
      quad->either.op3 = $$;
      print_quad(quad);
  }

  | factor                    { print_next_and_rule("T -> F");   }
  ;

// F -> C | id | id(X) | (E)
factor: 
    constant                  { print_next_and_rule("F -> C"); }
  | IDENTIFIER                { print_next_and_rule("F -> "); printf("%s", $1->lexeme);}
  | IDENTIFIER '(' ex_list ')'{ print_next_and_rule("F -> "); printf("%s(X)", $1->lexeme); }
  | '(' expresion ')'         { print_next_and_rule("F -> (E)"); 
    $$ = $2;//left = expression, not (, this took an hour to figure out :-(
  }
  ;

// C -> whole_number | real_number
constant: 
    INTEGER                   { print_next_and_rule("C -> "); printf("%s", $1->lexeme);}
  | REAL                      { print_next_and_rule("C -> "); printf("%s", $1->lexeme);}
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
