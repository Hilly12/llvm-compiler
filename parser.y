%{
#include "node.h"
#include <cstdio>
#include <cstdlib>
NProgram *ast; /* the top level root node of our final AST */

extern int yylex();
void yyerror(const char *s) { std::printf("Error: %s\n", s);std::exit(1); }
%}

/* Represents the many different ways we can access our data */
%union {
    Node *node;
    NProgram *program;
    NClass *cls;
    NBlock *block;
    NExpression *expr;
    NStatement *stmt;
    NIdentifier *ident;
    NVariableDeclaration *var_decl;
    std::vector<NVariableDeclaration*> *varvec;
    std::vector<NExpression*> *exprvec;
    std::string *string;
    int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> IDENTIFIER INTEGER DOUBLE
%token <token> CLASS
%token <token> ASSIGN TCEQ TCNE TCLT TCLE TCGT TCGE TEQUAL
%token <token> OP CP OC CC COMMA
%token <token> PLUS MINUS MULTIPLY DIVIDE
%token <token> RETURN

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type <ident> ident
%type <expr> numeric expr
%type <varvec> func_decl_args
%type <exprvec> call_args
%type <program> program classes
%type <cls> class
%type <block> stmts block
%type <stmt> stmt var_decl func_decl
%type <token> comparison

/* Operator precedence for mathematical operators */
%left PLUS MINUS
%left MULTIPLY DIVIDE

%start program

%%

program : classes { ast = $1; }
        ;

classes : class { $$ = new NProgram(); $$->classes.push_back($<class>1); }
        | classes class { $1->classes.push_back($<class>2)}
        ;

class : CLASS ident OC feature CC;

feature: var_decl
       | func_decl
       | var_decl feature
       | func_decl feature
       ;

class_var: var_decl | var_decl class_var;

class_func: var_decl | var_decl class_var;

stmts : stmt { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
      | stmts stmt { $1->statements.push_back($<stmt>2); }
      ;

stmt : var_decl | func_decl
     | expr { $$ = new NExpressionStatement(*$1); }
     | RETURN expr { $$ = new NReturnStatement(*$2); }
     ;

block : OC stmts CC { $$ = $2; }
      | OC CC { $$ = new NBlock(); }
      ;

var_decl : ident ident { $$ = new NVariableDeclaration(*$1, *$2); }
         | ident ident ASSIGN expr { $$ = new NVariableDeclaration(*$1, *$2, $4); }
         ;

func_decl : ident ident OP func_decl_args CP block 
            { $$ = new NFunctionDeclaration(*$1, *$2, *$4, *$6); delete $4; }
          ;
    
func_decl_args : /*blank*/  { $$ = new VariableList(); }
          | var_decl { $$ = new VariableList(); $$->push_back($<var_decl>1); }
          | func_decl_args COMMA var_decl { $1->push_back($<var_decl>3); }
          ;

ident : IDENTIFIER { $$ = new NIdentifier(*$1); delete $1; }
      ;

numeric : INTEGER { $$ = new NInteger(atol($1->c_str())); delete $1; }
        | DOUBLE { $$ = new NDouble(atof($1->c_str())); delete $1; }
        ;
    
expr : ident TEQUAL expr { $$ = new NAssignment(*$<ident>1, *$3); }
     | ident OP call_args CP { $$ = new NMethodCall(*$1, *$3); delete $3; }
     | ident { $<ident>$ = $1; }
     | numeric
         | expr MULTIPLY expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
         | expr DIVIDE expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
         | expr PLUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
         | expr MINUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
      | expr comparison expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | OP expr CP { $$ = $2; }
     ;
    
call_args : /*blank*/  { $$ = new ExpressionList(); }
          | expr { $$ = new ExpressionList(); $$->push_back($1); }
          | call_args COMMA expr  { $1->push_back($3); }
          ;

comparison : TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE;

%%