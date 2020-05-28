%{
	#include "node.h"
        #include <cstdio>
        #include <cstdlib>
	#include<vector>
	NBlock *programBlock; /* the top level root node of our final AST */

	extern int yylex();
	void yyerror(const char *s) { std::printf("Error: %s\n", s);std::exit(1); }
%}

/* Represents the many different ways we can access our data */
%union {
	Node *node;
	NBlock *block;
	NExpression *expr;
	NStatement *stmt;
	NIdentifier *ident;
	NVariableDeclaration *var_decl;
	NVariableDeclarationS *varlist_decl;
	NFunctionDeclaration *func_decl;
	std::vector<NVariableDeclaration*> *varvec;
	std::vector<NVariableDeclarationS*> *varlistvec;
	std::vector<NExpression*> *exprvec;
	std::vector<std::string> *stringvec;
	std::vector<NIdentifier *> *identlist;
	std::string *string;
	int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> TIDENTIFIER TINTEGER TDOUBLE
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE TEQUAL
%token <token> TLPAREN TRPAREN TLBRACE TRBRACE TCOMMA TDOT
%token <token> TPLUS TMINUS TMUL TDIV 
%token <token> TRETURN TEXTERN VAR COLON FUNCTION SEMICOLON PROGRAM IF ELSE THEN TBEGIN TEND TDO TFOR TTO 

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type <ident> ident
%type <expr> numeric expr  expr_block
%type <varvec> func_decl_args
%type <exprvec> call_args
%type <block> program stmts block main_stmt
%type <stmt> stmt extern_decl
%type <token> comparison
%type <identlist> idlist
%type <varlist_decl> var_decl
%type <func_decl> func_decl
/* Operator precedence for mathematical operators */
%left TPLUS TMINUS
%left TMUL TDIV

%start program

%%

program :  head stmts main_stmt 
		{ 
			programBlock = $2; 
			for(int i=0;i<$3->statements.size();i++){
				programBlock->statements.push_back($3->statements[i]);
			}
		}
		;
head : PROGRAM ident TLPAREN func_decl_args TRPAREN SEMICOLON
      ; 
main_stmt : block TDOT { $$ = $1;}

stmts : stmt { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
	  | stmts stmt { $1->statements.push_back($<stmt>2); }
	  ;

expr_block : expr { $$ = $1 ;}
	| block {$$ = $1;}
	;

stmt : var_decl | func_decl | extern_decl
	 | expr { $$ = new NExpressionStatement(*$1); }
	 | TRETURN expr { $$ = new NReturnStatement(*$2); }
	 | IF expr THEN expr_block ELSE expr_block { $$ = new NIFStatement(*$2,*$4,*$6,1); }
	 | IF expr THEN expr_block { $$ = new NIFStatement(*$2,*$4,*$4,0); }
	 | TFOR ident COLON TEQUAL expr TTO expr TDO expr_block { $$ = new FORStatement(*$2,*$5,*$7,*$9);}
     ;

block : TBEGIN stmts TEND { $$ = $2; }
	  | TBEGIN TEND { $$ = new NBlock(); }
	  ;

var_decl : VAR idlist COLON  ident {
	 		$$ = new NVariableDeclarationS();
			for(int i=0;i<$2->size();i++){
				($$->VariableDeclarationList).push_back(new NVariableDeclaration( *$4, *(*$2)[i]   ));
			}
	  	}
		;

idlist : idlist TCOMMA ident {$1->push_back($3);}
	| ident {$$ = new IdentifierList(); $$->push_back($1); }

extern_decl : TEXTERN ident ident TLPAREN func_decl_args TRPAREN
                { $$ = new NExternDeclaration(*$2, *$3, *$5); delete $5; }
            ;

func_decl : FUNCTION ident TLPAREN func_decl_args TRPAREN COLON ident  block SEMICOLON
			{ 
				$$ = new NFunctionDeclaration(*$7, *$2, *$4, *$8); delete $4;

			 }
		  ;
	
func_decl_args : /*blank*/  { $$ = new VariableList(); }
		  | var_decl { 
			  $$ = new VariableList(); 
			  for(int i=0;i<$1->VariableDeclarationList.size();i++){
				  $$->push_back($1->VariableDeclarationList[i]);
			  }
			}
		  | func_decl_args TCOMMA var_decl { 
			  for(int i=0;i<$3->VariableDeclarationList.size();i++){
				  $$->push_back($3->VariableDeclarationList[i]);
			  }
		   }
		  ;

ident : TIDENTIFIER { $$ = new NIdentifier(*$1); delete $1; }
	  ;

numeric : TINTEGER { $$ = new NInteger(atol($1->c_str())); delete $1; }
		| TDOUBLE { $$ = new NDouble(atof($1->c_str())); delete $1; }
		;
	
expr : ident COLON TEQUAL expr { $$ = new NAssignment(*$<ident>1, *$4); }
	 | ident TLPAREN call_args TRPAREN { $$ = new NMethodCall(*$1, *$3); delete $3; }
	 | ident { $<ident>$ = $1; }
	 | numeric
         | expr TMUL expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
         | expr TDIV expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
         | expr TPLUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
         | expr TMINUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
 	 | expr comparison expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     | TLPAREN expr TRPAREN { $$ = $2; }
	;
	
call_args : /*blank*/  { $$ = new ExpressionList(); }
		  | expr { $$ = new ExpressionList(); $$->push_back($1); }
		  | call_args TCOMMA expr  { $1->push_back($3); }
		  ;

comparison : TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE;

%%
