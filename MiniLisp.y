%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void yyerror(const char *message);
int result;
int i;
int equal_first,equal_number,temp,tempoint;
int var_count=0,param_count=0,fun_count=0;
struct Node{
	char data_type;
	char *id_name;
	int id_num;
	int infun_value;
	struct Node *left, *right,*mid;
};

struct VarParam_store{
	char* var_name;
	int var_value;
	int infun_value;
};
struct VarParam_store var_list[50],param_list[50]; 
struct Node *fun_list[50];
struct Node *root=NULL;

struct Node *newNode(struct Node *Left_pointer, struct Node *Right_pointer, char data);
void traversal(struct Node *node);
void plus_op(struct Node *node);
void minus_op(struct Node *node);
void multiply_op(struct Node *node);
void divide_op(struct Node *node);
void mod_op(struct Node *node);
void greater_op(struct Node *node);
void smaller_op(struct Node *node);
void equal_op(struct Node *node);
void and_op(struct Node *node);
void or_op(struct Node *node);
void store_params(struct Node *node);
void bind_params(struct Node *node);
%}
%union {
int ival;
char* word;
struct Node *nodeval;
}
%token PRINTNUM PRINTBOOL
%token AND OR NOT IF DEFINE
%token  MOD FUN
%token <word>  ID 
%token <ival> NUMBER
%token <ival> BOOLVAL

%type <nodeval>  exp  expp expm expa expo expe numop logicalop 
%type <nodeval> plus minus multiply divide mod greater smaller equal
%type <nodeval> andop orop notop 
%type <nodeval> ifexp testexp thenexp elseexp
%type <nodeval> stmt printstmt stmts program
%type <nodeval> defstmt variable  
%type <nodeval> funexp funids funbody funcall  param funidss funname
%left '+' '-'
%left '*' '/'
%left '(' ')'
%%
program
	: stmts 		{root = $1;}
	;
stmts
	:stmt stmts	{$$= newNode($1,$2,'S');}
	|stmt		{$$= $1;}
	;
stmt
	: exp		{$$= $1;}
	| printstmt	{$$= $1;}
	| defstmt	{$$= $1;}
	;
printstmt
	: '(' PRINTNUM exp ')' {$$= newNode($3,NULL,'P');}
	|'(' PRINTBOOL exp ')' {$$= newNode($3,NULL,'p');}
	;
defstmt
	: '(' DEFINE variable exp ')'	{$$= newNode($3,$4,'D');}
	;
exp
	: numop{$$ = $1;}
	|logicalop{$$ = $1;}
	|ifexp{$$ = $1;}
	|variable{$$ = $1;}
	|funcall{$$ = $1;}
	|funexp{$$ = $1;}
	|BOOLVAL {$$ = newNode(NULL,NULL,'B');$$->id_num=$1;}
	|NUMBER {$$ = newNode(NULL,NULL,'N');$$->id_num=$1;}
	;
numop
	:plus{$$ = $1;}
	|minus{$$ = $1;}
	|multiply{$$ = $1;}
	|divide{$$ = $1;}
	|mod{$$ = $1;}
	|greater{$$ = $1;}
	|smaller{$$ = $1;}
	|equal{$$ = $1;}
	;
variable
	:ID		{$$ = newNode(NULL, NULL, 'V');  $$->id_name = $1;}
	;
funexp
	: '(' FUN funids funbody ')'{$$=newNode($3,$4,'F');}
	;
funids
	: '(' funidss ')' {$$=$2;}
	;
funidss
	:funidss variable{$$=newNode($1,$2,'M');}
	|{$$=newNode(NULL,NULL,'X');}
	;
funbody
	: exp {$$ = $1;}
	;
funcall
	: '(' funexp param ')' { $$ = newNode($2, $3, 'C');}
	|'(' funname param ')' {$$ = newNode($2, $3, 'n');}
	;
param
	:exp param 	{$$ = newNode($1, $2, 'M');}
	|{$$=newNode(NULL,NULL,'X');}
	;
funname
	: ID{$$ = newNode(NULL,NULL,'f');$$->id_name = $1;}
	;
logicalop
	:andop{$$ = $1;}
	|orop{$$ = $1;}
	|notop{$$ = $1;}
	;
ifexp
	: '(' IF testexp thenexp elseexp ')' {$$ = newNode($3, $5, 'I');  $$->mid= $4;}
	;
testexp
	:exp {$$ = $1;}
	;
thenexp
	:exp{$$ = $1;}
	;
elseexp
	:exp{$$ = $1;}
	;
plus	
	: '(' '+' exp expp ')'		{ $$ = newNode($3, $4, '+');}
	;
minus	
	: '(' '-' exp exp ')'		{ $$ = newNode($3, $4, '-');}
	;
multiply
	: '(' '*' exp expm ')'		{ $$ = newNode($3, $4, '*');}
	;
divide
	: '(' '/' exp exp ')'		{ $$ = newNode($3, $4, '/');}
	;
mod
	: '(' MOD exp exp ')'	{ $$ = newNode($3, $4, '%');}
	;
greater
	: '(' '>' exp exp ')'		{ $$ = newNode($3, $4, '>');}
	;
smaller
	: '(' '<' exp exp ')'		{ $$ = newNode($3, $4, '<');}
	;
equal
	: '(' '=' exp expe ')'        { $$ = newNode($3, $4, '=');}
	;
expe
	: exp expe {$$= newNode($1,$2,'M');}
	|exp	{$$=$1;}
	;
expp
	: exp expp {$$= newNode($1,$2,'M');}
	|exp	{$$=$1;}
	;
expm
	: exp expm {$$= newNode($1,$2,'M');}
	|exp	{$$=$1;}
	;
andop
	: '(' AND exp expa ')' { $$ = newNode($3, $4, '&');}
	;
orop
	: '(' OR exp expo ')'   { $$ = newNode($3, $4, '|');}
	;
notop
	: '(' NOT exp  ')' 	     { $$ = newNode($3, NULL, '!');}
	;
expa
	: exp expa {$$= newNode($1,$2,'M');}
	|exp	{$$=$1;}
	;
expo
	: exp expo {$$= newNode($1,$2,'M');}
	|exp	{$$=$1;}
	;
%%
struct Node *newNode(struct Node *Left_pointer, struct Node *Right_pointer, char data) {
	 struct Node *node = (struct Node *) malloc( sizeof(struct Node) );

	node->id_num=0;
	node->data_type=data;
	node->left=Left_pointer;
	node->mid=NULL;
	node->right=Right_pointer;
	node->id_name="";
	node->infun_value =0;
	return node;
}
void traversal(struct Node *node){
	if(node == NULL){
		return;
	}
	if(node->data_type == '+'){
		traversal(node->left);
        	traversal(node->right);
        	result=0;
        	plus_op(node);
       		node->id_num=result;
	}
	else if(node->data_type == '-'){
		traversal(node->left);
        	traversal(node->right);	
		minus_op(node);
		node->id_num=result;
	}
	else if(node->data_type == '*'){
		traversal(node->left);
        	traversal(node->right);
		result=1;
        	multiply_op(node);
       		node->id_num=result;		
	}
	else if(node->data_type == '/'){
		traversal(node->left);
        	traversal(node->right);	
		divide_op(node);
		node->id_num=result;	
	}
	else if(node->data_type == '%'){
		traversal(node->left);
        	traversal(node->right);	
		mod_op(node);
		node->id_num=result;		
	}
	else if(node->data_type == '>'){
		traversal(node->left);
        	traversal(node->right);	
		greater_op(node);
		node->id_num=result;	
	}
	else if(node->data_type == '<'){
		traversal(node->left);
        	traversal(node->right);	
		smaller_op(node);
		node->id_num=result;	
	}
	else if(node->data_type == '='){
		traversal(node->left);
        	traversal(node->right);	
		equal_first = 0;
		result=1;
		equal_op(node);
		node->id_num=result;	
	}
	else if(node->data_type == '&'){
		traversal(node->left);
        	traversal(node->right);	
		result=1;
		and_op(node);
		node->id_num=result;	
	}
	else if(node->data_type == '|'){
		traversal(node->left);
        	traversal(node->right);	
		result=0;
		or_op(node);
		node->id_num=result;
	}
	else if(node->data_type == '!'){
		traversal(node->left);
		node->id_num=!node->left->id_num;
	}
	else if(node->data_type == 'P'){//print num
		traversal(node->left);
		printf("%d\n", node->left->id_num);
	}
	else if(node->data_type == 'p'){//print bool
		traversal(node->left);
		if(node->left->id_num==1){
			printf("#t\n");
		}
		else{
			printf("#f\n");
		}
	}
	else if(node->data_type == 'I'){//if
		traversal(node->left);
        	traversal(node->mid);
        	traversal(node->right);
		if(node->left->id_num==1){
			node->id_num=node->mid->id_num;
		}
		else{
			node->id_num=node->right->id_num;
		}
	}
	else if(node->data_type == 'D'){//define
		if(node->right->data_type == 'F'){
			if(node->right->left->data_type == 'X'){
				var_list[var_count].var_name = node->left->id_name;
				var_list[var_count].var_value = node->right->right->id_num;
				var_list[var_count].infun_value =0;
				var_count++;
			}
			else{
				fun_list[fun_count] = node;
				fun_count++;
			}
		}
		else{
			traversal(node->left);
        		traversal(node->right);
			var_list[var_count].var_name = node->left->id_name;
			var_list[var_count].var_value = node->right->id_num;
			var_count++;
		}
		
		
	}
	else if(node->data_type == 'V'){//variable
		for(i=0;i<var_count;i++){//make sure it is the infun value not variable
			if(var_list[i].infun_value == node->infun_value&&strcmp(var_list[i].var_name,node->id_name)==0){
				node->id_num = var_list[i].var_value;
				break;
			}
		}
	}
	else if(node->data_type == 'F'){
		traversal(node->left);
        	traversal(node->right);
	}
	else if(node->data_type == 'C'){
		param_count=0;
		store_params(node);
		temp = param_count;
		param_count=0;
		bind_params(node);
		traversal(node->left);
        	traversal(node->right);

		var_count = var_count -temp; //make var to know how many remain
		node->id_num = node->left->right->id_num;//the result store in funbody , get the value in right node
	}
	else if(node->data_type == 'n'){//funname
		if(node->right->left->data_type == 'n'){
			node->right->left->data_type = 'N';
			for(i=0;i<var_count;i++){
				if(var_list[i].infun_value == 0 && strcmp(var_list[i].var_name, node->right->left->left->id_name) == 0){
					node->right->left->id_num = var_list[i].var_value;
                    			break;
				}
			}
		}
		tempoint=0;//set the function pointer
		for(i=0;i<fun_count;i++){//which funname it is
			if(node->left->id_name == fun_list[i]->id_name){
				tempoint=i;
				break;
			}
		}
		param_count=0;
		store_params(node->right);
		temp = param_count;
		param_count=0;// start over
		bind_params(fun_list[tempoint]->right);
		
		traversal(fun_list[tempoint]->left);
		traversal(fun_list[tempoint]->right);
		var_count = var_count - temp;
		
		node->id_num = fun_list[tempoint]->right->right->id_num;
	}
	else{ 
        	traversal(node->left);
        	traversal(node->right);
	}
}
void plus_op(struct Node *node){
	if(node->left != NULL){
		result= result + node->left->id_num;
		if(node->left->data_type=='M'){
			plus_op(node->left);
		}	
	}
	if (node->right != NULL) {
        	result = result + node->right->id_num;
        	if(node->right->data_type == 'M'){
			plus_op(node->right);
		}
	}
}
void minus_op(struct Node *node){
	result=node->left->id_num - node->right->id_num ; 
}
void multiply_op(struct Node *node){
	if(node->left != NULL){
		if(node->left->data_type=='M'){
			multiply_op(node->left);
		}
		else{
			result= result * node->left->id_num;
		}	
	}
	if(node->right != NULL){
		if(node->right->data_type=='M'){
			multiply_op(node->right);
		}
		else{
			result= result * node->right->id_num;
		}	
	}
}
void divide_op(struct Node *node){
	if(node->left != NULL && node->right != NULL){
		result = node->left->id_num / node->right->id_num ; 
	}
}
void mod_op(struct Node *node){
	if(node->left != NULL && node->right != NULL){
		result = node->left->id_num % node->right->id_num ; 
	}
}
void greater_op(struct Node *node){
	if(node->left != NULL && node->right != NULL){
		if(node->left->id_num > node->right->id_num){
			result=1;
		}
		else{
			result=0;
		}
	}
}
void smaller_op(struct Node *node){
	if(node->left != NULL && node->right != NULL){
		if(node->left->id_num < node->right->id_num){
			result=1;
		}
		else{
			result=0;
		}
	}
}
void equal_op(struct Node *node){
	if(node->left != NULL){
		if(node->left->data_type == 'M'){
			equal_op(node->left);
		}
		else{
			if(equal_first==0){
				equal_number=node->left->id_num;
                		equal_first=1;
			}
			else{
				if(node->left->id_num != equal_number){
					result=0;
				}	
			}
		}
	}
	if(node->right != NULL){
		if(node->right->data_type == 'M'){
			equal_op(node->right);
		}
		else{
			if(equal_first==0){
				equal_number=node->right->id_num;
                		equal_first=1;
			}
			else{
				if(node->right->id_num != equal_number){
					result=0;
				}	
			}
		}
	}
}
void and_op(struct Node *node){
	if(node->left != NULL){
        	if(node->left->data_type != 'M'){
            		result = result & node->left->id_num;
		}
        	else{
            		and_op(node->left);
		}
	}
	if(node->right != NULL){
        	if(node->right->data_type != 'M'){
            		result = result & node->right->id_num;
		}
        	else{
            		and_op(node->right);
		}
	}
}
void or_op(struct Node *node){
	if(node->left != NULL){
        	if(node->left->data_type != 'M'){
            		result = result | node->left->id_num;
		}
        	else{
            		or_op(node->left);
		}
	}
	if(node->right != NULL){
        	if(node->right->data_type != 'M'){
            		result = result | node->right->id_num;
		}
        	else{
            		or_op(node->right);
		}
	}
}
void store_params(struct Node *node){
	if(node->left != NULL&& node->left->data_type != 'F'){//check if F because can't find the last node
		if(node->left->data_type == 'N'){
			param_list[param_count].var_value = node->left->id_num;
			param_count++;
		}
		store_params(node->left);
	}
	if(node->right != NULL&& node->right->data_type != 'F'){
		if(node->right->data_type == 'N'){
			param_list[param_count].var_value = node->right->id_num;
			param_count++;
		}
		store_params(node->right);
	}
}
void bind_params(struct Node *node){//bind param value to variable in function
	if(node->left != NULL){
		if(node->left->data_type == 'V'){
			var_list[var_count].var_name = node->left->id_name;
			var_list[var_count].var_value = param_list[param_count].var_value;
			param_count++;
			var_list[var_count].infun_value = 1;//infun has the value
			var_count++;
			node->left->infun_value = 1;//infun has the value
		}
		bind_params(node->left);
	}
	if(node->right!= NULL){
		if(node->right->data_type == 'V'){
			var_list[var_count].var_name = node->right->id_name;
			var_list[var_count].var_value = param_list[param_count].var_value;
			param_count++;
			var_list[var_count].infun_value = 1;//infun has the value
			var_count++;
			node->right->infun_value = 1;//infun has the value
		}
		bind_params(node->right);
	}
}
void yyerror (const char *message)
{
        fprintf (stderr, "%s\n",message);
}
int main(int argc, char *argv[]) {
	yyparse();
	traversal(root);
        return(0);
}
