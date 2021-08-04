/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    /* Symbol table function - you can add new function if needed. */
    static int scope_level = 0;
    static int addr = 0;
    static int line = 1;
    static int inloop = 0;

    static int Lcmptag = 0;
    static int fortag = 0;
    static int iftag = 0;
    static int RHS = 0;
    static int POST = 0;
    static int pmark = 0;
    static int rubbish = 0;
    static int rub = 0;
    bool HAS_ERROR = false;
    FILE *fp;
    FStack* fs; //stack for FOR
    FStack* is; //stack for IF
    FStack* iis;//another stack for IF
    PStack* ps; //it's a queue actually xD

    TABLE table[MAX_SIZE];
    static void init();
    static void create_symbol(char*,char*);
    static int  insert_symbol();
    static char *lookup_symbol(char*); //could only lookup type
    static void dump_symbol();
    static void IDmsg(char* id);
    static void type_conf(char*, char*, char*);
    static void cond_conf(char*);

    static void write_dcl(char*, char*); //no support array
    static void write_load(char*);
    static void write_post(char*, char*);//x++|x--
    static void write_print(char*, char*);
    static void write_unary(char*, char*);
    static int  better_lookup_symbol(char*); //better version
    static void write_assign(char*, char*);
    static void write_newarr(char*, char*);
    static void write_loadarr(char*);
    static void write_assignarr(char*);
    static void write_arrind(char*);
    static void write_converse(char*, char*);
    static void fstack_push(int);
    static int fstack_pop();
    static void istack_push(int);
    static int  istack_pop();
    static void iistack_push(int);
    static int  iistack_pop();
    static void pque_push(char*, int);
    static void pque_pop();
    const char e[] = "error happened here\n";


    int preced(char* s){
	if(!strcmp(s,"LOR\n"))
	    return 7;
	if(!strcmp(s,"LAND\n"))
	    return 6;
	if(!strcmp(s,"EQL\n") || !strcmp(s,"NEQ\n") || !strcmp(s,"LSS\n") || !strcmp(s,"LEQ\n") || !strcmp(s,"GTR\n") || !strcmp(s,"GEQ\n")){
	    return 5;
}
	if(!strcmp(s,"ADD\n") || !strcmp(s,"SUB\n")){
	    return 4;
}
	if(!strcmp(s,"MUL\n") || !strcmp(s,"QUO\n") || !strcmp(s,"REM\n"))
	    return 3;
	return -1;
    }
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    int b_val;
    char *type;
    char *op;
    char *id;
    /* ... */
}

/* Token without return */
%token VAR
%token NEWLINE
%token PRINT PRINTLN
%left IF ELSE FOR
%left LOR
%left LAND
%left GEQ LEQ EQL NEQ '>' '<' '!'
%right '='
%left '+' '-'
%left '*' '/' '%' INC DEC
%left '(' ')'
%left '{' '}'
%left '[' ']'
%token ';'
%token ','
%token '"'
%right ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <b_val> BOOL_LIT
%token <id> ID
%token <type> INT FLOAT BOOL STRING

/* Nonterminal with return, which need to sepcify type */
%type <type> Type TypeName ArrayType
%type <op> unary_op assign_op
%type <type> Expression UnaryExpr PrimaryExpr Operand Literal IndexExpr ConversionExpr Forbody LExpression
%type <type> PrintStmt

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList
;

StatementList
    : StatementList Statement
    | Statement
;

Statement
    : DeclarationStmt NEWLINE {line++;}
    | SimpleStmt NEWLINE      {line++;RHS=0;}
    | BLOCK NEWLINE           {line++;}
    | {inloop=1;}IfStmt NEWLINE  {inloop=0;line++;}
    | {inloop=1;}ForStmt NEWLINE {inloop=0;line++;}
    | {RHS=1;}PrintStmt NEWLINE       {line++;RHS=0;}
    | NEWLINE                 {line++;}
;

DeclarationStmt
    : VAR ID Type '=' Expression {if(insert_symbol($<id>2)==1)create_symbol($<type>3, $<id>2);$5=$<type>3;write_dcl($2,$5);}
    | VAR ID Type {if(insert_symbol($<id>2)==1)create_symbol($<type>3, $<id>2);write_dcl($2,NULL);write_newarr($2,$3);}
;

SimpleStmt
    : AssignmentStmt
    | ExpressionStmt
    | IncDecStmt
;



AssignmentStmt
    : LExpression assign_op Expression {type_conf($2, lookup_symbol($1), lookup_symbol($3));
				 	if(/*better_lookup_symbol($1)==-1 &&*/ better_lookup_symbol($1)==IsLIT){
					     char*dd = $1;
					     while(*dd!='\0'){
						if(*dd==' '){
						    *dd = '\0';
						    break;
						}
						dd+=1;
					     }
					     printf("error:%d: cannot assign to %s\n", yylineno, $1);
					     HAS_ERROR = true;
					}else{
					write_assign($1,$2);
					write_assignarr($1);
					RHS=0;}
					}
;

LExpression
    : Expression{$$=$1;int i = better_lookup_symbol($1);
		if(i!=IsLIT && i>=0  && strcmp(table[i].Type, "array")){
		    if(!strcmp(table[i].Type, "int32"))
			rub=1;
		    else if(!strcmp(table[i].Type, "float32"))
			rub=2;
		    else if(!strcmp(table[i].Type, "string"))
			rub=3;
		}
	    }
;

ExpressionStmt
    : Expression
;

IfStmt
    : IFIF IFCond IFBLOCK ELSEE IfStmt {int y = iistack_pop();fprintf(fp,"L_if%d_exit:\n", y);}
    | IFIF IFCond IFBLOCK ELSEE BLOCK {int y = iistack_pop();fprintf(fp,"L_if%d_exit:\n", y);}
    | IFIF IFCond IFBLOCK {int y = iistack_pop();fprintf(fp, "L_if%d_exit:\n",y);}
;

IFIF
    : IF{int x = iftag++;iistack_push(x);}
;

IFCond
    : Condition{int x=iftag++;istack_push(x);fprintf(fp,"ifeq L_if%d_exit\n",x);}
;

IFBLOCK
    : BLOCK{int y=istack_pop();int z = iistack_pop();iistack_push(z);fprintf(fp,"goto L_if%d_exit\n",z);fprintf(fp,"L_if%d_exit:\n",y);}
;

ELSEE
    : ELSE
;

ForStmt
    : FOR{int x=fortag++;fstack_push(x);fprintf(fp, "L_for%d_begin:\n", x);} Forbody
;

Forbody
    : Condition {int x=fstack_pop();fprintf(fp,"ifeq L_for%d_exit\n",x);fstack_push(x);} BLOCK{int y =fstack_pop();fprintf(fp,"goto L_for%d_begin\nL_for%d_exit:\n",y,y);}
    | ForClause BLOCK{int z=fstack_pop();pque_pop();pmark--;fprintf(fp,"goto L_for%d_begin\nL_for%d_exit:\n",z,z);fstack_pop();/*pop duplicated fstack*/}
;

BLOCK
   : '{'{scope_level++;} StatementList '}'{dump_symbol(scope_level);scope_level--;}
;

ForClause
    : InitStmt{int x=fortag++;fstack_push(x);fprintf(fp,"L_for%d_begin:\n",x);} ';' Condition{int y = fstack_pop();fprintf(fp,"ifeq L_for%d_exit\n",y);fstack_push(y);} ';'{POST=1;pmark++;} PostStmt{POST=0;}
;

InitStmt
    : SimpleStmt
;

PostStmt
    : SimpleStmt
;

PrintStmt
    : PRINT  '(' Expression ')'   {write_print($3, "print");}
    | PRINTLN'(' Expression ')'   {write_print($3, "println");}
;

Condition
    : Expression {cond_conf(lookup_symbol($1));}
;

assign_op
    : '='        {$$="ASSIGN";RHS=1;
		if(rub==1){int x = rubbish++;fprintf(fp,"ifeq rubbish%d\nrubbish%d:\n", x, x);}
		if(rub==2){int x = rubbish++;fprintf(fp, "fconst_0\nfcmpg\nifeq rubbish%d\nrubbish%d:\n", x,x);}
		if(rub==3){}
		rub=0;}
    | ADD_ASSIGN {$$="ADD_ASSIGN";RHS=1;rub=0;}
    | SUB_ASSIGN {$$="SUB_ASSIGN";RHS=1;rub=0;}
    | MUL_ASSIGN {$$="MUL_ASSIGN";RHS=1;rub=0;}
    | QUO_ASSIGN {$$="QUO_ASSIGN";RHS=1;rub=0;}
    | REM_ASSIGN {$$="REM_ASSIGN";RHS=1;rub=0;}
;

IncDecStmt
    : Expression INC {write_post($1, "INC");}
    | Expression DEC {write_post($1, "DEC");}
;

Type
    : TypeName {$$ = $1;}
    | ArrayType {$$ = $1;}
;

TypeName
    : INT    {$$ = $1;} 
    | FLOAT  {$$ = $1;}
    | BOOL   {$$ = $1;}
    | STRING {$$ = $1;}
;

ArrayType
    : '[' Expression ']' Type {char* a = strdup($<type>4);strcat(a,"array");$$=a;}
;

Expression
    : UnaryExpr {$$=$1;}
    | Expression '+' Expression {type_conf("ADD\n",lookup_symbol($1),lookup_symbol($3));if(preced("ADD\n")==5)$$="bool";else $$=$1;}
    | Expression '-' Expression {type_conf("SUB\n",lookup_symbol($1),lookup_symbol($3));if(preced("SUB\n")==5)$$="bool";else $$=$1;}
    | Expression '*' Expression {type_conf("MUL\n",lookup_symbol($1),lookup_symbol($3));if(preced("MUL\n")==5)$$="bool";else $$=$1;}
    | Expression '/' Expression {type_conf("QUO\n",lookup_symbol($1),lookup_symbol($3));if(preced("QUO\n")==5)$$="bool";else $$=$1;}
    | Expression '%' Expression {type_conf("REM\n",lookup_symbol($1),lookup_symbol($3));if(preced("REM\n")==5)$$="bool";else $$=$1;}
    | Expression LAND Expression {type_conf("LAND\n",lookup_symbol($1),lookup_symbol($3));if(preced("LAND\n")==5)$$="bool";else $$=$1;}
    | Expression LOR Expression {type_conf("LOR\n",lookup_symbol($1),lookup_symbol($3));if(preced("LOR\n")==5)$$="bool";else $$=$1;}
    | Expression EQL Expression {type_conf("EQL\n",lookup_symbol($1),lookup_symbol($3));if(preced("EQL\n")==5)$$="bool";else $$=$1;}
    | Expression NEQ Expression {type_conf("NEQ\n",lookup_symbol($1),lookup_symbol($3));if(preced("NEQ\n")==5)$$="bool";else $$=$1;}
    | Expression '>' Expression {type_conf("GTR\n",lookup_symbol($1),lookup_symbol($3));if(preced("GTR\n")==5)$$="bool";else $$=$1;}
    | Expression '<' Expression {type_conf("LSS\n",lookup_symbol($1),lookup_symbol($3));if(preced("LSS\n")==5)$$="bool";else $$=$1;}
    | Expression GEQ Expression {type_conf("GEQ\n",lookup_symbol($1),lookup_symbol($3));if(preced("GEQ\n")==5)$$="bool";else $$=$1;}
    | Expression LEQ Expression {type_conf("LEQ\n",lookup_symbol($1),lookup_symbol($3));if(preced("LEQ\n")==5)$$="bool";else $$=$1;}
;

UnaryExpr
    : PrimaryExpr {$$=$1;}
    | unary_op UnaryExpr {$$=$2;write_unary($1,$2);}
;

PrimaryExpr
    : Operand {$$=$1;}
    | IndexExpr {$$=$1;}
    | ConversionExpr {$$=$1;}
;

ConversionExpr
    : Type '(' Expression ')'{$$=$<type>1;write_converse($1,$3);}
;

IndexExpr
    : PrimaryExpr{write_loadarr($1);} '[' Expression ']' {$$=$1;write_arrind($1);}
;

Operand
    : Literal {$$=$1;}
    | ID {$$=$<id>1;
	if(!strcmp(lookup_symbol($1),"0ERR")){
	    HAS_ERROR = true;
	    printf("error:%d: undefined: %s\n", yylineno+1, $1);
	}
	else IDmsg($1);
	write_load($1);
    }
    | '(' Expression ')'{$$=$2;}
;

Literal
    : INT_LIT    {char type_val[100]={0};sprintf(type_val,"int32 %d",$1);$$=strdup(type_val);fprintf(fp, "ldc %d\n", $1);}
    | FLOAT_LIT  {char type_val[100]={0};sprintf(type_val, "float32 %f", $1);$$=strdup(type_val);fprintf(fp, "ldc %f\n", $1);}
    | BOOL_LIT   {if($<b_val>1==1){$$="bool true";fprintf(fp,"iconst_1\n");}else{$$="bool false";fprintf(fp,"iconst_0\n");}}
    | '"' STRING_LIT '"' {char type_val[100]={0};sprintf(type_val, "string %s", $2);$$=strdup(type_val);fprintf(fp, "ldc \"%s\"\n", $2);}
;

unary_op
    : '+' {$$ = "POS\n";}
    | '-' {$$ = "NEG\n";}
    | '!' {$$ = "NOT\n";fprintf(fp,"iconst_0\n");}
;

%%

/* C code section */
int main(int argc, char *argv[])
{
    fp = fopen("hw3.j", "w");
    char str[] = ".source hw3.j\n.class public Main\n.super java/lang/Object\n.method public static main([Ljava/lang/String;)V\n.limit stack 100\n.limit locals 100\n";
    fwrite(str, 1, sizeof(str)-1, fp);

    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }
    init();

    yylineno = 0;
    yyparse();


    dump_symbol(0);
	//printf("Total lines: %d\n", yylineno);
    fclose(yyin);

    if(HAS_ERROR){
	remove("hw3.j");
    }
    else{
	char str[] = "return\n.end method";
        fwrite(str, 1, strlen(str), fp);
        fclose(fp);
    }

    return 0;
}

static void create_symbol(char* type, char* id) {
    int isarr = 0;
    for(int i=0; type[i]!='\0'; i++){
        if(!strcmp(&type[i], "array")){
	    type[i] = '\0';
	    isarr = 1;
	}
    }
    int j = 0;
    int k = -1;
    for(int i=0; i<MAX_SIZE; i++){
	if(k==-1 && table[i].Index == -1)
	    k = i;
	if(table[i].scope == scope_level){
	    j++;
	    for(int n=i; n<MAX_SIZE; n++)
		if(table[n].Index == -1){
		    k = n;
		    break;
		}
	}
    }
    table[k].Name = id;
    if(isarr == 1)
	table[k].Type = "array";
    else
	table[k].Type = type;
    table[k].Address = addr++;
    table[k].Lineno = yylineno;
    table[k].Index = j;
    table[k].scope = scope_level;
    if(isarr == 1)
	table[k].Element_type = type;
     else
	table[k].Element_type = "-";
}

static int insert_symbol(char* id) {
    for(int i=0; id[i]!='\0'; i++){
        if(id[i]==' '){
             id[i] = '\0';
             break;
         }
    }
    for(int i=0; i<MAX_SIZE; i++){
	if(table[i].Index != -1){
	    if(!strcmp(table[i].Name, id) && table[i].scope == scope_level){
		HAS_ERROR = true;
	        printf("error:%d: %s redeclared in this block. previous declaration at line %d\n", yylineno, id, table[i].Lineno);
	        return 0;
             }
         }
     }
     return 1;
}

static char *lookup_symbol(char* s) {
    if(!strncmp(s, "int32", 5))
	return "int32";
    if(!strncmp(s, "float32", 7))
 	return "float32";
    if(!strncmp(s, "string", 6))
	return "string";
    if(!strncmp(s, "bool", 4)){
	return "bool";
    }
    for(int i=0; i<MAX_SIZE; i++){
	if(table[i].Index!=-1){
	    if(!strcmp(s, table[i].Name) && table[i].scope==scope_level){
		if(!strcmp(table[i].Type, "array"))
		    return table[i].Element_type;
		else
		    return table[i].Type;
	    }
	}
    }
    for(int i=0; i<MAX_SIZE; i++){
	if(table[i].Index!=-1){
	    if(!strcmp(s, table[i].Name) && table[i].scope==0){
		if(!strcmp(table[i].Type, "array"))
		    return table[i].Element_type;
		else
		    return table[i].Type;
	    }
	}
    }
    if(inloop==1){
	for(int i=0; i<MAX_SIZE; i++){
	    if(table[i].Index!=-1){
		if(!strcmp(s, table[i].Name) && table[i].scope==scope_level-1){
		    if(!strcmp(table[i].Type, "array"))
			return table[i].Element_type;
		    else
			return table[i].Type;
		}
	    }
	}
    }
    return "0ERR";
}

static void dump_symbol(int scope_lv) {
    for(int i=0; i<MAX_SIZE; i++){
        if(table[i].Index != -1 && table[i].scope == scope_lv){
	table[i].Name = NULL;
	table[i].Type = NULL;
	table[i].Address = -1;
	table[i].Lineno = -1;
	table[i].Element_type = NULL;
	table[i].Index = -1;
	table[i].scope = -1;
        }
    }
}

static void IDmsg(char* id){
   for(int i=0; i<MAX_SIZE; i++)
       if(table[i].Index != -1){
           if(!strcmp(id, table[i].Name) && (table[i].scope == scope_level)){
	       return;
           }
       }
    if(inloop == 1){
	for(int i=0; i<MAX_SIZE; i++)
	    if(table[i].Index != -1){
		if(!strcmp(id, table[i].Name) && table[i].scope == 0){ //original 0 => scoope_level-1
		    return;
		}
	    }
    }
}

static void type_conf(char* op,char *t1, char *t2){
    if(!strcmp(t1, "0ERR") || !strcmp(t2, "0ERR"))
	return;
    char*c = strdup(op);
    for(int i=0;;i++){
	if(c[i] == '\n')
	    c[i] = '\0';
	if(c[i] == '\0')
	    break;
    }
    if(!strcmp(c,"REM")){
	if(!strcmp(t1, "float32")){
	    HAS_ERROR = true;
            printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "REM", t1);
	}
	else if(!strcmp(t2, "float32")){
	    HAS_ERROR = true;
	    printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "REM", t2);
	}
    }
    else if(!strcmp(c, "LAND") || !strcmp(c,"LOR")){ //in09 L12 t1 t2 bool
	if(strcmp(t1, "bool")){
	    HAS_ERROR = true;
	    printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, c, t1);
	}
	else if(strcmp(t2, "bool")){
	    HAS_ERROR = true;
	    printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, c, t2);
	}
    }
    else if(strcmp(t1, t2)){
	HAS_ERROR = true;
        printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n",yylineno, c, t1, t2);
    }
   
   //case that no error happened
   if(!strcmp(c, "ADD")){
	if(!strcmp(t1, "int32"))
	    fwrite("iadd\n", 5, sizeof(char), fp);
	else if(!strcmp(t1, "float32"))
	    fwrite("fadd\n", 5, sizeof(char), fp);
   }
   else if(!strcmp(c, "SUB")){
	if(!strcmp(t1, "int32"))
	    fwrite("isub\n", 5, sizeof(char), fp);
	else if(!strcmp(t1, "float32"))
	    fwrite("fsub\n", 5, sizeof(char), fp);
   }
   else if(!strcmp(c, "MUL")){
	if(!strcmp(t1, "int32"))
	    fwrite("imul\n", 5, sizeof(char), fp);
	else if(!strcmp(t1, "float32"))
	    fwrite("fmul\n", 5, sizeof(char), fp);
   }
   else if(!strcmp(c, "QUO")){
	if(!strcmp(t1, "int32"))
	    fwrite("idiv\n", 5, sizeof(char), fp);
	else if(!strcmp(t1, "float32"))
	    fwrite("fdiv\n", 5, sizeof(char), fp);
   }
   else if(!strcmp(c, "REM")){ 
	fwrite("irem\n", 5, sizeof(char), fp);
   }
   else if(!strcmp(c, "LAND")){
	fwrite("iand\n", 5, sizeof(char), fp);
   }
   else if(!strcmp(c, "LOR")){
	fwrite("ior\n", 4, sizeof(char), fp);
   }
   else if(!strcmp(c, "EQL")){
	int tag1 = Lcmptag++;
	int tag2 = Lcmptag++;
	char a[150]={0};
	if(!strcmp(t1, "int32")){
	    sprintf(a, "isub\nifeq L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n", tag1, tag2, tag1, tag2);
	}
	else if(!strcmp(t1, "float32")){
	    sprintf(a, "fcmpg\nifeq L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n", tag1, tag2, tag1, tag2);
	}
	fwrite(a, 1, strlen(a), fp);
   }
   else if(!strcmp(c, "GTR")){
	int tag1 = Lcmptag++;
	int tag2 = Lcmptag++;
	char a[150]={0};
	if(!strcmp(t1, "int32")){
	    sprintf(a,"isub\nifgt L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",tag1,tag2,tag1,tag2);
	}
	else if(!strcmp(t1, "float32")){
	    sprintf(a, "fcmpg\nifgt L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n", tag1, tag2, tag1, tag2);//fcmpg gtr:1 eq:0 lss:-1
	} 
	fwrite(a, 1, strlen(a), fp);
   }
   else if(!strcmp(c, "LSS")){
	int tag1 = Lcmptag++;
	int tag2 = Lcmptag++;
	char a[150]={0};
	if(!strcmp(t1, "int32")){
	    sprintf(a,"isub\niflt L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",tag1,tag2,tag1,tag2);
	}
	else if(!strcmp(t1, "float32")){
	    sprintf(a, "fcmpg\niflt L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n", tag1, tag2, tag1, tag2);//fcmpg gtr:1 eq:0 lss:-1
	} 
	fwrite(a, 1, strlen(a), fp);
   }
   else if(!strcmp(c, "LEQ")){
	int tag1 = Lcmptag++;
	int tag2 = Lcmptag++;
	char a[150]={0};
	if(!strcmp(t1, "int32")){
	    sprintf(a,"isub\nifle L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",tag1,tag2,tag1,tag2);
	}
	else if(!strcmp(t1, "float32")){
	    sprintf(a, "fcmpg\nifle L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n", tag1, tag2, tag1, tag2);//fcmpg gtr:1 eq:0 lss:-1
	} 
	fwrite(a, 1, strlen(a), fp);
   }
   else if(!strcmp(c, "NEQ")){
	int tag1 = Lcmptag++;
	int tag2 = Lcmptag++;
	char a[150]={0};
	if(!strcmp(t1, "int32")){
	    sprintf(a,"isub\nifne L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n",tag1,tag2,tag1,tag2);
	}
	else if(!strcmp(t1, "float32")){
	    sprintf(a, "fcmpg\nifne L_cmp_%d\niconst_0\ngoto L_cmp_%d\nL_cmp_%d:\niconst_1\nL_cmp_%d:\n", tag1, tag2, tag1, tag2);//fcmpg gtr:1 eq:0 lss:-1
	} 
	fwrite(a, 1, strlen(a), fp);
   }
   free(c);

}

static void cond_conf(char *t){
    if(!strcmp(t, "bool"))
	return;
    else{
	HAS_ERROR = true;
	printf("error:%d: non-bool (type %s) used as for condition\n", yylineno+1, t);
    }
}

static void write_dcl(char* s, char* r){
    //would be here if and only if type1 == type2 so no worry!!
    int s_ind = better_lookup_symbol(s);
    char str[100] = {0};
    if(!strcmp(table[s_ind].Type, "int32")){ //istore
	if(r==NULL)
	    fwrite("iconst_0\n",1,9,fp);
	sprintf(str, "istore %d\n", table[s_ind].Address);
    }
    else if(!strcmp(table[s_ind].Type,"float32")){//fstore
	if(r==NULL)
	    fwrite("fconst_0\n",1,9,fp);
	sprintf(str, "fstore %d\n", table[s_ind].Address);
    }
    else if(!strcmp(table[s_ind].Type, "bool")){//bool istore
	if(r==NULL)
	    fwrite("iconst_0\n",1,9,fp);
	sprintf(str, "istore %d\n", table[s_ind].Address);
    }
    else if(!strcmp(table[s_ind].Type, "string")){//astore
	if(r==NULL)
	    fwrite("ldc \"\"\n",7,sizeof(char),fp);
	sprintf(str, "astore %d\n", table[s_ind].Address);
    }
    if(POST == 0)
	fwrite(str, 1, strlen(str), fp);
    else
	pque_push(str, pmark);
}

static void write_load(char* s){
    int s_ind = better_lookup_symbol(s);
    char str[100]= {0};
    if(s_ind == -1) return; //undefined
    //if(RHS==0) //citical
    //	return;
    if(!strcmp(table[s_ind].Type, "int32")){//iload
	sprintf(str, "iload %d\n", table[s_ind].Address);
    }
    else if(!strcmp(table[s_ind].Type, "float32")){//fload
	sprintf(str, "fload %d\n", table[s_ind].Address);
    }
    else if(!strcmp(table[s_ind].Type, "bool")){
	sprintf(str, "iload %d\n", table[s_ind].Address);
    }
    else if(!strcmp(table[s_ind].Type, "string")){//aload
	sprintf(str, "aload %d\n", table[s_ind].Address);
    }
    if(POST == 0)
        fwrite(str, 1, strlen(str), fp);
    else
 	pque_push(str, pmark);
}

static int better_lookup_symbol(char* n){
    if(!strncmp(n, "int32", 5) || !strncmp(n, "float32", 7) || !strncmp(n, "bool", 4) || !strncmp(n, "string", 6)){
	return IsLIT;
    }
    for(int i=0; i<MAX_SIZE; i++){
	if(table[i].Index!=-1){
	    if(!strcmp(n, table[i].Name) && table[i].scope==scope_level)
		return i;
	}
    }
    for(int i=0; i<MAX_SIZE; i++){
	if(table[i].Index!=-1){
	    if(!strcmp(n, table[i].Name) && table[i].scope==0)
		return i;
	}
    }
    if(inloop==1){
	for(int i=0; i<MAX_SIZE; i++){
	    if(table[i].Index!=-1){
		if(!strcmp(n, table[i].Name) && table[i].scope==(scope_level-1))
		    return i;
	    }
	}
    }
    return -1;
}

static void write_post(char* s, char* act){
    int s_ind = better_lookup_symbol(s);
    char jstore[10]={0};
    char jadd[10]={0};
    char jcon[10]={0};
    if(!strcmp(act, "INC")){
	if(!strcmp(table[s_ind].Type, "int32")){
	    strcpy(jstore, "istore");
	    strcpy(jadd, "iadd");
	    strcpy(jcon, "iconst_1");
	}
	else if(!strcmp(table[s_ind].Type, "float32")){
	    strcpy(jstore, "fstore");
	    strcpy(jadd, "fadd");
	    strcpy(jcon, "fconst_1");
	}
    }
    else if(!strcmp(act, "DEC")){
	if(!strcmp(table[s_ind].Type, "int32")){
	    strcpy(jstore, "istore");
	    strcpy(jadd, "isub");
	    strcpy(jcon, "iconst_1");
	}
	else if(!strcmp(table[s_ind].Type, "float32")){
	    strcpy(jstore, "fstore");
	    strcpy(jadd, "fsub");
	    strcpy(jcon, "fconst_1");
	}
    }
    char str[100] = {0};
    sprintf(str, "%s\n%s\n%s %d\n", jcon,jadd,jstore,table[s_ind].Address);
    if(POST == 0)
        fwrite(str, 1, strlen(str), fp);
    else
	pque_push(str, pmark);
}

static void write_print(char* t, char* instr){
	char c[50]={0};
	char a[200]={0};
	int t_ind = better_lookup_symbol(t);
	if(t_ind == IsLIT){
	    if(!strncmp(t, "int32", 5)){
	        strcpy(a,"swap");
	        strcpy(c,"I");
	    }
	    else if(!strncmp(t,"float32", 7)){
	        strcpy(a,"swap");
	        strcpy(c,"F");
	    }
	    else if(!strncmp(t, "bool", 4)){
		if(strcmp(t, "bool true")) 
	            strcpy(a,"ldc \"true\"");
		else if(strcmp(t, "bool false"))
		    strcpy(a,"ldc \"false\"");
		strcpy(c,"Ljava/lang/String;");
	    }
	    else if(!strncmp(t, "string" ,6)){
		strcpy(a,"swap");
		strcpy(c, "Ljava/lang/String;");
	    }
	}
	else{
	    if(!strcmp(table[t_ind].Type, "int32")||!strcmp(table[t_ind].Element_type,"int32")){
		strcpy(a,"swap");
		strcpy(c,"I");
	    }
	    else if(!strcmp(table[t_ind].Type, "float32")||!strcmp(table[t_ind].Element_type, "float32")){
		strcpy(a,"swap");
		strcpy(c,"F");
	    }
	    else if(!strcmp(table[t_ind].Type, "bool")){
		int tag1 = Lcmptag++;
		int tag2 = Lcmptag++;
		sprintf(a, "ifeq L_cmp_%d\nldc \"true\"\ngoto L_cmp_%d\nL_cmp_%d:\nldc \"false\"\nL_cmp_%d:\n", tag1, tag2, tag1, tag2);
		fwrite(a,1,strlen(a),fp);
		strcpy(a, "swap");
		strcpy(c,"Ljava/lang/String;");
	    }
	    else if(!strcmp(table[t_ind].Type, "string")){
		strcpy(a, "swap");
		strcpy(c, "Ljava/lang/String;");
	    }
	}
    if(!strcmp(instr, "print")){
	char s[150]={0};
	sprintf(s,"getstatic java/lang/System/out Ljava/io/PrintStream;\n%s\ninvokevirtual java/io/PrintStream/print(%s)V\n",a,c);
	fwrite(s, 1, strlen(s), fp);
    }
    else if(!strcmp(instr, "println")){
	char s[150]={0};
	sprintf(s,"getstatic java/lang/System/out Ljava/io/PrintStream;\n%s\ninvokevirtual java/io/PrintStream/println(%s)V\n",a,c);
	fwrite(s,1,strlen(s), fp);
    }
}

static void write_unary(char* op, char*expr){
   int expr_index = better_lookup_symbol(expr);
   char t[10]={0};
   if(!strncmp(expr, "int32", 5))
	strcpy(t, "int32");
   else if(!strncmp(expr, "float32", 7))
	strcpy(t, "float32");
   else if(!strncmp(expr, "bool", 4))
	strcpy(t,"bool");
   else
	strcpy(t, table[expr_index].Type);
   
	
   if(!strcmp(op, "POS\n")){
       //do nothing
   }
   else if(!strcmp(op, "NEG\n")){
	if(!strcmp(t, "int32"))
	    fwrite("ineg\n", 5, sizeof(char), fp);
	else if(!strcmp(t, "float32"))
	    fwrite("fneg\n", 5, sizeof(char), fp);
   }
   else if(!strcmp(op, "NOT\n")){
	if(!strcmp(t, "bool"))
	    fwrite("ixor\n", 5, sizeof(char), fp);
   } 
}

static void write_newarr(char* s, char* type){
    int s_ind = better_lookup_symbol(s);
    if(strcmp(table[s_ind].Type, "array"))
	return;
    char str[100] = {0};
    char ty[10] = {0};
    if(!strcmp(table[s_ind].Element_type, "int32"))
	strcpy(ty, "int");
    else if(!strcmp(table[s_ind].Element_type, "float32"))
	strcpy(ty, "float");
    sprintf(str, "newarray %s\nastore %d\n",ty, table[s_ind].Address);
    fwrite(str, 1, strlen(str), fp);
}

static void write_loadarr(char* s){
    int  s_ind = better_lookup_symbol(s);
    char str[100] = {0};
    sprintf(str, "aload %d\n", table[s_ind].Address);
    fwrite(str, 1, strlen(str), fp);
}

static void write_arrind(char *s){
    int s_ind = better_lookup_symbol(s);
    if(RHS==0)
    	return;
    if(!strcmp(table[s_ind].Element_type, "int32"))
	fwrite("iaload\n", 7, sizeof(char), fp);
    else if(!strcmp(table[s_ind].Element_type, "float32"))
	fwrite("faload\n", 7, sizeof(char), fp);
}

static void write_assignarr(char* s){
    int s_ind = better_lookup_symbol(s);
    if(s_ind == -1) return;
    if(!strcmp(table[s_ind].Type, "array")){// i|fastore
	if(!strcmp(table[s_ind].Element_type, "int32"))
	    fwrite("iastore\n", 8, sizeof(char), fp);
	if(!strcmp(table[s_ind].Element_type, "float32"))
	    fwrite("fastore\n", 8, sizeof(char), fp);
    }
}

static void write_assign(char* t, char* op){
    char a[100] = {0};
    int t_ind = better_lookup_symbol(t);
    if(t_ind == -1) return;
    if(!strcmp(op, "ASSIGN")){
	if(!strcmp(table[t_ind].Type,"int32"))
	    sprintf(a, "istore %d\n", table[t_ind].Address);
	else if(!strcmp(table[t_ind].Type ,"float32"))
	    sprintf(a, "fstore %d\n", table[t_ind].Address);
	else if(!strcmp(table[t_ind].Type, "string"))
	    sprintf(a, "astore %d\n", table[t_ind].Address);
	else if(!strcmp(table[t_ind].Type, "bool"))
	    sprintf(a, "istore %d\n", table[t_ind].Address);
    }
    if(!strcmp(op, "ADD_ASSIGN")){
	if(!strcmp(table[t_ind].Type,"int32"))
	    sprintf(a, "iadd\nistore %d\n", table[t_ind].Address);
	else if(!strcmp(table[t_ind].Type ,"float32"))
	    sprintf(a, "fadd\nfstore %d\n", table[t_ind].Address);
    }
    if(!strcmp(op, "SUB_ASSIGN")){
	if(!strcmp(table[t_ind].Type,"int32"))
	    sprintf(a, "isub\nistore %d\n", table[t_ind].Address);
	else if(!strcmp(table[t_ind].Type ,"float32"))
	    sprintf(a, "fsub\nfstore %d\n", table[t_ind].Address);
    }
    if(!strcmp(op, "MUL_ASSIGN")){
	if(!strcmp(table[t_ind].Type,"int32"))
	    sprintf(a, "imul\nistore %d\n", table[t_ind].Address);
	else if(!strcmp(table[t_ind].Type ,"float32"))
	    sprintf(a, "fmul\nfstore %d\n", table[t_ind].Address);
    }
    if(!strcmp(op, "QUO_ASSIGN")){
	if(!strcmp(table[t_ind].Type,"int32"))
	    sprintf(a, "idiv\nistore %d\n", table[t_ind].Address);
	else if(!strcmp(table[t_ind].Type ,"float32"))
	    sprintf(a, "fdiv\nfstore %d\n", table[t_ind].Address);
    }
    if(!strcmp(op, "REM_ASSIGN")){
	if(!strcmp(table[t_ind].Type,"int32"))
	    sprintf(a, "irem\nistore %d\n", table[t_ind].Address);
    }
    if(a != NULL){
	if(POST == 0)//Not PostStmt
            fwrite(a, 1, strlen(a), fp);
	else
	    pque_push(a, pmark);
    }
}

static void write_converse(char* t, char* v){
    int v_ind = better_lookup_symbol(v);
    if(!strncmp(t, "int32", 5)){
	if(!strncmp(v, "float32", 7))
	    fwrite("f2i\n", 4, sizeof(char),fp);
	else if(!strncmp(table[v_ind].Type, "float32", 7) || (!strcmp(table[v_ind].Type, "array")&&!strcmp(table[v_ind].Element_type, "float32")))
	    fwrite("f2i\n", 4, sizeof(char),fp);
    }
    else if(!strncmp(t, "float32", 7)){
	if(!strncmp(v, "int32", 5))
	    fwrite("i2f\n", 4, sizeof(char), fp);
	else if(!strncmp(table[v_ind].Type, "int32", 7) || (!strcmp(table[v_ind].Type, "array")&&!strcmp(table[v_ind].Element_type, "int32")))
	    fwrite("i2f\n", 4, sizeof(char), fp);
    }
}

static void fstack_push(int a){
    if(fs == NULL){
	fs = malloc(sizeof(FStack));
	fs->stack = a;
	fs->next = NULL;
    }
    else{
	FStack* k = malloc(sizeof(FStack));
	k->stack = a;
	k->next = fs;
	fs = k;
    }
}

static int  fstack_pop(){
    if(fs == NULL)
	return -100;
    else{
	int k = fs->stack;
	FStack *n = fs;
	fs = fs->next;
	free(n);
	return k;
    }
}

static void pque_push(char* a, int m){
    if(ps == NULL){
	ps = malloc(sizeof(PStack));
	sprintf(ps->pstmt, "%s", a);
	ps->mark = m;
	ps->next = NULL;
    }
    else{
	for(PStack* k=ps;k!=NULL;k=k->next){
	    if(k->mark == m){//cat to the identical mark
		sprintf(k->pstmt, "%s%s", k->pstmt, a);
	        return;
	    }
	}
	//no such mark
	PStack *k = malloc(sizeof(PStack));
	k->mark = m;
	sprintf(k->pstmt, "%s", a);
	k->next = ps;
	ps = k;
	
    }
}

static void pque_pop(){

    if(ps != NULL){
	PStack *n = ps;
	fwrite(ps->pstmt, 1, strlen(ps->pstmt), fp);
	ps = ps->next;
	free(n);
    }
}

static void istack_push(int a){
    if(is == NULL){
	is = malloc(sizeof(FStack));
	is->stack = a;
	is->next = NULL;
    }
    else{
	FStack* k = malloc(sizeof(FStack));
	k->stack = a;
	k->next = is;
	is = k;
    }
}

static int  istack_pop(){
    if(is == NULL)
	return -100;
    else{
	int k = is->stack;
	FStack *n = is;
	is = is->next;
	free(n);
	return k;
    }
}

static void iistack_push(int a){
    if(iis == NULL){
	iis = malloc(sizeof(FStack));
	iis->stack = a;
	iis->next = NULL;
    }
    else{
	FStack* k = malloc(sizeof(FStack));
	k->stack = a;
	k->next = iis;
	iis = k;
    }
}

static int  iistack_pop(){
    if(iis == NULL)
	return -100;
    else{
	int k = iis->stack;
	FStack *n = iis;
	iis = iis->next;
	free(n);
	return k;
    }
}

static void init(){
    ps = NULL;
    for(int i=0; i<MAX_SIZE; i++){
         table[i].Index = -1;
         table[i].Address = -1;
	 table[i].scope = -1;
    }
}

