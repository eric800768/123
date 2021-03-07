%{
#include "p2.tab.h"
%}
digit [0-9]
letter [a-z]
id {letter}({letter}|{digit}|-)*
%%
[ \t\r\n]+		{ /* do nothing */ }
"print-num" 	{return PRINTNUM;}
"print-bool"  	{return PRINTBOOL;}
"and"		{return AND;}
"or"			{return OR;}
"not"		{return NOT;}
"if"			{return IF;}
"mod"		{return MOD;}
"define"		{return DEFINE;}
"fun"		{return FUN;}
{id}			{yylval.word = strdup(yytext); return ID;}

[0-9] 		{ yylval.ival = atoi(yytext);return(NUMBER); }
[-][0-9] 		{ yylval.ival = atoi(yytext);return(NUMBER); }
[1-9][0-9]+ 	{ yylval.ival = atoi(yytext);return(NUMBER); }
[-][1-9][0-9]+ 	{ yylval.ival = atoi(yytext);return(NUMBER); }
#t			{ yylval.ival = 1; return(BOOLVAL);}
#f			{ yylval.ival = 0; return(BOOLVAL);}
"+" | "-" | "*" | "/" | "(" | ")" | ">" | "<" | "="	{ return(yytext[0]); }
.				{ return(yytext[0]); }
%%