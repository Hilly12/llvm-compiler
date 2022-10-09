%{
#include <string>
#include <math.h>
#include "node.h"
#include "parser.hpp"

#define SAVE_TOKEN      yylval.string = new std::string(yytext, yyleng)
#define SAVE_INTEGER    yylval.integer = std::stoll(std::string(yytext, yyleng))
#define SAVE_NUMBER     yylval.number = std::stod(std::string(yytext, yyleng))
#define SAVE_BOOLEAN    yylval.boolean = std::string(yytext, yyleng) == "true" ? 1 : 0
#define TOKEN(t)        (yylval.token = t)

int line_num = 1;
%}

%option noyywrap

ALPHA       [A-Za-z]
DIGIT       [0-9]
IDENTIFIER  ({ALPHA}|_)({ALPHA}|_|{DIGIT})*
INTEGER     -?{DIGIT}+
NUMBER_1    [-+]?{DIGIT}+\.([eE][-+]?{DIGIT}+)?
NUMBER_2    [-+]?{DIGIT}*\.{DIGIT}+([eE][-+]?{DIGIT}+)?
NUMBER      {NUMBER_1}|{NUMBER_2}
ESCAPE_CHAR ("\\n")|("\\t")|("\\r")|("\\\\")|("\\\"")
STRING_SQ   ("\'")(({ESCAPE_CHAR})|([^\\\'\"]))*("\'")
STRING_DQ   ("\"")(({ESCAPE_CHAR})|([^\\\'\"]))*("\"")
STRING      ({STRING_SQ})|({STRING_DQ})

%x comment

%%

"true"          { SAVE_BOOLEAN; return BOOLEAN }
"false"         { SAVE_BOOLEAN; return BOOLEAN; }
"int"           { return TOKEN(INT); }
"float"         { return TOKEN(FLOAT); }
"double"        { return TOKEN(DOUBLE); }
"return"        { return TOKEN(RETURN); }
"if"            { return TOKEN(IF); }
"else"          { return TOKEN(ELSE); }
"for"           { return TOKEN(FOR); }
"while"         { return TOKEN(WHILE); }
"class"         { return TOKEN(CLASS); }
"print"         { return TOKEN(PRINT); }
"="             { return TOKEN(ASSIGN); }
"+"             { return TOKEN(PLUS); }
"-"             { return TOKEN(MINUS); }
"*"             { return TOKEN(MULTIPLY); }
"/"             { return TOKEN(DIVIDE); }
"%"             { return TOKEN(MODULO); }
"~"             { return TOKEN(NEGATE); }
"&"             { return TOKEN(BITWISE_AND); }
"|"             { return TOKEN(BITWISE_OR); }
"^"             { return TOKEN(XOR); }
"<<"            { return TOKEN(LSHIFT); }
">>"            { return TOKEN(RSHIFT); }
"++"            { return TOKEN(INCREMENT); }
"--"            { return TOKEN(DECREMENT); }
"+="            { return TOKEN(PLUS_EQ); }
"-="            { return TOKEN(MINUS_EQ); }
"*="            { return TOKEN(MULT_EQ); }
"/="            { return TOKEN(DIVIDE_EQ); }
"=="            { return TOKEN(EQUAL); }
"!="            { return TOKEN(NOT_EQUAL); }
"&&"            { return TOKEN(AND); }
"||"            { return TOKEN(OR); }
"!"             { return TOKEN(NOT); }
"<="            { return TOKEN(LESS_THAN_EQUAL); }
">="            { return TOKEN(GREATER_THAN_EQUAL); }
"<"             { return TOKEN(LESS_THAN; }
">"             { return TOKEN(GREATER_THAN; }
";"             { return TOKEN(SEMICOLON); }
","             { return TOKEN(COMMA); }
"("             { return TOKEN(OP); }
")"             { return TOKEN(CP); }
"{"             { return TOKEN(OC); }
"}"             { return TOKEN(CC); }
"["             { return TOKEN(OB); }
"]"             { return TOKEN(CB); }

{IDENTIFIER}    { SAVE_TOKEN; return IDENTIFIER; }
{STRING}        { SAVE_TOKEN; return STRING; }
{INTEGER}       { SAVE_INTEGER; return INTEGER; }
{NUMBER}        { SAVE_NUMBER; return FLOAT; }

"//"[^\n]*  	        /* eat line comments */
"/*"			        { BEGIN(comment); }
<comment>[^*\n]*        /* eat anything that's not a '*' */
<comment>"*"+[^*/\n]*   /* eat up '*'s not followed by '/'s */
<comment>\n             { ++line_num; }
<comment>"*"+"/"        { BEGIN(INITIAL); }

[ \r\t]			/* skip whitespace */
\n 				{ ++line_num; }
<<EOF>> 	    { return EOF; }

.               { yyerror("Error in line %d: unrecognized literal '%s'", line_num, yytext); }

%%