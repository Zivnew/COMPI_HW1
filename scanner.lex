%{
#include "output.hpp"

#include <string>

using namespace output;

std::string str_buf;

%}

%option yylineno
%option noyywrap
digit       ([0-9])
letter      ([a-zA-Z])
whitespace  ([ \t\r\n])
printable   ([\x20-\x7E])

%x STR

%%
"void"      { printToken(yylineno, VOID, yytext); }
"int"       { printToken(yylineno, INT, yytext); }
"byte"      { printToken(yylineno, BYTE, yytext); }
"bool"      { printToken(yylineno, BOOL, yytext); }
"and"       { printToken(yylineno, AND, yytext); }
"or"        { printToken(yylineno, OR, yytext); }
"not"       { printToken(yylineno, NOT, yytext); }
"true"      { printToken(yylineno, TRUE, yytext); }
"false"     { printToken(yylineno, FALSE, yytext); }
"return"    { printToken(yylineno, RETURN, yytext); }
"if"        { printToken(yylineno, IF, yytext); }
"else"      { printToken(yylineno, ELSE, yytext); }
"while"     { printToken(yylineno, WHILE, yytext); }
"break"     { printToken(yylineno, BREAK, yytext); }
"continue"  { printToken(yylineno, CONTINUE, yytext); }

";"         { printToken(yylineno, SC, yytext); }
","         { printToken(yylineno, COMMA, yytext); }
"("         { printToken(yylineno, LPAREN, yytext); }
")"         { printToken(yylineno, RPAREN, yytext); }
"{"         { printToken(yylineno, LBRACE, yytext); }
"}"         { printToken(yylineno, RBRACE, yytext); }
"["         { printToken(yylineno, LBRACK, yytext); }
"]"         { printToken(yylineno, RBRACK, yytext); }
"="         { printToken(yylineno, ASSIGN, yytext); }

"=="|"!="|"<"|">"|"<="|">="         { printToken(yylineno, RELOP, yytext); }
"+"|"-"|"*"|"/"         { printToken(yylineno, BINOP, yytext); }

"//"[^\n\r]*         { printToken(yylineno, COMMENT, yytext); }

{letter}({letter}|{digit})*         { printToken(yylineno, ID, yytext); }

([1-9]{digit}*)|("0")        { printToken(yylineno, NUM, yytext); }

([1-9]{digit}*b)|("0b")        { printToken(yylineno, NUM_B, yytext); }

\"               { BEGIN(STR); str_buf = ""; }
<STR>\\\\      { str_buf += '\\'; }
<STR>\\\"      { str_buf += '\"'; }
<STR>\\n      { str_buf += '\n'; }
<STR>\\r      { str_buf += '\r'; }
<STR>\\t      { str_buf += '\t'; }
<STR>\\0      { str_buf += '\0'; }
<STR>\\x[0-9A-Fa-f]{2}  {
    int value = strtol(yytext+2, NULL, 16);
    str_buf += (char)value;
}
<STR>\"  {
    BEGIN(INITIAL);
    printToken(yylineno, STRING, str_buf.c_str());
}

{whitespace}+   ;
.   {
    errorUnknownChar(yytext[0]);
    exit(0);
}

%%