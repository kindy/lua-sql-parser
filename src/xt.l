/*
 * code from "flex & bison"
 * Scanner for mysql subset, GLR version
 */

%option noyywrap nodefault yylineno case-insensitive

%{

#include "xt.h"
#include "xt.tab.h"

YY_BUFFER_STATE bp;

%}

%x COMMENT
%s BTWMODE

%%

[0-9]+ {
    DD("lex got %s\n", yytext);
    yylval.node = new_node_i(NUMBER, atoi(yytext));
    return NUMBER;
}

[();] {
    DD("lex got %s\n", yytext);
    return yytext[0];
}

[-+*/] {
    DD("lex got %s\n", yytext);
    yylval.node = new_node_c(yytext[0], yytext);
    return yytext[0];
}

    /* everything else */
[ \t\n]         /* white space */
.               { yyerror("mystery character '%c'", *yytext); }

%%

int lex_init (const char *s, size_t l) {
    bp = yy_scan_bytes(s, l);
    yy_switch_to_buffer(bp);

    return 0;
}
int lex_clean () {
    yy_delete_buffer(bp);

    return 0;
}
