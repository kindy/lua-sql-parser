/*
 * code from "flex & bison"
 * Parser for mysql subset, GLR version
 */

%glr-parser

%defines

%{
#include <stdio.h>
#include <stdlib.h>
#include "xt.h"
%}

%union {
    node_t *node;
}

// {{{
%token <node> NUMBER
%token EOL

%left <node> '+' '-'
%left <node> '*' '/'

%token <node> STMT_LIST
%type <node> stmt stmt_list exp
%%

doc:
    stmt_list {
        set_root($1);
    }

stmt_list:
    stmt {
        $$ = new_node_c(STMT_LIST, "STMT_LIST");
        push_node($$, $1);
    }
    | stmt_list stmt {
        push_node($1, $2);
    }
    ;

stmt:
    exp ';' {
        $$ = $1;
    }
    ;

exp:
    '(' exp ')' { $$  = $2; }
    | exp '+' exp { $$ = push_two_node($2, $1, $3); }
    | exp '-' exp { $$ = push_two_node($2, $1, $3); }
    | exp '*' exp { $$ = push_two_node($2, $1, $3); }
    | exp '/' exp { $$ = push_two_node($2, $1, $3); }
    | NUMBER { $$ = $1; }
    ;

%%

int yy_clean() {}
