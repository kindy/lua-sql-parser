#ifndef XT_H
# define XT_H

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

extern int yylineno;
void yyerror(const char *s, ...);

typedef union {
    int i;
    double x;
    char *c;
} node_val_t;
typedef struct node_info {
    int line;
    int col;
    char type;
} node_info_t;
typedef struct node node_t;
struct node {
    int type;
    node_info_t *info;
    node_val_t val;
    node_t *sub;
    node_t *next;
};

int to_lua(lua_State *L);
int node_to_lua(lua_State *L, node_t *node);
int lex_init (const char *s, size_t l);
int lex_clean ();
int yy_clean();

node_t *set_root(node_t *node);
node_t *_new_node(int type);
node_t *new_node_i(int type, int val);
node_t *new_node_c(int type, const char *val);
node_t *new_node_x(int type, double val);
// 具体实现可能是 unshift 因为比较快
node_t *push_node(node_t *p, node_t *c);
node_t *push_two_node(node_t *p, node_t *a, node_t *b);
int free_node(node_t *node);

void _noop(char *s, ...);
#define DD _noop
#endif
