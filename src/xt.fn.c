#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "xt.h"
#include "xt.tab.h"

node_t *root;

node_t *set_root(node_t *node) {
    root = node;
}
// 为顶端的 table 添加 sub 数组，内容是 node->sub
void add_lua_node_sub(lua_State *L, node_t * node) {
    int idx = 1;
    node_t *t = node->sub;

    lua_newtable(L);

    while (t) {
        node_to_lua(L, t);
        lua_rawseti(L, -2, idx); // 消耗 node_to_lua 添加的 table
        idx++;
        t = t->next;
    }

    lua_rawseti(L, -2, 3);
}
// 生成一个table 并留在 L 顶端
int node_to_lua(lua_State *L, node_t *node) {
    char * type;
    // STMT_LIST + - * / -> {}
    // number -> ->val.i
    // node -> {type, value, subs, pos = {line = 1, col = 1}}
    lua_newtable(L);

    switch (node->type) {
        case NUMBER:
            type = "number";
            lua_pushnumber(L, (double) node->val.i);
            lua_rawseti(L, -2, 2);
            break;
        case STMT_LIST:
        case '+':
        case '-':
        case '*':
        case '/':
            type = node->val.c;
            add_lua_node_sub(L, node);
            break;
    };

    lua_pushstring(L, type);
    lua_rawseti(L, -2, 1);
    lua_newtable(L); // push .pos
    lua_pushnumber(L, node->info->line);
    lua_setfield(L, -2, "line"); // .pos.line
    if (node->info->col) {
        lua_pushnumber(L, node->info->col);
        lua_setfield(L, -2, "col"); // .pos.col
    }
    // lua_setfield(L, -2, "pos"); // pop .pos
    lua_rawseti(L, -2, 4); // pop .pos

    // 仅用于标识 lua 返回值数量
    return 1;
}
int to_lua (lua_State *L) {
    size_t l;
    const char *s = lua_tolstring(L, -1, &l);   /* any Lua string */

    //lua_pushstring(L, s);
    //return 1;

    lex_init(s, l);

    if (yyparse()) {
        lua_pushnil(L);
        lua_pushstring(L, "SQL parse failed");
        lex_clean();
        return 2;
    }

    lex_clean();
    yy_clean();

    if (! root) {
        lua_pushnil(L);
        lua_pushstring(L, "SQL parse failed 2");
        return 2;
    }

    int n = node_to_lua(L, root);
    free_node(root);
    return n;
}

int free_node(node_t *node) {
    if (node->info->type == 'c') {
        free(node->val.c);
    }

    node_t *t = node->sub;
    while (t) {
        free_node(t);
        t = t->next;
    }

    free(node->info);
    free(node);
    return 0;
}

node_t *_new_node(int type) {
    DD("_new_node %d\n", type);

    extern yylineno;
    node_t * node = malloc(sizeof(node_t));
    node_info_t *info = malloc(sizeof(node_info_t));

    if (!node || !info) {
        yyerror("out of memory");
        exit(0);
    }

    info->line = (int) yylineno;
    info->col = 0;
    info->type = 0;
    node->info = info;

    node->type = type;
    node->sub = NULL;
    node->next = NULL;

    return node;
}
node_t *new_node_c(int type, const char *val) {
    node_t *node = _new_node(type);
    node->info->type = 'c';
    node->val.c = strdup(val);
    DD("new_node_c %s\n", val);
    return node;
}
node_t *new_node_i(int type, int val) {
    node_t *node = _new_node(type);
    node->info->type = 'i';
    node->val.i = val;
    return node;
}
node_t *new_node_x(int type, double val) {
    node_t *node = _new_node(type);
    node->info->type = 'x';
    node->val.x = val;
    return node;
}

int node_len(node_t *node) {
    int n = 0;
    node_t *t = node->sub;

    if (t) {
        while (t) {
            n++;
            t = t->next;
        }
    }

    return n;
}

node_t *push_node(node_t *p, node_t *c) {
    if (! p->sub) {
        p->sub = c;
    } else {
        c->next = p->sub;
        p->sub = c;
    }

    return p;
}

node_t *push_two_node(node_t *p, node_t *a, node_t *b) {
    push_node(p, a);
    push_node(p, b);

    return p;
}

void _noop(char *s, ...) {
}

void
yyerror(const char *s, ...)
{
  extern yylineno;

  va_list ap;
  va_start(ap, s);

  fprintf(stderr, "%d: cust error: ", yylineno);
  vfprintf(stderr, s, ap);
  fprintf(stderr, " *cust end* \n");
}

