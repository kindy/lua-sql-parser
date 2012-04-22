#include <stdio.h>
#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

extern int to_lua(lua_State *L);

static int parse(lua_State * L) {
    return to_lua(L);
}

static const struct luaL_Reg sql_parser [] = {
    {"parse", parse},
    {NULL, NULL}  /* sentinel */
};

int luaopen_sql_parser (lua_State *L) {
    luaL_register(L, "sql_parser", sql_parser);
    return 1;
}
