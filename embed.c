#include <stdlib.h>
#include <stdio.h>

/* Include the Lua API header files. */
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "lpeg.h"
#include "script.lua.embed"  // defines script_lua, contents of script.lua

int main()
{
    int s=0;

    lua_State *L = lua_open();

    // load the libs
    luaL_openlibs(L);
    luaopen_lpeg(L);
    luaopen_bit32(L);

    luaL_dostring(L, script_lua);

    printf("\nI am done with Lua in C.\n");

    lua_close(L);

    return 0;
}
