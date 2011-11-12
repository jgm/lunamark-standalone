#include <stdlib.h>
#include <stdio.h>

/* Include the Lua API header files. */
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "lpeg.h"

int main()
{
    int s=0;

    lua_State *L = lua_open();

    // load the libs
    luaL_openlibs(L);
    luaopen_lpeg(L);
    luaopen_bit32(L);

    //run a Lua scrip here
    luaL_dofile(L,"script.lua");

    printf("\nI am done with Lua in C.\n");

    lua_close(L);

    return 0;
}
