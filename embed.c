#include <stdlib.h>
#include <stdio.h>

/* Include the Lua API header files. */
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "lpeg.h"
#include "script.squished.lub.embed"

int main()
{
    int s=0;

    lua_State *L = lua_open();

    // load the libs
    luaL_openlibs(L);
    luaopen_lpeg(L);
    luaopen_unicode(L);

    luaL_loadbuffer(L, script_squished_lub, script_squished_lub_len, "script_squished_lub");
    if (lua_pcall(L, 0, LUA_MULTRET, 0) != 0) {
      lua_error(L);
    }

//    printf("\nI am done with Lua in C.\n");

    lua_close(L);

    return 0;
}
