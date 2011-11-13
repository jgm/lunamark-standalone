OBJS=lpeg.o slnunico.o
COPT= -O2 -DNDEBUG
CWARNS= -Wall -Wextra -pedantic
CC=gcc $(COPT) $(CWARNS) -I$(LUADIR) -L$(LUADIR)
LUADIR=lua

lunamark: lunamark.c main.squished.lua.embed $(OBJS) $(LUADIR)/liblua.a
	$(CC) -o $@ $< $(OBJS) -llua -lm -ldl

$(LUADIR)/liblua.a : $(wildcard $(LUADIR)/*.h) $(wildcard $(LUADIR)/*.c) $(LUADIR)/Makefile
	make liblua.a -C $(LUADIR) MYCFLAGS=-DLUA_USE_LINUX
	# note: LUA_USE_LINUX is recommended for linux, osx, freebsd

main.squished.lua : src/main.lua
	(cd src && lua ../squish.lua)

lpeg.o : lpeg.c lpeg.h $(LUADIR)/liblua.a

slnunico.o : slnunico.c slnudata.c

%.embed : %
	xxd -i $< > $@

clean:
	make -C $(LUADIR) clean
	rm $(lunamarkS) $(OBJS) lunamark main.squished.lua.embed main.squished.lua
