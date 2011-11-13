OBJS=lpeg.o slnunico.o
COPT= -O2 -DNDEBUG
CWARNS= -Wall -Wextra -pedantic
CC=gcc $(COPT) $(CWARNS) -I$(LUADIR) -L$(LUADIR)
LUADIR=luasrc
ifeq ($(shell uname), Darwin)
  PLATFORM=macosx
else
  PLATFORM=linux
endif

lunamark: lunamark.c main.squished.lub.embed $(OBJS) $(LUADIR)/lua
	$(CC) -o $@ $< $(OBJS) -llua -lm -ldl

$(LUADIR)/lua : $(wildcard $(LUADIR)/*.h) $(wildcard $(LUADIR)/*.c) $(LUADIR)/Makefile
	make $(PLATFORM) -C $(LUADIR)

main.squished.lua : src/main.lua
	(cd src && lua ../squish.lua)

lpeg.o : lpeg.c lpeg.h $(LUADIR)/lua

slnunico.o : slnunico.c slnudata.c

%.lub : %.lua
	luac -o $@ $<

%.lub.embed : %.lub
	xxd -i $< > $@

clean:
	rm $(lunamarkS) $(OBJS) lunamark main.squished.lub.embed main.squished.lua
