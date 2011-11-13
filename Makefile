CC=gcc
OBJS=lpeg.o slnunico.o

embed: embed.c script.squished.lub.embed $(OBJS)
	$(CC) -o $@ $< $(OBJS) -llua

script.squished.lua : script.lua
	lua squish.lua

lpeg.o : lpeg.c lpeg.h

slnunico.o : slnunico.c slnudata.c

%.lub : %.lua
	luac -o $@ $<

%.lub.embed : %.lub
	xxd -i $< > $@

clean:
	rm $(EMBEDS) $(OBJS) embed script.squished.lub.embed
