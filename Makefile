CC=gcc
OBJS=lpeg.o slnunico.o

embed: embed.c main.squished.lub.embed $(OBJS)
	$(CC) -o $@ $< $(OBJS) -llua

main.squished.lua : main.lua
	lua squish.lua

lpeg.o : lpeg.c lpeg.h

slnunico.o : slnunico.c slnudata.c

%.lub : %.lua
	luac -o $@ $<

%.lub.embed : %.lub
	xxd -i $< > $@

clean:
	rm $(EMBEDS) $(OBJS) embed main.squished.lub.embed
