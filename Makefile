CC=gcc
OBJS=lpeg.o slnunico.o
ALL=embed $(OBJS) script.lua.embed
.PHONY: all
all: $(ALL)

lpeg.o : lpeg.c lpeg.h

slnunico.o : slnunico.c slnudata.c

script.lua.embed : luac.out
	xxd -i $< > $@

%.lub : %.lua
	luac -o $@ $<

%.lub.embed : %.lub
	xxd -i $< > $@

embed: embed.c script.lub.embed $(OBJS)
	$(CC) -o $@ $< $(OBJS) -llua

clean:
	rm $(ALL)
