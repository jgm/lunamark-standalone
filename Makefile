CC=gcc
OBJS=lpeg.o slnunico.o
ALL=embed $(OBJS) script.lua.embed
.PHONY: all
all: $(ALL)

lpeg.o : lpeg.c lpeg.h

slnunico.o : slnunico.c slnudata.c

%.lua.embed : %.lua
	xxd -i $< > $@

embed: embed.c script.lua.embed $(OBJS)
	$(CC) -o $@ $< $(OBJS) -llua

clean:
	rm $(ALL)
