CC=gcc
OBJS=lpeg.o slnunico.o
ALL=embed $(OBJS) script.lua.embed
.PHONY: all
all: $(ALL)

lpeg.o : lpeg.c lpeg.h

slnunico.o : slnunico.c slnudata.c

%.lua.embed : %.lua
	echo "unsigned char $(basename $<)_lua[] = {\n" > $@
	cat $< | xxd -i >> $@
	echo ", 0x00 };\n" >> $@

embed: embed.c script.lua.embed $(OBJS)
	$(CC) -o $@ $< $(OBJS) -llua

clean:
	rm $(ALL)
