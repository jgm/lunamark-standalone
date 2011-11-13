CC=gcc
BIT32=lbitlib-5.2.0-alpha-backport1
ALL=embed lpeg.o $(BIT32).o script.lua.embed

.PHONY: all
all: $(ALL)

lpeg.o : lpeg.c lpeg.h

$(BIT32).o : $(BIT32).c

%.lua.embed : %.lua
	echo "unsigned char $(basename $<)_lua[] = {\n" > $@
	cat $< | xxd -i >> $@
	echo ", 0x00 };\n" >> $@

embed: embed.c script.lua lpeg.o $(BIT32).o script.lua.embed
	$(CC) -o $@ $< lpeg.o $(BIT32).o -llua

clean:
	rm $(ALL)
