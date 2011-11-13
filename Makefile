CC=gcc
OBJS=lpeg.o slnunico.o

lunamark: lunamark.c main.squished.lub.embed $(OBJS)
	$(CC) -o $@ $< $(OBJS) -llua

main.squished.lua : src/main.lua
	(cd src && lua squish.lua)

lpeg.o : lpeg.c lpeg.h

slnunico.o : slnunico.c slnudata.c

%.lub : %.lua
	luac -o $@ $<

%.lub.embed : %.lub
	xxd -i $< > $@

clean:
	rm $(lunamarkS) $(OBJS) lunamark main.squished.lub.embed
