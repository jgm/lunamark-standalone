CC=gcc
OBJS=lpeg.o slnunico.o
EMBEDS=re.lub.embed cosmo.lub.embed alt_getopt.lub.embed script.lub.embed 

embed: embed.c $(EMBEDS) $(OBJS)
	$(CC) -o $@ $< $(OBJS) -llua

lpeg.o : lpeg.c lpeg.h

slnunico.o : slnunico.c slnudata.c

%.lub : %.lua
	luac -o $@ $<

%.lub.embed : %.lub
	xxd -i $< > $@

clean:
	rm $(EMBEDS) $(OBJS) embed
