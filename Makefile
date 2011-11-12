CC=gcc
ALL=embed lpeg.o

.PHONY: all
all: $(ALL)

lpeg.o : lpeg.c lpeg.h

embed: embed.c script.lua lpeg.o
	$(CC) -o $@ $< lpeg.o -llua

clean:
	rm $(ALL)
