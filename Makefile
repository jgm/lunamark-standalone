CC=gcc
BIT32=lbitlib-5.2.0-alpha-backport1
ALL=embed lpeg.o $(BIT32).o

.PHONY: all
all: $(ALL)

lpeg.o : lpeg.c lpeg.h

$(BIT32).o : $(BIT32).c

embed: embed.c script.lua lpeg.o $(BIT32).o
	$(CC) -o $@ $< lpeg.o $(BIT32).o -llua

clean:
	rm $(ALL)
