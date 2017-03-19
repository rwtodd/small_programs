# Set these variables for your system
PREFIX=$(HOME)
LUADIR=/usr/local

INC=-I$(LUADIR)/include
OPT=-std=c11 -march=native -O2

ifeq ($(shell uname -s),Linux)
  LOPT=-shared -fpic
else
  LOPT=-shared -fpic -undefined dynamic_lookup
endif

.PHONY: all install clean

all: hex.so casthex.luac

clib.o: clib.c hexnames.h
	cc $(OPT) $(INC) -fpic -c -o $@ clib.c

hex.so: clib.o 
	cc $(OPT) $(LOPT) -o $@ $< 

casthex.luac: casthex.lua
	$(LUADIR)/bin/luac -o $@ $<

install: all
	-mkdir -p $(PREFIX)/bin/f.casthex
	cp hex.so casthex.luac $(PREFIX)/bin/f.casthex
	cp casthex.sh $(PREFIX)/bin

clean:
	-rm casthex.luac hex.so clib.o 
