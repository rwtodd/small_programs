# Set these variables for your system
PREFIX=$(HOME)
LUADIR=$(HOME)

INC=-I$(LUADIR)/include
OPT=-march=native -O2

.PHONY: all install clean

all: hex.so casthex.luac

hex.so: clib.c hexnames.h
	cc $(OPT) $(INC) -fPIC -dynamiclib -undefined dynamic_lookup  -o $@ clib.c

casthex.luac: casthex.lua
	$(LUADIR)/bin/luac -o $@ $? 

install: all
	-mkdir -p $(PREFIX)/bin/f.casthex
	cp hex.so casthex.luac $(PREFIX)/bin/f.casthex
	cp casthex.sh $(PREFIX)/bin

clean:
	-rm casthex.luac hex.so
