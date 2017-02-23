# Set these variables for your system
PREFIX=$(HOME)
LUADIR=$(HOME)/brew

INC=-I$(LUADIR)/include
LIB=-L$(LUADIR)/lib

.PHONY: all install clean

all: hex.so casthex.luac

hex.so: clib.c hexnames.h
	cc clib.c $(INC) -fpic -shared $(LIB) -llua.5.2 -o $@

casthex.luac: casthex.lua
	luac -o $@ $? 

install: all
	-mkdir -p $(PREFIX)/bin/f.casthex
	cp hex.so casthex.luac $(PREFIX)/bin/f.casthex
	cp casthex.sh $(PREFIX)/bin

clean:
	-rm casthex.luac hex.so
