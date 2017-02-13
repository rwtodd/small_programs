all: casthex

# Change this to affect where stuff gets installed...
PREFIX=${HOME}

# if your commands are somewhere wierd or not on the path,
# then set these variables...
CC=cc
AR=ar
STRIP=strip
MKDIR=mkdir
MV=mv
CP=cp
RM=rm

# Change this to adjust build options
CCOPT=-std=c11 -march=native -O2

clean:
	-${RM} *.o *.a *~ casthex

install: all
	${MKDIR} -p ${PREFIX}/bin
	${MV} casthex ${PREFIX}/bin
	${MKDIR} -p ${PREFIX}/man/man1
	${CP} casthex.1 ${PREFIX}/man/man1

casthex: main.c  hexnames.h
	${CC} ${CCOPT} main.c -o casthex
	${STRIP} casthex

