all: iching

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
	-${RM} *.o *.a *~ iching

install: all
	${MKDIR} -p ${PREFIX}/bin
	${MV} iching ${PREFIX}/bin
	${MKDIR} -p ${PREFIX}/man/man1
	${CP} iching.1 ${PREFIX}/man/man1

iching: main.c  hex_data.h tri_data.h
	${CC} ${CCOPT} main.c -o iching -lncurses
	${STRIP} iching

