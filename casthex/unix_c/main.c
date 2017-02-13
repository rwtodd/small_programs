/* casthex: Display iching hexagrams (or pairs of them) based on castings.
 */

#include<stdio.h>
#include<string.h>
#include<stdbool.h>
#include<stdlib.h>
#include<time.h>

/* these are the four ways a line of a casting can be 
 * represented 
 */
static char *line_reps[] = {
  "  ---   ---  =>  ---------",
  "  ---------      ---------",
  "  ---   ---      ---   ---",
  "  ---------  =>  ---   ---"
};

/* we'll store the names of the hexagrams in a different file
 * to keep this one tidy.
 */
static char *names[] = {
#include "hexnames.h"
};


/* This is the casting we'll display. It will either be determined
 * by a random method, or provided on the command line.
 */
static char casting[7];

/* do_casting fills `casting` by calling `fn` 6 times. */
static void do_casting(char (*fn)()) {
  for(int i = 0; i < 6; ++i) {
    casting[i] = fn();
  }
}

static char coins_method() {
  int rnum = rand();
  return '6' + (rnum&1) + ((rnum&2)>>1) + ((rnum&4)>>2);
}

static char static_method() {
  return '7' + (rand()&1);
}

static char stalks_method() {
  int rnum = rand()&0x0f;
  if(rnum&1) {
    /* odd */
    return (rnum <= 5) ? '9' : '7';
  } 

  /* even */
  return (rnum == 0) ? '6' : '8';
}

/* determine_casting parses the input argument to determine
 * what string to put in `casting`. It returns 'true' if it
 * succeeded, and 'false' if the input was bad.
 */
static bool determine_casting(const char * const argument) {
  bool response = true;

  if(!strcmp(argument, "-coins")) {
     do_casting(coins_method);
  } else if(!strcmp(argument, "-stalks")) {
     do_casting(stalks_method);
  } else if(!strcmp(argument, "-static")) {
     do_casting(static_method);
  } else {
     /* copy the input to `casting`, checking that it looks
      * valid along the way
      */
     if(strlen(argument) == 6) {
       for(int i = 0; i < 6; ++i) {
          char ch = argument[i];
          if(ch < '6' || ch > '9') response = false; 
          casting[i] = ch;      
       }
     } else {
       response = false;
     }
  }
  casting[6] = '\0'; /* null terminator */
  return response; /* did we have an error? */
}

int main(int argc, char**argv) {
  const char *arg = "-coins";  /* default */
  if(argc > 1) {
     arg = argv[1];
  }

  srand(time(NULL));

  if(!determine_casting(arg)) {
    fputs("Usage: casthex [-coins|-stalks|-static|<casting>]\n",stderr);
    exit(1);
  }
  
  /* decode the casting into 2 king-wen numbers, and a list of 
   * line representations. 
   */
  int wen1 = 0; 
  int wen2 = 0; 
  char *rep[6]; 

  for(int i = 5; i >= 0; --i) {
     wen1 <<= 1;
     wen2 <<= 1;
     int current = casting[i]-'6';
     rep[i] = line_reps[current];
     switch(current) {
     case 0: wen2 |= 1; break;
     case 1: wen1|= 1; wen2 |= 1; break;
     case 3: wen1|= 1;  break;
     }
  }

  
  /* now, display the hexagram(s) */
  bool changed = wen1 != wen2;

  printf("Casting: <%s>\n\n", casting);

  char short_version[12]; /* buffer if we shorten the strings */
  for(int i = 5; i >= 0; --i) {
      if(changed) {
        puts(rep[i]);
      } else {
        strncpy(short_version, rep[i], 11);
        short_version[11] = '\0';
        puts(short_version);
      }         
  }
  
  printf("\n%s\n", names[wen1]);
  if (changed) {
    printf(" = Changing to =>\n%s\n", names[wen2]);
  }

  return 0;
}

