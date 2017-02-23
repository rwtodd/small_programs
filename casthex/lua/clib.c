#include<stdio.h>
#include<string.h>
#include<stdbool.h>
#include<ctype.h>

#include<lua.h>
#include<lualib.h>
#include<lauxlib.h>

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

/* display_casting prints a representation of 
 * the `arg' casting to the terminal.
 */
static int display_casting(lua_State *L) {
  const char *casting = luaL_checkstring(L, 1); 

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
  puts("\n");
  return 0;
}

static const struct luaL_Reg hexlib[] = { 
  {"disphex", display_casting},
  {NULL, NULL}
};

LUALIB_API int luaopen_hex(lua_State *L) { 
/*   luaL_register(L, "hex", hexlib); */
   luaL_newlib(L, hexlib); 
   lua_setglobal(L, "hex");
   return 1; 
}
