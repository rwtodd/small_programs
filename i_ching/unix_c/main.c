#include<ncurses.h>
#include<stdio.h>
#include<stdlib.h>

/* ******************************************************
 * D A T A
 * ******************************************************
 */

/* We need a type for a hexagram. It fits in
 * 8-bits, so unsigned char will do.
 */
typedef unsigned char hexagram;

/* Store the hexagram descriptions in another file to
 * keep this file tidy.
 */
static struct hex_data {
  const char *const name;
  const char *const desc1;
  const char *const desc2;
  hexagram lines;
} hex_data[] = {
#include "hex_data.h"
};

/* lookup_lines finds the hex_data entry that matches
 * the given hexagram `h'.  It uses a simple linear
 * search, which is OK since it searches at most
 * 64 items.
 */
static int lookup_lines(hexagram h) {
  int i = 0;
  for( ; i < 64; ++i)
     if(hex_data[i].lines == h) break;
  return i;
}

/* Store the trigram descriptions in anotehr file to
 * keep this file tidy.
 */
static const char *const trigrams[] = {
#include "tri_data.h"
};

/* ******************************************************
 * B R O W S I N G   S T A T E
 * ******************************************************
 */

/* We'll keep a 10-deep history of visited hexagrams. */
int history[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
int hidx = 0;


/* ******************************************************
 * U S E R   I N T E R F A C E 
 * ******************************************************
 */

void draw_hexagram(const struct hex_data *const hex) {
  mvprintw(2, 2, hex->name); clrtoeol();
}


int main(int arc, char **argv) {
  initscr();
  cbreak();
  noecho();
  curs_set(0);

  /* display a list of commands */
  mvprintw(18,2,
     "(n)ext/(p)rev (f)orw/(b)ack (i)nner in(v)ert (c)hange (g)oto (q)uit");

  int cur = history[hidx];
  while(1) {
     draw_hexagram(&hex_data[cur]);
     char c = getch();
     switch(c) {

     /* next wen sequence hexagram */
     case 'n':  
        if(++cur == 64) cur = 0;
        if(++hidx == 10) hidx = 0;
        history[hidx] = cur;
        break;

     /* previous wen sequence hexagram */
     case 'p':
        if(--cur == -1) cur = 63;
        if(++hidx == 10) hidx = 0;
        history[hidx] = cur;
        break;

     /* go back in the history */
     case 'b':
        if(--hidx == -1) hidx = 9;
        cur = history[hidx];
        break;

     /* go forward in the history */
     case 'f':
        if(++hidx == 10) hidx = 0;
        cur = history[hidx];
        break;

     /* go to a specific wen-sequence hexagram */
     case 'g':
        /* todo */ 
        break;

     /* invert the current hexagram */
     case 'i':
        cur = lookup_lines(~(hex_data[cur].lines) & 63);
        if(++hidx == 10) hidx = 0;
        history[hidx] = cur;
        break;
 
     /* quit */
     case 'q':
     case 'Q':
        goto done;
        break;
     }
  }

done:
  endwin();
  return 0;
}
