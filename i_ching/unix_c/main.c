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

/* Here's a function to generate an inner hexagram
 * from the given lines.
 */
static hexagram inner_hex(hexagram h) {
  return ((h << 1 & -7) | (h >> 1 & 7)) & 63;
}

/* change_line changes a single line in a given
 * hexagram
 */
static hexagram change_line(hexagram h, int which) {
   return (h ^ (1 << (which - 1))) & 63;
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

/* many commands will advance the history, so we will
 * make a macro for ergonomics 
 */
#define advance_history(c) \
if(++hidx == 10) hidx = 0; \
history[hidx] = (c)


/* ******************************************************
 * U S E R   I N T E R F A C E 
 * ******************************************************
 */

static void draw_hexagram(const struct hex_data *const hex) {
  mvprintw(2, 2, hex->name); clrtoeol();
}

/* clear_question prepares a line for questions, and can also
 * be used to clear the line afterward.
 */
static void clear_question(void) {
  move(20,2);
  clrtoeol();
}

/* which_line asks the user which line to change */
static int which_line(void) {
  clear_question();
  printw("Which line do you want to change (1 - 6)? ");
  refresh();
  int num = getch() - '0';
  if(num > 6 || num < 1)
     num = 7;
  clear_question();
  return num;
}

/* which_hex asks the user which hexagram to view */
static int which_hex(void) {
  clear_question();
  printw("Which hexagram do you want to visit (1 - 64)? ");
  refresh();
  int ans = getch();
  addch(ans);
  refresh();
  ans = ans - '0';
  int digit2 = getch();
  if(digit2 != '\n') ans = ans*10 + (digit2 - '0');
  clear_question();
  return (ans - 1) & 63;
}

/* ******************************************************
 * M A I N   E V E N T   L O O P
 * ******************************************************
 */

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
     refresh();
     char c = getch();
     switch(c) {

     /* next wen sequence hexagram */
     case 'n':  
        if(++cur == 64) cur = 0;
        advance_history(cur);
        break;

     /* previous wen sequence hexagram */
     case 'p':
        if(--cur == -1) cur = 63;
        advance_history(cur);
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
        cur = which_hex(); 
        advance_history(cur);
        break;

     /* Generate the inner hexagram */
     case 'i':
        cur = lookup_lines(inner_hex(hex_data[cur].lines));
        advance_history(cur);
        break;

     /* invert the current hexagram */
     case 'v':
        cur = lookup_lines(~(hex_data[cur].lines) & 63);
        advance_history(cur);
        break;

     /* change a hexagram line */
     case 'c':
        cur = lookup_lines(change_line(hex_data[cur].lines, which_line()));
        advance_history(cur);
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
