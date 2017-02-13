/* CastGeo -- a program to cast a geomantic shield.
 */

#include<time.h>
#include<stdlib.h>
#include<stdio.h>
#include<string.h>


/* We need storage for our geomantic figures.  Since we will work
 * one line at a time, we can use an x-by-4 array.  0 = yin, 1 = yang
 */
static char line[8][4];  /* raw storage */
static int fig_count;    /* how many figures are we using? */

/* Representation on-screen for yin and yang. */
static char *dots[] = { "*   *", "  *  " };
#define fig_width 5

/* make_moms_daughters generates the mothers at random, and the 
 * daughters by permuting the mothers.
 */
static void make_moms_daughters() {
   for(int i = 7; i > 3; --i) 
     for(int j = 0; j < 4; ++j) 
        line[i][j] = line[3-j][7-i] = (char)(rand()&1);

   fig_count = 8;
}

/* combine_figs takes adjacent figures and combines them,
 * overwriting the storage in the process.
 */
static void combine_figs() {
   fig_count >>= 1;

   for(int i = 0; i < fig_count; ++i) 
     for(int j = 0; j < 4; ++j) 
        line[i][j] = line[i*2][j] ^ line[i*2+1][j];
}

/* display_line outputs the current line of geomantic figures,
 * with `initial_space` spaces in front, and `mid_space` spaces
 * between each figure.
 */
static void display_line(int initial_space, int mid_space) {
   static char buf[2+fig_width*(8+7)+1];

   for(int j = 0; j < 4; ++j) {
      /* reset the buffer */
      memset(buf, ' ', sizeof(buf)-1);

      /* fill out a line of output */
      int loc = initial_space;
      strncpy(buf+loc, dots[line[0][j]],fig_width);
      for(int i = 1; i < fig_count; ++i) {
         loc += fig_width + mid_space;
         strncpy(buf+loc, dots[line[i][j]],fig_width);
      } 
      buf[loc+fig_width] = '\0';

      /* display it */
      puts(buf); 
   }

   putchar('\n');
}

int main(int argc, char **argv) {
    srand(time(NULL));

    /* generate the shield */ 
    make_moms_daughters();
    display_line(2,5);

    combine_figs();
    display_line(7,15);

    combine_figs();
    display_line(17,35);
    
    combine_figs();
    display_line(37,0);
}
