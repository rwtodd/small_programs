@* Introduction. This small program will allow an interactive
exploration of the I~Ching. The outline below sets us up with
|ncurses|, and runs a semi-infinite loop updating the display
and getting user~input. One might--correctly--assume that some
user input will cause us to |goto done|, terminating the program.
@d infinite_loop while(1)
@c
#include<ncurses.h>
#include<stdio.h>
#include<stdlib.h>

@<Hexagram Data@> @;
@<Hexagram Functions@> @;

@<UI Data@> @;
@<UI Functions@> @;

int main(int argc, char**argv) {
  initscr();
  cbreak();
  noecho(); 
  curs_set(0);

  @<Read in the hexagram descriptions@>;
  @<Display the command list@>;

  infinite_loop {
    @<Display the current hexagram@>;
    @<Parse user input@>;
  } 
done:  @;
  endwin(); 
  return 0;
}


@* Displaying Hexagrams. We will encode a hexagram as a 6-bit~number,
with each bit representing a line of the hexagram.
@<Hexagram Data@>=
typedef unsigned char hexagram;

@ To display a hexagram, we will split off bits one at a time,
treating 1~bits as {\sl yang} and 0~bits as {\sl yin}.  Here's
a function to do that:
@d HEX_BOTTOM 12 
@d LINE_COORD(line) 12 - ((line) << 1) , 2
@<Hexagram Functions@>=
void draw_hex(hexagram hex) {
  static const char yinyang[][14] = { "#####   #####", 
                                      "#############" };
  for(int ln = 0 ; ln < 6 ; ++ln) {
    mvprintw( LINE_COORD(ln) , yinyang[ hex & 1 ] );  
    hex = hex >> 1;
  }
}

@ We'll want to draw more than just the lines, though. It would 
be nice to display the {\sl trigram} information on-screen as well.
A second function will accomplish this.
@d LOWER_TRI_COORD HEX_BOTTOM - 2 , 19
@d UPPER_TRI_COORD HEX_BOTTOM - 8 , 19
@<Hexagram Functions@>=
void draw_trigrams(hexagram hex) { @/
  static const char trigrams[][31] = @<Trigram Names@> ;
  mvprintw( LOWER_TRI_COORD , trigrams[ hex & 0x07 ] );
  mvprintw( UPPER_TRI_COORD , trigrams[ hex >> 3   ] );
@/}

@ Finally, we'll want to put the name of the hexagram below it,
as well as a summary of what it means.
Later, we'll cover reading this data in from a file so I can 
change it without recompiling the program. For now, all that
matters is the structure which holds the data.
@<Hexagram Data@>=
struct {
  char *title;
  char *desc1;
  char *desc2;
} hex_info[64];

@ Once we have that structure, we can make a simple function to
display the data:
@d TEXT_COORD(ln) HEX_BOTTOM+2+ln, 2
@d TEXT_LN(ln,txt) wmove(stdscr,TEXT_COORD(ln)); clrtoeol(); mvprintw(TEXT_COORD(ln),txt)
@<Hexagram Functions@>=
void draw_title(hexagram hex) { @/
  TEXT_LN(0,hex_info[hex].title); 
  TEXT_LN(1,hex_info[hex].desc1); 
  TEXT_LN(2,hex_info[hex].desc2); 
@/}
  
@* Hexagram History. The user, when browsing hexagrams, may want to explore
back-and-forth through the hexagrams they have called up. For instance, 
if they progressively alter lines, or go through a series of inner hexagrams,
they might want to undo and redo their changes. So, we will maintain a
circular buffer that contains the last ten hexagrams displayed, and allow the
user to go forward and back through their recent history.
@<Hexagram Data@>=
hexagram history[10] = { 63 } ; 
int hidx = 0;

@ We'll provide some helper macros to navigate the history. Note that |NEW_HEX| 
does a little error-correcting on the input hexagram, making sure it falls within
the valid range of 0 to 63. We will make use of this in the UI code to make the
error cases smoother.
@d CURRENT_HEX history[hidx]
@d HEX_BACK if(--hidx < 0) hidx = 9 
@d HEX_FORW if(++hidx > 9) hidx = 0
@d NEW_HEX(hex) HEX_FORW; CURRENT_HEX = (hex) & 63;  

@ Finally, with the history in place, we can work out how to draw
the current hexagram for the user. 
@<Display the current...@>=
hexagram cur_top = CURRENT_HEX;
draw_hex(cur_top);
draw_trigrams(cur_top);
draw_title(cur_top);
refresh();

@* User Commands. We offer the user a number of commands through
keyboard interaction. Before the program really starts, we'll put
a list of commands at the bottom of the display area.
@<Display the command list@>=
mvprintw(HEX_BOTTOM+6,2,"(n)ext/(p)rev (f)orw/(b)ack (i)nner in(v)ert (c)hange (g)oto (q)uit");

@ Here is the code that grabs a key and figures out how to respond:
@<Parse user ...@>=
int cmd = getch();
switch(cmd) {
 case 'n':
   @< Go to the next King Wen hexagram @>; 
   break;
 case 'p':  
   @< Go to the previous King Wen hexagram @>;
   break;
 case 'q':
 case 'Q': goto done; 
 case 'f': HEX_FORW; break;
 case 'b': HEX_BACK; break;
 case 'c': 
   @< Change a hex line @>;
   break; 
 case 'g':
   @< Go to a specific King Wen hexagram @>;
   break;
 case 'i':
   @< Create the inner hexagram @>;
   break;
 case 'v':
   @< Create the inverted hexagram @>;
   break;
}

@ Inverting a hexagram is just basic bit-twiddling.
@<Create the inverted hexagram@>=
NEW_HEX( ~cur_top );

@ Creating an inner hexagram is slightly more advanced bit-twiddling.
@<Create the inner...@>=
NEW_HEX( cur_top << 1 & ~7 | cur_top >> 1 & 7 );

@ To change a hex line, we'll need to be able to ask the user which line to
change. Then, changing it is a simple exclusive or operation.
@<Change a hex line@>=
NEW_HEX( cur_top  ^   1 << which_line() - 1 );

@ The |which_line| function is the first question we need to ask the 
user. We'll define a couple of macros to help us write to and clear
a question area. In the case of |which_line|, if the user types 
invalid input, we just specify a non-existent line~7, which will
be ignored by the rest of the code. 
@d QUESTION move(HEX_BOTTOM + 8, 2)
@d CLR_QUESTION QUESTION; clrtoeol()
@<UI Functions@>=
int which_line(void) {
  QUESTION;
  printw("Which line do you want to change (1 - 6)? ");
  refresh();
  int num = getch() - '0';
  if(num > 6 || num < 1)  
    num = 7;
  CLR_QUESTION;
  return num;
}

@* The King Wen Sequence. Nearly all study of the I Ching happens in the context
of the ``King Wen'' arrangement of the hexagrams. So, it is natural to expect
that a user will want to explore the hexagrams in this order.  We'll define it
in a simple array:
@<Hexagram Data@>=
hexagram king_wen[] = {
63 , 0 , 17 , 34 , 23 , 58 , 2 , 16 , 55 , 59 , 7 , 
56 , 61 , 47 , 4 , 8 , 25 , 38 , 3 , 48 , 41 , 37 , 
32 , 1 , 57 , 39 , 33 , 30 , 18 , 45 , 28 , 14 , 60 , 
15 , 40 , 5 , 53 , 43 , 20 , 10 , 35 , 49 , 31 , 62 , 
24 , 6 , 26 , 22 , 29 , 46 , 9 , 36 , 52 , 11 , 13 , 
44 , 54 , 27 , 50 , 19 , 51 , 12 , 21 , 42 };


@ When we look up where we are in the sequence, we'll cache it in a
variable called |cur_wen|.  This will save us from scanning |king_wen|
to re-orient ourselves most of the time.
@d INC_WEN if(++cur_wen > 63) cur_wen = 0
@d DEC_WEN if(--cur_wen < 0) cur_wen = 63
@<UI Data@>=
int cur_wen = 0;

@ We'll need a way to make sure our cached |cur_wen| index is current. We
do this by comparing it to the current hexagram. If they don't match, we'll
need to scan the sequence until we find the current hexagram.
@<UI Functions@>=
void orient_wen_seq_idx(void) {
  if(king_wen[cur_wen] == CURRENT_HEX) return;

  cur_wen = 0;
  while(king_wen[cur_wen] != CURRENT_HEX) ++cur_wen;
}

@ Now it is easy to move to the next hexagram in King~Wen~order:
@< Go to the next King Wen hexagram @>= 
orient_wen_seq_idx();
INC_WEN;
NEW_HEX(king_wen[cur_wen]);

@ Ditto for previous:
@< Go to the previous King Wen hexagram @>=
orient_wen_seq_idx();
DEC_WEN;
NEW_HEX(king_wen[cur_wen]);

@ To go to a specific King~Wen~hexagram, we just need to look it up:
@<Go to a specific King Wen hexagram @>=
cur_wen = which_hex()&63;
NEW_HEX(king_wen[cur_wen]);

@ The more challenging part of the previous code is hidden in the |which_hex| function,
because of the user interaction.
@<UI Functions@>=
int which_hex(void) {
  QUESTION;
  printw("Which hexagram to visit? ( 1 - 64 )? ");
  refresh();
  int digit1 = getch();
  int digit2 = getch();
  int ans = digit1 - '0';
  if(digit2 != '\n') ans = ans*10 + digit2 - '0';
  CLR_QUESTION;
  return ans - 1 & 63;
}

@* I Ching Text Data. There are a couple sources of text data left to 
define.  The names of the trigrams are very static, and not very large,
so I've just included them in the code.
@<Trigram Names@>=
{
"K'UN / RECEPTIVE / EARTH      ", @/
"CHEN / AROUSING  / THUNDER    ", @/
"K'AN / ABYSMAL   / WATER      ", @/
"TUI  / JOYOUS    / LAKE       ", @/
"KEN / KEEPING STILL / MOUNTAIN", @/
"LI   / CLINGING  / FIRE       ", @/
"SUN  / GENTILE   / WIND       ", @/
"CH'IEN / CREATIVE / HEAVEN    " 
}

@ The other major source of text data is the file of hexagram names 
and descriptions. The format of the file is very simple... three
lines per hexagram mapping directly to |title, desc1, desc2| in
|hex_info|.  The descriptions are in ``King~Wen'' order, so I need to
map them to hexagrams as I read them in.
@<Read in the hexagram descriptions@>=
FILE *desc_file = fopen("iching.txt","r");
@<Complain if the description file isn't found@>;

int cur;
size_t str_sz;
for(int counter = 0; counter < 64; ++counter) {
  cur = king_wen[counter];

  @<Read a string-triple from the file@>
}


fclose(desc_file);

@ I allocate 31~bytes for the title, and 71~bytes for 
each description line. If the file lines are longer,
then |getline()| is guaranteed to allocate enough
memory. But, these should be good guesses. 
@d READ_DESC(which, sz)
   str_sz = sz;
   which = malloc(sz + 1);
   getline(&(which),&str_sz,desc_file)
@<Read a string-triple from the file@>=
  READ_DESC(hex_info[cur].title,31);
  READ_DESC(hex_info[cur].desc1,71);
  READ_DESC(hex_info[cur].desc2,71);

@ @<Complain if the description file isn't found@>=
if(!desc_file) { 
  mvprintw( 0, 0 , "Cannot find iching.txt!");
  refresh();
  getch();
  goto done;
} 

@* Index.

