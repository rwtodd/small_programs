% -----------------------------------------------------------------------------
@* Saved Game Fixup. I don't like keeping track of food and stuff like that
when playing a game. So this utility keeps me flush for supplies!
Since adjusting a saved game is risky business, we'll call the program
through a wrapper that puts us in the proper directory and makes a 
backup first. This way, the source below just opens the file and makes
changes directly without worrying.
@c
#include<stdio.h>

int main(int argc, char **argv) {
  FILE *saved_game;
  saved_game = fopen("SAVED.GAM","r+");
  @<Adjust health@>;
  @<Max out the basic items@>;
  @<Max out skull keys@>;
  @<Max out spells and potions@>;
  fclose(saved_game);
  return 0; /* we never fail! */
}

@ A character's maximum~hp is at offset |0x12| in each character record. 
The records are |0x20|~bytes each, starting at offset |0x02| in the file.
So, to update everyone's health to the maximum, I simply go read their
max, and then write their current hp to match.
@d NUM_RECORDS 16
@d CHAR_RECORD(x) 0x02 + (x)*0x20
@d CUR_HP_OFFSET 0x10
@d MAX_HP_OFFSET 0x12
@<Adjust health@>=
for(int i = 0; i < NUM_RECORDS; ++i) {
  fseek(saved_game, CHAR_RECORD(i)+MAX_HP_OFFSET, SEEK_SET);
  int lowbyte = fgetc(saved_game);
  int highbyte = fgetc(saved_game);
  fseek(saved_game, CHAR_RECORD(i)+CUR_HP_OFFSET, SEEK_SET);
  fputc(lowbyte,saved_game);
  fputc(highbyte,saved_game);
  @<Make the character `G'ood@>;
} 

@ {\sl Ultima} tracks a dispostion for each character in an |ASCII| letter.
For example, `G' for ``Good,'', `P', for ``Poisoned,'' etc. We'll set
each character to ``Good.''
@d DISPOSITION_OFFSET 0x0B
@<Make the character `G'ood@>=
fseek(saved_game, CHAR_RECORD(i)+DISPOSITION_OFFSET, SEEK_SET);
fputc('G',saved_game);

@ The basic supplies of the game are food, gold, keys, gems, and torches. 
The first two max out at 9999 (|0x270F|), while the others stop at 
99 (|0x63|).
@d write_99(cmt) fputc(0x63,saved_game)
@d write_9999(cmt) fputc(0x0f,saved_game); fputc(0x27, saved_game)
@d BASIC_ITEM_OFFSET 0x202
@<Max out the bas...@>=
fseek(saved_game,BASIC_ITEM_OFFSET,SEEK_SET);
write_9999(food);
write_9999(gold);
write_99(keys);
write_99(gems);
write_99(torches);

@ You can never have too many skull keys! 
@d SKULL_KEY_OFFSET 0x20b
@<Max out skull...@>=
fseek(saved_game,SKULL_KEY_OFFSET,SEEK_SET);
write_99(skull_keys);

@ It will be nice to know we already have all the spells and
potions available to us.
@d SPELL_OFFSET 0x24A
@d SPELL_COUNT 64
@<Max out spells...@>=
fseek(saved_game,SPELL_OFFSET,SEEK_SET);
for(int i = 0; i < SPELL_COUNT; ++i) {
  write_99(spells and potions);
}

@* Shell Script Wrapper. Especially during development of this
tool, I didn't want to accidentally obliterate my saved game. So,
I call it from the following simple shell~script, which
makes a backup copy of the file before altering it.

{ \tt\obeylines\obeyspaces
\Y\B\4\X:\.{u5fix.sh}\X${}\E{}$\6!\#/bin/bash
cd /cygdrive/c/GOG\\ Games/Ultima456/Ultima\\ 5
cp SAVED.GAM SAVED.BAK
/usr/local/bin/u5sav
}

@* Index.

