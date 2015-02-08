# FAT FS Disk Image Parser

This is a _very_ incomplete disk image parser for FAT16 disk images, written in Prolog.
I had some floppy disk images I wanted to inspect, and this code got me far
enough to see what I needed to see.  It `assert`s a few facts about the
header area of the FAT filesystem.  Most importantly, it told me where on the 
disk I could find the main directory tree. 

For my purposes, I then went to the directory location with a hex editor and looked
at the filenames.  Since none of the files I wanted were on the disk images I had,
I didn't need to develop this program any further. 

But, it will live on forever in GitHub in case I ever _do_ need to complete it.

I used SWI-Prolog 6.4.1 for this program.


