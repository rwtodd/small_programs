# Ascii Pic

A simple image-to-ascii converter in scala. 
I had run across a [clojure version](http://www.bestinclass.dk/blog/tribute-steve-ballmer) 
and wanted to see how a scala implementation would compare.  NB: I used a more sophisticated 
brightness formula and I did not implement the HTML output option, so the two are not 100% equivalent.


I have since made a Go version, [also on github](https://github.com/rwtodd/misc-go/tree/master/asciipic).

Here, I convert a picture of a soccer ball:
 
~~~~~~
$ sbt
> run soccer.jpg 40
[info] Running rwt.asciipic.AsciiPic soccer.jpg 40
              :=+$+=+$%$=,              
           ,++:.  ,%%%$%%AA+,           
         .$=.      %$$$$%@A@%+,         
        *#         %%%%%%@@A#$==        
       @#,         +@@@@@AA##A@ %.      
     ,##,          =AAAAAA###A#%.%.     
    .##,          .  ,+AA######.  $,    
   .A#:.         .       :@#AA,    +    
   % .          .          .:       +   
  *. .         .            ,       .:  
 .$  .                               +  
 =    ,:=$%@@#               .        = 
 +   ,%%$$%@@A,              ,        % 
*.   %$$$$%%@A$              .        =:
+   .@%$$$%%@@#                       :+
+   *@%$$$%%@A#,                      :@
:   %@%%$$%%@AA=             =%,      :A
.   A@%%%%%@@@A@            +%%A$     *#
   .A@@%%%%@@AA#.         .$%%%%@A.   *#
   .A@@@@@@@AAA+ ...      %$$$%%@AA. , %
   .@AA@@@@@@A@      .   %%$$$$%@@AA,  .
  . .A@@@@@AA%.        .+A%%$$$%@@A#.   
.,   .AAAAA#=           A@@%%%%%@@A#    
%*    .@AA%             $A@@%%%%@@A@   ,
A=      %,              *AA@@@@@@@A=   :
$=                      ,#AA@@@@@A#.   *
*+       .              .###AAAAAA@    :
 +       .               ####AAAA#,   * 
 =       .               %########    * 
 .+.      .              *######=, . =. 
  *.      .              .       .   :  
   %                    .           +   
    %      =,          .           =.   
    .$,   :$%%$:     ..          ,A.    
     ,$. +$+$@@AA%* .           +A.     
      .%.@$%%%@A###A          :A@.      
        ++%%%@@A##A#,        +#+        
         ,%A@@A#####$       $%,         
           ,$A#######.    :+,           
              :+%@%+=++=:..             

~~~~~~
