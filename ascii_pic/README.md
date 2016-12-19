# Ascii Pic

A simple image-to-ascii converter in scala and .NET CORE. 
I had run across a [clojure version](http://www.bestinclass.dk/blog/tribute-steve-ballmer) 
and wanted to see how a scala implementation would compare.  NB: I used a more sophisticated 
brightness formula and I did not implement the HTML output option, so the two are not 100% equivalent.

I have also made a Go version, [also on github](https://github.com/rwtodd/misc-go/tree/master/asciipic).

Here, I convert a picture of a soccer ball:
 
~~~~~~
> j:\asciipic.bat
Usage: asciipic [-help] [-w width] [-ar aspect-ratio] fname
   default width: 72
   default aspect ratio of text (w/h): 1.5


> j:\asciipic.bat -ar 2 soccer.jpg

                             ..,,:,,.......
                       ..,:::::,,..      ....,,,,.
                   .,,,:==.                     ,*:,..
                .,*=$@####$.                      .,,.,,.
             .*=%A##########*                       .,  .,,
           ,$@###############$.                    .:$@:   ,,
         ,,+################@=.....,,,,...      ,=@#####@*   ,,
       .:.*###############@*             ..,,,=@##########A=  .:.
      ,, .##############@*                   .A#############A*  ,,
     ,,  +############@*                      ,###############@, ,,
    :,   %##########A*                         =################+.,:
   ,,  ,,..,,,::::*+,                           %###############%..*,
   :  :.           ,.                           .A##############:   :.
  : .:             .:                            ,#############@     :
  : :               :                            .=+%@A########*     :.
 ,,:                .:                          ,,     .,*=+%@%      .,
 :*.                 ,,                       ,,              ,.      :
 =A                   ,,                    ,,.                *      :
 =#,                   .*..               ,,.                  ,.     :
 :#$                   =###A@@%$+=*:,.. ,,.                    .:    .:
  A#,                .%################A,                       *    :.
  +#A.              ,A##################$                       :    :
  .A#@.            ,A###################A.                      *,  :.
   *##@,.         ,A#####################:                     *A#@+:
    +#A..,,,.....,@######################+                   .%####$
     +#,          ,%#####################%                  *A####+
      =+            =A###################A.               :@#####=
       ,:            .$#################%=,,...         :%#####@:
         ,,            ,$##########A%+:.      ........*A######+
          .,.            .+@#A@$=:.                   =#####+.
            .,,            .*                         A##A=.
               ,,.         .:                        =A$:
                  .,,.      :                     .,,,
                     ..,:*=+%:.             ...,,..
                          .,,:::,,,,,,,,.....


~~~~~~
