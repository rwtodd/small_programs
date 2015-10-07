# Atbash Cipher

I was interested after seeing on reddit that
if you apply the [atbash cipher][1] to the
word "wizard" you get "draziw" (which is
the same word backwards).  So, here is
a little forth program that applies the
cipher to all input text.

    echo "wizard" | gforth atbash.forth
	wizard
	DRAZIW

or
    cat file.txt | gforth atbash.forth
	...

_Edit 2015-05-08:_ Added a scala version which
performs atbash on its first argument.

_Edit 2015-06-09:_ Added a c++ version.

_Edit 2015-08-10:_ Added an F# (fsharp) version.
And, since fsharp like the .fs extension, I changed
the forth versions to .forth.

_Edit 2015-10-07:_ Added a haskell version.

[1]: http://en.wikipedia.org/wiki/Atbash

