# Atbash Cipher

I was interested after seeing on reddit that
if you apply the [atbash cipher][1] to the
word "wizard" you get "draziw" (which is
the same word backwards).  So, here is
a little forth program that applies the
cipher to all input text.

    echo "wizard" | gforth atbash.fs
	wizard
	DRAZIW

or
    cat file.txt | gforth atbash.fs
	...

_Edit 2015-05-08:_ Added a scala version which
performs atbash on its first argument.

[1]: http://en.wikipedia.org/wiki/Atbash

