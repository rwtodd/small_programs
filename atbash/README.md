# Atbash Cipher

I was interested after seeing on reddit that
if you apply the [atbash cipher][1] to the
word "wizard" you get "draziw" (which is
the same word backwards).  So, here is
a little forth program that applies the
cipher to all input text.

    cat file.txt | gforth atbash.fs

[1]: http://en.wikipedia.org/wiki/Atbash

