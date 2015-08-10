\ vim: set filetype=forth :
\ Atbash cipher
\ Reverse the order of the alphabet and re-write a string.

: alpha? ( ch -- f ) [char] A [char] Z 1+ within ;
: a->z ( ch -- ch' ) dup alpha? 
    IF [char] A -  [char] Z swap - THEN ;

: getline ( -- caddr u ) pad 256 accept pad swap ;
: main ( -- ) getline bounds 
   cr ?DO I c@ toupper a->z emit LOOP cr
   recurse ;

main
