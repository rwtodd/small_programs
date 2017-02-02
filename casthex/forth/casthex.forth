\ ---------------------------- \
 \  C A S T  H E X              \
  \  Cast an I-Ching hexagram    \
   \  (c) 2017 Richard Todd       \
    \ ---------------------------- \


\ ********** random.fs from gforth distribution *****************
Variable seed
$10450405 Constant generator
: rnd  ( -- n )  seed @ generator um* drop 1+ dup seed ! ;
: random ( n -- 0..n-1 )  rnd um* nip ;
\ ***************************************************************


\ ********** record the names of hexagrams **********************
: stored" POSTPONE c" POSTPONE over POSTPONE ! POSTPONE cell+ ;
  IMMEDIATE
CREATE hexnames 64 cells allot
: init(hexnames)
  hexnames
    stored" 02. K'un -- Earth"
    stored" 24. Fu -- Return"
    stored" 07. Shih -- The Army"
    stored" 19. Lin -- Overseeing"
    stored" 15. Ch'ien -- Humility"
    stored" 36. Ming I -- Concealment of Illumination"
    stored" 46. Sheng -- Rising"
    stored" 11. T'ai -- Tranquility"
    stored" 16. Yu -- Enthusiasm"
    stored" 51. Chen -- Thunder"
    stored" 40. Hsieh -- Liberation"
    stored" 54. Kuei Mei -- Making a Young Girl Marry"
    stored" 62. Hsiao Kuo -- Predominance of the Small"
    stored" 55. Feng -- Richness"
    stored" 32. Heng -- Constancy"
    stored" 34. Ta Chuang -- Great Power"
    stored" 08. Pi -- Accord"
    stored" 03. Chun -- Difficulty"
    stored" 29. K'an -- Mastering Pitfalls"
    stored" 60. Chieh -- Discipline"
    stored" 39. Chien -- Halting"
    stored" 63. Chi Chi -- Settled"
    stored" 48. Ching -- The Well"
    stored" 05. Hsu  -- Waiting"
    stored" 45. Ts'ui -- Gathering"
    stored" 17. Sui -- Following"
    stored" 47. K'un -- Exhaustion"
    stored" 58. Tui -- Joy"
    stored" 31. Hsien -- Sensitivity"
    stored" 49. Ko -- Revolution"
    stored" 28. Ta Kuo -- Excess of the Great"
    stored" 43. Kuai -- Parting"
    stored" 23. Po -- Stripping Away"
    stored" 27. I -- Lower Jaw (Nourishment)"
    stored" 04. Meng -- Darkness"
    stored" 41. Sun -- Reduction"
    stored" 52. Ken -- Mountain"
    stored" 22. Pi -- Adornment"
    stored" 18. Ku -- Degeneration"
    stored" 26. Ta Ch'u -- Nurturance of the Great"
    stored" 35. Chin -- Advance"
    stored" 21. Shih Ho -- Biting Through"
    stored" 64. Wei Chi -- Unsettled"
    stored" 38. K'uei -- Disharmony"
    stored" 56. Lu -- Travel"
    stored" 30. Li -- Fire"
    stored" 50. Ting -- The Cauldron"
    stored" 14. Ta Yu -- Great Possession"
    stored" 20. Kuan -- Observation"
    stored" 42. I -- Increase"
    stored" 59. Huan -- Dispersal"
    stored" 61. Chung Fu -- Faithfulness in the Center"
    stored" 53. Chien -- Gradual Progress"
    stored" 37. Chia Jen -- People in the Home"
    stored" 57. Sun -- Wind"
    stored" 09. Hsiao Ch'u -- Nurturance by the Small"
    stored" 12. Pi -- Obstruction"
    stored" 25. Wu Wang -- Fidelity (No Error)"
    stored" 06. Sung -- Contention"
    stored" 10. Lu -- Treading"
    stored" 33. Tun -- Withdrawal"
    stored" 13. T'ung Je^n -- Sameness with People"
    stored" 44. Kou -- Meeting"
    stored" 01. Chi'en -- Heaven"
  drop ;
\ ***************************************************************


\ ********** Casting methods ************************************
CREATE casting 6 chars allot
: cast ( f -- )
  6 0 DO
    dup execute    casting I chars + c!
  LOOP drop ;

: coins ( -- char ) [CHAR] 6  2 random 2 random 2 random + + + ;
: static ( -- char ) [CHAR] 7 2 random + ;
: stalks ( -- char ) 16 random   dup 1 and 0=
    IF ( even ) 0 > 2 and    ELSE ( odd ) 7 < 2 and 1+  THEN
    [CHAR] 6 + ;
\ ***************************************************************


\ ********** beginning of actual program ************************
VARIABLE wen1   VARIABLE wen2   ( the wen numbers of the hex )
CREATE reps 6 cells allot       ( string reps for lines )

s" Malformed Casting!" exception VALUE !BAD-CASTING!

: changed? wen1 @ wen2 @ <> ;

: mem2* dup @ 2 * swap ! ;
: mem1+ dup @ 1+  swap ! ;
: decode ( -- )
  0 wen1 !  0 wen2 !
  0 5 DO
    wen1 mem2*   wen2 mem2*
    casting I chars + c@
    CASE
    [CHAR] 6 OF wen2 mem1+  s"  ---   ---  =>  ---------"  ENDOF
    [CHAR] 7 OF wen1 mem1+  wen2 mem1+
                            s"  ---------      ---------"  ENDOF
    [CHAR] 8 OF             s"  ---   ---      ---   ---"  ENDOF
    [CHAR] 9 OF wen1 mem1+  s"  ---------  =>  ---   ---"  ENDOF
    !BAD-CASTING! throw
    ENDCASE
    drop reps I cells + !
  -1 +LOOP ;

: .wen ( n -- ) cells hexnames + @   count type cr ;
: .casting ( -- )
  ." Casting: "    casting 6 type   cr cr
  changed? 
  0 5 DO
    reps I cells + @   over 14 and 11 +   type cr
  -1 +LOOP  cr
  wen1 @ .wen  IF ."  = Changing To =>" cr wen2 @ .wen THEN
  cr ;
  
: generate ( -- )
  ( use "-coins" if there was no command line arg )
  next-arg 2dup 0 0 d= IF  2drop s" -coins" THEN
  ( parse the argument... )
       2dup s" -coins"  compare 0= IF ['] coins cast   2drop
  ELSE 2dup s" -static" compare 0= IF ['] static cast  2drop
  ELSE 2dup s" -stalks" compare 0= IF ['] stalks cast  2drop  
  ELSE drop casting 6 cmove
  THEN THEN THEN ;

utime xor seed !
init(hexnames)
cr cr
generate  decode .casting
bye
