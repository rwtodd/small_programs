\ vim: set filetype=forth :
\ text to accompany the hexagrams...  the main program is iching.fs

: array CREATE cells allot DOES> swap cells + ;

8 array (trigrams) 
: init(trigrams)
  c" K'UN / RECEPTIVE / EARTH      " 0 (trigrams) !
  c" CHEN / AROUSING / THUNDER     " 1 (trigrams) ! 
  c" K'AN / ABYSMAL / WATER        " 2 (trigrams) !  
  c" TUI / JOYOUS / LAKE           " 3 (trigrams) ! 
  c" KEN / KEEPING STILL / MOUNTAIN" 4 (trigrams) ! 
  c" LI / CLINGING / FIRE          " 5 (trigrams) ! 
  c" SUN / GENTLE / WIND           " 6 (trigrams) ! 
  c" CH'IEN / CREATIVE / HEAVEN    " 7 (trigrams) ! 
  ;

64 array (hexagrams) 
128 array (descriptions)

\ here's some helpers to make defining the hexagrams easy!
: title-" POSTPONE 1- POSTPONE c" 
          POSTPONE over POSTPONE (hexagrams) POSTPONE ! ; immediate
: line1-" POSTPONE 2* POSTPONE (descriptions) POSTPONE c" 
          POSTPONE over POSTPONE ! ; immediate
: line2-" POSTPONE c" POSTPONE swap 
          POSTPONE cell POSTPONE + POSTPONE ! ; immediate

: init(descriptions)
01
   title-" Chi\'en -- Heaven"
   line1-" One of the four timeless hexagrams; pure yang, it"
   line2-" represents strength, firmness, primal unified energy."

02
   title-" K\'un -- Earth"
   line1-" One of the four timeless hexagrams; pure yin; represents"
   line2-" receptivity, humility, obedience, yielding, flexibility, essence."

03
   title-" Chun -- Difficulty" 
   line1-" Beginning to foster strength of primal energy, the difficult"
   line2-" process of breaking out of acquired temporal conditioning."

04
   title-" Meng -- Darkness" 
   line1-" Starting yin-convergence, making unruly yin submit, integrating"
   line2-" with the world yet preventing mundanity from taking over. Innocence."

05
   title-" Hsu  -- Waiting" 
   line1-" Waiting for the proper timing in gathering primal energy."
   line2-" Gradually restoring the primordial in the midst of the temporal."

06
   title-" Sung -- Contention" 
   line1-" Depening on strength because of danger, causing danger by depending"
   line2-" on strength. Conflict due to imbalance."

07
   title-" Shih -- The Army" 
   line1-" Using the primordial to repel conditioning, using the real"
   line2-" to get rid of the false."

08
   title-" Pi -- Accord" 
   line1-" Learning through association; obedience in danger, rectification of"
   line2-" imbalance."

09
   title-" Hsiao Ch\'u -- Nurturance by the Small" 
   line1-" Nurturing inner strength by outward submissiveness, being firm but"
   line2-" not impetuous, growing through humility."

10
   title-" Lu -- Treading" 
   line1-" Progress; advance of the positive, prevention of danger by caution,"
   line2-" empowerment by calmness and trust."

11
   title-" T\'ai -- Tranquility" 
   line1-" Harmony of strength and flexibility." 
   line2-" "

12
   title-" Pi -- Obstruction" 
   line1-" Submitting inwardly to personal desire, acting aggressive" 
   line2-" outwardly."

13
   title-" T\'ung Je^n -- Sameness with People" 
   line1-" Mixing in with the ordinary world, concealing illumination,"
   line2-" assimilating to others."

14
   title-" Ta Yu -- Great Possession" 
   line1-" Mutual enhancement of strength and lucidity; continuous renewal,"
   line2-" inward discipline."

15
   title-" Ch\'ien -- Humility" 
   line1-" Inwardly firm, outwardly flexible, having personal attainment"
   line2-" but not dwelling on it."

16
   title-" Yu -- Enthusiasm"  
   line1-" Proper timing in action." 
   line2-" "

17
   title-" Sui -- Following"  
   line1-" According to conditions, going along with human desire to"
   line2-" gradually introduce guidance."

18
   title-" Ku -- Degeneration" 
   line1-" Correcting degeneration by abandoning the false and returning"
   line2-" to the true."

19
   title-" Lin -- Overseeing" 
   line1-" Keeping watch over the restoration and growth of primal energy"
   line2-" and repulsioin of acquired energy of conditioning."

20
   title-" Kuan -- Observation" 
   line1-" Alertness, gradually progressing through receptivity to the"
   line2-" requirements of the time, inner vigilance."

21
   title-" Shih Ho -- Biting Through." 
   line1-" Investigating things and finding out principles." 
   line2-" "

22
   title-" Pi -- Adornment" 
   line1-" Private cultivation, without ostentation, mutual complementarity"
   line2-" of clarity and stillness."

23
   title-" Po -- Stripping Away" 
   line1-" Submission to desire, acquired mundanity dissolving away the"
   line2-" celestial energy."

24
   title-" Fu -- Return" 
   line1-" Return of consciousness of reality, activity obeying the mind." 
   line2-" "

25
   title-" Wu Wang -- Fidelity (No Error)" 
   line1-" Vigorous advancement of celestial energy, attention to reality." 
   line2-" "

26
   title-" Ta Ch\'u -- Nurturance of the Great" 
   line1-" Being able to be still when strong; practicing nondoing to"
   line2-" nurture encipient enlightenment."

27
   title-" I -- Lower Jaw (Nourishment)" 
   line1-" Discernment of good, becoming empty to seek fulfillment." 
   line2-" "

28
   title-" Ta Kuo -- Excess of the Great" 
   line1-" Following desires, delighting in externals, inability to"
   line2-" control strength."

29
   title-" K\'an -- Mastering Pitfalls" 
   line1-" One of the four timeless hexagrams; restoring the celestial"
   line2-" within the mundane."

30
   title-" Li -- Fire" 
   line1-" One of the four timeless hexagrams; illumination with inner" 
   line2-" openness."

31
   title-" Hsien -- Sensitivity"
   line1-" Harmonization of the celestial and the earthly." 
   line2-" "

32
   title-" Heng -- Constancy"
   line1-" Singleminded application, perseverance in real practice." 
   line2-" "

33
   title-" Tun -- Withdrawal"
   line1-" Storing positive energy, subduing energy, exercising strength"
   line2-" with restraint, not using power arbitrarily."

34
   title-" Ta Chuang -- Great Power"
   line1-" Promoting vigorous positive energy; ability to act or not act,"
   line2-" at will; transcendence of ordinary capacities."

35
   title-" Chin -- Advance"
   line1-" Clearminded sincerity advancing illumination." 
   line2-" "

36
   title-" Ming I -- Concealment of Illumination"
   line1-" Withdrawing effort after illumination, not using illumination"
   line2-" lightly; being enlightened yet conforming to the times."

37
   title-" Chia Jen -- People in the Home"
   line1-" Governing th einner: refining the self, mastering the mind,"
   line2-" turning the attention inward."

38
   title-" K\'uei -- Disharmony"
   line1-" Focus on externals; requires inner emptiness to restore harmony." 
   line2-" "

39
   title-" Chien -- Halting"
   line1-" Preserving the primordial in the midst of the temporal." 
   line2-" "

40
   title-" Hsieh -- Liberation"
   line1-" Taking advantage of the appropriate time to liberate positive"
   line2-" primordial energy from influences of negative, conditioned energy."

41
   title-" Sun -- Reduction"
   line1-" Removing acquired conditioning, stilling mundane attraction." 
   line2-" "

42
   title-" I -- Increase"
   line1-" Entering the Dao gradually, without either rushing or lagging,"
   line2-" increasing the positive while decreasing the negative."

43
   title-" Kuai -- Parting"
   line1-" Detachment from discriminating consciousness, repelling the"
   line2-" energy of external influences."

44
   title-" Kou -- Meeting"
   line1-" Warding off negative influences, preserving the positive." 
   line2-" "

45
   title-" Ts\'ui -- Gathering"
   line1-" Unifying vitality, energy, and spirit." 
   line2-" "

46
   title-" Sheng -- Rising"
   line1-" Climbing from lowliness to the heights; carefully watching over"
   line2-" the development process."

47
   title-" K\'un -- Exhaustion"
   line1-" Refining body and mind." 
   line2-" "

48
   title-" Ching -- The Well"
   line1-" Accumulation of effort to cultivate character; nurturance of" 
   line2-" others"

49
   title-" Ko -- Revolution"
   line1-" Refining personal desires to become unselfish." 
   line2-" "

50
   title-" Ting -- The Cauldron"
   line1-" Refining illumination." 
   line2-" "

51
   title-" Chen -- Thunder"
   line1-" Practicing introspection in action." 
   line2-" "

52
   title-" Ken -- Mountain"
   line1-" Nurturing energy by quietude." 
   line2-" "

53
   title-" Chien -- Gradual Progress"
   line1-" Gradual practice following appropriate order." 
   line2-" "

54
   title-" Kuei Mei -- Making a Young Girl Marry"
   line1-" Seeing the real within the false." 
   line2-" "

55
   title-" Feng -- Richness"
   line1-" Balance of understanding and action, preventing danger by awareness." 
   line2-" "

56
   title-" Lu -- Travel"
   line1-" Stabilizing and nurturing illumination, transcending the world." 
   line2-" "

57
   title-" Sun -- Wind"
   line1-" Progress by flexible obedience." 
   line2-" "

58
   title-" Tui -- Joy"
   line1-" Communion of the inward and the outward; joy in practicing the Dao." 
   line2-" "

59
   title-" Huan -- Dispersal"
   line1-" Confusion of yin and yang followed by reordering." 
   line2-" "

60
   title-" Chieh -- Discipline"
   line1-" Being able to be joyful even in trouble; practicing obedience in"
   line2-" trying circumstances, adaptably keeping to the Dao."

61
   title-" Chung Fu -- Faithfulness in the Center"
   line1-" Avoidance of both obsession and negligence, seeking fulfillment by"
   line2-" becoming empty."

62
   title-" Hsiao Kuo -- Predominance of the Small"
   line1-" Being fulfilled but acting empty; inwardly strong, outwardly"
   line2-" yielding; inwardly firm, outwardly flexible."

63
   title-" Chi Chi -- Settled"
   line1-" Mutual completion of yin and yang; forestalling danger to stabilize"
   line2-" attainment."

64
   title-" Wei Chi -- Unsettled"
   line1-" Refining the self, repelling mundanity, waiting for the time to"
   line2-" restore the primordial celestial positivity."
   ;

