package rwt.casthex

import java.util.Random

// casting methods
object Methods {
  private val rnd = new Random()

  def coins() : String = 
       (1 to 6).map { _ => ('6' + rnd.nextInt(2) + rnd.nextInt(2) + rnd.nextInt(2)).toChar }
               .mkString
 
  def random() : String =
       (1 to 6).map { _ => ('7' + rnd.nextInt(2)).toChar }
               .mkString

  def stalks() : String = 
       (1 to 6).map { _ => rnd.nextInt(16) match {
                              case 0            => '6'
                              case x if x <= 5  => '7'
                              case x if x <= 12 => '8'
                              case _            => '9'
                           } 
                    }. mkString
}

object CastHex {

  private val LINES = Array("\u2584\u2584   \u2584\u2584", "\u2584\u2584\u2584\u2584\u2584\u2584\u2584")

  private def display(casting: String) : Unit = {
     val sixOrNine = (ch:Char) => ch == '6' || ch == '9'
     val changes = casting.exists(sixOrNine)
     System.out.println(s"Casting: $casting")
     var wen1 = 0
     var wen2 = 0
     val format = if(changes) "  %s   %s   %s\n" else "   %s\n" 
     casting.reverse.foreach { ch =>
         val idx1 = ch & 1    
         val idx2 = if(sixOrNine(ch)) (1 - idx1) else idx1  
         wen1 = (wen1 << 1) | idx1
         wen2 = (wen2 << 1) | idx2
         System.out.printf(format,LINES(idx1), 
                                  if(idx1==idx2) "   " else "-->",   
                                  LINES(idx2))
     }
     System.out.printf("\n%s\n", hexName(wen1))
     if(changes) {
        System.out.printf(" - Changing To -->\n%s\n", hexName(wen2))
     }
  }

  private def usage() : Unit = {
     System.err.println("Usage: casthex (-coins|-stalks|-static|<casting>)")
     System.err.println("  -coins   3-coins method\n  -stalks  yarrow stalks method")
     System.err.println("  -static  a random hexagram")
     System.err.println("  <casting> 6 digits from the set {6,7,8,9}")
     System.exit(1)
  }

  def main(args : Array[String]) : Unit = {
     val arg = if(args.length == 0) "-coins" else args(0)

     val casting = arg match {
       case "-coins" => Methods.coins()
       case "-stalks" => Methods.stalks()
       case "-static" => Methods.random()
       case given if given.length == 6 && given.forall( ch => ch >= '6' && ch <= '9' ) => given
       case _ => usage() ; ""
     }
     display(casting)
  }

  private val hexName = Array[String](
    "02. K'un -- Earth",
    "24. Fu -- Return",
    "07. Shih -- The Army",
    "19. Lin -- Overseeing",
    "15. Ch'ien -- Humility",
    "36. Ming I -- Concealment of Illumination",
    "46. Sheng -- Rising",
    "11. T'ai -- Tranquility",
    "16. Yu -- Enthusiasm",
    "51. Chen -- Thunder",
    "40. Hsieh -- Liberation",
    "54. Kuei Mei -- Making a Young Girl Marry",
    "62. Hsiao Kuo -- Predominance of the Small",
    "55. Feng -- Richness",
    "32. Heng -- Constancy",
    "34. Ta Chuang -- Great Power",
    "08. Pi -- Accord",
    "03. Chun -- Difficulty",
    "29. K'an -- Mastering Pitfalls",
    "60. Chieh -- Discipline",
    "39. Chien -- Halting",
    "63. Chi Chi -- Settled",
    "48. Ching -- The Well",
    "05. Hsu  -- Waiting",
    "45. Ts'ui -- Gathering",
    "17. Sui -- Following",
    "47. K'un -- Exhaustion",
    "58. Tui -- Joy",
    "31. Hsien -- Sensitivity",
    "49. Ko -- Revolution",
    "28. Ta Kuo -- Excess of the Great",
    "43. Kuai -- Parting",
    "23. Po -- Stripping Away",
    "27. I -- Lower Jaw (Nourishment)",
    "04. Meng -- Darkness",
    "41. Sun -- Reduction",
    "52. Ken -- Mountain",
    "22. Pi -- Adornment",
    "18. Ku -- Degeneration",
    "26. Ta Ch'u -- Nurturance of the Great",
    "35. Chin -- Advance",
    "21. Shih Ho -- Biting Through",
    "64. Wei Chi -- Unsettled",
    "38. K'uei -- Disharmony",
    "56. Lu -- Travel",
    "30. Li -- Fire",
    "50. Ting -- The Cauldron",
    "14. Ta Yu -- Great Possession",
    "20. Kuan -- Observation",
    "42. I -- Increase",
    "59. Huan -- Dispersal",
    "61. Chung Fu -- Faithfulness in the Center",
    "53. Chien -- Gradual Progress",
    "37. Chia Jen -- People in the Home",
    "57. Sun -- Wind",
    "09. Hsiao Ch'u -- Nurturance by the Small",
    "12. Pi -- Obstruction",
    "25. Wu Wang -- Fidelity (No Error)",
    "06. Sung -- Contention",
    "10. Lu -- Treading",
    "33. Tun -- Withdrawal",
    "13. T'ung Je^n -- Sameness with People",
    "44. Kou -- Meeting",
    "01. Chi'en -- Heaven" 
  ) 
}