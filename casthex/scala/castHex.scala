package rwt.casthex

import java.util.Random

// casting methods
object Methods {
  private val rnd = new Random()

  def generateFrom( method : => Char ) = (1 to 6).map { _ => method }.mkString

  def coins = ('6' + rnd.nextInt(2) + rnd.nextInt(2) + rnd.nextInt(2)).toChar 
  def random = ('7' + rnd.nextInt(2)).toChar 
  def stalks = rnd.nextInt(16) match {
                    case 0            => '6'
                    case x if x <= 5  => '7'
                    case x if x <= 12 => '8'
                    case _            => '9'
               } 
}

object CastHex {

  private def decode(casting: String) : Tuple3[Int,Int,Vector[String]] = 
     casting.foldRight( (0,0,Vector[String]()) ) { (ch, ans) =>
        ch match {
           case '6' => (ans._1*2,   ans._2*2+1, ans._3 :+ "  ---   ---   -->  ---------")
           case '7' => (ans._1*2+1, ans._2*2+1, ans._3 :+ "  ---------        ---------")
           case '8' => (ans._1*2,   ans._2*2,   ans._3 :+ "  ---   ---        ---   ---")
           case '9' => (ans._1*2+1, ans._2*2,   ans._3 :+ "  ---------   -->  ---   ---")
        } 
     }

  private def display(casting: String) : Unit = {
     println(s"Casting: $casting\n")

     val (wen1, wen2, lines) = decode(casting)
     val changes = wen1 != wen2

     lines.map( if(changes) identity else (_.take(11)) ).foreach(println)

     println(s"\n${hexName(wen1)}")
     if(changes) {
        println(s" - Changing To -->\n${hexName(wen2)}")
     }
     println()
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
       case "-coins" => Methods.generateFrom(Methods.coins)
       case "-stalks" => Methods.generateFrom(Methods.stalks)
       case "-static" => Methods.generateFrom(Methods.random)
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
