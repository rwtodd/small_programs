//
//  main.swift
//  CastHex
//
//  Created by Richard Todd on 2/9/17.
//  Copyright © 2017 Richard Todd. All rights reserved.
//

import Foundation

let hexNames = [
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
]

/// Generates a casting from a per-line `method` closure.
func casting(method: () -> Int) -> [Character] {
    var ans = Array<Character>(repeating: "6", count: 6)
    for i in 0..<6 {
        let value = String(method())
        ans[i] = value[value.startIndex]
    }
    return ans
}

func nonMoving() -> Int { return 7 + Int(arc4random_uniform(2)) }
func coins() -> Int { return 6 + Int(arc4random_uniform(2)+arc4random_uniform(2)+arc4random_uniform(2)) }
func stalks() -> Int {
    var ans = 0
    switch arc4random_uniform(16) {
    case 0:
        ans = 6
    case 1...5:
        ans = 7
    case 6...12:
        ans = 8
    default:
        ans = 9
    }
    return ans
}

/// Converts an input string into a casting, checking for validity.
func casting(input: String) -> [Character]? {
    let arr = Array(input.characters)
    if arr.count != 6 || arr.contains(where: {$0 < "6" || $0 > "9"}) {
        return nil
    }
    return arr
}

/// Decodes a cating into king wen hexagram numbers, and a string representation.
func decode(_ casting: [Character]) -> (Int, Int, [String]) {
    return casting.reversed().reduce((0,0,[]),
                              { var (w1,w2,reps) = $0
                                w1 *= 2
                                w2 *= 2
                                switch $1 {
                                    case "6":
                                        reps.append("  ---   ---  ==>  ---------")
                                       return (w1,w2+1,reps)
                                    case "7":
                                        reps.append("  ---------       ---------")
                                        return (w1+1,w2+1, reps)
                                    case "8":
                                        reps.append("  ---   ---       ---   ---")
                                        return (w1,w2,reps)
                                    default:
                                        reps.append("  ---------  ==>  ---   ---")
                                        return (w1+1,w2,reps)
                                }
    })
}

/// Formats a casting on stdout.
func display(_ casting: [Character]) -> Void {
    let (w1,w2,reps) = decode(casting)
    let changed = w1 != w2
    print("Casting: <\(String(casting))>")
    print()
    reps.map( changed ? { $0 } : { $0.substring(to: $0.index($0.startIndex, offsetBy: 11)) })
        .forEach( { print($0) })
    print()
    print(hexNames[w1])
    if changed {
        print(" = Changing To ==>\n\(hexNames[w2])")
    }
}

var opt = "-coins"
if CommandLine.argc > 1 { opt = CommandLine.arguments[1] }
var lines : [Character]
switch opt {
case "-coins":
    lines = casting(method: coins)
case "-stalks":
    lines = casting(method: stalks)
case "-static":
    lines = casting(method: nonMoving)
default:
    if let cast = casting(input: opt) {
        lines = cast
    } else {
        print("Options: -coins|-stalks|-static|<casting>")
        exit(2)
    }
}
display(lines)



