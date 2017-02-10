//
//  main.swift
//  CastGeo
//
//  Created by Richard Todd on 2/9/17.
//  Copyright © 2017 Richard Todd. All rights reserved.
//

import Foundation


func displayFigs(_ figs: [[String]], initSpace: Int, midSpace: Int) -> Void {
    let initsp = String(repeating: " ", count: initSpace)
    let midsp = String(repeating: " ", count: midSpace)
    
    for y in 0..<figs[0].count {
        print(initsp, figs[0][y],separator:"",terminator:"")
        for x in 1..<figs.count {
            print(midsp, figs[x][y],separator:"",terminator:"")
        }
        print()
    }
    print()
}


func combineFigs(_ figs: [[String]]) -> [[String]] {
    var ans = Array(repeating: Array(repeating: "", count: 4), count: figs.count/2)
    for idx in 0..<ans.count {
        for fig in 0..<4 {
            ans[idx][fig] = yinyang[(figs[idx*2][fig] == figs[idx*2+1][fig]) ? 0 : 1]
        }
    }
    return ans
}


// set up moms and daughters
let yinyang  = ["*   *", "  *  "]
var line1 = Array(repeating: Array(repeating: yinyang[0] ,count: 4), count: 8)
for i in 4..<8 {
    for j in 0..<4 {
        let selection = yinyang[Int(arc4random_uniform(2))]
        line1[i][j] = selection
        line1[3-j][7-i] = selection
    }
}

// set up the derived lines
let neices = combineFigs(line1)
let witnesses = combineFigs(neices)
let judge = combineFigs(witnesses)

displayFigs(line1,     initSpace: 2, midSpace: 5)
displayFigs(neices,    initSpace: 7, midSpace: 15)
displayFigs(witnesses, initSpace: 17, midSpace: 35)
displayFigs(judge,     initSpace: 37, midSpace: 0)
