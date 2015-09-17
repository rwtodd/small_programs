package main

// A direct translation of the scala version.
// It highlights that you can have nested functions
// in golang by just assigning a closure to a name.
// This isn't very idiomatic, as far as I know, but
// it's part of the experimentation I do when learning a
// language.

import "fmt"

func draw(height int) {
	draw1 := func(width int) {
		cur := 1
		for loc := 0; loc <= width; loc++ {
			fmt.Printf("%d ", cur)
			cur = (width - loc) * cur / (loc + 1)
		}
		fmt.Println("")
	}

	for idx := 0; idx < height; idx++ {
		draw1(idx)
	}

}

func main() { draw(16) }
