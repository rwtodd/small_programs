#!/usr/bin/env lua

print( "Ultima V Saved Game Adjuster... lua version.\n" );

fname = os.getenv('HOME') .. '/saved.gam' 
bak = fname:gsub('%.gam$','.bak') 

-- backup the original
print("Backing up:", fname, "to", bak)
os.execute( string.format("cp '%s' '%s'",fname,bak) )

game = io.open(fname, "r+b")

-- **************************************************
print('Adjusting Player Chars')
local STATS_LEN = 32
game:seek('set', 2)

for idx = 0,15 do
	local block = game:read(STATS_LEN)
	local fixed = table.concat {
		block:sub(1,0x0B), 
		'G', 
		block:sub(0x0D,0x10), 
		block:sub(0x13,0x14), 
		block:sub(0x13) 
	}
    game:seek('cur', -STATS_LEN)
    game:write(fixed)
end

-- **************************************************
print('Adjusting inventory')

local bytes9999 = string.pack('<i2', 9999)
local bytes99   = string.char(99)

game:seek('set', 0x202)
game:write(bytes9999:rep(2)) -- food, gold
game:write(bytes99:rep(3))   -- keys, gems, torches

game:seek('set', 0x20B)
game:write(bytes99)   -- skull keys

game:seek('set', 0x24A)
game:write(bytes99:rep(64))  -- spells

-- **************************************************
game:close()
print('Done!')


