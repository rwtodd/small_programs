-- --------------------------------------------------------------------
-- lua C A S T H E X 
-- --------------------------------------------------------------------

-- casting takes a function 'm', and generates a casting with it.
-- 'm()' will either return a casting, or a number which is 1 line
-- of a casting.  Castings are validated, and invalid castings result
-- in a nil return.
function casting(m) 
  local ans = m()
  if type(ans) == "number" then
     ans = string.char(ans, m(), m(), m(), m(), m())
  end
  if type(ans) == "string" and 
     ans:len() == 6 and 
     not ans:find("[^6-9]")
  then return ans
  else return nil
  end
end

-- here are the supported casting methods
methods = {
  coins = function() 
             return string.byte('3',1) + 
                    math.random(2) + math.random(2) + math.random(2)
          end,

  static = function() 
              return string.byte('6',1) + math.random(2) 
           end,

  stalks = function()
              local v = math.random(16)
              if v == 1 then v = 0
              elseif v <= 8 then v = 2 
              elseif v <= 11 then v = 3
              else v = 1
              end
              return string.byte('6',1) + v
           end
}

methods = setmetatable(methods, {
  -- anything not named in our table just becomes a 
  -- function that returns the key
  __index = function(t,i)  
               return function() return i end 
            end
})

require 'hex' -- our C library

-- proc processes a casting named by 'w', which it looks up
-- in the methods table.
function proc(w)
  local c = casting(methods[w])
  if c then hex.disphex(c) 
       else print("Bad input: ", w)
  end
end

math.randomseed(os.time())

-- if we had cmdline args, process them and exit
if #arg > 0 then
  for i,w in ipairs(arg) do
    proc(w)
  end
  return
end

-- if we had no cmdlne args, process stdin
while true do  
   ln = io.read()
   if ln == nil then return end

   for w in ln:gmatch("%w+") do
     if w == "exit" then return end
     proc(w)
   end
end

