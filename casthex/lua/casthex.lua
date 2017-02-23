-- lua version...
function casting(m) 
  local c1 = m()
  if type(c1) == "string" then
     return c1
  else 
     return string.char(c1, m(), m(), m(), m(), m())
  end
end

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

require 'hex' -- our C library

function proc(w)
  local selection = methods[w] or function() return w end
  hex.disphex(casting(selection))
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

