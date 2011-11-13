-- subscript=require("subscript")
-- lpeg=require("lpeg")
-- utf8=require("unicode.utf8")
-- print(utf8.char(12345))
-- print("hello there")
-- x=lpeg.match(lpeg.P("A"),"Aez")
-- print(x)
-- print(mysub("itit"))

local lunamark = require("lunamark")
for k,v in pairs(lunamark) do
  print(k)
end
local writer = lunamark.writer.html.new()
local parse = lunamark.reader.markdown.new(writer, { smart = true })
local inp = io.read("*all")
local result, metadata = parse(inp)
print(result)

