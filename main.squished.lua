package.preload['re'] = (function (...)
-- $Id: re.lua,v 1.39 2010/11/04 19:44:18 roberto Exp $

-- imported functions and modules
local tonumber, type, print, error = tonumber, type, print, error
local setmetatable = setmetatable
local m = require"lpeg"

-- 'm' will be used to parse expressions, and 'mm' will be used to
-- create expressions; that is, 're' runs on 'm', creating patterns
-- on 'mm'
local mm = m

-- pattern's metatable 
local mt = getmetatable(mm.P(0))



-- No more global accesses after this point
local version = _VERSION
if version == "Lua 5.2" then _ENV = nil end


local any = m.P(1)


-- Pre-defined names
local Predef = { nl = m.P"\n" }


local mem
local fmem
local gmem


local function updatelocale ()
  mm.locale(Predef)
  Predef.a = Predef.alpha
  Predef.c = Predef.cntrl
  Predef.d = Predef.digit
  Predef.g = Predef.graph
  Predef.l = Predef.lower
  Predef.p = Predef.punct
  Predef.s = Predef.space
  Predef.u = Predef.upper
  Predef.w = Predef.alnum
  Predef.x = Predef.xdigit
  Predef.A = any - Predef.a
  Predef.C = any - Predef.c
  Predef.D = any - Predef.d
  Predef.G = any - Predef.g
  Predef.L = any - Predef.l
  Predef.P = any - Predef.p
  Predef.S = any - Predef.s
  Predef.U = any - Predef.u
  Predef.W = any - Predef.w
  Predef.X = any - Predef.x
  mem = {}    -- restart memoization
  fmem = {}
  gmem = {}
  local mt = {__mode = "v"}
  setmetatable(mem, mt)
  setmetatable(fmem, mt)
  setmetatable(gmem, mt)
end


updatelocale()



local I = m.P(function (s,i) print(i, s:sub(1, i-1)); return i end)


local function getdef (id, Defs)
  local c = Defs and Defs[id]
  if not c then error("undefined name: " .. id) end
  return c
end


local function patt_error (s, i)
  local msg = (#s < i + 20) and s:sub(i)
                             or s:sub(i,i+20) .. "..."
  msg = ("pattern error near '%s'"):format(msg)
  error(msg, 2)
end

local function mult (p, n)
  local np = mm.P(true)
  while n >= 1 do
    if n%2 >= 1 then np = np * p end
    p = p * p
    n = n/2
  end
  return np
end

local function equalcap (s, i, c)
  if type(c) ~= "string" then return nil end
  local e = #c + i
  if s:sub(i, e - 1) == c then return e else return nil end
end


local S = (m.S(" \f\n\r\t\v") + "--" * (any - Predef.nl)^0)^0

local name = m.R("AZ", "az") * m.R("AZ", "az", "__", "09")^0

local arrow = S * "<-"

local exp_follow = m.P"/" + ")" + "}" + ":}" + "~}" + (name * arrow) + -1

name = m.C(name)


-- identifiers only have meaning in a given environment
local Identifier = name * m.Carg(1)

local num = m.C(m.R"09"^1) * S / tonumber

local String = "'" * m.C((any - "'")^0) * "'" +
               '"' * m.C((any - '"')^0) * '"'


local defined = "%" * Identifier / function (c,Defs)
  local cat =  Defs and Defs[c] or Predef[c]
  if not cat then error ("name '" .. c .. "' undefined") end
  return cat
end

local Range = m.Cs(any * (m.P"-"/"") * (any - "]")) / mm.R

local item = defined + Range + m.C(any)

local Class =
    "["
  * (m.C(m.P"^"^-1))    -- optional complement symbol
  * m.Cf(item * (item - "]")^0, mt.__add) /
                          function (c, p) return c == "^" and any - p or p end
  * "]"

local function adddef (t, k, Defs, exp)
  if t[k] then
    error("'"..k.."' already defined as a rule")
  else
    t[k] = exp
  end
  return t
end

local function firstdef (n, Defs, r) return adddef({n}, n, Defs, r) end



local exp = m.P{ "Exp",
  Exp = S * ( m.V"Grammar"
            + m.Cf(m.V"Seq" * ("/" * S * m.V"Seq")^0, mt.__add) );
  Seq = m.Cf(m.Cc(m.P"") * m.V"Prefix"^0 , mt.__mul)
        * (#exp_follow + patt_error);
  Prefix = "&" * S * m.V"Prefix" / mt.__len
         + "!" * S * m.V"Prefix" / mt.__unm
         + m.V"Suffix";
  Suffix = m.Cf(m.V"Primary" * S *
          ( ( m.P"+" * m.Cc(1, mt.__pow)
            + m.P"*" * m.Cc(0, mt.__pow)
            + m.P"?" * m.Cc(-1, mt.__pow)
            + "^" * ( m.Cg(num * m.Cc(mult))
                    + m.Cg(m.C(m.S"+-" * m.R"09"^1) * m.Cc(mt.__pow))
                    )
            + "->" * S * ( m.Cg(String * m.Cc(mt.__div))
                         + m.P"{}" * m.Cc(nil, m.Ct)
                         + m.Cg(Identifier / getdef * m.Cc(mt.__div))
                         )
            + "=>" * S * m.Cg(Identifier / getdef * m.Cc(m.Cmt))
            ) * S
          )^0, function (a,b,f) return f(a,b) end );
  Primary = "(" * m.V"Exp" * ")"
            + String / mm.P
            + Class
            + defined
            + "{:" * (name * ":" + m.Cc(nil)) * m.V"Exp" * ":}" /
                     function (n, p) return mm.Cg(p, n) end
            + "=" * name / function (n) return mm.Cmt(mm.Cb(n), equalcap) end
            + m.P"{}" / mm.Cp
            + "{~" * m.V"Exp" * "~}" / mm.Cs
            + "{" * m.V"Exp" * "}" / mm.C
            + m.P"." * m.Cc(any)
            + name * -arrow / mm.V
            + "<" * name * ">" / mm.V;
  Definition = Identifier * arrow * m.V"Exp";
  Grammar = m.Cf(m.V"Definition" / firstdef * m.Cg(m.V"Definition")^0, adddef) /
                mm.P
}

local pattern = S * exp / mm.P * (-any + patt_error)


local function compile (p, defs)
  if mm.type(p) == "pattern" then return p end   -- already compiled
  local cp = pattern:match(p, 1, defs)
  if not cp then error("incorrect pattern", 3) end
  return cp
end

local function match (s, p, i)
  local cp = mem[p]
  if not cp then
    cp = compile(p)
    mem[p] = cp
  end
  return cp:match(s, i or 1)
end

local function find (s, p, i)
  local cp = fmem[p]
  if not cp then
    cp = compile(p)
    cp = mm.P{ mm.Cp() * cp + 1 * mm.V(1) }
    fmem[p] = cp
  end
  return cp:match(s, i or 1)
end

local function gsub (s, p, rep)
  local g = gmem[p] or {}   -- ensure gmem[p] is not collected while here
  gmem[p] = g
  local cp = g[rep]
  if not cp then
    cp = compile(p)
    cp = mm.Cs((cp / rep + 1)^0)
    g[rep] = cp
  end
  return cp:match(s)
end


-- exported names
local re = {
  compile = compile,
  match = match,
  find = find,
  gsub = gsub,
  updatelocale = updatelocale,
}

if version == "Lua 5.1" then _G.re = re end

return re
 end)
package.preload['cosmo'] = (function (...)
local require = require

local grammar = require "cosmo.grammar"
local interpreter = require "cosmo.fill"
local loadstring = loadstring

module(..., package.seeall)

yield = coroutine.yield

local preamble = [[
    local is_callable, insert, concat, setmetatable, getmetatable, type, wrap, tostring, check_selector = ...
    local function prepare_env(env, parent)
      local __index = function (t, k)
			local v = env[k]
			if not v then
			  v = parent[k]
			end
			return v
		      end
      local __newindex = function (t, k, v)
			   env[k] = v
			 end
      return setmetatable({ self = env }, { __index = __index, __newindex = __newindex })
    end
    local id = function () end
    local template_func = %s
    return function (env, opts) 
	     opts = opts or {}
	     local out = opts.out or {}
	     template_func(out, env)
	     return concat(out, opts.delim)
	   end
]]

local compiled_template = [[
    function (out, env)
      if type(env) == "string" then env = { it = env } end
      $parts[=[
	  insert(out, $quoted_text)
      ]=],
      [=[
	  local selector_name = $selector
	  local selector = $parsed_selector
	  $if_subtemplate[==[
	      local subtemplates = {}
	      $subtemplates[===[
		  subtemplates[$i] = $subtemplate
	      ]===]
	      $if_args[===[
		  check_selector(selector_name, selector)
		  for e, literal in wrap(selector), $args, true do
		    if literal then
		      insert(out, tostring(e))
		    else
		      if type(e) ~= "table" then
			e = prepare_env({ it = tostring(e) }, env)
		      else
			e = prepare_env(e, env)
		      end
		      (subtemplates[e.self._template or 1] or id)(out, e)
		    end
		  end
	      ]===],
	      [===[
		  if type(selector) == 'table' then
		    for _, e in ipairs(selector) do
		      if type(e) ~= "table" then
			e = prepare_env({ it = tostring(e) }, env)
		      else
			e = prepare_env(e, env)
		      end
		      (subtemplates[e.self._template or 1] or id)(out, e)
		    end
		  else
		    check_selector(selector_name, selector)
		    for e, literal in wrap(selector), nil, true do
		      if literal then
			insert(out, tostring(e))
		      else
			if type(e) ~= "table" then
			  e = prepare_env({ it = tostring(e) }, env)
			else
			  e = prepare_env(e, env)
			end
			(subtemplates[e.self._template or 1] or id)(out, e)
		      end
		    end
		  end
	      ]===]
	  ]==],
	  [==[
	      $if_args[===[
		  check_selector(selector_name, selector)
		  selector = selector($args, false)
		  insert(out, tostring(selector))
	      ]===],
	      [===[
		  if is_callable(selector) then
		    insert(out, tostring(selector()))
		  else
		    insert(out, tostring(selector or ""))
		  end
	      ]===]
	  ]==]
      ]=]
    end
]]

local function is_callable(f)
  if type(f) == "function" then return true end
  local meta = getmetatable(f)
  if meta and meta.__call then return true end
  return false
end

local function check_selector(name, selector)
  if not is_callable(selector) then
    error("selector " .. name .. " is not callable but is " .. type(selector))
  end
end

local function compile_template(chunkname, template_code)
   local template_func, err = loadstring(string.format(preamble, template_code), chunkname)
   if not template_func then
     error("syntax error when compiling template: " .. err)
   else
     return template_func(is_callable, table.insert, table.concat, setmetatable, getmetatable, type,
			  coroutine.wrap, tostring, check_selector)
   end
end

local compiler = {}

function compiler.template(template)
  assert(template.tag == "template")
  local parts = {}
  for _, part in ipairs(template.parts) do
    parts[#parts+1] = compiler[part.tag](part)
  end
  return interpreter.fill(compiled_template, { parts = parts })
end

function compiler.text(text)
  assert(text.tag == "text")
  return { _template = 1, quoted_text = string.format("%q", text.text) }
end

function compiler.appl(appl)
  assert(appl.tag == "appl")
  local selector, args, subtemplates = appl.selector, appl.args, appl.subtemplates
  local ta = { _template = 2, selector = string.format("%q", selector), 
      parsed_selector = selector }
   local do_subtemplates = function ()
			     for i, subtemplate in ipairs(subtemplates) do
			       yield{ i = i, subtemplate = compiler.template(subtemplate) }
			     end
			   end
   if #subtemplates == 0 then
     if args and args ~= "" and args ~= "{}" then
       ta.if_subtemplate = { { _template = 2, if_args = { { _template = 1, args = args } } } }
     else
       ta.if_subtemplate = { { _template = 2, if_args = { { _template = 2 } } } }
     end
   else
     if args and args ~= "" and args ~= "{}" then
       ta.if_subtemplate = { { _template = 1, subtemplates = do_subtemplates,
			       if_args = { { _template = 1, args = args } } } }
     else
       ta.if_subtemplate = { { _template = 1, subtemplates = do_subtemplates,
			       if_args = { { _template = 2 } } } }
     end
   end
   return ta
end

local cache = {}
setmetatable(cache, { __index = function (tab, key)
				   local new = {}
				   tab[key] = new
				   return new
				end,
		      __mode = "v" })

function compile(template, chunkname)
  template = template or ""
  chunkname = chunkname or template
  local compiled_template = cache[template][chunkname]
  if not compiled_template then
    compiled_template = compile_template(chunkname, compiler.template(grammar.ast:match(template)))
    cache[template][chunkname] = compiled_template
  end
  return compiled_template
end

local filled_templates = {}

function fill(template, env)
   template = template or ""
   local start = template:match("^(%[=*%[)")
   if start then template = template:sub(#start + 1, #template - #start) end
   if filled_templates[template] then 
      return compile(template)(env)
   else
      filled_templates[template] = true
      return interpreter.fill(template, env)
   end
end

local nop = function () end

function cond(bool, table)
   if bool then
      return function () yield(table) end
   else
      return nop
   end
end

f = compile

function c(bool)
   if bool then 
      return function (table)
		return function () yield(table) end
	     end
   else
      return function (table) return nop end
   end
end

function map(arg, has_block)
   if has_block then
      for _, item in ipairs(arg) do
	 cosmo.yield(item)
      end
   else
      return table.concat(arg)
   end
end

function inject(arg)
   cosmo.yield(arg)
end

function cif(arg, has_block)
  if not has_block then error("this selector needs a block") end
  if arg[1] then
    arg._template = 1
  else
    arg._template = 2
  end
  cosmo.yield(arg)
end

function concat(arg)
  local list, sep = arg[1], arg[2] or ", "
  local size = #list
  for i, e in ipairs(list) do
    if type(e) == "table" then
      if i ~= size then
	cosmo.yield(e)
	cosmo.yield(sep, true)
      else
	cosmo.yield(e)
      end
    else
      if i ~= size then
	cosmo.yield{ it = e }
	cosmo.yield(sep, true)
      else
	cosmo.yield{ it = e }
      end
    end
  end
end

function make_concat(list)
  return function (arg)
	   local sep = (arg and arg[1]) or ", "
	   local size = #list
	   for i, e in ipairs(list) do
	     if type(e) == "table" then
	       if i ~= size then
		 cosmo.yield(e)
		 cosmo.yield(sep, true)
	       else
		 cosmo.yield(e)
	       end
	     else
	       if i ~= size then
		 cosmo.yield{ it = e }
		 cosmo.yield(sep, true)
	       else
		 cosmo.yield{ it = e }
	       end
	     end
	   end
	 end
end

function cfor(args)
  local name, list, args = args[1], args[2], args[3]
  if type(list) == "table" then
    for i, item in ipairs(list) do
      cosmo.yield({ [name] = item, i = i })
    end
  else
    for item, literal in coroutine.wrap(list), args, true do
      if literal then
	cosmo.yield(item, true)
      else
	cosmo.yield({ [name] = item })
      end
    end
  end
end
 end)
package.preload['cosmo.grammar'] = (function (...)

local lpeg = require "lpeg"
local re = require "re"

module(..., package.seeall)

local function parse_selector(selector, env)
  env = env or "env"
  selector = string.sub(selector, 2, #selector)
  local parts = {}
  for w in string.gmatch(selector, "[^|]+") do
    local n = tonumber(w)
    if n then
      table.insert(parts, "[" .. n .. "]")
    else
      table.insert(parts, "['" .. w .. "']")
    end
  end
  return env .. table.concat(parts)
end

local function parse_exp(exp)
  return exp
end

local start = "[" * lpeg.P"="^1 * "["

local start_ls = "[" * lpeg.P"="^0 * "["

local longstring1 = lpeg.P{
  "longstring",
  longstring = lpeg.P"[[" * (lpeg.V"longstring" + (lpeg.P(1) - lpeg.P"]]"))^0 * lpeg.P"]]"
}

local longstring2 = lpeg.P(function (s, i)
  local l = lpeg.match(start, s, i)
  if not l then return nil end
  local p = lpeg.P("]" .. string.rep("=", l - i - 2) .. "]")
  p = (1 - p)^0 * p
  return lpeg.match(p, s, l)
end)

local longstring = #("[" * lpeg.S"[=") * (longstring1 + longstring2)

local function parse_longstring(s)
  local start = s:match("^(%[=*%[)")
  if start then 
    return string.format("%q", s:sub(#start + 1, #s - #start))
  else
    return s
  end
end

local alpha =  lpeg.R('__','az','AZ','\127\255') 

local n = lpeg.R'09'

local alphanum = alpha + n

local name = alpha * (alphanum)^0

local number = (lpeg.P'.' + n)^1 * (lpeg.S'eE' * lpeg.S'+-'^-1)^-1 * (alphanum)^0
number = #(n + (lpeg.P'.' * n)) * number

local shortstring = (lpeg.P'"' * ( (lpeg.P'\\' * 1) + (1 - (lpeg.S'"\n\r\f')) )^0 * lpeg.P'"') +
  (lpeg.P"'" * ( (lpeg.P'\\' * 1) + (1 - (lpeg.S"'\n\r\f")) )^0 * lpeg.P"'")

local space = (lpeg.S'\n \t\r\f')^0
 
local syntax = [[
  template <- (<item>* -> {} !.) -> compiletemplate
  item <- <text> / <templateappl> / (. => error)
  text <- ({~ (!<selector> ('$$' -> '$' / .))+ ~}) -> compiletext
  selector <- ('$(' %s {~ <exp> ~} %s ')') -> parseexp / 
              ('$' %alphanum+ ('|' %alphanum+)*) -> parseselector
  templateappl <- ({~ <selector> ~} {~ <args>? ~} !'{' 
		   ({%longstring} -> compilesubtemplate)? (%s ','? %s ({%longstring} -> compilesubtemplate))* -> {} !(','? %s %start)) 
		     -> compileapplication
  args <- '{' %s '}' / '{' %s <arg> %s (',' %s <arg> %s)* ','? %s '}'
  arg <- <attr> / <exp>
  attr <- <symbol> %s '=' !'=' %s <exp> / '[' !'[' !'=' %s <exp> %s ']' %s '=' %s <exp>
  symbol <- %alpha %alphanum*
  explist <- <exp> (%s ',' %s <exp>)* (%s ',')?
  exp <- <simpleexp> (%s <binop> %s <simpleexp>)*
  simpleexp <- <args> / %string / %longstring -> parsels / %number / 'true' / 'false' / 
     'nil' / <unop> %s <exp> / <prefixexp> / (. => error)
  unop <- '-' / 'not' / '#' 
  binop <- '+' / '-' / '*' / '/' / '^' / '%' / '..' / '<=' / '<' / '>=' / '>' / '==' / '~=' /
     'and' / 'or'
  prefixexp <- ( <selector> / {%name} -> addenv / '(' %s <exp> %s ')' ) 
    ( %s <args> / '.' %name / ':' %name %s ('(' %s ')' / '(' %s <explist> %s ')') / 
    '[' %s <exp> %s ']' / '(' %s ')' / '(' %s <explist> %s ')' / 
    %string / %longstring -> parsels %s )*
]]

local function pos_to_line(str, pos)
  local s = str:sub(1, pos)
  local line, start = 1, 0
  local newline = string.find(s, "\n")
  while newline do
    line = line + 1
    start = newline
    newline = string.find(s, "\n", newline + 1)
  end
  return line, pos - start
end

local function ast_text(text)
  return { tag = "text", text = text }
end

local function ast_template_application(selector, args, ast_first_subtemplate, ast_subtemplates)
  if not ast_subtemplates then
    ast_first_subtemplate = nil
  end
  local subtemplates = { ast_first_subtemplate, unpack(ast_subtemplates or {}) }
  return { tag = "appl", selector = selector, args = args, subtemplates = subtemplates }
end

local function ast_template(parts)
  return { tag = "template", parts = parts }
end

local function ast_subtemplate(text)
  local start = text:match("^(%[=*%[)")
  if start then text = text:sub(#start + 1, #text - #start) end
  return _M.ast:match(text)
end

local syntax_defs = {
  start = start_ls,
  alpha = alpha,
  alphanum = alphanum,
  name = name,
  number = number,
  string = shortstring,
  longstring = longstring,
  s = space,
  parseselector = parse_selector,
  parseexp = parse_exp,
  parsels = parse_longstring,
  addenv = function (s) return "env['" .. s .. "']" end,
  error = function (tmpl, pos)
    	        local line, pos = pos_to_line(tmpl, pos)
		error("syntax error in template at line " .. line .. " position " .. pos)
	      end,
  compiletemplate = ast_template,
  compiletext = ast_text,
  compileapplication = ast_template_application,
  compilesubtemplate = ast_subtemplate
}

ast = re.compile(syntax, syntax_defs)
 end)
package.preload['cosmo.fill'] = (function (...)

local grammar = require "cosmo.grammar"

module(..., package.seeall)

local function is_callable(f)
  if type(f) == "function" then return true end
  local meta = getmetatable(f)
  if meta and meta.__call then return true end
  return false
end

local insert = table.insert
local concat = table.concat

local function prepare_env(env, parent)
  local __index = function (t, k)
		    local v = env[k]
		    if not v then
		      v = parent[k]
		    end
		    return v
		  end
  local __newindex = function (t, k, v)
		       env[k] = v
		     end
  return setmetatable({ self = env }, { __index = __index, __newindex = __newindex })
end

local interpreter = {}

function interpreter.text(state, text)
  assert(text.tag == "text")
  insert(state.out, text.text)
end

local function check_selector(name, selector)
  if not is_callable(selector) then
    error("selector " .. name .. " is not callable but is " .. type(selector))
  end
end

function interpreter.appl(state, appl)
  assert(appl.tag == "appl")
  local selector, args, subtemplates = appl.selector, appl.args, appl.subtemplates
  local env, out = state.env, state.out
  local selector_name = selector
  selector = loadstring("local env = (...); return " .. selector)(env) or function () return '' end
  if #subtemplates == 0 then
    if args and args ~= "" and args ~= "{}" then
      check_selector(selector_name, selector)
      selector = selector(loadstring("local env = (...); return " .. args)(env), false)
      insert(out, tostring(selector))
    else
      if is_callable(selector) then
	insert(out, tostring(selector()))
      else
	insert(out, tostring(selector or ""))
      end
    end
  else
    if args and args ~= "" and args ~= "{}" then
      check_selector(selector_name, selector)
      args = loadstring("local env = (...); return " .. args)(env)
      for e, literal in coroutine.wrap(selector), args, true do
	if literal then
	  insert(out, tostring(e))
	else
	  if type(e) ~= "table" then
	    e = prepare_env({ it = tostring(e) }, env)
	  else
	    e = prepare_env(e, env)
	  end
	  interpreter.template({ env = e, out = out }, subtemplates[e.self._template or 1])
	end
      end
    else
      if type(selector) == 'table' then
	for _, e in ipairs(selector) do
	  if type(e) ~= "table" then
	    e = prepare_env({ it = tostring(e) }, env)
	  else
	    e = prepare_env(e, env) 
	  end
	  interpreter.template({ env = e, out = out }, subtemplates[e.self._template or 1])
	end
      else
	check_selector(selector_name, selector)
	for e, literal in coroutine.wrap(selector), nil, true do
	  if literal then
	    insert(out, tostring(e))
	  else
	    if type(e) ~= "table" then
	      e = prepare_env({ it = tostring(e) }, env)
	    else
	      e = prepare_env(e, env)
	    end
	    interpreter.template({ env = e, out = out }, subtemplates[e.self._template or 1])
	  end
	end
      end
    end
  end
end

function interpreter.template(state, template)
  if template then
    assert(template.tag == "template")
    for _, part in ipairs(template.parts) do
      interpreter[part.tag](state, part)
    end
  end
end

function fill(template, env, opts)
   opts = opts or {}
   local out = opts.out or {}
   if type(env) == "string" then env = { it = env } end
   interpreter.template({ env = env, out = out }, grammar.ast:match(template))
   return concat(out, opts.delim)
end
 end)
package.preload['alt_getopt'] = (function (...)
-- Copyright (c) 2009 Aleksey Cheusov <vle@gmx.net>
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

local type, pairs, ipairs, io, os = type, pairs, ipairs, io, os

module ("alt_getopt")

local function convert_short2long (opts)
   local i = 1
   local len = #opts
   local ret = {}

   for short_opt, accept_arg in opts:gmatch("(%w)(:?)") do
      ret[short_opt]=#accept_arg
   end

   return ret
end

local function exit_with_error (msg, exit_status)
   io.stderr:write (msg)
   os.exit (exit_status)
end

local function err_unknown_opt (opt)
   exit_with_error ("Unknown option `-" ..
		    (#opt > 1 and "-" or "") .. opt .. "'\n", 1)
end

local function canonize (options, opt)
   if not options [opt] then
      err_unknown_opt (opt)
   end

   while type (options [opt]) == "string" do
      opt = options [opt]

      if not options [opt] then
	 err_unknown_opt (opt)
      end
   end

   return opt
end

function get_ordered_opts (arg, sh_opts, long_opts)
   local i      = 1
   local count  = 1
   local opts   = {}
   local optarg = {}

   local options = convert_short2long (sh_opts)
   for k,v in pairs (long_opts) do
      options [k] = v
   end

   while i <= #arg do
      local a = arg [i]

      if a == "--" then
	 i = i + 1
	 break

      elseif a == "-" then
	 break

      elseif a:sub (1, 2) == "--" then
	 local pos = a:find ("=", 1, true)

	 if pos then
	    local opt = a:sub (3, pos-1)

	    opt = canonize (options, opt)

	    if options [opt] == 0 then
	       exit_with_error ("Bad usage of option `" .. a .. "'\n", 1)
	    end

	    optarg [count] = a:sub (pos+1)
	    opts [count] = opt
	 else
	    local opt = a:sub (3)

	    opt = canonize (options, opt)

	    if options [opt] == 0 then
	       opts [count] = opt
	    else
	       if i == #arg then
		  exit_with_error ("Missed value for option `" .. a .. "'\n", 1)
	       end

	       optarg [count] = arg [i+1]
	       opts [count] = opt
	       i = i + 1
	    end
	 end
	 count = count + 1

      elseif a:sub (1, 1) == "-" then
	 local j
	 for j=2,a:len () do
	    local opt = canonize (options, a:sub (j, j))

	    if options [opt] == 0 then
	       opts [count] = opt
	       count = count + 1
	    elseif a:len () == j then
	       if i == #arg then
		  exit_with_error ("Missed value for option `-" .. opt .. "'\n", 1)
	       end

	       optarg [count] = arg [i+1]
	       opts [count] = opt
	       i = i + 1
	       count = count + 1
	       break
	    else
	       optarg [count] = a:sub (j+1)
	       opts [count] = opt
	       count = count + 1
	       break
	    end
	 end
      else
	 break
      end

      i = i + 1
   end

   return opts,i,optarg
end

function get_opts (arg, sh_opts, long_opts)
   local ret = {}

   local opts,optind,optarg = get_ordered_opts (arg, sh_opts, long_opts)
   for i,v in ipairs (opts) do
      if optarg [i] then
	 ret [v] = optarg [i]
      else
	 ret [v] = 1
      end
   end

   return ret,optind
end
 end)
package.preload['lunamark'] = (function (...)
-- (c) 2009-2011 John MacFarlane.  Released under MIT license.
-- See the file LICENSE in the source for details.

--- Copyright &copy; 2009-2011 John MacFarlane.
--
-- Released under the MIT license (see LICENSE in the source for details).
--
-- ## Description
--
-- Lunamark is a lua library for conversion of markdown to
-- other textual formats. Currently HTML, Docbook, ConTeXt,
-- LaTeX, and Groff man are the supported output formats,
-- but lunamark's modular architecture makes it easy to add
-- writers and modify the markdown parser (written with a PEG
-- grammar).
--
-- Lunamark's markdown parser currently supports the following
-- extensions (which can be turned on or off individually):
--
--   - Smart typography (fancy quotes, dashes, ellipses)
--   - Significant start numbers in ordered lists
--   - Footnotes
--   - Definition lists
--
-- More extensions will be supported in later versions.
--
-- The library is as portable as lua and has very good performance.
-- It is slightly faster than the author's own C library
-- [peg-markdown](http://github.com/jgm/peg-markdown).
--
-- ## Simple usage example
--
--     local lunamark = require("lunamark")
--     local writer = lunamark.writer.html.new()
--     local parse = lunamark.reader.markdown.new(writer, { smart = true })
--     local result, metadata = parse("Here's 'my *text*'...")
--     print(result)
--     assert(result == 'Here’s ‘my <em>text</em>’…')
--
-- ## Customizing the writer
--
-- Render emphasized text as ALL CAPS, rather than italics:
--
--     local unicode = require("unicode")
--     local util = require("lunamark.util")
--     local utf8 = unicode.utf8
--     function writer.emphasis(s)
--       return util.walk(s, utf8.upper)
--     end
--     local parse = lunamark.reader.markdown.new(writer, { smart = true })
--     local result, metadata = parse("*Beiß* nicht in die Hand, die dich *füttert*.")
--     print(result)
--     assert(result == 'BEIß nicht in die Hand, die dich FÜTTERT.')
--
-- Eliminate hyperlinks:
--
--     function writer.link(lab,url,tit)
--       return lab
--     end
--     local parse = lunamark.reader.markdown.new(writer, { smart = true })
--     local result, metadata = parse("[hi](/url) there")
--     print(result)
--     assert(result == 'hi there')
--
-- ## Customizing the parser
--
-- Parse CamelCase words as wikilinks:
--
--     lpeg = require("lpeg")
--     local writer = lunamark.writer.html.new()
--     function add_wikilinks(syntax)
--       local capword = lpeg.R("AZ")^1 * lpeg.R("az")^1
--       local parse_wikilink = lpeg.C(capword^2)
--                            / function(wikipage)
--                                return writer.link(writer.string(wikipage),
--                                                   "/" .. wikipage,
--                                                   "Go to " .. wikipage)
--                              end
--       syntax.Inline = parse_wikilink + syntax.Inline
--       return syntax
--     end
--     local parse = lunamark.reader.markdown.new(writer, { alter_syntax = add_wikilinks })
--     local result, metadata = parse("My text with WikiLinks.\n")
--     print(result)
--     assert(result == 'My text with <a href="/WikiLinks" title="Go to WikiLinks">WikiLinks</a>.')
--

local G = {}

setmetatable(G,{ __index = function(t,name)
                             local mod = require("lunamark." .. name)
                             rawset(t,name,mod)
                             return t[name]
                            end })

return G
 end)
package.preload['lunamark.util'] = (function (...)
-- (c) 2009-2011 John MacFarlane.  Released under MIT license.
-- See the file LICENSE in the source for details.

--- Utility functions for lunamark.

local M = {}
local cosmo = require("cosmo")
local rep  = string.rep
local insert = table.insert
local lpeg = require("lpeg")
local Cs, P, S, lpegmatch = lpeg.Cs, lpeg.P, lpeg.S, lpeg.match
local any = lpeg.P(1)

--- Find a template and return its contents (or `false` if
-- not found). The template is sought first in the
-- working directory, then in `templates`, then in
-- `$HOME/.lunamark/templates`, then in the Windows
-- `APPDATA` directory.
function M.find_template(name)
  local base, ext = name:match("([^%.]*)(.*)")
  if (not ext or ext == "") and format then ext = "." .. format end
  local fname = base .. ext
  local file = io.open(fname, "read")
  if not file then
    file = io.open("templates/" .. fname, "read")
  end
  if not file then
    local home = os.getenv("HOME")
    if home then
      file = io.open(home .. "/.lunamark/templates/" .. fname, "read")
    end
  end
  if not file then
    local appdata = os.getenv("APPDATA")
    if appdata then
      file = io.open(appdata .. "/lunamark/templates/" .. fname, "read")
    end
  end
  if file then
    return file:read("*all")
  else
    return false, "Could not find template '" .. fname .. "'"
  end
end

--- Implements a `sepby` directive for cosmo templates.
-- `$sepby{ myarray }[[$it]][[, ]]` will render the elements
-- of `myarray` separated by commas. If `myarray` is a string,
-- it will be treated as an array with one element.  If it is
-- `nil`, it will be treated as an empty array.
function M.sepby(arg)
  local a = arg[1]
  if not a then
    a = {}
  elseif type(a) ~= "table" then
    a = {a}
  end
  for i,v in ipairs(a) do
     if i > 1 then cosmo.yield{_template=2} end
     cosmo.yield{it = a[i], _template=1}
  end
end

--[[
-- extend(t) returns a table that falls back to t for non-found values
function M.extend(prototype)
  local newt = {}
  local metat = { __index = function(t,key)
                              return prototype[key]
                            end }
  setmetatable(newt, metat)
  return newt
end
--]]

--- Print error message and exit.
function M.err(msg, exit_code)
  io.stderr:write("lunamark: " .. msg .. "\n")
  os.exit(exit_code or 1)
end

--- Shallow table copy including metatables.
function M.table_copy(t)
  local u = { }
  for k, v in pairs(t) do u[k] = v end
  return setmetatable(u, getmetatable(t))
end

--- Expand tabs in a line of text.
-- `s` is assumed to be a single line of text.
-- From *Programming Lua*.
function M.expand_tabs_in_line(s, tabstop)
  local tab = tabstop or 4
  local corr = 0
  return (s:gsub("()\t", function(p)
          local sp = tab - (p - 1 + corr)%tab
          corr = corr - 1 + sp
          return rep(" ",sp)
        end))
end

--- Walk a rope `t`, applying a function `f` to each leaf element in order.
-- A rope is an array whose elements may be ropes, strings, numbers,
-- or functions. If a leaf element is a function, call it and get the return value
-- before proceeding.
local function walk(t, f)
    local typ = type(t)
    if typ == "string" then
      return f(t)
    elseif typ == "table" then
      local i = 1
      local n
      n = t[i]
      while n do
        t[i] = walk(n, f)
        i = i + 1
        n = t[i]
      end
      return t
    elseif typ == "function" then
      local ok, val = pcall(t)
      if ok then
        return walk(val,f)
      end
    else
      return f(tostring(t))
    end
end

M.walk = walk

--- Flatten an array `ary`.
local function flatten(ary)
  local new = {}
  for i,v in ipairs(ary) do
    if type(v) == "table" then
      for j,w in ipairs(flatten(v)) do
        new[#new + 1] = w
      end
    else
      new[#new + 1] = v
    end
  end
  return new
end

M.flatten = flatten

--- Convert a rope to a string.
local function rope_to_string(rope)
  local buffer = {}
  walk(rope, function(x) buffer[#buffer + 1] = x end)
  return table.concat(buffer)
end

M.rope_to_string = rope_to_string

-- assert(rope_to_string{"one","two"} == "onetwo")
-- assert(rope_to_string{"one",{"1","2"},"three"} == "one12three")

--- Return the last item in a rope.
local function rope_last(rope)
  if #rope == 0 then
    return nil
  else
    local l = rope[#rope]
    if type(l) == "table" then
      return rope_last(l)
    else
      return l
    end
  end
end

-- assert(rope_last{"one","two"} == "two")
-- assert(rope_last{} == nil)
-- assert(rope_last{"one",{"2",{"3","4"}}} == "4")

M.rope_last = rope_last

--- Given an array `ary`, return a new array with `x`
-- interspersed between elements of `ary`.
function M.intersperse(ary, x)
  local new = {}
  local l = #ary
  for i,v in ipairs(ary) do
    local n = #new
    new[n + 1] = v
    if i ~= l then
      new[n + 2] = x
    end
  end
  return new
end

--- Given an array `ary`, return a new array with each
-- element `x` of `ary` replaced by `f(x)`.
function M.map(ary, f)
  local new = {}
  for i,v in ipairs(ary) do
    new[i] = f(v)
  end
  return new
end

--- Given a table `char_escapes` mapping escapable characters onto
-- their escaped versions and optionally `string_escapes` mapping
-- escapable strings (or multibyte UTF-8 characters) onto their
-- escaped versions, returns a function that escapes a string.
-- This function uses lpeg and is faster than gsub.
function M.escaper(char_escapes, string_escapes)
  local char_escapes_list = ""
  for i,_ in pairs(char_escapes) do
    char_escapes_list = char_escapes_list .. i
  end
  local escapable = S(char_escapes_list) / char_escapes
  if string_escapes then
    for k,v in pairs(string_escapes) do
      escapable = P(k) / v + escapable
    end
  end
  local escape_string = Cs((escapable + any)^0)
  return function(s)
    return lpegmatch(escape_string, s)
  end
end


return M
 end)
package.preload['lunamark.entities'] = (function (...)
-- (c) 2009-2011 John MacFarlane.  Released under MIT license.
-- See the file LICENSE in the source for details.

--- Functions for dealing with HTML/XML entities.

local M = {}

local unicode=require("unicode")
local utf8=unicode.utf8

local character_entities = {
  ["quot"] = 0x0022,
  ["amp"] = 0x0026,
  ["apos"] = 0x0027,
  ["lt"] = 0x003C,
  ["gt"] = 0x003E,
  ["nbsp"] = 160,
  ["iexcl"] = 0x00A1,
  ["cent"] = 0x00A2,
  ["pound"] = 0x00A3,
  ["curren"] = 0x00A4,
  ["yen"] = 0x00A5,
  ["brvbar"] = 0x00A6,
  ["sect"] = 0x00A7,
  ["uml"] = 0x00A8,
  ["copy"] = 0x00A9,
  ["ordf"] = 0x00AA,
  ["laquo"] = 0x00AB,
  ["not"] = 0x00AC,
  ["shy"] = 173,
  ["reg"] = 0x00AE,
  ["macr"] = 0x00AF,
  ["deg"] = 0x00B0,
  ["plusmn"] = 0x00B1,
  ["sup2"] = 0x00B2,
  ["sup3"] = 0x00B3,
  ["acute"] = 0x00B4,
  ["micro"] = 0x00B5,
  ["para"] = 0x00B6,
  ["middot"] = 0x00B7,
  ["cedil"] = 0x00B8,
  ["sup1"] = 0x00B9,
  ["ordm"] = 0x00BA,
  ["raquo"] = 0x00BB,
  ["frac14"] = 0x00BC,
  ["frac12"] = 0x00BD,
  ["frac34"] = 0x00BE,
  ["iquest"] = 0x00BF,
  ["Agrave"] = 0x00C0,
  ["Aacute"] = 0x00C1,
  ["Acirc"] = 0x00C2,
  ["Atilde"] = 0x00C3,
  ["Auml"] = 0x00C4,
  ["Aring"] = 0x00C5,
  ["AElig"] = 0x00C6,
  ["Ccedil"] = 0x00C7,
  ["Egrave"] = 0x00C8,
  ["Eacute"] = 0x00C9,
  ["Ecirc"] = 0x00CA,
  ["Euml"] = 0x00CB,
  ["Igrave"] = 0x00CC,
  ["Iacute"] = 0x00CD,
  ["Icirc"] = 0x00CE,
  ["Iuml"] = 0x00CF,
  ["ETH"] = 0x00D0,
  ["Ntilde"] = 0x00D1,
  ["Ograve"] = 0x00D2,
  ["Oacute"] = 0x00D3,
  ["Ocirc"] = 0x00D4,
  ["Otilde"] = 0x00D5,
  ["Ouml"] = 0x00D6,
  ["times"] = 0x00D7,
  ["Oslash"] = 0x00D8,
  ["Ugrave"] = 0x00D9,
  ["Uacute"] = 0x00DA,
  ["Ucirc"] = 0x00DB,
  ["Uuml"] = 0x00DC,
  ["Yacute"] = 0x00DD,
  ["THORN"] = 0x00DE,
  ["szlig"] = 0x00DF,
  ["agrave"] = 0x00E0,
  ["aacute"] = 0x00E1,
  ["acirc"] = 0x00E2,
  ["atilde"] = 0x00E3,
  ["auml"] = 0x00E4,
  ["aring"] = 0x00E5,
  ["aelig"] = 0x00E6,
  ["ccedil"] = 0x00E7,
  ["egrave"] = 0x00E8,
  ["eacute"] = 0x00E9,
  ["ecirc"] = 0x00EA,
  ["euml"] = 0x00EB,
  ["igrave"] = 0x00EC,
  ["iacute"] = 0x00ED,
  ["icirc"] = 0x00EE,
  ["iuml"] = 0x00EF,
  ["eth"] = 0x00F0,
  ["ntilde"] = 0x00F1,
  ["ograve"] = 0x00F2,
  ["oacute"] = 0x00F3,
  ["ocirc"] = 0x00F4,
  ["otilde"] = 0x00F5,
  ["ouml"] = 0x00F6,
  ["divide"] = 0x00F7,
  ["oslash"] = 0x00F8,
  ["ugrave"] = 0x00F9,
  ["uacute"] = 0x00FA,
  ["ucirc"] = 0x00FB,
  ["uuml"] = 0x00FC,
  ["yacute"] = 0x00FD,
  ["thorn"] = 0x00FE,
  ["yuml"] = 0x00FF,
  ["OElig"] = 0x0152,
  ["oelig"] = 0x0153,
  ["Scaron"] = 0x0160,
  ["scaron"] = 0x0161,
  ["Yuml"] = 0x0178,
  ["fnof"] = 0x0192,
  ["circ"] = 0x02C6,
  ["tilde"] = 0x02DC,
  ["Alpha"] = 0x0391,
  ["Beta"] = 0x0392,
  ["Gamma"] = 0x0393,
  ["Delta"] = 0x0394,
  ["Epsilon"] = 0x0395,
  ["Zeta"] = 0x0396,
  ["Eta"] = 0x0397,
  ["Theta"] = 0x0398,
  ["Iota"] = 0x0399,
  ["Kappa"] = 0x039A,
  ["Lambda"] = 0x039B,
  ["Mu"] = 0x039C,
  ["Nu"] = 0x039D,
  ["Xi"] = 0x039E,
  ["Omicron"] = 0x039F,
  ["Pi"] = 0x03A0,
  ["Rho"] = 0x03A1,
  ["Sigma"] = 0x03A3,
  ["Tau"] = 0x03A4,
  ["Upsilon"] = 0x03A5,
  ["Phi"] = 0x03A6,
  ["Chi"] = 0x03A7,
  ["Psi"] = 0x03A8,
  ["Omega"] = 0x03A9,
  ["alpha"] = 0x03B1,
  ["beta"] = 0x03B2,
  ["gamma"] = 0x03B3,
  ["delta"] = 0x03B4,
  ["epsilon"] = 0x03B5,
  ["zeta"] = 0x03B6,
  ["eta"] = 0x03B7,
  ["theta"] = 0x03B8,
  ["iota"] = 0x03B9,
  ["kappa"] = 0x03BA,
  ["lambda"] = 0x03BB,
  ["mu"] = 0x03BC,
  ["nu"] = 0x03BD,
  ["xi"] = 0x03BE,
  ["omicron"] = 0x03BF,
  ["pi"] = 0x03C0,
  ["rho"] = 0x03C1,
  ["sigmaf"] = 0x03C2,
  ["sigma"] = 0x03C3,
  ["tau"] = 0x03C4,
  ["upsilon"] = 0x03C5,
  ["phi"] = 0x03C6,
  ["chi"] = 0x03C7,
  ["psi"] = 0x03C8,
  ["omega"] = 0x03C9,
  ["thetasym"] = 0x03D1,
  ["upsih"] = 0x03D2,
  ["piv"] = 0x03D6,
  ["ensp"] = 0x2002,
  ["emsp"] = 0x2003,
  ["thinsp"] = 0x2009,
  ["ndash"] = 0x2013,
  ["mdash"] = 0x2014,
  ["lsquo"] = 0x2018,
  ["rsquo"] = 0x2019,
  ["sbquo"] = 0x201A,
  ["ldquo"] = 0x201C,
  ["rdquo"] = 0x201D,
  ["bdquo"] = 0x201E,
  ["dagger"] = 0x2020,
  ["Dagger"] = 0x2021,
  ["bull"] = 0x2022,
  ["hellip"] = 0x2026,
  ["permil"] = 0x2030,
  ["prime"] = 0x2032,
  ["Prime"] = 0x2033,
  ["lsaquo"] = 0x2039,
  ["rsaquo"] = 0x203A,
  ["oline"] = 0x203E,
  ["frasl"] = 0x2044,
  ["euro"] = 0x20AC,
  ["image"] = 0x2111,
  ["weierp"] = 0x2118,
  ["real"] = 0x211C,
  ["trade"] = 0x2122,
  ["alefsym"] = 0x2135,
  ["larr"] = 0x2190,
  ["uarr"] = 0x2191,
  ["rarr"] = 0x2192,
  ["darr"] = 0x2193,
  ["harr"] = 0x2194,
  ["crarr"] = 0x21B5,
  ["lArr"] = 0x21D0,
  ["uArr"] = 0x21D1,
  ["rArr"] = 0x21D2,
  ["dArr"] = 0x21D3,
  ["hArr"] = 0x21D4,
  ["forall"] = 0x2200,
  ["part"] = 0x2202,
  ["exist"] = 0x2203,
  ["empty"] = 0x2205,
  ["nabla"] = 0x2207,
  ["isin"] = 0x2208,
  ["notin"] = 0x2209,
  ["ni"] = 0x220B,
  ["prod"] = 0x220F,
  ["sum"] = 0x2211,
  ["minus"] = 0x2212,
  ["lowast"] = 0x2217,
  ["radic"] = 0x221A,
  ["prop"] = 0x221D,
  ["infin"] = 0x221E,
  ["ang"] = 0x2220,
  ["and"] = 0x2227,
  ["or"] = 0x2228,
  ["cap"] = 0x2229,
  ["cup"] = 0x222A,
  ["int"] = 0x222B,
  ["there4"] = 0x2234,
  ["sim"] = 0x223C,
  ["cong"] = 0x2245,
  ["asymp"] = 0x2248,
  ["ne"] = 0x2260,
  ["equiv"] = 0x2261,
  ["le"] = 0x2264,
  ["ge"] = 0x2265,
  ["sub"] = 0x2282,
  ["sup"] = 0x2283,
  ["nsub"] = 0x2284,
  ["sube"] = 0x2286,
  ["supe"] = 0x2287,
  ["oplus"] = 0x2295,
  ["otimes"] = 0x2297,
  ["perp"] = 0x22A5,
  ["sdot"] = 0x22C5,
  ["lceil"] = 0x2308,
  ["rceil"] = 0x2309,
  ["lfloor"] = 0x230A,
  ["rfloor"] = 0x230B,
  ["lang"] = 0x27E8,
  ["rang"] = 0x27E9,
  ["loz"] = 0x25CA,
  ["spades"] = 0x2660,
  ["clubs"] = 0x2663,
  ["hearts"] = 0x2665,
  ["diams"] = 0x2666,
}

--- Given a string of decimal digits, returns a UTF-8 encoded
-- string encoding a unicode character.
function M.dec_entity(s)
  return utf8.char(tonumber(s))
end

--- Given a string of hexadecimal digits, returns a UTF-8 encoded
-- string encoding a unicode character.
function M.hex_entity(s)
  return utf8.char(tonumber("0x"..s))
end

--- Given a character entity name (like `ouml`), returns a UTF-8 encoded
-- string encoding a unicode character.
function M.char_entity(s)
  local n = character_entities[s]
  return utf8.char(n)
end

return M
 end)
package.preload['lunamark.writer'] = (function (...)
-- (c) 2009-2011 John MacFarlane.  Released under MIT license.
-- See the file LICENSE in the source for details.

--- Provides access to all lunamark writers without preloading
-- them.  Writer modules are loaded only when needed.
--
--     local writers = require("lunamark.writer")
--     local htmlwriter = writers.html       -- html writer loaded now
--     local myformat = 'latex'
--     local mywriter = writers[myformat]    -- latex writer loaded now

local G = {}

setmetatable(G,{ __index = function(t,name)
                             local mod = require("lunamark.writer." .. name)
                             rawset(t,name,mod)
                             return t[name]
                            end })

return G
 end)
package.preload['lunamark.writer.generic'] = (function (...)
-- (c) 2009-2011 John MacFarlane. Released under MIT license.
-- See the file LICENSE in the source for details.

--- Generic writer for lunamark.
-- This serves as generic documentation for lunamark writers,
-- which all export a table with the same functions defined.
--
-- New writers can simply modify the generic writer: for example,
--
--     local Xml = generic.new(options)
--
--     Xml.linebreak = "<linebreak />"
--
--     local escaped = {
--          ["<" ] = "&lt;",
--          [">" ] = "&gt;",
--          ["&" ] = "&amp;",
--          ["\"" ] = "&quot;",
--          ["'" ] = "&#39;"
--     }
--
--     function Xml.string(s)
--       return s:gsub(".",escaped)
--     end

local util = require("lunamark.util")
local M = {}
local W = {}

local meta = {}
meta.__index =
  function(_, key)
    io.stderr:write(string.format("WARNING: Undefined writer function '%s'\n",key))
    return (function(...) return table.concat(arg," ") end)
  end
setmetatable(W, meta)

--- Returns a table with functions defining a generic lunamark writer,
-- which outputs plain text with no formatting.  `options` is an optional
-- table with the following fields:
--
-- `layout`
-- :   `minimize` (no space between blocks)
-- :   `compact` (no extra blank lines between blocks)
-- :   `default` (blank line between blocks)
function M.new(options)

--- The table contains the following fields:

  options = options or {}
  local metadata = {}

  --- Set metadata field `key` to `val`.
  function W.set_metadata(key, val)
    metadata[key] = val
    return ""
  end

  --- Add `val` to an array in metadata field `key`.
  function W.add_metadata(key, val)
    local cur = metadata[key]
    if type(cur) == "table" then
      table.insert(cur,val)
    elseif cur then
      metadata[key] = {cur, val}
    else
      metadata[key] = {val}
    end
  end

  --- Return metadata table.
  function W.get_metadata()
    return metadata
  end

  --- A space (string).
  W.space = " "

  --- Setup tasks at beginning of document.
  function W.start_document()
    return ""
  end

  --- Finalization tasks at end of document.
  function W.stop_document()
    return ""
  end

  --- Plain text block (not formatted as a pragraph).
  function W.plain(s)
    return s
  end

  --- A line break (string).
  W.linebreak = "\n"

  --- Line breaks to use between block elements.
  W.interblocksep = "\n\n"

  --- Line breaks to use between a container (like a `<div>`
  -- tag) and the adjacent block element.
  W.containersep = "\n"

  if options.layout == "minimize" then
    W.interblocksep = ""
    W.containersep = ""
  elseif options.layout == "compact" then
    W.interblocksep = "\n"
    W.containersep = "\n"
  end

  --- Ellipsis (string).
  W.ellipsis = "…"

  --- Em dash (string).
  W.mdash = "—"

  --- En dash (string).
  W.ndash = "–"

  --- String in curly single quotes.
  function W.singlequoted(s)
    return {"‘", s, "’"}
  end

  --- String in curly double quotes.
  function W.doublequoted(s)
    return {"“", s, "”"}
  end

  --- String, escaped as needed for the output format.
  function W.string(s)
    return s
  end

  --- Inline (verbatim) code.
  function W.code(s)
    return s
  end

  --- A link with link text `label`, uri `uri`,
  -- and title `title`.
  function W.link(label, uri, title)
    return lab
  end

  --- An image link with alt text `label`,
  -- source `src`, and title `title`.
  function W.image(label, src, title)
    return lab
  end

  --- A paragraph.
  function W.paragraph(s)
    return s
  end

  --- A bullet list with contents `items` (an array).  If
  -- `tight` is true, returns a "tight" list (with
  -- minimal space between items).
  function W.bulletlist(items,tight)
    return table.concat(items,W.interblocksep)
  end

  --- An ordered list with contents `items` (an array). If
  -- `tight` is true, returns a "tight" list (with
  -- minimal space between items). If optional
  -- number `startnum` is present, use it as the
  -- number of the first list item.
  function W.orderedlist(items,tight,startnum)
    return table.concat(items,W.interblocksep)
  end

  --- Inline HTML.
  function W.inline_html(s)
    return ""
  end

  --- Display HTML (HTML block).
  function W.display_html(s)
    return ""
  end

  --- Emphasized text.
  function W.emphasis(s)
    return s
  end

  --- Strongly emphasized text.
  function W.strong(s)
    return s
  end

  --- Block quotation.
  function W.blockquote(s)
    return s
  end

  --- Verbatim block.
  function W.verbatim(s)
    return s
  end

  --- Header level `level`, with text `s`.
  function W.header(s, level)
    return s
  end

  --- Horizontal rule.
  W.hrule = ""

  --- A footnote or endnote.
  function W.note(contents)
    return contents
  end

  --- A definition list. `items` is an array of tables,
  -- each of the form `{ term = t, definitions = defs, tight = tight }`,
  -- where `t` is a string and `defs` is an array of strings.
  -- `tight` is a boolean, true if it is a tight list.
  function W.definitionlist(items, tight)
    local buffer = {}
    for _,item in ipairs(items) do
      buffer[#buffer + 1] = item.t
      buffer[#buffer + 1] = table.concat(item.definitions, W.interblocksep)
    end
    return table.concat(buffer,W.interblocksep)
  end

  --- A cosmo template to be used in producing a standalone document.
  -- `$body` is replaced with the document body, `$title` with the
  -- title, and so on.
  W.template = [[
$body
]]

  return util.table_copy(W)
end

return M
 end)
package.preload['lunamark.writer.xml'] = (function (...)
-- (c) 2009-2011 John MacFarlane. Released under MIT license.
-- See the file LICENSE in the source for details.

--- Generic XML writer for lunamark.
-- It extends [lunamark.writer.generic] and is extended by
-- [lunamark.writer.html] and [lunamark.writer.docbook].

local M = {}

local generic = require("lunamark.writer.generic")
local util = require("lunamark.util")

--- Returns a new XML writer.
-- For a list of fields, see [lunamark.writer.generic].
function M.new(options)
  local options = options or {}
  local Xml = generic.new(options)

  Xml.container = "section"
  --  {1,2} means: a second level header inside a first-level
  local header_level_stack = {}

  function Xml.start_section(level)
    header_level_stack[#header_level_stack + 1] = level
    return "<" .. Xml.container .. ">"
  end

  function Xml.stop_section(level)
    local len = #header_level_stack
    if len == 0 then
      return ""
    else
      local last = header_level_stack[len]
      local res = {}
      while last >= level do
        header_level_stack[len] = nil
        table.insert(res, "</" .. Xml.container .. ">")
        len = len - 1
        last = (len > 0 and header_level_stack[len]) or 0
      end
      return table.concat(res, Xml.containersep)
    end
  end

  Xml.linebreak = "<linebreak />"

  local escape = util.escaper {
     ["<" ] = "&lt;",
     [">" ] = "&gt;",
     ["&" ] = "&amp;",
     ["\"" ] = "&quot;",
     ["'" ] = "&#39;"
  }

  Xml.string = escape

  return Xml
end

return M
 end)
package.preload['lunamark.writer.docbook'] = (function (...)
-- (c) 2009-2011 John MacFarlane. Released under MIT license.
-- See the file LICENSE in the source for details.

--- DocBook writer for lunamark.
-- Extends [lunamark.writer.xml].

local M = {}

local xml = require("lunamark.writer.xml")
local util = require("lunamark.util")
local gsub = string.gsub
local map, intersperse = util.map, util.intersperse

--- Returns a new DocBook writer.
-- For a list of all the fields, see [lunamark.writer.generic].
function M.new(options)
  local options = options or {}
  local Docbook = xml.new(options)

  Docbook.linebreak = "<literallayout>&#xA;</literallayout>"

  function Docbook.code(s)
    return {"<literal>", Docbook.string(s), "</literal>"}
  end

  function Docbook.link(lab,src,tit)
    local titattr
    -- if tit and string.len(tit) > 0
    --   then titattr = format(" xlink:title=\"%s\"", Docbook.string(tit))
    --   else titattr = ""
    --   end
    return {"<ulink url=\"", Docbook.string(src), "\">", lab, "</ulink>"}
  end

  function Docbook.image(lab,src,tit)
    local titattr, altattr
    if tit and string.len(tit) > 0
       then titattr = "<objectinfo><title>" .. Docbook.string(tit) .. "</title></objectinfo>"
       else titattr = ""
       end
    return "<inlinemediaobject><imageobject>" .. titattr .. "<imagedata fileref=\"" .. Docbook.string(src) .. "\" /></imageobject></inlinemediaobject>"
  end

  function Docbook.paragraph(s)
    return {"<para>", s, "</para>"}
  end

  Docbook.plain = Docbook.paragraph

  local function listitem(s)
    return {"<listitem>", s, "</listitem>"}
  end

  function Docbook.bulletlist(items)
    return {"<itemizedlist>", Docbook.containersep, intersperse(map(items, listitem), Docbook.containersep), Docbook.containersep, "</itemizedlist>"}
  end

  function Docbook.orderedlist(items)
    return {"<orderedlist>", Docbook.containersep, intersperse(map(items, listitem), Docbook.containersep), Docbook.containersep, "</orderedlist>"}
  end

  function Docbook.inline_html(s)
    return s
  end

  function Docbook.display_html(s)
    return s
  end

  function Docbook.emphasis(s)
    return {"<emphasis>", s, "</emphasis>"}
  end

  function Docbook.strong(s)
    return {"<emphasis role=\"strong\">", s, "</emphasis>"}
  end

  function Docbook.blockquote(s)
    return {"<blockquote>", Docbook.containersep, s, Docbook.containersep, "</blockquote>"}
  end

  function Docbook.verbatim(s)
    return {"<programlisting>", Docbook.string(s), "</programlisting>"}
  end

  function Docbook.stop_document()
    local stop = Docbook.stop_section(1) -- close section containers
    if stop ~= "" then stop = Docbook.containersep .. stop end
    return stop
  end

  function Docbook.header(s,level)
    local sep = {}
    local stop
    if options.slides or options.containers then
      local lev = (options.slides and 1) or level
      local stop = Docbook.stop_section(lev)
      if stop ~= "" then
        stop = stop .. Docbook.interblocksep
      end
      sep = {stop, Docbook.start_section(lev), Docbook.containersep}
    end
    return {sep, "<title>", s, "</title>"}
  end

  Docbook.hrule = ""

  function Docbook.note(contents)
    return {"<footnote>", Docbook.containersep, contents, Docbook.containersep, "</footnote>"}
  end

  function Docbook.definitionlist(items)
    local buffer = {}
    for _,item in ipairs(items) do
      local defs = {}
      for _,def in ipairs(item.definitions) do
        defs[#defs + 1] = {"<listitem>", Docbook.containersep, def, Docbook.containersep, "</listitem>"}
      end
      buffer[#buffer + 1] = {"<varlistentry>", Docbook.containersep, "<term>", item.term, "</term>", Docbook.containersep, intersperse(defs, Docbook.containersep), Docbook.containersep, "</varlistentry>"}
    end
    local contents = intersperse(buffer, Docbook.containersep)
    return {"<variablelist>", Docbook.containersep, contents, Docbook.containersep, "</variablelist>"}
  end

  Docbook.template = [[
<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE article PUBLIC "-//OASIS//DTD DocBook XML V4.4//EN" "http://www.oasis-open.org/docbook/xml/4.4/docbookx.dtd">
<article>
<articleinfo>
<title>$title</title>
</articleinfo>
$body
</article>
]]

  return Docbook
end

return M
 end)
package.preload['lunamark.writer.html'] = (function (...)
-- (c) 2009-2011 John MacFarlane. Released under MIT license.
-- See the file LICENSE in the source for details.

--- HTML writer for lunamark.
-- Extends [lunamark.writer.xml].

local M = {}

local xml = require("lunamark.writer.xml")
local util = require("lunamark.util")
local gsub = string.gsub
local flatten, intersperse, map = util.flatten, util.intersperse, util.map

--- Return a new HTML writer.
-- For a list of all fields in the writer, see [lunamark.writer.generic].
--
--`options` is a table that can contain the following fields:
--
-- `containers`
-- :   Put sections in `<div>` tags.
--
-- `slides`
-- :   Do not allow containers to nest; when a subsection begins,
--     close the section's container and start a new one.
--
-- `layout`
-- :   `minimize` removes semantically insignificant white space.
-- :   `compact` removes unneeded blank lines.
-- :   `default` puts blank lines between block elements.
function M.new(options)
  local options = options or {}
  local Html = xml.new(options)
  local options = options or {}
  local endnotes = {}
  local containersep = Html.containersep
  local interblocksep = Html.interblocksep

  Html.container = "div"
  Html.linebreak = "<br/>"

  function Html.code(s)
    return {"<code>", Html.string(s), "</code>"}
  end

  function Html.link(lab,src,tit)
    local titattr
    if type(tit) == "string" and #tit > 0
       then titattr = " title=\"" .. Html.string(tit) .. "\""
       else titattr = ""
       end
    return {"<a href=\"", Html.string(src), "\"", titattr, ">", lab, "</a>"}
  end

  function Html.image(lab,src,tit)
    local titattr, altattr
    if type(tit) == "string" and #tit > 0
       then titattr = " title=\"" .. Html.string(tit) .. "\""
       else titattr = ""
       end
    return {"<img src=\"", Html.string(src), "\" alt=\"", lab, "\"", titattr, " />"}
  end

  function Html.paragraph(s)
    return {"<p>", s, "</p>"}
  end

  local function listitem(s)
    return {"<li>", s, "</li>"}
  end

  function Html.bulletlist(items,tight)
    return {"<ul>", containersep, intersperse(map(items, listitem), containersep), containersep, "</ul>"}
  end

  function Html.orderedlist(items,tight,startnum)
    local start = ""
    if startnum and startnum ~= 1 then
      start = " start=\"" .. startnum .. "\""
    end
    return {"<ol", start, ">", containersep, intersperse(map(items, listitem), containersep), containersep, "</ol>"}
  end

  function Html.inline_html(s)
    return s
  end

  function Html.display_html(s)
    return s
  end

  function Html.emphasis(s)
    return {"<em>", s, "</em>"}
  end

  function Html.strong(s)
    return {"<strong>", s, "</strong>"}
  end

  function Html.blockquote(s)
    return {"<blockquote>", containersep, s, containersep, "</blockquote>"}
  end

  function Html.verbatim(s)
    return {"<pre><code>", Html.string(s), "</code></pre>"}
  end

  function Html.header(s,level)
    local sep = {}
    local stop
    if options.slides or options.containers then
      local lev = (options.slides and 1) or level
      local stop = Html.stop_section(lev)
      if stop ~= "" then
        stop = stop .. Html.interblocksep
      end
      sep = {stop, Html.start_section(lev), Html.containersep}
    end
    return {sep, "<h", level, ">", s, "</h", level, ">"}
  end

  Html.hrule = "<hr />"

  function Html.note(contents)
    return function()
      local num = #endnotes + 1
      local backref = ' <a href="#fnref' .. num .. '" class="footnoteBackLink">↩</a>'
      contentsf = flatten(contents)
      if contentsf[#contentsf] == "</p>" then
        table.insert(contentsf, #contentsf, backref)
      else
        contentsf[#contentsf + 1] = backref
      end
      endnotes[num] = {'<li id="fn', num, '">', contentsf, '</li>'}
      return {'<sup><a href="#fn', num, '" class="footnoteRef" id="fnref', num, '">', num, '</a></sup>'}
    end
  end

  function Html.start_document()
    endnotes = {}
    return ""
  end

  function Html.stop_document()
    return function()
      local stop = Html.stop_section(1) -- close section containers
      if stop ~= "" then stop = Html.containersep .. stop end
      if #endnotes == 0 then
        return stop
      else
        return {stop, interblocksep, '<hr />', interblocksep, '<ol class="notes">',
           containersep, intersperse(endnotes, interblocksep), containersep, '</ol>'}
      end
    end
  end

  function Html.definitionlist(items, tight)
    local buffer = {}
    local sep
    if tight then sep = "" else sep = Html.containersep end
    for _,item in ipairs(items) do
      local defs = {}
      for _,def in ipairs(item.definitions) do
        defs[#defs + 1] = {"<dd>", sep, def, sep, "</dd>"}
      end
      buffer[#buffer + 1] = {"<dt>", item.term, "</dt>", containersep, intersperse(defs, containersep)}
    end
    return {"<dl>", containersep, intersperse(buffer, containersep), containersep, "</dl>"}
  end

  Html.template = [[
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>$title</title>
</head>
<body>
$body
</body>
</html>
]]

  return Html
end

return M
 end)
package.preload['lunamark.writer.html5'] = (function (...)
-- (c) 2009-2011 John MacFarlane. Released under MIT license.
-- See the file LICENSE in the source for details.

--- HTML 5 writer for lunamark.
-- Extends [lunamark.writer.html], but uses `<section>` tags for sections
-- if `options.containers` is true.

local M = {}

local util = require("lunamark.util")
local html = require("lunamark.writer.html")
local format = string.format

--- Returns a new HTML 5 writer.
-- `options` is as in `lunamark.writer.html`.
-- For a list of fields, see [lunamark.writer.generic].
function M.new(options)
  local options = options or {}
  local Html5 = html.new(options)

  Html5.container = "section"

  Html5.template = [[
<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>$title</title>
</head>
<body>
$body
</body>
</html>
]]

  return Html5
end

return M
 end)
package.preload['lunamark.writer.dzslides'] = (function (...)
-- (c) 2009-2011 John MacFarlane. Released under MIT license.
-- See the file LICENSE in the source for details.

--- dzslides writer for lunamark.
-- dzslides is a lightweight HTML5 slide engine by Paul Rouget:
-- <https://github.com/paulrouget/dzslides>.
-- This writer extends [lunamark.writer.html5].

local M = {}

local util = require("lunamark.util")
local html5 = require("lunamark.writer.html5")
local format = string.format

--- Returns a new dzslides writer.
-- `options` is as in `lunamark.writer.html5`. However,
-- `options.slides` is ignored and set to `true`.
-- For a list of fields, see [lunamark.writer.generic].
function M.new(options)
  local options = options or {}
  options.slides = true
  local DZSlides = html5.new(options)

  DZSlides.template = [===[
<!DOCTYPE html>
<head>
<meta charset="utf-8">
<title>$title</title>
</head>
$if{ title }[=[
<section>
  <h1>$title</h1>
</section>
]=]
$body
<!-- Your Style -->
<!-- Define the style of you presentation -->

<!-- Maybe a font from http://www.google.com/webfonts ? -->
<link href='http://fonts.googleapis.com/css?family=Oswald' rel='stylesheet'>

<style>
  html { background-color: black; }
  body { background-color: white; }
  /* A section is a slide. Its size is 800x600, and this will never change */
  section {
      /* The font from Google */
      font-family: 'Oswald', arial, serif;
      font-size: 2em;
      padding-left: 1em;
  }
  h1, h2, h3 {
      margin-top: 50px;
      text-align: center;
  }
  ul {
      margin-left: 40px;
  }
  a { color: #FF0066; } a:hover { text-decoration: underline; }
  footer { position: absolute; bottom: 50px; right: 50px; }

  .images {
    margin: 20px;
    text-align: center;
  }

  /* Transition effect */
  /* Feel free to change the transition effect for original
     animations. See here:
     https://developer.mozilla.org/en/CSS/CSS_transitions
     How to use CSS3 Transitions: */
  section {
      -moz-transition: left 400ms linear 0s;
      -webkit-transition: left 400ms linear 0s;
      -ms-transition: left 400ms linear 0s;
      transition: left 400ms linear 0s;
  }

  /* Before */
  section { left: -150%; }
  /* Now */
  section[aria-selected] { left: 0; }
  /* After */
  section[aria-selected] ~ section { left: +150%; }

  /* Increment with lists */
  .incremental > * { opacity: 1; }
  .incremental > *[aria-selected] { color: red; opacity: 1; }
  .incremental > *[aria-selected] ~ * { opacity: 0.2; }
  /* Increment with images */
  .incremental > img[aria-selected] { box-shadow: 0 0 10px #000 }

</style>


<!-- {{{{ *****************  DZSlides CORE 2.0b1 *************************** -->
<!-- *********************************************************************** -->
<!-- *********************************************************************** -->
<!-- *********************************************************************** -->
<!-- *********************************************************************** -->

<!-- This block of code is not supposed to be edited, but if you want to change the behavior of the slides, feel free to hack it ;) -->

<!-- Default Style -->
<style>
  * { margin: 0; padding: 0; }
  details { display: none; }
  body {
    width: 800px; height: 600px;
    margin-left: -400px; margin-top: -300px;
    position: absolute; top: 50%; left: 50%;
    overflow: hidden;
  }
  html {
    overflow: hidden;
  }
  section {
    position: absolute;
    pointer-events: none;
    width: 95%; height: 100%;
  }
  section[aria-selected] { pointer-events: auto; }
  body { display: none; }
  body.loaded { display: block; }
  .incremental {visibility: hidden}
  .incremental[active] {visibility: visible}
</style>

<script>
  var friendWindows = [];
  var idx, step;
  var slides;

  /* main() */

  window.onload = function() {
    slides = document.querySelectorAll("body > section");
    onhashchange();
    document.body.className = "loaded";
    setupTouchEvents();
    onresize();
  }

  /* Handle keys */

  window.onkeydown = function(e) {
    // Don't intercept keyboard shortcuts
    if (e.altKey || e.ctrlKey || e.metaKey || e.shiftKey) {
      return;
    }
    if ( e.keyCode == 37 // left arrow
      || e.keyCode == 38 // up arrow
      || e.keyCode == 33 // page up
    ) {
      e.preventDefault();
      back();
    }
    if ( e.keyCode == 39 // right arrow
      || e.keyCode == 40 // down arrow
      || e.keyCode == 34 // page down
    ) {
      e.preventDefault();
      forward();
    }
    if (e.keyCode == 35) { // end
      e.preventDefault();
      end();
    }
    if (e.keyCode == 36) { // home
      e.preventDefault();
      start();
    }

    if ( e.keyCode == 32) { // space
        e.preventDefault();
        toggleContent();
    }
  }

  /* Touch Events */

  function setupTouchEvents() {
    var orgX, newX;
    var tracking = false;

    var db = document.body;
    db.addEventListener("touchstart", start, false);
    db.addEventListener("touchmove", move, false);

    function start(e) {
      e.preventDefault();
      tracking = true;
      orgX = e.changedTouches[0].pageX;
    }

    function move(e) {
      if (!tracking) return;
      newX = e.changedTouches[0].pageX;
      if (orgX - newX > 100) {
        tracking = false;
        forward();
      } else {
        if (orgX - newX < -100) {
          tracking = false;
          back();
        }
      }
    }
  }

  /* Adapt the size of the slides to the window */

  window.onresize = function() {
    var db = document.body;
    var sx = db.clientWidth / window.innerWidth;
    var sy = db.clientHeight / window.innerHeight;
    var transform = "scale(" + (1/Math.max(sx, sy)) + ")";
    db.style.MozTransform = transform;
    db.style.WebkitTransform = transform;
    db.style.OTransform = transform;
    db.style.msTransform = transform;
    db.style.transform = transform;
  }
  function getDetails(idx) {
    var s = document.querySelector("section:nth-of-type("+ idx +")");
    var d = s.querySelector("details");
    return d?d.innerHTML:"";
  }
  window.onmessage = function(e) {
    var msg = e.data;
    var win = e.source;
    if (msg === "register") {
      friendWindows.push(win);
      win.postMessage(JSON.stringify({method: "registered", title: document.title, count: slides.length}), "*");
      win.postMessage(JSON.stringify({method: "newslide", details: getDetails(idx), idx: idx}), "*");
      return;
    }
    if (msg === "back") back();
    if (msg === "forward") forward();
    if (msg === "toggleContent") toggleContent();
    // setSlide(42)
    var r = /setSlide\((\d+)\)/.exec(msg);
    if (r) {
        setSlide(r[1]);
    }
  }

  /* If a Video is present in this new slide, play it.
     If a Video is present in the previous slide, stop it. */

  function toggleContent() {
    var s = document.querySelector("section[aria-selected]");
    if (s) {
        var video = s.querySelector("video");
        if (video) {
            if (video.ended || video.paused) {
                video.play();
            } else {
                video.pause();
            }
        }
    }
  }

  /* If the user change the slide number in the URL bar, jump
     to this slide. */

  function setCursor(aIdx, aStep) {
    aStep = (aStep != 0 && typeof aStep !== "undefined") ? "." + aStep : "";
    window.location.hash = "#" + aIdx + aStep;
  }
  window.onhashchange = function(e) {
    var cursor = window.location.hash.split("#"), newidx = 1, newstep = 0;
    if (cursor.length == 2) {
      newidx = ~~cursor[1].split(".")[0];
      newstep = ~~cursor[1].split(".")[1];
    }
    if (newidx != idx) setSlide(newidx);
    setIncremental(newstep);
  }

  /* Slide controls */

  function back() {
    if (idx == 1 && step == 0) return;
    if (step == 0)
      setCursor(idx - 1, slides[idx - 2].querySelectorAll('.incremental > *').length);
    else 
      setCursor(idx, step - 1);
  }
  function forward() {
    if (idx >= slides.length && step >= slides[idx - 1].querySelectorAll('.incremental > *').length) return;
    if (step >= slides[idx - 1].querySelectorAll('.incremental > *').length)
      setCursor(idx + 1, 0);
    else
      setCursor(idx, step + 1);
  }
  function start() {
    setCursor(1, 0);
  }
  function end() {
    var lastIdx = slides.length;
    var lastStep = slides[lastIdx - 1].querySelectorAll('.incremental > *').length;
    setCursor(lastIdx, lastStep);
  }

  function setSlide(aIdx) {
    idx = aIdx;
    var old = document.querySelector("section[aria-selected]");
    var next = document.querySelector("section:nth-of-type("+ idx +")");
    if (old) {
      old.removeAttribute("aria-selected");
      var video = old.querySelector("video");
      if (video) { video.pause(); }
    }
    if (next) {
      next.setAttribute("aria-selected", "true");
      var video = next.querySelector("video");
      if (video) { video.play(); }
    } else {
      idx = 0;
      for (var i = 0, l = slides.length; i < l; i++) {
          if (slides[i].hasAttribute("aria-selected")) {
              idx = i + 1;
          }
      }
    }
    for (var i = 0, l = friendWindows.length; i < l; i++) {
        friendWindows[i].postMessage(JSON.stringify({method: "newslide", details: getDetails(idx), idx: idx}), "*");
    }
  }
  function setIncremental(aStep) {
    step = aStep;
    var old = slides[idx-1].querySelector('.incremental > *[aria-selected]');
    if (old)
      old.removeAttribute('aria-selected');
    var incrementals = slides[idx-1].querySelectorAll('.incremental');
    if (step == 0) {
      for (var i = 0; i < incrementals.length; i++) {
        incrementals[i].removeAttribute('active');
      }
      return;
    }
    var next = slides[idx-1].querySelectorAll('.incremental > *')[step-1];
    if (next) {
      next.setAttribute('aria-selected', true);
      next.parentNode.setAttribute('active', true);
      var found = false;
      for (var i = 0; i < incrementals.length; i++) {
        if (incrementals[i] != next.parentNode)
          if (found)
            incrementals[i].removeAttribute('active');
          else
            incrementals[i].setAttribute('active', true);
        else
          found = true;
      }
    } else {
      setCursor(idx, 0);
    }
    return next;
  }
</script>
<!-- vim: set fdm=marker: }}} -->
]===]

  return DZSlides
end

return M
 end)
package.preload['lunamark.writer.tex'] = (function (...)
-- (c) 2009-2011 John MacFarlane. Released under MIT license.
-- See the file LICENSE in the source for details.

--- Generic TeX writer for lunamark.
-- It extends [lunamark.writer.generic] and is extended by
-- [lunamark.writer.latex] and [lunamark.writer.context].

local M = {}

local util = require("lunamark.util")
local generic = require("lunamark.writer.generic")
local entities = require("lunamark.entities")
local format = string.format

--- Returns a new TeX writer.
-- For a list ofy fields, see [lunamark.writer.generic].
function M.new(options)
  local options = options or {}
  local TeX = generic.new(options)

  TeX.interblocksep = "\n\n"  -- insensitive to layout

  TeX.containersep = "\n"

  TeX.linebreak = "\\\\"

  TeX.ellipsis = "\ldots{}"

  TeX.mdash = "---"

  TeX.ndash = "--"

  function TeX.singlequoted(s)
    return {"`", s, "'"}
  end

  function TeX.doublequoted(s)
    return {"``", s, "''"}
  end

  TeX.escaped = {
     ["{"] = "\\{",
     ["}"] = "\\}",
     ["$"] = "\\$",
     ["%"] = "\\%",
     ["&"] = "\\&",
     ["_"] = "\\_",
     ["#"] = "\\#",
     ["^"] = "\\^{}",
     ["\\"] = "\\char92{}",
     ["~"] = "\\char126{}",
     ["|"] = "\\char124{}",
     ["<"] = "\\char60{}",
     [">"] = "\\char62{}",
     ["["] = "{[}", -- to avoid interpretation as optional argument
     ["]"] = "{]}",
   }

  local str_escaped = {
     ["\226\128\156"] = "``",
     ["\226\128\157"] = "''",
     ["\226\128\152"] = "`",
     ["\226\128\153"] = "'",
     ["\226\128\148"] = "---",
     ["\226\128\147"] = "--",
     ["\194\160"]     = "~",
   }

  local escaper = util.escaper(TeX.escaped, str_escaped)

  TeX.string = escaper

  function TeX.inline_html(s)
    return ""
  end

  function TeX.display_html(s)
    return ""
  end

  function TeX.paragraph(s)
    return s
  end

  return TeX
end

return M
 end)
package.preload['lunamark.writer.latex'] = (function (...)
-- (c) 2009-2011 John MacFarlane. Released under MIT license.
-- See the file LICENSE in the source for details.

--- LaTeX writer for lunamark.
-- Extends the [lunamark.writer.tex].

local M = {}

local tex = require("lunamark.writer.tex")
local util = require("lunamark.util")
local intersperse, map = util.intersperse, util.map

--- Returns a new LaTeX writer.
-- For a list of fields in the writer, see [lunamark.writer.generic].
function M.new(options)
  local options = options or {}
  local LaTeX = tex.new(options)

  function LaTeX.code(s)
    return {"\\texttt{", LaTeX.string(s), "}"}
  end

  function LaTeX.link(lab,src,tit)
    return {"\\href{", LaTeX.string(src), "}{", lab}
  end

  function LaTeX.image(lab,src,tit)
    return {"\\includegraphics{",LaTeX.string(src), "}"}
  end

  local function listitem(s)
    return {"\\item ", s}
  end

  function LaTeX.bulletlist(items)
    return {"\\begin{itemize}\n", intersperse(map(items, listitem), "\n"), "\n\\end{itemize}"}
  end

  function LaTeX.orderedlist(items)
    return {"\\begin{enumerate}\n", intersperse(map(items, listitem), "\n"), "\n\\end{enumerate}"}
  end

  function LaTeX.emphasis(s)
    return {"\\emph{", s, "}"}
  end

  function LaTeX.strong(s)
    return {"\\textbf{", s, "}"}
  end

  function LaTeX.blockquote(s)
    return {"\\begin{quote}\n", s, "\n\\end{quote}"}
  end

  function LaTeX.verbatim(s)
    return {"\\begin{verbatim}\n", s, "\\end{verbatim}"}
  end

  function LaTeX.header(s,level)
    local cmd
    if level == 1 then
      cmd = "\\section"
    elseif level == 2 then
      cmd = "\\subsection"
    elseif level == 3 then
      cmd = "\\subsubsection"
    elseif level == 4 then
      cmd = "\\paragraph"
    elseif level == 5 then
      cmd = "\\subparagraph"
    else
      cmd = ""
    end
    return {cmd, "{", s, "}"}
  end

  LaTeX.hrule = "\\hspace{\\fill}\\rule{.6\\linewidth}{0.4pt}\\hspace{\\fill}"

  function LaTeX.note(contents)
    return {"\\footnote{", contents, "}"}
  end

  function LaTeX.definitionlist(items)
    local buffer = {}
    for _,item in ipairs(items) do
      buffer[#buffer + 1] = {"\\item[", item.term, "]\n", intersperse(item.definitions, LaTeX.interblocksep)}
    end
    local contents = intersperse(buffer, LaTeX.containersep)
    return {"\\begin{description}\n", contents, "\n\\end{description}"}
  end

  LaTeX.template = [===[
\documentclass{article}
\usepackage{amssymb,amsmath}
\usepackage{ifxetex,ifluatex}
\ifxetex
  \usepackage{fontspec,xltxtra,xunicode}
  \defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}
\else
  \ifluatex
    \usepackage{fontspec}
    \defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}
  \else
    \usepackage[utf8]{inputenc}
  \fi
\fi
\ifxetex
  \usepackage[setpagesize=false, % page size defined by xetex
              unicode=false, % unicode breaks when used with xetex
              xetex]{hyperref}
\else
  \usepackage[unicode=true]{hyperref}
\fi
\hypersetup{breaklinks=true, pdfborder={0 0 0}}
\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt plus 2pt minus 1pt}
\setlength{\emergencystretch}{3em}  % prevent overfull lines
\setcounter{secnumdepth}{0}

\title{$title}
\author{$sepby{author}[=[$it]=][=[ \and ]=]}
\date{$date}

\begin{document}

$if{ title }[[\maketitle
]]
$body

\end{document}
]===]

  return LaTeX
end

return M
 end)
package.preload['lunamark.writer.context'] = (function (...)
-- (c) 2009-2011 John MacFarlane, Khaled Hosny, Hans Hagen.
-- Released under MIT license. See the file LICENSE in the source for details.

--- ConTeXt writer for lunamark.
-- Extends [lunamark.writer.tex].

local M = {}

local tex = require("lunamark.writer.tex")
local util = require("lunamark.util")
local map, intersperse = util.map, util.intersperse

--- Returns a new ConTeXt writer
-- For a list of all the fields, see [lunamark.writer.generic].
function M.new(options)
  local options = options or {}
  local ConTeXt = tex.new(options)

  -- we don't try to escape utf-8 characters in context
  local escape = util.escaper(ConTeXt.escaped)
  ConTeXt.string = escape

  function ConTeXt.singlequoted(s)
    return {"\\quote{", s, "}"}
  end

  function ConTeXt.doublequoted(s)
    return {"\\quotation{",s,"}"}
  end

  function ConTeXt.code(s)
    return {"\\type{",s,"}"}  -- escape here?
  end

  function ConTeXt.link(lab,src,tit)
    return {"\\goto{", lab, "}[url(", ConTeXt.string(src), ")]"}
  end

  function ConTeXt.image(lab,src,tit)
    return {"\\externalfigure[", ConTeXt.string(src), "]"}
  end

  local function listitem(s)
    return {"\\item ",s,"\n"}
  end

  function ConTeXt.bulletlist(items,tight)
    local opt = ""
    if tight then opt = "[packed]" end
    return {"\\startitemize", opt, "\n", map(items, listitem), "\\stopitemize"}
  end

  function ConTeXt.orderedlist(items,tight,startnum)
    local tightstr = ""
    if tight then tightstr = ",packed" end
    local opt = string.format("[%d%s]", startnum or 1, tightstr)
    return {"\\startitemize", opt, "\n", map(items, listitem), "\\stopitemize"}
  end

  function ConTeXt.emphasis(s)
    return {"{\\em ",s,"}"}
  end

  function ConTeXt.strong(s)
    return {"{\\bf ",s,"}"}
  end

  function ConTeXt.blockquote(s)
    return {"\\startblockquote\n",s,"\\stopblockquote"}
  end

  function ConTeXt.verbatim(s)
    return {"\\starttyping\n",s,"\\stoptyping"}
  end

  function ConTeXt.header(s,level)
    local cmd
    if level == 1 then
      cmd = "\\section"
    elseif level == 2 then
      cmd = "\\subsection"
    elseif level == 3 then
      cmd = "\\subsubsection"
    elseif level == 4 then
      cmd = "\\paragraph"
    elseif level == 5 then
      cmd = "\\subparagraph"
    else
      cmd = ""
    end
    return {cmd, "{", s, "}"}
  end

  ConTeXt.hrule = "\\hairline"

  function ConTeXt.note(contents)
    return {"\\footnote{", contents, "}"}
  end

  function ConTeXt.definitionlist(items)
    local buffer = {}
    for _,item in ipairs(items) do
      buffer[#buffer + 1] = {"\\startdescription{", item.term, "}\n", intersperse(item.definitions, ConTeXt.interblocksep), "\n\\stopdescription"}
    end
    return intersperse(buffer, ConTeXt.containersep)
  end

  ConTeXt.template = [===[
\startmode[*mkii]
  \enableregime[utf-8]
  \setupcolors[state=start]
\stopmode

% Enable hyperlinks
\setupinteraction[state=start, color=middleblue]

\setuppapersize [letter][letter]
\setuplayout    [width=middle,  backspace=1.5in, cutspace=1.5in,
                 height=middle, topspace=0.75in, bottomspace=0.75in]

\setuppagenumbering[location={footer,center}]

\setupbodyfont[11pt]

\setupwhitespace[medium]

\setuphead[section]      [style=\tfc]
\setuphead[subsection]   [style=\tfb]
\setuphead[subsubsection][style=\bf]

\definedescription
  [description]
  [headstyle=bold, style=normal, location=hanging, width=broad, margin=1cm]

\setupitemize[autointro]    % prevent orphan list intro
\setupitemize[indentnext=no]

\setupthinrules[width=15em] % width of horizontal rules

\setupdelimitedtext
  [blockquote]
  [before={\blank[medium]},
   after={\blank[medium]},
   indentnext=no,
  ]

\starttext
$if{ title }[=[
\startalignment[center]
\blank[2*big]
{\tfd $title}
$if{ author }[[
\blank[3*medium]
{\tfa $sepby{author}[==[$it]==][==[\crlf ]==]}
]]
$if{ date }[[
\blank[2*medium]
{\tfa $date}
]]
\blank[3*medium]
\stopalignment

]=]
$if{ toc }[[{\placecontent}
]]
$body

\stoptext
]===]

  return ConTeXt
end

return M
 end)
package.preload['lunamark.writer.groff'] = (function (...)
-- (c) 2009-2011 John MacFarlane. Released under MIT license.
-- See the file LICENSE in the source for details.

--- Generic groff writer for lunamark.
-- This is currently used as the basis for [lunamark.writer.man].
-- In principle other groff-based writers could also extend it.

local M = {}

local format = string.format
local util = require("lunamark.util")
local generic = require("lunamark.writer.generic")
local entities = require("lunamark.entities")

--- Returns a new Groff writer.
-- For a list of all fields, see [lunamark.writer.generic].
function M.new(options)
  local options = options or {}
  local Groff = generic.new(options)

  Groff.interblocksep = "\n\n"  -- insensitive to layout

  Groff.containersep = "\n"

  Groff.linebreak = ".br\n"

  Groff.ellipsis = "\\&..."

  Groff.mdash = "\\[em]"

  Groff.ndash = "\\[en]"

  function Groff.singlequoted(s)
    return {"`", s, "'"}
  end

  function Groff.doublequoted(s)
    return {"\\[lq]",s,"\\[rq]"}
  end

  Groff.escaped = {
     ["@"] = "\\@",
     ["\\"] = "\\\\",
   }

  local escaped_utf8_triplet = {
    ["\226\128\156"] = "\\[lq]",
    ["\226\128\157"] = "\\[rq]",
    ["\226\128\152"] = "`",
    ["\226\128\153"] = "'",
    ["\226\128\148"] = "\\[em]",
    ["\226\128\147"] = "\\[en]",
    ["\194\160"]     = "\\ ",
  }

  local escape = util.escaper(Groff.escaped, escaped_utf8_triplet)

  Groff.string = escape

  function Groff.inline_html(s)
  end

  function Groff.display_html(s)
  end

  function Groff.code(s)
    return {"\\f[C]", Groff.string(s), "\\f[]"}
  end

  function Groff.emphasis(s)
    return {"\\f[I]", s, "\\f[]"}
  end

  function Groff.strong(s)
    return {"\\f[B]", s, "\\f[]"}
  end

  return Groff
end

return M
 end)
package.preload['lunamark.writer.man'] = (function (...)
-- (c) 2009-2011 John MacFarlane. Released under MIT license.
-- See the file LICENSE in the source for details.

--- Groff man writer for lunamark.
-- Extends [lunamark.writer.groff].
--
-- Note: continuation paragraphs in lists are not
-- handled properly.

local M = {}

local groff = require("lunamark.writer.groff")
local util = require("lunamark.util")
local gsub = string.gsub
local map, intersperse = util.map, util.intersperse

--- Returns a new groff writer.
-- For a list of fields, see [lunamark.writer.generic].
function M.new(options)
  local options = options or {}
  local Man = groff.new(options)

  local endnotes = {}

  function Man.link(lab,src,tit)
    return {lab, "(", Man.string(src), ")"}
  end

  function Man.image(lab,src,tit)
    return {"[IMAGE (", lab, ")]"}
  end

  -- TODO handle continuations properly.
  -- pandoc does this:
  -- .IP \[bu] 2
  -- one
  -- .RS 2
  -- .PP
  -- cont
  -- .RE

  function Man.paragraph(contents)
    return {".PP\n", contents}
  end

  function Man.bulletlist(items,tight)
    return intersperse(map(items, function(s) return {".IP \\[bu] 2\n", s} end))
  end

  function Man.orderedlist(items,tight,startnum)
    local buffer = {}
    local num = startnum or 1
    for _,item in ipairs(items) do
      buffer[#buffer + 1] = {".IP \"", num, "\" 4\n", item}
      num = num + 1
    end
    return intersperse(buffer, Man.containersep)
  end

  function Man.blockquote(s)
    return {".RS\n", s, ".RE"}
  end

  function Man.verbatim(s)
    return {".IP\n.nf\n\\f[C]\n",s,".fi"}
  end

  function Man.header(s,level)
    local hcode = ".SS "
    if level == 1 then hcode = ".SH " end
    return {hcode, s}
  end

  Man.hrule = ".PP\n * * * * *"

  function Man.note(contents)
    return function()
      local num = #endnotes + 1
      endnotes[num] = {'.SS [' .. num .. ']\n', contents}
      return '[' .. tostring(num) .. ']'
    end
  end

  function Man.definitionlist(items,tight)
    local buffer = {}
    for _,item in ipairs(items) do
      if tight then
        buffer[#buffer + 1] = {".TP\n.B ", item.term, "\n", intersperse(item.definitions, "\n.RS\n.RE"), "\n.RS\n.RE"}
      else
        buffer[#buffer + 1] = {".TP\n.B ", item.term, "\n.RS\n", intersperse(item.definitions, "\n.RS\n.RE"), "\n.RE"}
      end
    end
    return intersperse(buffer, "\n")
  end

  function Man.start_document()
    endnotes = {}
    return ""
  end

  function Man.stop_document()
    return function()
      if #endnotes == 0 then
        return ""
      else
        return {'\n.SH NOTES\n', intersperse(endnotes, '\n')}
      end
    end
  end

  Man.template = [===[
.TH "$title" "$section" "$date" "$left_footer" "$center_header"
$body
]===]

  return Man
end

return M
 end)
package.preload['lunamark.reader'] = (function (...)
-- (c) 2009-2011 John MacFarlane.  Released under MIT license.
-- See the file LICENSE in the source for details.

--- Provides access to all lunamark readers without preloading
-- them.  Reader modules are loaded only when needed.
--
--     local readers = require("lunamark.reader")
--     local htmlreader = readers.html       -- html reader loaded now
--     local myformat = 'markdown'
--     local myreader = readers[myformat]    -- markdown reader loaded now

local G = {}

setmetatable(G,{ __index = function(t,name)
                             local mod = require("lunamark.reader." .. name)
                             rawset(t,name,mod)
                             return t[name]
                            end })

return G
 end)
package.preload['lunamark.reader.markdown'] = (function (...)
-- (c) 2009-2011 John MacFarlane, Hans Hagen.  Released under MIT license.
-- See the file LICENSE in the source for details.

local util = require("lunamark.util")
local lpeg = require("lpeg")
local entities = require("lunamark.entities")
local lower, upper, gsub, rep, gmatch, format, length =
  string.lower, string.upper, string.gsub, string.rep, string.gmatch,
  string.format, string.len
local concat = table.concat
local P, R, S, V, C, Cg, Cb, Cmt, Cc, Cf, Ct, B, Cs =
  lpeg.P, lpeg.R, lpeg.S, lpeg.V, lpeg.C, lpeg.Cg, lpeg.Cb,
  lpeg.Cmt, lpeg.Cc, lpeg.Cf, lpeg.Ct, lpeg.B, lpeg.Cs
local lpegmatch = lpeg.match
local expand_tabs_in_line = util.expand_tabs_in_line
local unicode = require("unicode")
local utf8 = unicode.utf8

local M = {}

local rope_to_string = util.rope_to_string

-- Normalize a markdown reference tag.  (Make lowercase, and collapse
-- adjacent whitespace characters.)
local function normalize_tag(tag)
  return utf8.lower(gsub(rope_to_string(tag), "[ \n\r\t]+", " "))
end

local any                = P(1)
local fail               = any - 1
local eof                = - any


local L = {}
L.PERCENT                = P("%")
L.ASTERISK               = P("*")
L.DASH                   = P("-")
L.PLUS                   = P("+")
L.UNDERSCORE             = P("_")
L.PERIOD                 = P(".")
L.HASH                   = P("#")
L.AMPERSAND              = P("&")
L.BACKTICK               = P("`")
L.LESS                   = P("<")
L.MORE                   = P(">")
L.SPACE                  = P(" ")
L.SQUOTE                 = P("'")
L.DQUOTE                 = P('"')
L.LPARENT                = P("(")
L.RPARENT                = P(")")
L.LBRACKET               = P("[")
L.RBRACKET               = P("]")
L.CIRCUMFLEX             = P("^")
L.SLASH                  = P("/")
L.EQUAL                  = P("=")
L.COLON                  = P(":")
L.SEMICOLON              = P(";")
L.EXCLAMATION            = P("!")
L.DIGIT                  = R("09")
L.HEXDIGIT               = R("09","af","AF")
L.LETTER                 = R("AZ","az")
L.ALPHANUMERIC           = R("AZ","az","09")
L.DOUBLEASTERISKS        = P("**")
L.DOUBLEUNDERSCORES      = P("__")
L.TAB                    = P("\t")
L.SPACECHAR              = S("\t ")
L.SPACING                = S(" \n\r\t")
L.NEWLINE                = P("\n")
L.NONSPACECHAR           = any - L.SPACING
L.OPTIONALSPACE          = L.SPACECHAR^0
L.NONINDENTSPACE         = L.SPACE^-3 * - L.SPACECHAR
L.INDENT                 = L.SPACE^-3 * L.TAB + P("    ") / ""
L.LINECHAR               = P(1 - L.NEWLINE)
L.BLANKLINE              = L.OPTIONALSPACE * L.NEWLINE / "\n"
L.BLANKLINES             = L.BLANKLINE^0
L.SKIPBLANKLINES         = (L.OPTIONALSPACE * L.NEWLINE)^0
L.INDENTEDLINE           = L.INDENT    /"" * C(L.LINECHAR^1 * L.NEWLINE^-1)
L.OPTIONALLYINDENTEDLINE = L.INDENT^-1 /"" * C(L.LINECHAR^1 * L.NEWLINE^-1)
L.SP                     = L.SPACING^0
L.SPNL                   = L.OPTIONALSPACE * (L.NEWLINE * L.OPTIONALSPACE)^-1
L.LINE                   = L.LINECHAR^0 * L.NEWLINE
                         + L.LINECHAR^1 * eof
L.NONEMPTYLINE           = L.LINE - L.BLANKLINE
L.CHUNK                  = L.LINE * (L.OPTIONALLYINDENTEDLINE - L.BLANKLINE)^0


------------------------------------------------------------------------------
-- HTML
------------------------------------------------------------------------------

local H = {}
-- case-insensitive match (we assume s is lowercase)
local function keyword_exact(s)
  local parser = P(0)
  s = utf8.lower(s)
  for i=1,#s do
    local c = s:sub(i,i)
    local m = c .. upper(c)
    parser = parser * S(m)
  end
  return parser
end

local keyword       = L.LETTER * L.ALPHANUMERIC^0

local block_keyword =
    keyword_exact("address") + keyword_exact("blockquote") +
    keyword_exact("center") + keyword_exact("del") +
    keyword_exact("dir") + keyword_exact("div") +
    keyword_exact("p") + keyword_exact("pre") + keyword_exact("li") +
    keyword_exact("ol") + keyword_exact("ul") + keyword_exact("dl") +
    keyword_exact("dd") + keyword_exact("form") + keyword_exact("fieldset") +
    keyword_exact("isindex") + keyword_exact("ins") +
    keyword_exact("menu") + keyword_exact("noframes") +
    keyword_exact("frameset") + keyword_exact("h1") + keyword_exact("h2") +
    keyword_exact("h3") + keyword_exact("h4") + keyword_exact("h5") +
    keyword_exact("h6") + keyword_exact("hr") + keyword_exact("script") +
    keyword_exact("noscript") + keyword_exact("table") +
    keyword_exact("tbody") + keyword_exact("tfoot") +
    keyword_exact("thead") + keyword_exact("th") +
    keyword_exact("td") + keyword_exact("tr")

-- There is no reason to support bad html, so we expect quoted attributes
local htmlattributevalue  = L.SQUOTE * (any - (L.BLANKLINE + L.SQUOTE))^0 * L.SQUOTE
                          + L.DQUOTE * (any - (L.BLANKLINE + L.DQUOTE))^0 * L.DQUOTE

local htmlattribute       = L.SPACING^1 * (L.ALPHANUMERIC + S("_-"))^1 * L.SP * L.EQUAL
                          * L.SP * htmlattributevalue

local htmlcomment         = P("<!--") * (any - P("-->"))^0 * P("-->")

local htmlinstruction     = P("<?")   * (any - P("?>" ))^0 * P("?>" )

local openelt_any = L.LESS * keyword * htmlattribute^0 * L.SP * L.MORE

local function openelt_exact(s)
  return (L.LESS * L.SP * keyword_exact(s) * htmlattribute^0 * L.SP * L.MORE)
end

local openelt_block = L.LESS * L.SP * block_keyword * htmlattribute^0 * L.SP * L.MORE

local closeelt_any = L.LESS * L.SP * L.SLASH * keyword * L.SP * L.MORE

local function closeelt_exact(s)
  return (L.LESS * L.SP * L.SLASH * keyword_exact(s) * L.SP * L.MORE)
end

local emptyelt_any = L.LESS * L.SP * keyword * htmlattribute^0 * L.SP * L.SLASH * L.MORE

local function emptyelt_exact(s)
  return (L.LESS * L.SP * keyword_exact(s) * htmlattribute^0 * L.SP * L.SLASH * L.MORE)
end

local emptyelt_block = L.LESS * L.SP * block_keyword * htmlattribute^0 * L.SP * L.SLASH * L.MORE

local displaytext         = (any - L.LESS)^1

-- return content between two matched L.HTML tags
local function in_matched(s)
  return { openelt_exact(s)
         * (V(1) + displaytext + (L.LESS - closeelt_exact(s)))^0
         * closeelt_exact(s) }
end

local function parse_matched_tags(s,pos)
  local t = utf8.lower(lpegmatch(L.LESS * C(keyword),s,pos))
  return lpegmatch(in_matched(t),s,pos)
end

local in_matched_block_tags = Cmt(#openelt_block, parse_matched_tags)

H.displayhtml = htmlcomment
              + emptyelt_block
              + openelt_exact("hr")
              + in_matched_block_tags
              + htmlinstruction

H.inlinehtml  = emptyelt_any
              + htmlcomment
              + htmlinstruction
              + openelt_any
              + closeelt_any


--- Create a new markdown parser.
--
-- *   `writer` is a writer table (see [lunamark.writer.generic]).
--
-- *   `options` is a table with parsing options.
--     The following fields are significant:
--
--     `alter_syntax`
--     :   Function from syntax table to syntax table,
--         allowing the user to change or extend the markdown syntax.
--         For an example, see the documentation for `lunamark`.
--
--     `references`
--     :   A table of references to be used in resolving links
--         in the document.  The keys should be all lowercase, with
--         spaces and newlines collapsed into single spaces.
--         Example:
--
--             { foo: { url = "/url", title = "my title" },
--               bar: { url = "http://fsf.org" } }
--
--     `preserve_tabs`
--     :   Preserve tabs instead of converting to spaces.
--
--     `smart`
--     :   Parse quotation marks, dashes, ellipses intelligently.
--
--     `startnum`
--     :   Make the opening number in an ordered list significant.
--
--     `notes`
--     :   Enable footnotes as in pandoc.
--
--     `definition_lists`
--     :   Enable definition lists as in pandoc.
--
--     `pandoc_title_blocks`
--     :   Parse pandoc-style title block at the beginning of document:
--
--             % Title
--             % Author1; Author2
--             % Date
--
--     `lua_metadata`
--     :   Enable lua metadata.  This is an HTML comment block
--         that starts with `<!--@` and contains lua code.
--         The lua code is interpreted in a sandbox, and
--         any variables defined are added to the metadata.
--         The function `markdown` (also `m`) is defined and can
--         be used to ensure that string fields are parsed
--         as markdown; otherwise, they will be read literally.
--
--     `citeproc`
--     :   Automatic citations as in pandoc.  Requires installation of
--         the standalone `citeproc` executable.  The bibliography
--         or bibliographies must be specified using the `bibliography`
--         variable or metadata.  (The bibliography can be
--         MODS, BibTeX, BibLaTeX, RIS, EndNote, EndNote XML, ISI,
--         MEDLINE, Copac, or JSON citeproc.)  The CSL stylesheet must
--         be specified using the `csl` variable or metadata. A
--         primer on creating and modifying CSL styles can be found
--         at <http://citationstyles.org/downloads/primer.html>.
--         A repository of CSL styles can be found at
--         <https://github.com/citation-style-language/styles>. See also
--         <http://zotero.org/styles> for easy browsing.
--
--         Citations go inside square brackets and are separated by semicolons.
--         Each citation must have a key, composed of '@' + the citation
--         identifier from the database, and may optionally have a prefix,
--         a locator, and a suffix.  Here are some examples:
--
--             Blah blah [see @doe99, pp. 33-35; also @smith04, ch. 1].
--
--             Blah blah [@doe99, pp. 33-35, 38-39 and *passim*].
--
--             Blah blah [@smith04; @doe99].
--
--         A minus sign (`-`) before the `@` will suppress mention of
--         the author in the citation.  This can be useful when the
--         author is already mentioned in the text:
--
--             Smith says blah [-@smith04].
--
--         You can also write an in-text citation, as follows:
--
--             @smith04 says blah.
--
--             @smith04 [p. 33] says blah.
--
--         If the style calls for a list of works cited, it will replace the
--         template variable `references`.
--
--     `require_blank_before_blockquote`
--     :   Require a blank line between a paragraph and a following
--         block quote.
--
--     `require_blank_before_header`
--     :   Require a blank line between a paragraph and a following
--         header.
--
--     `hash_enumerators`
--     :   Allow `#` instead of a digit for an ordered list enumerator
--         (equivalent to `1`).
--
-- *   Returns a converter function that converts a markdown string
--     using `writer`, returning the parsed document as first result,
--     and a table containing any extracted metadata as the second
--     result. The converter assumes that the input has unix
--     line endings (newline).  If the input might have DOS
--     line endings, a simple `gsub("\r","")` should take care of them.
function M.new(writer, options)
  local options = options or {}

  local function expandtabs(s)
    if s:find("\t") then
      return s:gsub("[^\n]*",expand_tabs_in_line)
    else
      return s
    end
  end

  if options.preserve_tabs then
    expandtabs = function(s) return s end
  end

  ------------------------------------------------------------------------------

  local syntax
  local blocks
  local inlines

  parse_blocks =
    function(str)
      local res = lpegmatch(blocks, str)
      if res == nil
        then error(format("parse_blocks failed on:\n%s", str:sub(1,20)))
        else return res
        end
    end

  parse_inlines =
    function(str)
      local res = lpegmatch(inlines, str)
      if res == nil
        then error(format("parse_inlines failed on:\n%s", str:sub(1,20)))
        else return res
        end
    end

  parse_inlines_no_link =
    function(str)
      local res = lpegmatch(inlines_no_link, str)
      if res == nil
        then error(format("parse_inlines_no_link failed on:\n%s", str:sub(1,20)))
        else return res
        end
    end

  ------------------------------------------------------------------------------
  -- Generic parsers
  ------------------------------------------------------------------------------

  -- block followed by 0 or more optionally
  -- indented blocks with first line indented.
  local function indented_blocks(bl)
    return Cs( bl
             * (L.BLANKLINE^1 * L.INDENT * -L.BLANKLINE * bl)^0
             * L.BLANKLINE^1 )
  end

  local escapable              = S("\\`*_{}[]()+_.!<>#-~:^")
  local anyescaped             = P("\\") / "" * escapable
                               + any

  local tightblocksep          = P("\001")

  local specialchar
  if options.smart then
    specialchar                = S("*_`&[]<!\\'\"-.")
  else
    specialchar                = S("*_`&[]<!\\")
  end

  local normalchar             = any -
                                 (specialchar + L.SPACING + tightblocksep)
  -----------------------------------------------------------------------------
  -- Parsers used for markdown lists
  -----------------------------------------------------------------------------

  -- gobble spaces to make the whole bullet or enumerator four spaces wide:
  local function gobbletofour(s,pos,c)
      if length(c) >= 3
         then return lpegmatch(L.SPACE^-1,s,pos)
      elseif length(c) == 2
         then return lpegmatch(L.SPACE^-2,s,pos)
      else return lpegmatch(L.SPACE^-3,s,pos)
      end
  end

  local bulletchar = C(L.PLUS + L.ASTERISK + L.DASH)

  local bullet     = ( bulletchar * #L.SPACING * (L.TAB + L.SPACE^-3)
                     + L.SPACE * bulletchar * #L.SPACING * (L.TAB + L.SPACE^-2)
                     + L.SPACE * L.SPACE * bulletchar * #L.SPACING * (L.TAB + L.SPACE^-1)
                     + L.SPACE * L.SPACE * L.SPACE * bulletchar * #L.SPACING
                     ) * -bulletchar

  if options.hash_enumerators then
    dig = L.DIGIT + L.HASH
  else
    dig = L.DIGIT
  end

  local enumerator = C(dig^3 * L.PERIOD) * #L.SPACING
                   + C(dig^2 * L.PERIOD) * #L.SPACING * (L.TAB + L.SPACE^1)
                   + C(dig * L.PERIOD) * #L.SPACING * (L.TAB + L.SPACE^-2)
                   + L.SPACE * C(dig^2 * L.PERIOD) * #L.SPACING
                   + L.SPACE * C(dig * L.PERIOD) * #L.SPACING * (L.TAB + L.SPACE^-1)
                   + L.SPACE * L.SPACE * C(dig^1 * L.PERIOD) * #L.SPACING

  -----------------------------------------------------------------------------
  -- Parsers used for markdown code spans
  -----------------------------------------------------------------------------

  local openticks   = Cg(L.BACKTICK^1, "ticks")

  local function captures_equal_length(s,i,a,b)
    return #a == #b and i
  end

  local closeticks  = L.SPACE^-1 *
                      Cmt(C(L.BACKTICK^1) * Cb("ticks"), captures_equal_length)

  local intickschar = (any - S(" \n\r`"))
                    + (L.NEWLINE * -L.BLANKLINE)
                    + (L.SPACE - closeticks)
                    + (L.BACKTICK^1 - closeticks)

  local inticks     = openticks * L.SPACE^-1 * C(intickschar^1) * closeticks

  -----------------------------------------------------------------------------
  -- Parsers used for markdown tags and links
  -----------------------------------------------------------------------------

  local leader        = L.SPACE^-3

  -- in balanced brackets, parentheses, quotes:
  local bracketed     = P{ L.LBRACKET
                         * ((anyescaped - (L.LBRACKET + L.RBRACKET + L.BLANKLINE^2)) + V(1))^0
                         * L.RBRACKET }

  local inparens      = P{ L.LPARENT
                         * ((anyescaped - (L.LPARENT + L.RPARENT + L.BLANKLINE^2)) + V(1))^0
                         * L.RPARENT }

  local squoted       = P{ L.SQUOTE * L.ALPHANUMERIC
                         * ((anyescaped - (L.SQUOTE + L.BLANKLINE^2)) + V(1))^0
                         * L.SQUOTE }

  local dquoted       = P{ L.DQUOTE * L.ALPHANUMERIC
                         * ((anyescaped - (L.DQUOTE + L.BLANKLINE^2)) + V(1))^0
                         * L.DQUOTE }

  -- bracketed 'tag' for markdown links, allowing nested brackets:
  local tag           = L.LBRACKET
                      * Cs((L.ALPHANUMERIC^1
                           + bracketed
                           + inticks
                           + (anyescaped - (L.RBRACKET + L.BLANKLINE^2)))^0)
                      * L.RBRACKET

  -- url for markdown links, allowing balanced parentheses:
  local url           = L.LESS * Cs((anyescaped-L.MORE)^0) * L.MORE
                      + Cs((inparens + (anyescaped-L.SPACING-L.RPARENT))^1)

  -- quoted text possibly with nested quotes:
  local title_s       = L.SQUOTE  * Cs(((anyescaped-L.SQUOTE) + squoted)^0) * L.SQUOTE

  local title_d       = L.DQUOTE  * Cs(((anyescaped-L.DQUOTE) + dquoted)^0) * L.DQUOTE

  local title_p       = L.LPARENT
                      * Cs((inparens + (anyescaped-L.RPARENT))^0)
                      * L.RPARENT

  local title         = title_d + title_s + title_p

  local optionaltitle = L.SPNL * title * L.SPACECHAR^0
                      + Cc("")

  ------------------------------------------------------------------------------
  -- Footnotes
  ------------------------------------------------------------------------------

  local rawnotes = {}

  local function strip_first_char(s)
    return s:sub(2)
  end

  -- like indirect_link
  local function lookup_note(ref)
    return function()
      local found = rawnotes[normalize_tag(ref)]
      if found then
        return writer.note(parse_blocks(found))
      else
        return {"[^", ref, "]"}
      end
    end
  end

  local function register_note(ref,rawnote)
    rawnotes[normalize_tag(ref)] = rawnote
    return ""
  end

  local RawNoteRef = #(L.LBRACKET * L.CIRCUMFLEX) * tag / strip_first_char

  local NoteRef    = RawNoteRef / lookup_note

  local NoteBlock

  if options.notes then
    NoteBlock = leader * RawNoteRef * L.COLON * L.SPNL * indented_blocks(L.CHUNK)
              / register_note
  else
    NoteBlock = fail
  end

  ------------------------------------------------------------------------------
  -- Helpers for links and references
  ------------------------------------------------------------------------------

  -- List of references defined in the document
  local references

  -- add a reference to the list
  local function register_link(tag,url,title)
      references[normalize_tag(tag)] = { url = url, title = title }
      return ""
  end

  -- parse a reference definition:  [foo]: /bar "title"
  local define_reference_parser =
    leader * tag * L.COLON * L.SPACECHAR^0 * url * optionaltitle * L.BLANKLINE^1

  -- lookup link reference and return either
  -- the link or nil and fallback text.
  local function lookup_reference(label,sps,tag)
      local tagpart
      if not tag then
          tag = label
          tagpart = ""
      elseif tag == "" then
          tag = label
          tagpart = "[]"
      else
          tagpart = {"[", parse_inlines(tag), "]"}
      end
      if sps then
        tagpart = {sps, tagpart}
      end
      local r = references[normalize_tag(tag)]
      if r then
        return r
      else
        return nil, {"[", parse_inlines(label), "]", tagpart}
      end
  end

  -- lookup link reference and return a link, if the reference is found,
  -- or a bracketed label otherwise.
  local function indirect_link(label,sps,tag)
    return function()
      local r,fallback = lookup_reference(label,sps,tag)
      if r then
        return writer.link(parse_inlines_no_link(label), r.url, r.title)
      else
        return fallback
      end
    end
  end

  -- lookup image reference and return an image, if the reference is found,
  -- or a bracketed label otherwise.
  local function indirect_image(label,sps,tag)
    return function()
      local r,fallback = lookup_reference(label,sps,tag)
      if r then
        return writer.image(writer.string(label), r.url, r.title)
      else
        return {"!", fallback}
      end
    end
  end

  ------------------------------------------------------------------------------
  -- Entities
  ------------------------------------------------------------------------------

  local hexentity = L.AMPERSAND * L.HASH * S("Xx") * C(L.HEXDIGIT    ^1) * L.SEMICOLON
  local decentity = L.AMPERSAND * L.HASH           * C(L.DIGIT       ^1) * L.SEMICOLON
  local tagentity = L.AMPERSAND *                  C(L.ALPHANUMERIC^1) * L.SEMICOLON

  ------------------------------------------------------------------------------
  -- Inline elements
  ------------------------------------------------------------------------------

  local Inline    = V("Inline")

  local Str       = normalchar^1 / writer.string

  local Ellipsis  = P("...") / writer.ellipsis

  local Dash      = P("---") * -L.DASH / writer.mdash
                  + P("--") * -L.DASH / writer.ndash
                  + P("-") * #L.DIGIT * B(L.DIGIT, 2) / writer.ndash

  local DoubleQuoted = L.DQUOTE * Ct((Inline - L.DQUOTE)^1) * L.DQUOTE
                     / writer.doublequoted

  local squote_start = L.SQUOTE * -L.SPACING

  local squote_end = L.SQUOTE * B(L.NONSPACECHAR, 2)

  local SingleQuoted = squote_start * Ct((Inline - squote_end)^1) * squote_end
                     / writer.singlequoted

  local Apostrophe = L.SQUOTE * B(L.NONSPACECHAR, 2) / "’"

  local Smart      = Ellipsis + Dash + SingleQuoted + DoubleQuoted + Apostrophe

  local Symbol    = (specialchar - tightblocksep) / writer.string

  local Code      = inticks / writer.code

  local bqstart      = L.MORE
  local headerstart  = L.HASH
                     + (L.LINE * (L.EQUAL^1 + L.DASH^1) * L.OPTIONALSPACE * L.NEWLINE)

  if options.require_blank_before_blockquote then
    bqstart = fail
  end

  if options.require_blank_before_header then
    headerstart = fail
  end

  local Endline   = L.NEWLINE * -( -- newline, but not before...
                        L.BLANKLINE -- paragraph break
                      + tightblocksep  -- nested list
                      + eof       -- end of document
                      + bqstart
                      + headerstart
                    ) * L.SPACECHAR^0 / writer.space

  local Space     = L.SPACECHAR^2 * Endline / writer.linebreak
                  + L.SPACECHAR^1 * Endline^-1 * eof / ""
                  + L.SPACECHAR^1 * Endline^-1 * L.OPTIONALSPACE / writer.space

  -- parse many p between starter and ender
  local function between(p, starter, ender)
      local ender2 = B(L.NONSPACECHAR) * ender
      return (starter * #L.NONSPACECHAR * Ct(p * (p - ender2)^0) * ender2)
  end

  local Strong = ( between(Inline, L.DOUBLEASTERISKS, L.DOUBLEASTERISKS)
                 + between(Inline, L.DOUBLEUNDERSCORES, L.DOUBLEUNDERSCORES)
                 ) / writer.strong

  local Emph   = ( between(Inline, L.ASTERISK, L.ASTERISK)
                 + between(Inline, L.UNDERSCORE, L.UNDERSCORE)
                 ) / writer.emphasis

  local urlchar = anyescaped - L.NEWLINE - L.MORE

  local AutoLinkUrl   = L.LESS
                      * C(L.ALPHANUMERIC^1 * P("://") * urlchar^1)
                      * L.MORE
                      / function(url) return writer.link(writer.string(url),url) end

  local AutoLinkEmail = L.LESS
                      * C((L.ALPHANUMERIC + S("-._+"))^1 * P("@") * urlchar^1)
                      * L.MORE
                      / function(email) return writer.link(writer.string(email),"mailto:"..email) end

  local DirectLink    = (tag / parse_inlines_no_link)  -- no links inside links
                      * L.SPNL
                      * L.LPARENT
                      * (url + Cc(""))  -- link can be empty [foo]()
                      * optionaltitle
                      * L.RPARENT
                      / writer.link

  local IndirectLink = tag * (C(L.SPNL) * tag)^-1 / indirect_link

  -- parse a link or image (direct or indirect)
  local Link          = DirectLink + IndirectLink

  local DirectImage   = L.EXCLAMATION
                      * (tag / parse_inlines)
                      * L.SPNL
                      * L.LPARENT
                      * (url + Cc(""))  -- link can be empty [foo]()
                      * optionaltitle
                      * L.RPARENT
                      / writer.image

  local IndirectImage  = L.EXCLAMATION * tag * (C(L.SPNL) * tag)^-1 / indirect_image

  local Image         = DirectImage + IndirectImage

  -- avoid parsing long strings of * or _ as emph/strong
  local UlOrStarLine  = L.ASTERISK^4 + L.UNDERSCORE^4 / writer.string

  local EscapedChar   = S("\\") * C(escapable) / writer.string

  local InlineHtml    = C(H.inlinehtml) / writer.inline_html

  local HtmlEntity    = hexentity / entities.hex_entity  / writer.string
                      + decentity / entities.dec_entity  / writer.string
                      + tagentity / entities.char_entity / writer.string

  ------------------------------------------------------------------------------
  -- Block elements
  ------------------------------------------------------------------------------

  local Block          = V("Block")

  local DisplayHtml    = C(H.displayhtml) / expandtabs / writer.display_html

  local Verbatim       = Cs( (L.BLANKLINES
                           * ((L.INDENTEDLINE - L.BLANKLINE))^1)^1
                           ) / expandtabs / writer.verbatim

  -- strip off leading > and indents, and run through blocks
  local Blockquote     = Cs((
            ((leader * L.MORE * L.SPACE^-1)/"" * L.LINECHAR^0 * L.NEWLINE)^1
          * (-L.BLANKLINE * L.LINECHAR^1 * L.NEWLINE)^0
          * L.BLANKLINE^0
          )^1) / parse_blocks / writer.blockquote

  local function lineof(c)
      return (leader * (P(c) * L.OPTIONALSPACE)^3 * L.NEWLINE * L.BLANKLINE^1)
  end

  local HorizontalRule = ( lineof(L.ASTERISK)
                         + lineof(L.DASH)
                         + lineof(L.UNDERSCORE)
                         ) / writer.hrule

  local Reference      = define_reference_parser / register_link

  local Paragraph      = L.NONINDENTSPACE * Ct(Inline^1) * L.NEWLINE
                       * ( L.BLANKLINE^1
                         + #L.HASH
                         + #(leader * L.MORE * L.SPACE^-1)
                         )
                       / writer.paragraph

  local Plain          = L.NONINDENTSPACE * Ct(Inline^1) / writer.plain

  ------------------------------------------------------------------------------
  -- Lists
  ------------------------------------------------------------------------------

  local starter = bullet + enumerator

  -- we use \001 as a separator between a tight list item and a
  -- nested list under it.
  local NestedList            = Cs((L.OPTIONALLYINDENTEDLINE - starter)^1)
                              / function(a) return "\001"..a end

  local ListBlockLine         = L.OPTIONALLYINDENTEDLINE
                                - L.BLANKLINE - (L.INDENT^-1 * starter)

  local ListBlock             = L.LINE * ListBlockLine^0

  local ListContinuationBlock = L.BLANKLINES * (L.INDENT / "") * ListBlock

  local function TightListItem(starter)
      return -HorizontalRule
             * (Cs(starter / "" * ListBlock * NestedList^-1) / parse_blocks)
             * -(L.BLANKLINES * L.INDENT)
  end

  local function LooseListItem(starter)
      return -HorizontalRule
             * Cs( starter / "" * ListBlock * Cc("\n")
               * (NestedList + ListContinuationBlock^0)
               * (L.BLANKLINES / "\n\n")
               ) / parse_blocks
  end

  local BulletList = ( Ct(TightListItem(bullet)^1)
                       * Cc(true) * L.SKIPBLANKLINES * -bullet
                     + Ct(LooseListItem(bullet)^1)
                       * Cc(false) * L.SKIPBLANKLINES ) / writer.bulletlist

  local function ordered_list(s,tight,startnum)
    if options.startnum then
      startnum = tonumber(listtype) or 1  -- fallback for '#'
    else
      startnum = nil
    end
    return writer.orderedlist(s,tight,startnum)
  end

  local OrderedList = Cg(enumerator, "listtype") *
                      ( Ct(TightListItem(Cb("listtype")) * TightListItem(enumerator)^0)
                        * Cc(true) * L.SKIPBLANKLINES * -enumerator
                      + Ct(LooseListItem(Cb("listtype")) * LooseListItem(enumerator)^0)
                        * Cc(false) * L.SKIPBLANKLINES
                      ) * Cb("listtype") / ordered_list

  local defstartchar = S("~:")
  local defstart     = ( defstartchar * #L.SPACING * (L.TAB + L.SPACE^-3)
                     + L.SPACE * defstartchar * #L.SPACING * (L.TAB + L.SPACE^-2)
                     + L.SPACE * L.SPACE * defstartchar * #L.SPACING * (L.TAB + L.SPACE^-1)
                     + L.SPACE * L.SPACE * L.SPACE * defstartchar * #L.SPACING
                     )

  local dlchunk = Cs(L.LINE * (L.INDENTEDLINE - L.BLANKLINE)^0)

  local function definition_list_item(term, defs, tight)
    return { term = parse_inlines(term), definitions = defs }
  end

  local DefinitionListItemLoose = C(L.LINE) * L.SKIPBLANKLINES
                           * Ct((defstart * indented_blocks(dlchunk) / parse_blocks)^1)
                           * Cc(false)
                           / definition_list_item

  local DefinitionListItemTight = C(L.LINE)
                           * Ct((defstart * dlchunk / parse_blocks)^1)
                           * Cc(true)
                           / definition_list_item

  local DefinitionList =  ( Ct(DefinitionListItemLoose^1) * Cc(false)
                          +  Ct(DefinitionListItemTight^1)
                             * (L.SKIPBLANKLINES * -DefinitionListItemLoose * Cc(true))
                          ) / writer.definitionlist

  ------------------------------------------------------------------------------
  -- Lua metadata
  ------------------------------------------------------------------------------

  local function lua_metadata(s)  -- run lua code in comment in sandbox
    local env = { m = parse_markdown, markdown = parse_blocks }
    local scode = s:match("^<!%-%-@%s*(.*)%-%->")
    local untrusted_table, message = loadstring(scode)
    if not untrusted_table then
      util.err(message, 37)
    end
    setfenv(untrusted_table, env)
    local ok, msg = pcall(untrusted_table)
    if not ok then
      util.err(msg)
    end
    for k,v in pairs(env) do
      writer.set_metadata(k,v)
    end
    return ""
  end

  local LuaMeta = fail
  if options.lua_metadata then
    LuaMeta = #P("<!--@") * htmlcomment / lua_metadata
  end

  ------------------------------------------------------------------------------
  -- Pandoc title block parser
  ------------------------------------------------------------------------------

  local pandoc_title =
      L.PERCENT * L.OPTIONALSPACE
    * C(L.LINE * (L.SPACECHAR * L.NONEMPTYLINE)^0) / parse_inlines
  local pandoc_author =
      L.SPACECHAR * L.OPTIONALSPACE
    * C((anyescaped - L.NEWLINE - L.SEMICOLON)^0)
    * (L.SEMICOLON + L.NEWLINE)
  local pandoc_authors =
    L.PERCENT * Cs((pandoc_author / parse_inlines)^0) * L.NEWLINE^-1
  local pandoc_date =
    L.PERCENT * L.OPTIONALSPACE * C(L.LINE) / parse_inlines
  local pandoc_title_block =
      (pandoc_title + Cc(""))
    * (pandoc_authors + Cc({}))
    * (pandoc_date + Cc(""))
    * C(P(1)^0)

  ------------------------------------------------------------------------------
  -- Blank
  ------------------------------------------------------------------------------

  local Blank          = L.BLANKLINE / ""
                       + LuaMeta
                       + NoteBlock
                       + Reference
                       + (tightblocksep / "\n")

  ------------------------------------------------------------------------------
  -- Headers
  ------------------------------------------------------------------------------

  -- parse Atx heading start and return level
  local HeadingStart = #L.HASH * C(L.HASH^-6) * -L.HASH / length

  -- parse setext header ending and return level
  local HeadingLevel = L.EQUAL^1 * Cc(1) + L.DASH^1 * Cc(2)

  local function strip_atx_end(s)
    return s:gsub("[#%s]*\n$","")
  end

  -- parse atx header
  local AtxHeader = Cg(HeadingStart,"level")
                     * L.OPTIONALSPACE
                     * (C(L.LINE) / strip_atx_end / parse_inlines)
                     * Cb("level")
                     / writer.header

  -- parse setext header
  local SetextHeader = #(L.LINE * S("=-"))
                     * (C(L.LINE) / parse_inlines)
                     * HeadingLevel
                     * L.OPTIONALSPACE * L.NEWLINE
                     / writer.header

  ------------------------------------------------------------------------------
  -- Syntax specification
  ------------------------------------------------------------------------------

  syntax =
    { "Blocks",

      Blocks                = Blank^0 *
                              Block^-1 *
                              (Blank^0 / function() return writer.interblocksep end * Block)^0 *
                              Blank^0 *
                              eof,

      Blank                 = Blank,

      Block                 = V("Blockquote")
                            + V("Verbatim")
                            + V("HorizontalRule")
                            + V("BulletList")
                            + V("OrderedList")
                            + V("Header")
                            + V("DefinitionList")
                            + V("DisplayHtml")
                            + V("Paragraph")
                            + V("Plain"),

      Blockquote            = Blockquote,
      Verbatim              = Verbatim,
      HorizontalRule        = HorizontalRule,
      BulletList            = BulletList,
      OrderedList           = OrderedList,
      Header                = AtxHeader + SetextHeader,
      DefinitionList        = DefinitionList,
      DisplayHtml           = DisplayHtml,
      Paragraph             = Paragraph,
      Plain                 = Plain,

      Inline                = V("Str")
                            + V("Space")
                            + V("Endline")
                            + V("UlOrStarLine")
                            + V("Strong")
                            + V("Emph")
                            + V("NoteRef")
                            + V("Link")
                            + V("Image")
                            + V("Code")
                            + V("AutoLinkUrl")
                            + V("AutoLinkEmail")
                            + V("InlineHtml")
                            + V("HtmlEntity")
                            + V("EscapedChar")
                            + V("Smart")
                            + V("Symbol"),

      Str                   = Str,
      Space                 = Space,
      Endline               = Endline,
      UlOrStarLine          = UlOrStarLine,
      Strong                = Strong,
      Emph                  = Emph,
      NoteRef               = NoteRef,
      Link                  = Link,
      Image                 = Image,
      Code                  = Code,
      AutoLinkUrl           = AutoLinkUrl,
      AutoLinkEmail         = AutoLinkEmail,
      InlineHtml            = InlineHtml,
      HtmlEntity            = HtmlEntity,
      EscapedChar           = EscapedChar,
      Smart                 = Smart,
      Symbol                = Symbol,
    }

  if not options.definition_lists then
    syntax.DefinitionList = fail
  end

  if not options.notes then
    syntax.NoteRef = fail
  end

  if not options.smart then
    syntax.Smart = fail
  end

  if options.alter_syntax and type(options.alter_syntax) == "function" then
    syntax = options.alter_syntax(syntax)
  end

  blocks = Ct(syntax)

  local inlines_t = util.table_copy(syntax)
  inlines_t[1] = "Inlines"
  inlines_t.Inlines = Inline^0 * (L.SPACING^0 * eof / "")
  inlines = Ct(inlines_t)

  inlines_no_link_t = util.table_copy(inlines_t)
  inlines_no_link_t.Link = fail
  inlines_no_link = Ct(inlines_no_link_t)

  ------------------------------------------------------------------------------
  -- Exported conversion function
  ------------------------------------------------------------------------------

  -- inp is a string; line endings are assumed to be LF (unix-style)
  -- and tabs are assumed to be expanded.
  return function(inp)
      references = options.references or {}
      if options.pandoc_title_blocks then
        local title, authors, date, rest = lpegmatch(pandoc_title_block, inp)
        writer.set_metadata("title",title)
        writer.set_metadata("author",authors)
        writer.set_metadata("date",date)
        inp = rest
      end
      local result = { writer.start_document(), parse_blocks(inp), writer.stop_document() }
      return rope_to_string(result), writer.get_metadata()
  end

end

return M
 end)
-- lunamark program

--[===[
@startman
# NAME

lunamark - converts markdown to many formats

# SYNOPSIS

lunamark [options] [file..]

# DESCRIPTION

Lunamark is a lua library and command-line program for conversion of markdown
to other textual formats. Currently HTML, Docbook, ConTeXt, LaTeX, and Groff man
are the supported output formats, but it is easy to add new writers or modify
existing ones. The markdown parser is written using a PEG grammar and can also
be modified by the user.

Lunamark's markdown parser currently supports the following extensions (which
can be turned on or off individually):

  - Smart typography (fancy quotes, dashes, ellipses)
  - Significant start numbers in ordered lists
  - Footnotes
  - Definition lists
  - Metadata
  - Pandoc title blocks
  - Citations using citeproc

More extensions will be supported in later versions.

The library is as portable as lua and has very good performance.
It is slightly faster than the author's own C library
[peg-markdown](http://github.com/jgm/peg-markdown).

# OPTIONS

`--to,-t` *format*
:   Specify format for output.
    *format* can be `html`, `html5`, `dzslides`, `docbook`, `latex`, `context`, or `man`.
`--layout,-l` *layout*
:   Control whitespace in output.
    *layout* can be `default` (blank lines between block-level elements),
    `compact` (avoid unnecessary blank lines), or `minimize` (avoid
    all unnecessary space).
`--extensions,-X` *extensions*
:   Use the specified syntax extensions in parsing markdown.
    *extensions* is a comma-separated list of extensions, each optionally
    prefixed by `+` (enable) or `-` (disable).  See EXTENSIONS, below, for
    a list of supported extensions.  The keyword 'all' may also be used,
    to set all extensions simultaneously.
`--output,-o` *file*
:   Write output to *file*.
`--standalone,-s`
:   Add header and footer to the document, so that it is a functional
    standalone document, not a fragment.  Use the default template
    for the writer, unless `--template,-T` is used.
`--template,-T` *file*
:   Insert converted text and metadata into a template.  See TEMPLATES,
    below, for template format.  Implies `--standalone,-s`.
`--data,-d` *key=value[,key=value..]*
:   Set metadata fields to be passed to template.  Argument is a list
    of *key=value* pairs, separated by commas.  If keys are repeated,
    an array value will be formed.
`--strict,-0`
:   Disable all markdown extensions.
`--version,-V`
:   Print version information.
`--help,-h`
:   This message

# EXTENSIONS

The following extensions are defined, with the default setting given in
parentheses.  The defaults can be changed locally by defining an
environment variable `LUNAMARK_EXTENSIONS` (see ENVIRONMENT below).

`(-) containers`
:   Put sections in containers (`div` tags for `html` writer,
    `section` tags for `html5` or `dzslides`).

`(-) slides`
:   Like `containers`, but do not allow containers to nest.
    The container for a section will end before the container for
    a subsection begins.  This is usually what is wanted
    for HTML5 slide shows (and is selected by default for `dzslides`).

`(-) startnum`
:   Start number of an ordered list is significant. In standard
    markdown, the starting number is irrelevant.  If this
    extension is selected, the starting number determines
    the starting number of the list in the output.  (The
    subsequent numbers are ignored, as in standard markdown.)

`(-) smart`
:   Smart typography. Straight quotes are turned into curly
    quotes, `--` into en dashes, `---` into em dashes, `...`
    into an ellipsis character.

`(-) preserve_tabs`
:   Don't expand tabs to spaces. Standard markdown expands all
    tabs, using a tabstop of 4 spaces.  This extension allows
    tabs to be preserved in literal code contexts.

`(-) notes`
:   Footnotes. A footnote marker is like a markdown bracketed
    reference, but beginning with a circumflex (`^`).  The note
    itself goes in a separate block, which begins with the marker,
    followed by a colon and space, and ends with a blank line.
    The note can contain multiple paragraphs or other block elements,
    but each block must be indented four spaces. Example:

        Here is a note.[^mynote]

        [^mynote]: This note has two paragraphs.

            Here is the second one.

`(-) definition_lists`
:   Definition lists.  A list item consists of a term, on a single
    line, followed by one or more definitions.  Each definition
    begins with a colon (`:`), but is otherwise indented four spaces.
    A definition may contain multiple block elements.  The colon
    need not be flush-left, but may be preceded by one or two spaces.
    Alternatively, a tilde (`~`) may be used instead of a colon.
    Example:

        rake
        :   a tool for gathering leaves.

            (paragraph two of definition one.)

        :   a ruby build system.

`(-) lua_metadata`
:   Lua metadata.  An HTML comment beginning `<!--@` and containing
    lua variable declarations is treated as metadata.  Note that strings
    are read literally unless they are explicitly marked as markdown
    using the `markdown` (or `m`) function. Lua metadata can occur anywhere
    in a document. Example:

        <!--@
        title = m"My title with *italics*"
        abstract = m[[
          This is my abstract.

          * point one
          * point two
          ]]
        author = { "Me", "You" }
        -->

`(-) pandoc_title_blocks`
:   Pandoc style title blocks at the beginning of a document:

        % Title
        % Author1; Author2
        % Date

        content starts here...

`(-) citeproc`
:   Automatic citations as in pandoc.  Requires installation of
    the standalone `citeproc` executable.  The bibliography
    or bibliographies must be specified using the `bibliography`
    variable or metadata.  (The bibliography can be
    MODS, BibTeX, BibLaTeX, RIS, EndNote, EndNote XML, ISI,
    MEDLINE, Copac, or JSON citeproc.)  The CSL stylesheet must
    be specified using the `csl` variable or metadata. A
    primer on creating and modifying CSL styles can be found
    at <http://citationstyles.org/downloads/primer.html>.
    A repository of CSL styles can be found at
    <https://github.com/citation-style-language/styles>. See also
    <http://zotero.org/styles> for easy browsing.

    Citations go inside square brackets and are separated by semicolons.
    Each citation must have a key, composed of '@' + the citation
    identifier from the database, and may optionally have a prefix,
    a locator, and a suffix.  Here are some examples:

        Blah blah [see @doe99, pp. 33-35; also @smith04, ch. 1].

        Blah blah [@doe99, pp. 33-35, 38-39 and *passim*].

        Blah blah [@smith04; @doe99].

    A minus sign (`-`) before the `@` will suppress mention of
    the author in the citation.  This can be useful when the
    author is already mentioned in the text:

        Smith says blah [-@smith04].

    You can also write an in-text citation, as follows:

        @smith04 says blah.

        @smith04 [p. 33] says blah.

    If the style calls for a list of works cited, it will replace the
    template variable `references`.

`(-) require_blank_before_blockquote`
:   Require a blank line between a paragraph and a following blockquote.

`(-) require_blank_before_header`
:   Require a blank line between a paragraph and a following header.

`(-) hash_enumerators`
:   Allow `#` instead of a digit for an ordered list enumerator
    (equivalent to `1`).

# TEMPLATES

By default, lunamark will produce a fragment.  If the
`--standalone` or `--template` options are specified, it will insert
this fragment into a template, producing a standalone document with
appropriate header and footer.  `--standalone` uses the default template
built into the writer, while `--template` specifies a custom template,
which is sought first in the working directory, then in
`templates`, and finally in `$HOME/lunamark/templates`. If no
extension is given, the name of the writer will be used as an
extension. So, for example, one can put the template `letter.html`
in the `$HOME?lunamark/templates` directory, and use it anywhere
with `lunamark --template letter`.

The templates are [cosmo](http://cosmo.luaforge.net/) templates.
Conditionals are enabled, so you can use the `$if` keyword
as follows:

    $if{ #people == 1 }[[There is only one.]][[There are many.]]

A `sepby` keyword is also enabled:

    $sepby{ people }[[$it]][[ and ]]

will render "`Sid`" if `people == {"Sid"}` and
"`Sid and Nancy`" if `people == {"Sid","Nancy"}`.

The following variables are set by default; others may be set
by the reader (if metadata extensions are used) or through
the `--data` option:

`body`
:   the fragment converted from markdown
`sources`
:   array of the source files specified on the command line
`timestamp`
:   the current time

# EXAMPLES

    lunamark

acts as a filter, reading markdown from stdin and writing
HTML to stdout.

    lunamark -Xsmart,definition_lists -t latex

acts as a filter, reading markdown with smart typography
and definition list extensions from stdin, and writing
LaTeX to stdout.

    lunamark -t latex -s -o mybook.tex ch{1,2,3}.txt references.txt

reads `ch1.txt`, `ch2.txt`, `ch3.txt`, and `references.txt`,
concatenates them, and converts the result from markdown to LaTeX,
using the default template and saving the result as `mybook.tex`.

    lunamark -Xall --template letter -d cc=Smith,cc="Jim Jones",sign="yes" \
      -t context -o myletter.ctx myletter.txt

produces a ConTeXt file using the template `letter.context`,
and setting the variable `cc` to `{"Smith","Jim Jones"}`
and `sign` to `"yes"`.  All lunamark etensions are enabled.

# ENVIRONMENT

The environment variable `LUNAMARK_EXTENSIONS` can contain
a comma-separated list of extensions, optionally prefixed by
`+` or `-`, that will serve as defaults. These defaults can
be overridden using the command-line option `--extensions`.

# AUTHORS

Most of lunamark is written by John MacFarlane.  Hans Hagen
made some major performance improvements to the markdown
parser.  Khaled Hosny added the original ConTeXt writer.

@stopman
--]===]

local lunamark = require("lunamark")
local alt_getopt = require("alt_getopt")
local cosmo = require("cosmo")

local function ensure_one_of(optval,s,ary)
  for i=1,#ary do
    if ary[i]==s then return true end
  end
  lunamark.util.err("Illegal value for " .. optval ..
     "\nLegal values are: " .. table.concat(ary,", "))
end

local version = [[
lunamark 0.2
Copyright (C) 2009-2011 John MacFarlane
]]

local usage = [[
Usage: lunamark [options] [file..] - convert markdown to other formats

Options:
  --to,-t FORMAT             Target format
  --layout,-l LAYOUT         Whitespace in output (default|compact|minimize)
  --extensions,-X EXTENSIONS Syntax extensions to use
  --output,-o FILE           Output file
  --standalone,-s            Add header and footer
  --template,-T FILE         Insert output into template
  --data,-d K=V[,K=V..]      Set metadata to be passed to template
  --strict,-0                Disable markdown extensions
  --version,-V               Version information
  --help,-h                  This message

FORMAT can be html, html5, docbook, latex, context, or man.

LAYOUT can be default, compact (no unnecessary blank lines), or
minimize (no unnecessary blank space).

EXTENSIONS is a comma-separated list of extensions, each optionally prefixed
by + (enable) or - (disable).  The following extensions are defined,
with the default setting given in parentheses:
  (-) containers        Put sections in containers (e.g. div or section tags)
  (-) slides            Like containers, but do not nest them
  (-) startnum          Start number of an ordered list is significant
  (-) smart             Smart typography (quotes, dashes, ellipses)
  (-) preserve_tabs     Don't expand tabs to spaces
  (-) notes             Footnotes
  (-) definition_lists  Definition lists
The keyword 'all' may also be used, to set all extensions simultaneously.
Setting the environment variable LUNAMARK_EXTENSIONS can change the
defaults.
]]

local long_opts = {
  to = "t",
  layout = "l",
  extensions = "X",
  output = "o",
  standalone = "s",
  template = "T",
  data = "d",
  strict = "0",
  version = "V",
  help = "h"
}

local short_opts = "t:l:X:o:sT:d:0Vh"

local optarg,optind = alt_getopt.get_opts(arg, short_opts, long_opts)

if optarg.h then
  io.write(usage)
  os.exit(0)
end

if optarg.V then
  io.write(version)
  os.exit(0)
end

local to = optarg.t or "html"
ensure_one_of("--to,-t", to,
  {"markdown","html","html5","dzslides","docbook","latex","context","man"})

local reader = lunamark.reader.markdown

local extensions = {  -- defaults
  containers = false,
  slides = false,
  startnum = false,
  smart = false,
  preserve_tabs = false,
  notes = false,
  definition_lists = false,
  lua_metadata = false,
  pandoc_title_blocks = false,
  citeproc = false,
  require_blank_before_blockquote = false,
  require_blank_before_header = false,
  hash_enumerators = false,
}

if optarg["0"] then
  extensions = {}
end

local default_extensions = os.getenv("LUNAMARK_EXTENSIONS") or ""
local extensions_opt = default_extensions .. "," .. (optarg.X or "")
for x in extensions_opt:gmatch("[%+%-]?[%a_]+") do
  local val = true
  if x:sub(1,1) == "+" then
    val = true
    x = x:sub(2)
  elseif x:sub(1,1) == "-" then
    val = false
    x = x:sub(2)
  end
  if x == "all" then
    for k,_ in pairs(extensions) do
      extensions[k] = val
    end
  elseif type(extensions[x]) ~= "boolean" then
    lunamark.util.err("Unrecognized extension: " .. x, 15)
  else
    extensions[x] = val
  end
end

local output = optarg.o
local ok, msg = pcall(function() io.output(output) end)
if not ok then
  lunamark.util.err("Could not open '" .. output .. "' for writing.\n" .. msg, 9)
end

local writer_options = extensions

local layout = optarg["l"] or "default"
ensure_one_of("--layout,-l",layout,{"default","compact","minimize"})
writer_options.layout = layout

local writer = lunamark.writer[to].new(writer_options)
if not writer then
  lunamark.util.err("Unknown writer: " .. tostring(to), 5)
end

local reader_options = extensions

local parse = reader.new(writer, reader_options)

local args = {}
for i=optind,#arg do
  table.insert(args,arg[i])
end

local inp
if #args == 0 then
  inp = io.read("*all") .. "\n"
else
  inpt = {}
  for _,f in ipairs(args) do
    local ok, msg = pcall(function() io.input(f) end)
    if ok then
      table.insert(inpt, io.read("*all"))
    else
      lunamark.util.err("Could not open file '" .. f .. "'", 7)
    end
  end
  inp = table.concat(inpt, "\n") .. "\n"
end
if inp:find("\r",1,true) then
  inp = inp:gsub("\r\n","\n") -- convert DOS line endings
end

local body, metadata = parse(inp)

local standalone = optarg.s
local template = optarg.T

local template_contents

if standalone and not template then
  template_contents = writer.template
end

if template then
  local template_name = template
  -- use writer name as extension if none provided
  if not template_name:match("%..+$") then
    template_name = template_name .. "." .. to
  end
  template_contents = lunamark.util.find_template(template_name)
  if not template_contents then
    lunamark.util.err("Could not find template '" .. template_name .. "'")
  end
end

if template_contents then
  local data = metadata or {}
  data.timestamp = os.date("%Y-%m-%d %X")
  data.sources = args
  local keys = {}
  if optarg.d then
    for x in string.gmatch(optarg.d, "[%w_]+=[^,]+") do
      local k,v = string.match(x, "([^=]*)=(.*)")
      if keys[k] then
        if keys[k] == "string" then
          data[k] = {data[k], v}
        elseif keys[k] == "array" then
          data[k] = table.insert(data[k],v)
        end
        keys[k] = "array"
      else
        data[k] = writer.string(v)
        keys[k] = "string"
      end
    end
  end
  data.body = body
  data["if"] = cosmo.cif  -- this activates the "if" keyword
  data.sepby = lunamark.util.sepby  -- actives "sepby" keyword
  local result = cosmo.fill(template_contents, data)
  io.write(result)
else
  io.write(body)
end

if not template_contents then
  io.write("\n")
end


