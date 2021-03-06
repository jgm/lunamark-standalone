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
