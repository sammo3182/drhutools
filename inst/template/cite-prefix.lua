--[[
  cite-prefix.lua — route citations to Alexandria bibliographies.

  Adds an Alexandria prefix to every citation key so that citations are
  rendered by the matching #bibliographyx() call:
    * key only in the main .bib      -> "main:"     (main reference list)
    * key only in the appendix .bib  -> "appendix:" (OSM reference list)
    * key in BOTH .bib files         -> by location: citations before the
      OSM heading get "main:", those after get "appendix:" (so a work cited
      in both the body and the OSM appears in both lists).

  Bib paths and the appendix heading id are read from document metadata:
    alexandria-main-bib, alexandria-appendix-bib, alexandria-appendix-id
]]

local main_keys, app_keys = {}, {}

local function load_keys(path, set)
  if not path then return end
  local f = io.open(path, "r")
  if not f then
    io.stderr:write("[cite-prefix] WARNING: cannot open bib file: " .. path .. "\n")
    return
  end
  for line in f:lines() do
    -- match @article{Key,  /  @book{ Key ,  etc.
    local key = line:match("^%s*@%w+%s*{%s*([^,%s]+)%s*,?")
    if key then set[key] = true end
  end
  f:close()
end

-- Quarto cross-reference keys (e.g. @fig-trend, @sec-app1) also appear as Cite
-- elements but are not bibliography citations — leave them untouched.
local crossref_types = {
  fig = true, tbl = true, sec = true, eq = true, lst = true, thm = true,
  lem = true, cor = true, prp = true, cnj = true, def = true, exm = true,
  exr = true, sol = true, rem = true, fnref = true, note = true,
}

local function is_crossref(id)
  local kind = id:match("^(%a+)%-")
  return kind ~= nil and crossref_types[kind] == true
end

local function prefixed(id, in_appendix)
  if is_crossref(id) then return id end
  local in_main = main_keys[id]
  local in_app = app_keys[id]
  if in_main and in_app then
    return (in_appendix and "appendix:" or "main:") .. id
  elseif in_main then
    return "main:" .. id
  elseif in_app then
    return "appendix:" .. id
  else
    io.stderr:write("[cite-prefix] WARNING: key not found in either bib: " .. id .. "\n")
    return id
  end
end

local appendix_id = "online-supplementary-materials"

function Meta(m)
  local function s(v) return v and pandoc.utils.stringify(v) or nil end
  load_keys(s(m["alexandria-main-bib"]), main_keys)
  load_keys(s(m["alexandria-appendix-bib"]), app_keys)
  if m["alexandria-appendix-id"] then
    appendix_id = s(m["alexandria-appendix-id"])
  end
end

function Pandoc(doc)
  local in_appendix = false
  local function fix_cites(blk)
    return pandoc.walk_block(blk, {
      Cite = function(c)
        for _, cit in ipairs(c.citations) do
          cit.id = prefixed(cit.id, in_appendix)
        end
        return c
      end,
    })
  end

  local out = {}
  for _, blk in ipairs(doc.blocks) do
    if blk.t == "Header" and blk.identifier == appendix_id then
      in_appendix = true
    end
    table.insert(out, fix_cites(blk))
  end
  doc.blocks = pandoc.Blocks(out)
  return doc
end

-- Ensure Meta runs before Pandoc.
return { { Meta = Meta }, { Pandoc = Pandoc } }
