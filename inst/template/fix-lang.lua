-- fix-lang.lua
-- 把非中文条目的 language 字段清空，让 CSL 能用 variable="language" 区分中英文

function Meta(meta)
  if meta.references then
    for _, ref in ipairs(meta.references) do
      local lang = ""
      if ref.language then
        local t = type(ref.language)
        if t == "string" then
          lang = ref.language
        else
          lang = pandoc.utils.stringify(ref.language)
        end
      end
      if lang ~= "chinese" then
        ref.language = nil
      end
    end
  end
  return meta
end
