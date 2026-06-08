-- center-caption.lua
-- 让 docx 输出中的表格标题段落使用 Table Caption 样式（已在模板中设为居中）
function Table(tbl)
  if tbl.caption and tbl.caption.long then
    -- 遍历 caption 内容，给每个段落加上 custom-style
    local new_blocks = {}
    for _, block in ipairs(tbl.caption.long) do
      if block.t == "Para" then
        block.attributes = block.attributes or {}
        block.attr = pandoc.Attr("", {}, {{"custom-style", "Table Caption"}})
      end
      table.insert(new_blocks, block)
    end
    tbl.caption.long = new_blocks
  end
  return tbl
end
