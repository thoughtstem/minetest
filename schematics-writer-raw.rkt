#lang racket

(provide schematics-writer-raw)
(define schematics-writer-raw
  "
local mts_save = function(name, schematic)
	local s = minetest.serialize_schematic(schematic, 'mts', {})
	local path = minetest.get_modpath('my_racket_mod') .. '/schematics'
	local filename = path .. '/' .. name .. '.mts'
	local file, err = io.open(filename, 'wb')
	if err == nil then
		file:write(s)
		file:flush()
		file:close()
	end
	print('Wrote: ' .. filename)
end
")
