
local types = require "common.types"

types.parseMany [[
BlockType: "whileloop" | "repeatloop" | "foreachloop" | "forloop" | "function" | "general"
Block: { "meta" = { "type" = BlockType }, number = Statement } | {}
]]

function parseBlock( source, blocktype )
	local lexer = source.lexer
	local block = source:begin( blocktype )

	if lexer:skip( "Symbol", "{" ) then
		while not lexer:skip( "Symbol", "}" ) do
			if lexer:isEOF() then
				throw( lexer, "expected '}' to close block" )
			end

			parseStatement( source )
		end
	else
		parseStatement( source )
	end

	local t = source:pop()

	if #t == 1 and t[1].type == "DoStatement" then
		return t[1].block
	end

	return t
end

function serializeBlock( t )
	if #t == 0 then
		return "{}"
	end

	if #t == 1 then
		return "\n\t" .. serializeStatement( t[1] ):gsub( "\n", "\n\t" )
	end

	local b = {}

	for i = 1, #t do
		b[i] = serializeStatement( t[i] ):gsub( "\n", "\n\t" )
	end

	return "{\n\t" .. table.concat( b, "\n\t" ) .. "\n}"
end
