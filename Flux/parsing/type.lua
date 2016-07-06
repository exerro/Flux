
local types = require "common.types"

types.parseMany [[

Typename: { "type" = "Typename", "name" = string }
FunctionType: { "type" = "FunctionType", "returns" = Type, "parameters" = { number = Type } | {} }
ArrayType: { "type" = "ArrayType", "value" = Type }
TableType: { "type" = "TableType", "value" = Type, "index" = Type }

Type: Typename | FunctionType | ArrayType | TableType | "auto" | "void"

]]

local function parseFunctionType( source ) -- function int(int) etc
	local lexer = source.lexer
	local returns = parseType( source )
	local parameters = {}

	if not lexer:skip( "Symbol", "(" ) then
		throw( lexer, "expected '(' after return type" )
	end

	if not lexer:skip( "Symbol", ")" ) then
		while true do
			parameters[#parameters + 1] = parseType( source )

			if not lexer:skip( "Symbol", "," ) then
				break
			end
		end

		if not lexer:skip( "Symbol", ")" ) then
			throw( lexer, "expected ')' after parameters" )
		end
	end

	return { type = "FunctionType", returns = returns, parameters = parameters }
end

function parseTypename( source ) -- int, string{int}, bool[], etc
	local lexer = source.lexer

	if lexer:skip( "Keyword", "function" ) then
		return parseFunctionType( source )
	else
		return { type = "Typename", name = parseName( source ) or throw( lexer, "expected typename" ) }
	end
end

function parseTypeModifiers( source, v )
	local lexer = source.lexer

	while true do
		if lexer:skip( "Symbol", "{" ) then
			v = { type = "TableType", index = parseType( source ), value = v }

			if not lexer:skip( "Symbol", "}" ) then
				throw( lexer, "expected '}' after type" )
			end

		elseif lexer:skip( "Symbol", "[" ) then
			v = { type = "ArrayType", value = v }

			if not lexer:skip( "Symbol", "]" ) then
				throw( lexer, "expected ']' after '['" )
			end

		else
			break

		end
	end

	return v
end

function parseType( source )
	return parseTypeModifiers( source, parseTypename( source ) )
end

function serializeType( t )
	if t == "auto" or t == "void" then
		return t

	elseif t.type == "ArrayType" then
		return serializeType( t.value ) .. "[]"

	elseif t.type == "TableType" then
		return serializeType( t.value ) .. "{" .. serializeType( t.index ) .. "}"

	elseif t.type == "FunctionType" then
		local s = serializeType( t.returns ) .. "("
		local p = {}

		for i = 1, #t.parameters do
			p[i] = serializeType( t.parameters[i] )
		end

		return s .. table.concat( p, ", " ) .. ")"

	elseif t.type == "Typename" then
		return t.name

	end
end
