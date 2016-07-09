
local types = require "common.types"

types.parseMany [[

Typename: { "type" = "Typename", "name" = string }
FunctionType: { "type" = "FunctionType", "returns" = Type, "parameters" = { number = Type } | {} }
ArrayType: { "type" = "ArrayType", "value" = Type }
TableType: { "type" = "TableType", "value" = Type, "index" = Type }

Type: Typename | FunctionType | ArrayType | TableType | "auto" | "void"

]]

function wrapFunctionType( returns, parameters )
	return { type = "FunctionType", returns = returns, parameters = parameters }
end

function wrapTypename( name )
	return { type = "Typename", name = name }
end

local function parseFunctionType( source ) -- function int(int) etc
	local lexer = source.lexer
	local returns = parseType( source )
	local parameters = {}

	if not returns then
		return false, lexer:formatError( "expected return type after 'function'", false, lexer:get().position )
	end

	if not lexer:skip( "Symbol", "(" ) then
		return false, lexer:formatError( "expected '(' after return type", false, lexer:get().position )
	end

	if not lexer:skip( "Symbol", ")" ) then
		while true do
			local t, err = parseType( source )

			if not t then
				return false, err
			end

			parameters[#parameters + 1] = t

			if not lexer:skip( "Symbol", "," ) then
				break
			end
		end

		if not lexer:skip( "Symbol", ")" ) then
			return false, lexer:formatError( "expected ')' after parameters", false, lexer:get().position )
		end
	end

	return wrapFunctionType( returns, parameters )
end

function parseTypename( source ) -- int, string{int}, bool[], etc
	local lexer = source.lexer
	local start = lexer:mark()

	if lexer:skip( "Keyword", "function" ) then
		local v, e = parseFunctionType( source )
		if not v then
			lexer:jump( start )
		end
		return v, e
	else
		local name = parseName( source )

		if name then
			return wrapTypename( name )
		else
			return false, lexer:formatError( "expected typename", false, lexer:get().position )
		end
	end
end

function parseTypeModifiers( source, v, e )
	local lexer = source.lexer
	local start = lexer:mark()

	if not v then
		return false, e
	end

	while true do
		if lexer:skip( "Symbol", "{" ) then
			v = { type = "TableType", index = parseType( source ), value = v }

			if not lexer:skip( "Symbol", "}" ) then
				lexer:jump( start )
				return false, lexer:formatError( "expected '}' after type", false, lexer:get().position )
			end

		elseif lexer:skip( "Symbol", "[" ) then
			v = { type = "ArrayType", value = v }

			if not lexer:skip( "Symbol", "]" ) then
				lexer:jump( start )
				return false, lexer:formatError( "expected ']' after '['", false, lexer:get().position )
			end

		else
			break

		end
	end

	return v
end

function parseType( source )
	local start = source.lexer:mark()
	local class, err = parseTypeModifiers( source, parseTypename( source ) )

	if not class then
		source.lexer:jump( start )
	end

	return class, err
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
