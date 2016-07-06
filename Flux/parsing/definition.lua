
--[[
int x;
int x = 1;
int x[] = [1];
int x = 1, y = 2;

type T {} f() {}
type T {} f() = v;
type T {} f();

type T {} a:b() {}

void -> function
auto -> any
type T -> function
]]

local types = require "common.types"

types.parseMany [[
FunctionDefinitionParameters: { number = { "class" = Type, "name" = string } } | {}
GenericDefinition: { "const" = boolean, "name" = string } & HasPosition

FunctionDefinition: { "type" = "FunctionDefinition", "body" = Block | nil, "parameters" = FunctionDefinitionParameters, "returns" = Type } & GenericDefinition
Definition: { "type" = "Definition", "value" = Expression | nil, "class" = Type } & GenericDefinition

ClassFunctionDefinition: { "public" = boolean, "static" = boolean } & FunctionDefinition
ClassDefinition: { "public" = boolean, "static" = boolean } & Definition
]]

function parseFunctionDefinitionParameters( source )
	local lexer = source.lexer

	if not lexer:skip( "Symbol", "(" ) then
		throw( lexer, "expected '(' to start function parameters" )
	end

	if lexer:skip( "Symbol", ")" ) then
		return {}
	end

	local last_class
	local parameters = {}

	while true do
		local class
		local name

		if lexer:skip( "Keyword", "auto" ) then
			class = "auto"

		elseif lexer:test "Identifier" then
			local start = lexer:mark()
			
			class = parseType( source )

			if lexer:test "Identifier" then
				last_class = class
			elseif last_class then
				lexer:jump( start )
				class = last_class
			else
				lexer:jump( start )
				throw( lexer, "expected type" )
			end
		end

		name = lexer:skipValue "Identifier" or throw( lexer, "expected parameter name" )

		parameters[#parameters + 1] = {
			name = name;
			class = class;
		}

		if not lexer:skip( "Symbol", "," ) then
			break
		end
	end

	if not lexer:skip( "Symbol", ")" ) then
		throw( lexer, "expected ')' to close parameter list" )
	end

	return parameters
end

function parseFunctionBody( source )
	local lexer = source.lexer

	if lexer:skip( "Symbol", "=" ) then
		local expr = parseExpression( source ) or throw( lexer, "expected expression after '='" )

		if not expectSemicolon( lexer ) then
			throw( source, "expected ';' after expression" )
		end

		source:begin "function"
		source:push( wrapReturnStatement( expr ) )

		return source:pop()
	else
		return parseBlock( source, "function" )
	end
end

local function parseDefinition( source )
	-- try to parse a template

	local lexer = source.lexer
	local typename = lexer:skipValue( "Keyword", "auto" ) or lexer:skipValue( "Keyword", "void" ) or parseName( source )

	if not typename then
		return
	end
end

function serializeDefinition( t )

	if t.type == "Definition" then
		return (t.const and "const " or "") .. serializeType( t.class ) .. " " .. t.name .. (t.value and " = " .. serializeExpression( t.value ) or "")

	elseif t.type == "FunctionDefinition" then
		local p = {}

		for i = 1, #t.parameters do
			p[i] = serializeType( t.parameters[i].class ) .. " " .. t.parameters[i].name
		end

		return (t.const and "const " or "") .. serializeType( t.returns ) .. " " .. t.name .. "(" .. table.concat( p, ", " ) .. ") " .. (t.body and serializeBlock( t.body ) or "")

	else
		return "<serialization of " .. t.type .. " isn't written>"

	end

end
