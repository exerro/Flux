
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
			
			class = assertType( parseType( source ) )

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

function parseDefinition( source )
	-- try to parse a template

	local lexer = source.lexer
	local start = lexer:mark()
	local const = lexer:skip( "Keyword", "const" ) and true or false
	local class = lexer:skipValue( "Keyword", "auto" ) or lexer:skipValue( "Keyword", "void" ) or parseType( source )
	local wasNonFuncDecl = false

	if not class then
		if const then
			throw( lexer, "expected typename after 'const'" )
		else
			return false
		end
	end

	if not lexer:test "Identifier" then
		lexer:jump( start )
		return false
	end

	repeat
		local position = lexer:get().position
		local name = parseName( source ) or throw( lexer, "expected name" )
		local method = lexer:skip( "Symbol", ":" ) and (lexer:skipValue "Identifier" or throw( lexer, "expected name after ':'" ))

		if lexer:test( "Symbol", "(" ) then
			if wasNonFuncDecl then
				throw( lexer, "unexpected '(', expected '='" )
			end

			local parameters = parseFunctionDefinitionParameters( source )
			local body = not lexer:skip( "Symbol", ";" ) and parseFunctionBody( source ) or nil
			
			if method then
				local object = wrapStringAsReference( name, position )
				local object_indexed = wrapDotIndex( object, method, position )
				local set_expression = wrapSetExpression( object_indexed, {
					type = "FunctionExpression";
					returns = class;
					parameters = parameters;
					body = body;
					position = position;
				}, position )

				source:push( wrapExpressionStatement( set_expression ) )
			else
				source:push {
					type = "FunctionDefinition";
					returns = class;
					parameters = parameters;
					body = body;
					name = name;
					const = const;
					position = position;
				}
			end

			break

		elseif method then
			throw( lexer, "expected '(' after method name" )

		else
			wasNonFuncDecl = true

			source:push {
				type = "Definition";
				value = lexer:skip( "Symbol", "=" ) and (parseExpression( source ) or throw( lexer, "expected expression after '='" ));
				class = class;
				const = const;
				name = name;
				position = position;
			}
		end

	until not lexer:skip( "Symbol", "," )

	if wasNonFuncDecl and not expectSemicolon( lexer ) then
		throw( lexer, "expected ';'" )
	end

	return true
end

function serializeDefinition( t )

	if t.type == "Definition" then
		return (t.const and "const " or "") .. serializeType( t.class ) .. " " .. t.name .. (t.value and " = " .. serializeExpression( t.value ) or "") .. ";"

	elseif t.type == "FunctionDefinition" then
		local p = {}

		for i = 1, #t.parameters do
			p[i] = serializeType( t.parameters[i].class ) .. " " .. t.parameters[i].name
		end

		return (t.const and "const " or "") .. serializeType( t.returns ) .. " " .. t.name .. "(" .. table.concat( p, ", " ) .. ") " .. (t.body and serializeBlock( t.body ) or ";")

	else
		return "<serialization of " .. t.type .. " isn't written>"

	end

end
