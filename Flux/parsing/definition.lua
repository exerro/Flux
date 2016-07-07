
local types = require "common.types"

types.parseMany [[
FunctionDefinitionParameters: { number = { "class" = Type, "name" = string, "nullable" = boolean } } | {}
GenericDefinition: { "const" = boolean, "name" = string } & HasPosition

FunctionDefinition: { "type" = "FunctionDefinition", "body" = Block | nil, "parameters" = FunctionDefinitionParameters, "returns" = Type } & GenericDefinition
Definition: { "type" = "Definition", "value" = Expression | nil, "class" = Type } & GenericDefinition
TemplateDefinition: { "type" = "TemplateDefinition", "definition" = FunctionDefinition, "template" = { number = { "name" = string, "limits" = {} } } } & HasPosition

ClassFunctionDefinition: { "public" = boolean, "static" = boolean } & FunctionDefinition
ClassDefinition: { "public" = boolean, "static" = boolean } & Definition
]]

local function parseTemplateLimits( source )
	return {}
end

function parseFunctionTemplate( source, pos )
	local lexer = source.lexer

	if not lexer:skip( "Symbol", "<" ) then
		throw( lexer, "expected '<' after 'template'" )
	end

	local template_classes = { {
		name = lexer:skipValue "Identifier" or throw( lexer, "expected name for template class" );
		limits = parseTemplateLimits( source );
	} }

	while lexer:skip( "Symbol", "," ) or lexer:skip( "Symbol", ";" ) and not lexer:test( "Symbol", ">" ) do
		template_classes[#template_classes + 1] = {
			name = lexer:skipValue "Identifier" or throw( lexer, "expected name for template class" );
			limits = parseTemplateLimits( source );
		}
	end

	if not lexer:skip( "Symbol", ">" ) then
		throw( lexer, "expected '>' after template classes" )
	end

	source:begin "template"

	if lexer:skip( "Keyword", "let" ) then
		parseLetStatement( source, lexer:peek( -1 ).position, true )
	else
		parseDefinition( source, true )
	end

	local block = source:pop()

	source:push {
		type = "TemplateDefinition";
		definition = block[1];
		template = template_classes;
		position = pos;
	}
end

function parseFunctionDefinitionParameters( source )
	local lexer = source.lexer

	if not lexer:skip( "Symbol", "(" ) then
		throw( lexer, "expected '(' to start function parameters" )
	end

	if lexer:skip( "Symbol", ")" ) then
		return {}
	end

	local last_class
	local last_nullable = false
	local parameters = {}

	while true do
		local thisNullable = lexer:skip( "Keyword", "null" )

		if lexer:skip( "Keyword", "auto" ) then
			last_class = "auto"

		else
			local start = lexer:mark()
			local class, err = parseType( source )

			if class and lexer:test "Identifier" then -- [null] type pname
				last_class = class
				last_nullable = thisNullable

			elseif class and thisNullable then -- null pname ,(
				throw( lexer, "expected parameter name after type" )

			elseif class and last_class then -- pname
				lexer:jump( start )

			elseif class then -- pname without prior class
				throw( lexer, "expected parameter name after type" )

			else -- null ,
				error( err, 0 )
			end
		end

		local name = lexer:skipValue "Identifier" or throw( lexer, "expected parameter name" )

		parameters[#parameters + 1] = {
			name = name;
			class = last_class;
			nullable = last_nullable;
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

function parseDefinition( source, expectFunction )
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
		local name = source:resolveDefinitionName( parseName( source ) or throw( lexer, "expected name" ) )
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

		elseif expectFunction then
			throw( lexer, "expected '(' for function definition" )

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
			p[i] = (t.parameters[i].nullable and "null " or "") .. serializeType( t.parameters[i].class ) .. " " .. t.parameters[i].name
		end

		return (t.const and "const " or "") .. serializeType( t.returns ) .. " " .. t.name .. "(" .. table.concat( p, ", " ) .. ") " .. (t.body and serializeBlock( t.body ) or ";")

	elseif t.type == "TemplateDefinition" then
		local c = {}

		for i = 1, #t.template do
			c[i] = t.template[i].name
		end

		return "template <" .. table.concat( c, ", " ) .. ">\n" .. serializeDefinition( t.definition )

	else
		return "<serialization of " .. t.type .. " isn't written>"

	end

end
