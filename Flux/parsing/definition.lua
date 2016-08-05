
local lang = require "Flux.lang"
local types = require "common.types"

types.parseMany [[
FunctionDefinitionParameters: { number = { "class" = Type, "name" = string, "nullable" = boolean, "default" = Expression | nil } } | {}
GenericDefinition: { "const" = boolean, "name" = string } & HasPosition

Definition: { "type" = "Definition", "value" = Expression | nil, "class" = Type } & GenericDefinition
TemplateDefinition: { "type" = "TemplateDefinition", "definition" = Definition | ExpressionStatement, "template" = { number = { "name" = string, "limits" = {} } } } & HasPosition
]]

local function trimParameters( p, i )
	local t = {}

	for i = 1, i do
		t[i] = p[i].default and { name = p[i].name, class = p[i].class, nullable = p[i].nullable } or p[i]
	end

	return t
end

function wrapDefinition( name, class, value, const, static, position )
	return {
		type = "Definition";
		name = name;
		class = class;
		value = value;
		const = const;
		static = static;
		position = position;
	}
end

function wrapTemplateDefinition( template, definition, position )
	return {
		type = "TemplateDefinition";
		definition = definition;
		template = template;
		position = position or definition.position;
	}
end

function dealWithDefaultBody( block, parameters )
	for i = 1, #parameters do
		if parameters[i].default then
			local parameter_name = wrapStringAsReference( parameters[i].name, parameters[i].default.position )
			local condition_if_parameter_is_null = wrapEqualityCheck( parameter_name, nullExpression( parameters[i].default.position ) )
			local block_if_parameter_is_null = {
				meta = { type = "general" };
				[1] = wrapExpressionStatement( wrapSetExpression( parameter_name, parameters[i].default ) );
			}

			parameters[i].default = nil
			table.insert( block, 1, wrapIfStatement( condition_if_parameter_is_null, block_if_parameter_is_null ) )
		end
	end

	return block
end

function dealWithDefaultOverloads( source, const, class, name, defaults, parameters, static, position )
	if not lang.ADD_DEFAULT_VALUE_OVERLOAD_FUNCTIONS then
		return
	end

	local llim = #defaults + 1

	for i = #defaults, 1, -1 do
		if not defaults[i] then
			break
		end
		llim = llim - 1
	end

	for i = #defaults, llim, -1 do
		local p = {}
		local body = { meta = { type = "function" } }
		local parameter_types = {}

		for n = 1, i-1 do
			p[n] = wrapStringAsReference( parameters[n].name, position )
			parameter_types[n] = parameters[n].class
		end

		p[i] = defaults[i]
		body[1] = wrapReturnStatement( wrapFunctionCall( wrapStringAsReference( name, position ), p ) )

		source:push( wrapDefinition( name, wrapFunctionType( class, parameter_types ), wrapFunction( class, trimParameters( parameters, i - 1 ), body ), const, static, position ) )
	end
end

local function parseTemplateLimits( source )
	local lexer = source.lexer

	if lexer:skip( "Symbol", ":" ) then
		return { assertType( parseType( source ) ) }

	elseif lexer:skip( "Symbol", "[" ) then
		local classes = {}

		repeat
			classes[#classes + 1] = assertType( parseType( source ) )
		until not lexer:skip( "Symbol", "," )

		if not lexer:skip( "Symbol", "]" ) then
			throw( lexer, "expected ']'" )
		end

		return classes
	else
		return {}

	end
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

	source:beginCopy()

	if lexer:skip( "Keyword", "let" ) and not source:getClassName() then
		parseLetStatement( source, lexer:peek( -1 ).position, true )

	elseif not parseDefinition( source, true ) then
		throw( lexer, "expected definition after template" )

	end

	local block = source:pop()

	for i = 1, #block do
		source:push( wrapTemplateDefinition( template_classes, block[i], pos ) )
	end
end

function parseFunctionDefinitionParameters( source, allowDefaults, limitToOneParameter )
	local lexer = source.lexer

	if not lexer:skip( "Symbol", "(" ) then
		throw( lexer, "expected '(' to start function parameters" )
	end

	if lexer:skip( "Symbol", ")" ) then
		return {}, {}
	end

	local last_class
	local last_nullable = false
	local parameters = {}
	local defaults = {}
	local finalDefaultsStarted = false

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
		local default = lexer:skip( "Symbol", "=" ) and
			(
			 (allowDefaults or last_nullable) and (parseExpression( source ) or throw( lexer, "expected expression after '='" ))
			 or throw( lexer, "default values for non-nullable parameters aren't permitted here" )
			)
			or finalDefaultsStarted and throw( lexer, "expected '=' for default value" )

		finalDefaultsStarted = finalDefaultsStarted or allowDefaults and default and not last_nullable

		parameters[#parameters + 1] = {
			name = name;
			class = last_class;
			nullable = last_nullable;
			default = last_nullable and default or nil;
		}

		defaults[#parameters] = default or false

		if not lexer:skip( "Symbol", "," ) then
			break

		elseif limitToOneParameter then
			lexer:back()
			throw( lexer, "expected ')'" )

		end
	end

	if not lexer:skip( "Symbol", ")" ) then
		throw( lexer, "expected ')' to close parameter list" )
	end

	return parameters, defaults
end

function parseFunctionBody( source )
	local lexer = source.lexer

	if lexer:skip( "Symbol", "=" ) then
		local expr = parseExpression( source ) or throw( lexer, "expected expression after '='" )
		
		source:begin "function"

		while lexer:skip( "Keyword", "where" ) do
			local position = lexer:peek(-1).position
			local name = lexer:skipValue "Identifier" or throw( lexer, "expected name" )
			local value = lexer:skip( "Symbol", "=" ) and (parseExpression( source ) or throw( lexer, "expected expression after '='" )) or throw( lexer, "expected '='" )

			source:push( wrapDefinition( name, "auto", value, true, nil, position ) )
		end

		source:push( wrapReturnStatement( expr ) )

		if not expectSemicolon( lexer ) then
			throw( lexer, "expected ';' after expression" )
		end

		return source:pop()
	else
		return parseBlock( source, "function" )
	end
end

function parseDefinition( source, expectFunction )
	local lexer = source.lexer
	local start = lexer:mark()
	local const = lexer:skip( "Keyword", "const" ) and true or false
	local classname = source:getClassName()
	local static = classname and lexer:skip( "Keyword", "static" ) and true or false
	local position = lexer:get().position

	if not classname then
		static = nil
	end

	if classname and not static and lexer:skip( "Identifier", classname:gsub( lang.REPLACE_COLONS_WITH_UNDERSCORES and ".+__" or ".+::", "" ) ) then
		if lexer:test( "Symbol", "(" ) then
			local parameters, defaults = parseFunctionDefinitionParameters( source, true )
			local body = not lexer:skip( "Symbol", ";" ) and dealWithDefaultBody( parseFunctionBody( source ), parameters ) or nil
			local methodname = classname:gsub( ".+::", "" )
			local parameter_types = {}
			local class = wrapTypename( classname )

			table.insert( parameters, 1, { class = wrapTypename( classname ), name = "self", nullable = false, default = nil } )
			table.insert( defaults, 1, false )

			if body then
				table.insert( body, wrapReturnStatement( wrapStringAsReference( "self", position ) ) )
			end
		
			for i = 1, #parameters do
				parameter_types[i] = parameters[i].class
			end

			source:push( wrapDefinition( methodname, wrapFunctionType( class, parameter_types ), body and wrapFunction( class, parameters, body ) or nil, const, static, position ) )

			dealWithDefaultOverloads( source, const, class, methodname, defaults, parameters, static, position )

			return true

		else
			lexer:jump( start )
		end

	elseif classname and lexer:skip( "Keyword", "setter" ) then -- it's a setter
		local name = lexer:skipValue "Identifier" or throw( lexer, "expected variable name" )

		if not lexer:test( "Symbol", "(" ) then
			throw( lexer, "expected '('" )
		end

		local parameters, defaults = parseFunctionDefinitionParameters( source, true, true )
		local body = not lexer:skip( "Symbol", ";" ) and dealWithDefaultBody( parseFunctionBody( source ), parameters ) or nil
		local methodname = "setter " .. name
		local parameter_types = {}
		local class = wrapTypename( classname )

		if body == nil then
			body = { meta = { type = "function" } }
			body[1] = wrapExpressionStatement( wrapSetExpression( wrapDotIndex( wrapStringAsReference( "self", position ), name ), wrapStringAsReference( name, position ) ) )
		end

		table.insert( parameters, 1, { class = wrapTypename( classname ), name = "self", nullable = false, default = nil } )
		table.insert( defaults, 1, false )
		table.insert( body, wrapReturnStatement( wrapStringAsReference( "self", position ) ) )
	
		for i = 1, #parameters do
			parameter_types[i] = parameters[i].class
		end

		source:push( wrapDefinition( methodname, wrapFunctionType( class, parameter_types ), body and wrapFunction( class, parameters, body ) or nil, const, static, position ) )
		dealWithDefaultOverloads( source, const, class, methodname, defaults, parameters, static, position )

		return true

	elseif classname and lexer:skip( "Keyword", "getter" ) then -- it's a getter
		local name = lexer:skipValue "Identifier" or throw( lexer, "expected variable name" )

		if lexer:skip( "Symbol", "(" ) then
			if not lexer:skip( "Symbol", ")" ) then
				throw( lexer, "expected ')'" )
			end
		end

		local body = not lexer:skip( "Symbol", ";" ) and parseFunctionBody( source ) or nil
		local methodname = "getter " .. name
		local parameter_types = { wrapTypename( classname ) }
		local class = "auto"
		local parameters = { { class = wrapTypename( classname ), name = "self", nullable = false, default = nil } }

		if body == nil then
			body = { meta = { type = "function" } }
			body[1] = wrapReturnStatement( wrapDotIndex( wrapStringAsReference( "self", position ), name ) )
		end

		source:push( wrapDefinition( methodname, wrapFunctionType( class, parameter_types ), body and wrapFunction( class, parameters, body ) or nil, const, static, position ) )

		return true

	end

	local class = lexer:skipValue( "Keyword", "auto" ) or lexer:skipValue( "Keyword", "void" ) or parseType( source )
	local wasNonFuncDecl = false

	if not class then
		if const then
			throw( lexer, "expected typename after 'const'" )
		else
			return false
		end
	end

	if not lexer:test "Identifier" and (not classname or not lexer:test( "Keyword", "operator" )) then
		lexer:jump( start )
		return false
	end

	repeat
		local position = lexer:get().position
		local name, wasOperator, method = nil, false, nil

		if classname then
			name, wasOperator = parseMethodName( source )
			name = source:resolveDefinitionName( name or throw( lexer, "expected name" ) )
			method = lexer:skip( "Symbol", ":" ) and throw( lexer, "unexpected method name in class definition" )

		else
			name = source:resolveDefinitionName( parseName( source ) or throw( lexer, "expected name" ) )
			method = lexer:skip( "Symbol", ":" ) and (parseMethodName( source ) or throw( lexer, "expected name after ':'" ))

		end

		if lexer:test( "Symbol", "(" ) then
			if wasNonFuncDecl then
				throw( lexer, "unexpected '(', expected '='" )
			end

			local parameters, defaults = parseFunctionDefinitionParameters( source, not method )
			local body = not lexer:skip( "Symbol", ";" ) and dealWithDefaultBody( parseFunctionBody( source ), parameters ) or nil
			local parameter_types = {}

			if classname then
				table.insert( parameters, 1, { class = wrapTypename( classname ), name = "self", nullable = false, default = nil } )
				table.insert( defaults, 1, false )
			end
			
			for i = 1, #parameters do
				parameter_types[i] = parameters[i].class
			end
			
			if method then
				table.insert( parameters, 1, {
					class = wrapTypename( name );
					name = "self";
					nullable = false;
				} )

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
				source:push( wrapDefinition( name, wrapFunctionType( class, parameter_types ), body and wrapFunction( class, parameters, body ) or nil, const, static, position ) )
				dealWithDefaultOverloads( source, const, class, name, defaults, parameters, static, position )
			end

			return true

		elseif expectFunction then
			throw( lexer, "expected '(' for function definition" )

		elseif wasOperator then
			throw( lexer, "expected '(' after operator name" )

		elseif method then
			throw( lexer, "expected '(' after method name" )

		else
			wasNonFuncDecl = true

			source:push( wrapDefinition( name, class, lexer:skip( "Symbol", "=" ) and (parseExpression( source ) or throw( lexer, "expected expression after '='" )), const, static, position ) )
		end

	until not lexer:skip( "Symbol", "," )

	if not expectSemicolon( lexer ) then
		throw( lexer, "expected ';'" )
	end

	return true
end

function serializeFunctionDefinitionParameters( t )
	local p = {}

	for i = 1, #t do
		p[i] = (t[i].nullable and "null " or "") .. serializeType( t[i].class ) .. " " .. t[i].name .. (t[i].default and " = " .. serializeExpression( t[i].default ) or "")
	end

	return "(" .. table.concat( p, ", " ) .. ")"
end

function serializeFunctionTemplate( t )
	local c = {}

	for i = 1, #t do
		local l = {}

		for n = 1, #t[i].limits do
			l[n] = serializeType( t[i].limits[n] )
		end

		c[i] = t[i].name .. (#t[i].limits > 0 and " [" .. table.concat( l, ", " ) .. "]" or "")
	end

	return table.concat( c, ", " )
end

function serializeDefinition( t )

	if t.type == "Definition" then
		return (t.const and "const " or "") .. serializeType( t.class ) .. " " .. t.name .. (t.value and " = " .. serializeExpression( t.value ) or "") .. ";"

	elseif t.type == "TemplateDefinition" then
		return "template <" .. serializeFunctionTemplate( t.template ) .. ">\n" .. (t.definition.type == "Definition" and serializeDefinition( t.definition ) or serializeStatement( t.definition ))

	else
		return "<serialization of " .. t.type .. " isn't written>"

	end

end

function compileDefinition( emitter, t )

	if t.type == "Definition" then
		local isFunctionDefinition = t.class.type == "FunctionType"

		emitter:define( t.name )
		
		if isFunctionDefinition then
			if not t.value then
				emitter:pushWord "local"
			end

			emitter:pushWord "function"
			emitter:pushWord( t.name )

			if t.value then
				emitter:pushSymbol "("

				for i = 1, #t.value.parameters do
					emitter:pushWord( t.value.parameters[i].name )

					if i < #t.value.parameters then
						emitter:pushDelimiter ","
					end
				end

				emitter:pushSymbol ")"

				compileBlock( emitter, t.value.body )

				emitter:pushLineBreak()
				emitter:pushWord "end"
			else
				emitter:pushSymbol "()"
				emitter:pushWord "end"
			end
		else
			emitter:pushWord "local"
			emitter:pushWord( t.name )

			if t.value then
				emitter:pushOperator "="

				compileExpression( emitter, t.value )

				emitter:pushSymbol ";"
			end

		end


	elseif t.type == "TemplateDefinition" then
		if t.definition.type == "Definition" then
			return compileDefinition( emitter, t.definition )
		else
			return compileExpressionStatement( emitter, t.definition )
		end

	else
		emitter:push( "<compilation of " .. t.type .. " isn't written>" )

	end

end
