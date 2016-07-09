
local types = require "common.types"

types.parseMany [[
NotYetImplemented: nil

ClassBlockMember:
	  { "type" = "Member", "name" = string, "class" = Type, "public" = boolean, "static" = boolean, "const" = boolean } & HasPosition
	| { "type" = "Cast" } & NotYetImplemented & HasPosition

ClassDefinition: any
InterfaceDefinition: any
]]

local function parseClassBodyStatements( source, name )
	local lexer = source.lexer
	local public = true
	local classname = name:gsub( ".+::", "" )
	
	source:begin "class" .name = name

	local classblock = source:pop()

	while not lexer:skip( "Symbol", "}" ) do
		if lexer:isEOF() then
			throw( lexer, "expected '}' to close class body" )
		end

		local this_public = public
		local this_public_changed = false

		if lexer:test( "Keyword", "public" ) or lexer:test( "Keyword", "private" ) then
			this_public = lexer:skip( "Keyword", "public" ) and true or lexer:skip( "Keyword", "private" ) and false
			this_public_changed = true

			--[[
			if Lua had continue:

				if lexer:skip( "Symbol", ":" ) then
					public = this_public
					continue
				end
			]]
		end

		if lexer:skip( "Symbol", ":" ) and this_public_changed then
			public = this_public

		else
			source:begin "class" .name = name

			if lexer:skip( "Symbol", "->" ) then -- it's a cast
				throw( lexer, "casting isn't implemented go away" )

			elseif lexer:skip( "Keyword", "template" ) then
				parseFunctionTemplate( source, lexer:peek(-1).position )

			else
				if not parseDefinition( source, false ) then
					throw( lexer, "expected definition" )
				end

			end

			local block = source:pop()

			for i = 1, #block do
				block[i].public = this_public

				classblock[#classblock + 1] = block[i]
			end

		end

	end

	return classblock
end

local function formatBlock( statements )
	local block = {}

	for i = 1, #statements do
		if statements[i].type == "Definition" then
			block[i] = {
				type = "Member";
				name = statements[i].name;
				class = statements[i].class;
				public = statements[i].public;
				static = statements[i].static;
				position = statements[i].position;
				const = statements[i].const;
			}

		elseif statements[i].type == "TemplateDefinition" then
			block[i] = {
				type = "Template";
				template = statements[i].template;
				definition = {
					public = statements[i].public;
					static = statements[i].definition.static;
					name = statements[i].definition.name;
					class = statements[i].definition.class;
					position = statements[i].definition.position;
					const = statements[i].definition.const;
				};
			}

		end
	end

	return block
end

local function doMemberAssignment( source, name, statements, position )
	local classref = wrapStringAsReference( name, position )

	for i = 1, #statements do
		if statements[i].type == "Definition" and statements[i].value then

			source:push( wrapExpressionStatement(
				wrapSetExpression( 
					wrapDotIndex( classref, statements[i].name ),
					statements[i].value
				)
			) )

		elseif statements[i].type == "TemplateDefinition" and statements[i].definition.value then

			source:push( wrapTemplateDefinition( statements[i].template, wrapExpressionStatement(
				wrapSetExpression( 
					wrapDotIndex( classref, statements[i].definition.name ),
					statements[i].definition.value
				)
			) ) )

		end
	end
end

local function parseClassAliasDefinition( source, name )
	-- parse stuff
end

function parseClassDefinition( source, pos )
	local lexer = source.lexer
	local name = source:resolveDefinitionName( parseName( source ) or throw( lexer, "expected class name" ) )

	if lexer:skip( "Symbol", "=" ) then
		return parseClassAliasDefinition( source, name )
	end

	local container1 = lexer:skip( "Symbol", "(" ) and assertType( parseType( source ) )
	local container2 = container1 and lexer:skip( "Symbol", "," ) and assertType( parseType( source ) )

	if container1 and not lexer:skip( "Symbol", ")" ) then
		throw( lexer, "expected ')'" )
	end

	if container1 and not container2 then
		container2 = container1
		container1 = wrapTypename "int"
	end

	local extends = lexer:skip( "Keyword", "extends" ) and (parseName( source ) or throw( lexer, "expected super class name" ))
	local implements = {}

	if lexer:skip( "Keyword", "implements" ) then
		repeat
			implements[#implements + 1] = parseName( source ) or throw( lexer, "expected interface name" )
		until not lexer:skip( "Symbol", "," )
	end

	if lexer:skip( "Symbol", "{" ) then
		local statements = parseClassBodyStatements( source, name )
		local block = formatBlock( statements )

		source:push {
			type = "ClassDefinition";
			name = name;
			container = container1 and { container1, container2 };
			extends = extends;
			implements = implements;
			block = block;
			position = pos;
		}

		doMemberAssignment( source, name, statements, position )

	elseif extends or #implements > 0 or container1 then
		throw( lexer, "expected '{' for class body" )

	end

end

function parseInterfaceDefinition( source, pos )
	local lexer = source.lexer
	local name = source:resolveDefinitionName( parseName( source ) or throw( lexer, "expected interface name" ) )

	local implements = {}

	if lexer:skip( "Keyword", "implements" ) then
		repeat
			implements[#implements + 1] = parseName( source ) or throw( lexer, "expected interface name" )
		until not lexer:skip( "Symbol", "," )
	end

	if lexer:skip( "Symbol", "{" ) then
		local statements = parseClassBodyStatements( source, name )
		local block = formatBlock( statements )

		source:push {
			type = "InterfaceDefinition";
			name = name;
			implements = implements;
			block = block;
		}

		doMemberAssignment( source, name, statements, position )

	else
		throw( lexer, "expected '{' for interface body" )

	end

end

local function serializeMember( member )
	return (member.public and "public " or "private ") .. (member.static and "static " or "") .. (member.const and "const " or "") .. serializeType( member.class ) .. " " .. member.name .. ";"
end

local function serializeTemplate( member )
	return "template <" .. serializeFunctionTemplate( member.template ) .. "> " .. serializeMember( member.definition )
end

function serializeClassDefinition( t )
	local i = {}
	local b = {}

	for n = 1, #t.implements do
		i[n] = t.implements[n]
	end

	for i = 1, #t.block do
		b[i] = 
				t.block[i].type == "Member" and serializeMember( t.block[i] )
			 or t.block[i].type == "Template" and serializeTemplate( t.block[i] )
			 or ("<serialiation of " .. t.block[i].type .. " isn't implemented yet>")
	end

	return "class " .. t.name .. " "
		.. (t.container and "(" .. serializeType( t.container[1] ) .. ", " .. serializeType( t.container[2] ) .. ") " or "")
		.. (t.extends and "extends " .. t.extends .. " " or "")
		.. (#i > 0 and "implements " .. table.concat( i, ", " ) .. " " or "")
		.. " {\n\t" .. table.concat( b, "\n" ):gsub( "\n", "\n\t" ) .. "\n}"
end

function serializeInterfaceDefinition( t )
	local i = {}
	local b = {}

	for n = 1, #t.implements do
		i[n] = t.implements[n]
	end

	for i = 1, #t.block do
		b[i] =
				t.block[i].type == "Member" and serializeMember( t.block[i] )
			 or t.block[i].type == "Template" and serializeTemplate( t.block[i] )
			 or ("<serialiation of " .. t.block[i].type .. " isn't implemented yet>")
	end

	return "interface " .. t.name .. " "
		.. (#i > 0 and "implements " .. table.concat( i, ", " ) .. " " or "")
		.. " {\n\t" .. table.concat( b, "\n" ):gsub( "\n", "\n\t" ) .. "\n}"
end

function compileClassDefinition( emitter, t )
	emitter:define( t.name )
	emitter:pushWord( t.name )
	emitter:pushOperator "="
	emitter:pushBlockText( t.extends and "setmetatable( {}, { __index = " .. t.extends .. " } )" or "{}" )
	emitter:pushLineBreak()

	for i = 1, #t.implements do
		emitter:pushBlockText( "for k, v in pairs(" .. t.implements[i] .. ") do\n"
		.. "\t" .. t.name .. "[k] = v\n"
		.. "end" )
	end

	emitter:pushBlockText( "function " .. t.name .. ":new(...)\n"
	.. "\tlocal obj = setmetatable( {}, {__index = self} )\n"
	.. "\tobj:" .. t.name .. "(...)\n"
	.. "\treturn obj\n"
	.. "end" )
end

function compileInterfaceDefinition( emitter, t )
	emitter:define( t.name )
	emitter:pushWord( t.name )
	emitter:pushOperator "="
	emitter:pushSymbol "{}"
	emitter:pushLineBreak()

	for i = 1, #t.implements do
		emitter:pushBlockText( "for k, v in pairs(" .. t.implements[i] .. ") do\n"
		.. "\t" .. t.name .. "[k] = v\n"
		.. "end" )
	end
end
