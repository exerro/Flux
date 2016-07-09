
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
		local block = {}

		for i = 1, #statements do
			block[i] = {
				type = "Member";
				name = statements[i].name;
				class = statements[i].class;
				public = statements[i].public;
				static = statements[i].static;
				position = statements[i].position;
				const = statements[i].const;
			}
		end

		source:push {
			type = "ClassDefinition";
			name = name;
			container = container1 and { container1, container2 };
			extends = extends;
			implements = implements;
			block = block;
			position = pos;
		}

		local classref = wrapStringAsReference( name, position )

		for i = 1, #statements do
			if statements[i].value then

				source:push( wrapExpressionStatement(
					wrapSetExpression( 
						wrapDotIndex( classref, statements[i].name ),
						statements[i].value
					)
				) )

			end
		end

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
		local block = {}

		for i = 1, #statements do
			block[i] = {
				type = "Member";
				name = statements[i].name;
				class = statements[i].class;
				public = statements[i].public;
				static = statements[i].static;
				position = statements[i].position;
				const = statements[i].const;
			}
		end

		source:push {
			type = "InterfaceDefinition";
			name = name;
			implements = implements;
			block = block;
		}

		local interfaceref = wrapStringAsReference( name, position )

		for i = 1, #statements do
			if statements[i].value then

				source:push( wrapExpressionStatement(
					wrapSetExpression( 
						wrapDotIndex( interfaceref, statements[i].name ),
						statements[i].value
					)
				) )

			end
		end

	else
		throw( lexer, "expected '{' for interface body" )

	end

end

function serializeClassDefinition( t )
	local i = {}
	local b = {}

	for n = 1, #t.implements do
		i[n] = t.implements[n]
	end

	for i = 1, #t.block do
		b[i] = (t.block[i].public and "public " or "private ")
			.. (t.block[i].static and "static " or "")
			.. (t.block[i].type == "Member"
			and ((t.block[i].const and "const " or "") .. serializeType( t.block[i].class ) .. " " .. t.block[i].name .. ";")
			 or ("<serialiation of " .. t.block[i].type .. " isn't implemented yet>")
			)
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
		b[i] = (t.block[i].public and "public " or "private ")
			.. (t.block[i].static and "static " or "")
			.. (t.block[i].type == "Member"
			and ((t.block[i].const and "const " or "") .. serializeType( t.block[i].class ) .. " " .. t.block[i].name .. ";")
			 or ("<serialiation of " .. t.block[i].type .. " isn't implemented yet>")
			)
	end

	return "interface " .. t.name .. " "
		.. (#i > 0 and "implements " .. table.concat( i, ", " ) .. " " or "")
		.. " {\n\t" .. table.concat( b, "\n" ):gsub( "\n", "\n\t" ) .. "\n}"
end
