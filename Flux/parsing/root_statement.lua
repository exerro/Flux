
local types = require "common.types"

types.parseMany [[
RootBlock: nil

NamespaceStatement: { "type" = "NamespaceStatement", "name" = string, "block" = RootBlock } & HasPosition
UsingStatement: { "type" = "UsingStatement", "name" = string } & HasPosition
EnumDefinition: { "type" = "EnumDefinition", "name" = string, "members" = { number = string } } & HasPosition

RootStatement:
	  NamespaceStatement
	| UsingStatement
	| ClassDefinition
	| InterfaceDefinition
	| EnumDefinition
	| Definition
	| TemplateDefinition
	| ExpressionStatement

RootBlock: { "meta" = { "type" = string }, number = RootStatement } | {}
]]

local function parseImportStatement( source, pos )
	local lexer = source.lexer
	local name = lexer:skipValue "Identifier" or lexer:skipValue "String" or throw( lexer, "expected name after 'import'" )

	while lexer:skip( "Symbol", "." ) do
		name = name .. "." .. (lexer:skipValue "Identifier" or lexer:skipValue "String" or throw( lexer, "expected name after '.'" ))
	end

	if not expectSemicolon( lexer ) then
		throw( lexer, "expected ';' after import statement" )
	end

	source:import( name, pos )
end

local function parseNamespaceStatement( source, pos )
	local lexer = source.lexer
	local name = source:resolveDefinitionName( parseName( source ) or throw( lexer, "expected namespace name" ) )

	source:begin "namespace" .name = name

	if lexer:skip( "Symbol", "{" ) then
		while not lexer:skip( "Symbol", "}" ) do
			if lexer:isEOF() then
				throw( lexer, "expected '}' to close namespace" )
			end

			parseRootStatement( source )
		end

	elseif lexer:skip( "Symbol", ";" ) then
		while not lexer:isEOF() do
			parseRootStatement( source )
		end

	else
		throw( lexer, "expected '{'" )

	end

	local block = source:pop()

	source:push {
		type = "NamespaceStatement";
		name = name;
		block = block;
		position = pos;
	}

end

local function parseUsingStatement( source, pos )
	local lexer = source.lexer
	local name = parseName( source )

	if not expectSemicolon( lexer ) then
		throw( lexer, "expected ';' after 'using' statement" )
	end

	source:push {
		type = "UsingStatement";
		name = name;
		position = pos;
	}
end

local function parseEnumDefinition( source, pos )
	local lexer = source.lexer
	local name = source:resolveDefinitionName( parseName( source ) or throw( lexer, "expected enum name" ) )

	if not lexer:skip( "Symbol", "{" ) then
		throw( lexer, "expected '{' after enum name" )
	end

	local members = { lexer:skipValue "Identifier" or throw( lexer, "expected initial enum value" ) }

	while lexer:skip( "Symbol", "," ) or lexer:skip( "Symbol", ";" ) and not lexer:test( "Symbol", "}" ) do
		members[#members + 1] = lexer:skipValue "Identifier" or throw( lexer, "expected enum value after ','" )
	end

	if not lexer:skip( "Symbol", "}" ) then
		throw( lexer, "expected '}' after enum body" )
	end

	source:push {
		type = "EnumDefinition";
		name = name;
		members = members;
		position = pos;
	}
end

function parseRootStatement( source )
	local lexer = source.lexer
	local keyword = lexer:skip "Keyword"

	if keyword then
		if keyword.value == "namespace" then
			return parseNamespaceStatement( source, keyword.position )

		elseif keyword.value == "using" then
			return parseUsingStatement( source, keyword.position )

		elseif keyword.value == "class" then
			return parseClassDefinition( source, keyword.position )

		elseif keyword.value == "interface" then
			return parseInterfaceDefinition( source, keyword.position )

		elseif keyword.value == "enum" then
			return parseEnumDefinition( source, keyword.position )

		elseif keyword.value == "let" then
			return parseLetStatement( source, keyword.position )

		elseif keyword.value == "new" then
			return parseNewStatement( source, keyword.position )

		elseif keyword.value == "template" then
			return parseFunctionTemplate( source, keyword.position )

		end

		lexer:back()
	end

	if not parseDefinition( source ) then
		throw( lexer, "expected definition or valid statement" )
	end

end

function parseFileBody( source )
	local lexer = source.lexer

	while not lexer:isEOF() do
		if lexer:skip( "Keyword", "import" ) then
			parseImportStatement( source )
		else
			parseRootStatement( source )
		end
	end
end

function serializeRootStatement( t )
	local initial = t.filename and "@" .. ("%q"):format( t.filename ) .. "\n" or ""
	
	if t.type == "NamespaceStatement" then
		local b = {}

		for i = 1, #t.block do
			b[i] = serializeRootStatement( t.block[i] )
		end

		return initial .. "namespace " .. t.name .. " {\n\t" .. table.concat( b, "\n" ):gsub( "\n", "\n\t" ) .. "\n}"

	elseif t.type == "UsingStatement" then
		return initial .. "using " .. t.name .. ";"

	elseif t.type == "ClassDefinition" then
		return initial .. serializeClassDefinition( t )

	elseif t.type == "InterfaceDefinition" then
		return initial .. serializeInterfaceDefinition( t )

	elseif t.type == "EnumDefinition" then
		return initial .. "enum " .. t.name .. " {\n\t" .. table.concat( t.members, ";\n\t" ) .. ";\n}"

	elseif t.type == "Definition" or t.type == "TemplateDefinition" then
		return initial .. serializeDefinition( t )

	elseif t.type == "ExpressionStatement" then
		return serializeExpression( t.value ) .. ";"

	else
		return "<serialization of " .. t.type .. " isn't written>"

	end
end
