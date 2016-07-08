
local types = require "common.types"

types.parseMany [[
ClassDefinition: { "public" = boolean, "static" = boolean } & Definition

ClassDefinition: nil
InterfaceDefinition: nil
]]

local function parseClassBodyStatements()

end

local function parseClassAliasDefinition( source, name )
	-- parse stuff
end

function parseClassDefinition( source )
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

	if not container2 then
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
end

function parseInterfaceDefinition( source )

end

function serializeClassDefinition( t )

end

function serializeInterfaceDefinition( t )

end
