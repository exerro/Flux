
local Flux = require "Flux"
local types = require "common.types"

local test_case = "stuff"

local source = Source()
local code = love.filesystem.read( "Flux/tests/parsing " .. test_case .. ".flx" )

print( code )
print ""
print( string.rep( "-", 50 ) )
print ""

source.lexer = Lexer( code )

while not source.lexer:isEOF() do
	parseStatement( source )
end

for i = 1, #source.statements do
	print( serializeStatement( source.statements[i] ) )
	print ""

	if not types.check( source.statements[i], "Statement" ) then
		local s = types.whynot( source.statements[i], "Statement" )
		love.filesystem.write( "log.txt", s )
		error( s )
	end
end
