
local wordc = ... == "-w"
local Flux = require "Flux.init"

local source = Source "./fluxc;."
local emitter = Emitter()

for i = 1, 5 do
	if not source.hasMainFile then
		source = Source( ("../"):rep( i ) .. "fluxc" )
	end
end

if source.hasMainFile then
	source:import "main"
else
	error( "path 'fluxc' has no main file", 0 )
end

print "Flux source parsed"

for i = 1, #source.statements do
	compileRootStatement( emitter, source.statements[i] )
	emitter:pushLineBreak()
	emitter:pushLineBreak()
end

print "Flux source compiled"

if wordc then
	local t = {}
	for k, v in pairs( Lexer.words ) do
		t[#t + 1] = { k, v }
	end

	table.sort( t, function( a, b ) return a[2] > b[2] end )

	for i = 1, #t do
		print( t[i][1] .. ": " .. t[i][2] .. " occurrences" )
	end
end

local ok, err = (loadstring or load)( emitter.output )

if not ok then
	error( err, 0 )
end

local h = io.open( "./fluxc.lua", "w" )

h:write( emitter.output .. "\nmain(...)" )
h:close()
