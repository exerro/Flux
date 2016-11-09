
package.path = package.path .. ";/home/ben/Code/Flux/?/init.lua"
package.path = package.path .. ";/home/ben/Code/Flux/?.lua"

local Flux = require "Flux"

local source = Source "FluxInFlux"
local emitter = Emitter()

for i = 1, 5 do
	if not source.hasMainFile then
		source = Source( ("../"):rep( i ) .. "FluxInFlux" )
	end
end

local ok, err --= pcall( function(...)
	if source.hasMainFile then
		source:import "main"
	else
		error( "path 'FluxInFlux' has no main file", 0 )
	end

	print "Flux source parsed"

	for i = 1, #source.statements do
		compileRootStatement( emitter, source.statements[i] )
		emitter:pushLineBreak()
		emitter:pushLineBreak()
	end

	print "Flux source compiled"

	local t = {}
	for k, v in pairs( Lexer.words ) do
		t[#t + 1] = { k, v }
	end

	table.sort( t, function( a, b ) return a[2] > b[2] end )

	for i = 1, #t do
		-- print( t[i][1] .. ": " .. t[i][2] .. " occurrences" )
	end

	if ... then
		io.write( emitter.output )
	end
	assert( (loadstring or load)( emitter.output ) )()

	main()
--end, ... )

if err and not ok then
	error( err, 0 )
end
