
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

	for i = 1, #source.statements do
		compileRootStatement( emitter, source.statements[i] )
		emitter:pushLineBreak()
		emitter:pushLineBreak()
	end
	
	if ... then
		io.write( emitter.output )
	end
	assert( loadstring( emitter.output ) )()

	main()
--end, ... )

if not ok then
	error( err, 0 )
end
