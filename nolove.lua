
package.path = package.path .. ";/home/ben/Code/Flux/?/init.lua"

local Flux = require "Flux"

local source = Source "FluxInFlux"
local emitter = Emitter()

local ok, err = pcall( function()
	if source.hasMainFile then
		source:import "main"
	else
		print( "path has no main file", 0 )
	end

	for i = 1, #source.statements do
		compileRootStatement( emitter, source.statements[i] )
		emitter:pushLineBreak()
		emitter:pushLineBreak()
	end
	
	assert( loadstring( emitter.output ) )()

	main()
end )

if not ok then
	print( err )
end
