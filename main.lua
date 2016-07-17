
local console = {}
local font = love.graphics.newFont( "font.ttf", 14 )
local scroll = 0
local oldprint = print
local RUN_COMPILED = true

function print( ... )
	for segment in tostring( ... ):gmatch "[^\n]+" do
		console[#console + 1] = segment
	end
	return oldprint( ... )
end

local Flux = require "Flux"
local types = require "common.types"

local test_case = "stuff"

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

	if RUN_COMPILED then
		love.filesystem.write( "log.txt", emitter.output )
		assert( loadstring( emitter.output ) )()
	else
		assert( loadstring( love.filesystem.read "log.txt" ) )()
	end

	main()
end )

if not ok then
	print( err )
end

function love.keypressed()
	love.event.quit()
end

function love.draw()
	local fheight = font:getHeight()
	local y = 0

	love.graphics.translate( 0, scroll )
	love.graphics.setFont( font )

	for i = 1, #console do
		love.graphics.print( console[i], 0, y )
		y = y + fheight
	end
end
