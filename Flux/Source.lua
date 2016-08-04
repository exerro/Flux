
local FLUX_STD_LIB_PATH = "Flux/std"
local lang = require "Flux.lang"

local function isfile( path )
	local h = io.open( path )
	if h then
		h:close()
		return true
	end
	return false
end

local function readfile( path )
	local h = io.open( path )
	if h then
		local c = h:read "*a"
		h:close()
		return c
	end
end

if love then
	isfile = love.filesystem.isFile
	readfile = love.filesystem.read
end

class "Source" {
	path = "";
	filename = "";
	current_include_path = {};
	statements = {};
	imported = {};
	hasMainFile = false;
}

function Source:Source( path )
	self.path = (path or "") .. ";" .. FLUX_STD_LIB_PATH
	self.statements = { meta = { type = "dobody" } }
	self.blocks = { self.statements }
	self.imported = {}
	self.current_include_path = {}

	for path in self.path:gmatch "[^;]+" do
		if isfile( path .. "/main.flx" ) then
			self.hasMainFile = true
			break
		end
	end
end

function Source:resolveDefinitionName( name )
	local block = self.blocks[#self.blocks]

	if block and block.meta.type == "namespace" then
		return block.meta.name .. (lang.REPLACE_COLONS_WITH_UNDERSCORES and "__" or "::") .. name
	end

	return name
end

function Source:push( statement )
	if #self.blocks == 1 then
		statement.filename = self.filename
	end

	self.blocks[#self.blocks][#self.blocks[#self.blocks] + 1] = statement
end

function Source:begin( blocktype )
	local b = { meta = { type = blocktype } }
	self.blocks[#self.blocks + 1] = b
	return b.meta
end

function Source:beginCopy()
	local b = { meta = self:block().meta }
	self.blocks[#self.blocks + 1] = b
	return b.meta
end

function Source:pop()
	return table.remove( self.blocks, #self.blocks )
end

function Source:block( n )
	return self.blocks[#self.blocks - (n or 0)]
end

function Source:isLoop()
	local b, i = self:block(), 1

	while b do
		if b.meta.type == "function" then
			return false
		elseif b.meta.type == "forloop" or b.meta.type == "foreachloop" or b.meta.type == "whileloop" or b.meta.type == "repeatloop" then
			return true
		end

		b = self:block( i )
		i = i + 1
	end
end

function Source:getClassName()
	local b = self:block()
	return b and b.meta.type == "class" and b.meta.name
end


function Source:import( name, position )
	for path in self.path:gmatch "[^;]+" do
		local ip = self.current_include_path
		local p = path .. "/" .. (ip[1] and #ip[#ip] > 0 and ip[#ip] .. "/" or "") .. name:gsub( "%.", "/" )
		local found = isfile( p .. ".flxh" ) or isfile( p .. ".flxc" ) or isfile( p .. ".flx" )

		if not found and ip[1] then
			p = path .. "/" .. name:gsub( "%.", "/" )
			found = isfile( p .. ".flxh" ) or isfile( p .. ".flxc" ) or isfile( p .. ".flx" )
		end

		if self.imported[p] then
			return

		elseif found then
			self.imported[p] = true
			self.current_include_path[#self.current_include_path + 1] = p:sub( #path + 2 ):match "^(.+)/" or ""

			if isfile( p .. ".flxh" ) then
				self:parseContent( readfile( p .. ".flxh" ), name .. " header", p )

			end

			if isfile( p .. ".flxc" ) then
				self:push {
					type = "LuaScript";
					value = readfile( p .. ".flxc" );
					position = position;
				}

			elseif isfile( p .. ".flx" ) then
				self:parseContent( readfile( p .. ".flx" ), name, p )

			end

			self.current_include_path[#self.current_include_path] = nil

			return

		end
	end

	throw( self.lexer, "attempt to import file '" .. name .. "': file not found", position )
end

function Source:parseContent( content, source, filename )
	local l = self.lexer
	local n = self.filename

	self.lexer = Lexer( content, source )
	self.filename = filename or n

	parseFileBody( self )

	self.lexer = l
	self.filename = n
end
