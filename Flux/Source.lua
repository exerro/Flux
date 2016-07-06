
local FLUX_STD_LIB_PATH = "Flux/std"

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

class "Source" {
	path = "";
	statements = {}
}

function Source:Source( path )
	self.path = (path or "") .. ";" .. FLUX_STD_LIB_PATH
	self.statements = { meta = { type = "dobody" } }
	self.blocks = { self.statements }
end

function Source:push( statement )
	self.blocks[#self.blocks][#self.blocks[#self.blocks] + 1] = statement
end

function Source:begin( blocktype )
	self.blocks[#self.blocks + 1] = { meta = { type = blocktype } }
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

function Source:import( name, position )
	for path in self.path:gmatch "[^;]+" do
		local p = path .. "/" .. name:gsub( "%.", "/" )

		if isfile( p .. ".flxh" ) then
			self:parseContent( readfile( path .. "/" .. name .. ".flxh" ), name .. " header" )

		end

		if isfile( p .. ".flxc" ) then
			self:push { source = position.source, line = position.line, character = position.character, strline = position.strline;
				type = "lua-embed";
				value = readfile( path .. "/" .. name .. ".flxc" );
			}

		elseif isfile( p .. ".flx" ) then
			self:parseContent( readfile( path .. "/" .. name .. ".flx" ), name )

		end
	end
end

function Source:parseContent( content, source )
	local l = self.lexer

	self.lexer = Lexer( content, source )

	 -- parse it

	self.lexer = l
end
