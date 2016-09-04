
local lang = require "Flux.lang"

local function getline( text, line )
	for i = 1, line - 1 do
		text = text:gsub( "^.-\n", "" )
	end
	return text:gsub( "\n.*$", "" )
end

local escaped_characters = setmetatable( {
	["n"] = "\n", ["0"] = "\0", ["r"] = "\r", ["t"] = "\t";
}, { __index = function( _, v )
	return v
end } )

class "Lexer" {
	position = 1;
	text = "";

	source = "string";
	line = 1;
	character = 1;
	strline = "";

	buffer = {};
	buffer_pos = 1;
	words = {};
}

function Lexer:Lexer(text, source)
	self.text = text:gsub( "%s+$", "" )
	self.source = source or "string"
	self.buffer = {}
	self.strline = getline( self.text, self.line )
end

function Lexer:getPosition( n )
	local c = self.character
	self.character = self.character + ( n or 0 )
	return Position( self.source, self.line, c, self.strline )
end

function Lexer:formatError( err, showNear, position )
	local source = position.source
	local line = position.line
	local strline = position.strline:gsub( "\t", " " )
	local pointer = (" "):rep( position.character - 1 ) .. "^"
	local near_text = showNear and " (near " .. self:get():getNearString() .. ")\n\t" or "\n\t"

	return source .. "[" .. line .. "]: " .. err .. near_text .. strline .. "\n\t" .. pointer
end

function Lexer:consumePattern( type, pat )
	local value = self.text:match( "^" .. pat, self.position )
	if not value then return false end
	self.position = self.position + #value
	return Token( type, value, self:getPosition( #value ) )
end

function Lexer:consumeSymbol()
	local position = self.position
	local s3, s2, s1 = self.text:sub( position, position + 2 ), self.text:sub( position, position + 1 ), self.text:sub( position, position )

	if lang.symbols[s3] then
		self.position = position + 3
		return Token( "Symbol", s3, self:getPosition( 3 ) )
	elseif lang.symbols[s2] then
		self.position = position + 2
		return Token( "Symbol", s2, self:getPosition( 2 ) )
	elseif lang.symbols[s1] then
		self.position = position + 1
		return Token( "Symbol", s1, self:getPosition( 1 ) )
	else
		return error( self:formatError( "unexpected symbol '" .. s1 .. "'", false, self:getPosition() ), 0 )
	end
end

function Lexer:consumeNewline()
	self.line = self.line + 1
	self.character = 1
	self.position = self.position + 1
	self.strline = getline( self.text, self.line )
end

function Lexer:consumeString()
	local escaped = false
	local s = {}
	local pos = self:getPosition( 1 )
	local open = self.text:sub( self.position, self.position )

	for i = self.position + 1, #self.text do
		local char = self.text:sub( i, i )
		self.character = self.character + 1

		if escaped then -- if the previous character was a \
			s[#s + 1] = escaped_characters[char]
			escaped = false

		elseif char == "\\" then -- a \ not preceeded by a \
			escaped = true

		elseif char == "\n" then -- a \n
			s[#s + 1] = "\n"
			self.line = self.line + 1
			self.strline = getline( self.text, self.line )
			self.character = 1


		elseif char == open then -- the closing string tag
			self.position = i + 1
			return Token( "String", table.concat( s ), pos )

		else -- any other character
			s[#s + 1] = char
		end
	end

	return error( self:formatError( "missing end of string", false, pos ), 0 )
end

function Lexer:consumeCharacter()
	local char = self.text:sub( self.position + 1, self.position + 1 )
	local length = 3
	local pos = self:getPosition()

	if char == "\\" then
		char = self.text:sub( self.position + 2, self.position + 2 )
		length = 4

		if char == "\n" then
			self.line = self.line + 1
			self.strline = getline( self.text, self.line )
			self.character = 3
			self.position = self.position + length
			length = 0
		end

		char = escaped_characters[char]
	elseif char == "\n" then
		self.line = self.line + 1
		self.strline = getline( self.text, self.line )
		self.character = 3
		self.position = self.position + length
		length = 0
	end

	self.position = self.position + length
	self.character = self.character + length

	return Token( "Character", char, pos )
end

function Lexer:consumeIdentifier()
	local token = self:consumePattern( "Identifier", "[%w_]+" )

	local ext = self:consumePattern( "", "::[%w_]+" )
	while ext do
		token.value = token.value .. ext.value
		ext = self:consumePattern( "", "::[%w_]+" )
	end

	self.words[token.value] = (self.words[token.value] or 0) + 1;

	if lang.keywords[token.value] then
		token.type = "Keyword"

	elseif token.value == "true" or token.value == "false" then
		token.type = "Boolean"

	elseif token.value == "null" then
		token.type = "Null"

	end

	return token
end

function Lexer:consumeBinaryNumber()
	return self:consumePattern( "Binary", "0b[01]+" )
end

function Lexer:consumeHexNumber()
	return self:consumePattern( "Hexadecimal", "0x%x+" )
end

function Lexer:consumeNumber()
	return self:consumePattern( "Float", "%d*%.%d+e[%+%-]?%d+" ) or self:consumePattern( "Float", "%d*%.%d+" )
	or self:consumePattern( "Byte", "%d+e[%+%-]?%d+b" ) or self:consumePattern( "Byte", "%d+b" )
	or self:consumePattern( "Integer", "%d+e[%+%-]?%d+" ) or self:consumePattern( "Integer", "%d+" )
end

function Lexer:consumeComment()
	if self.text:find( "^//", self.position ) then
		self.line = self.line + 1
		self.strline = getline( self.text, self.line )
		self.character = 1
		self.position = self.position + #( self.text:match( "^.-\n", self.position ) or self.text:sub( self.position ) )
	else
		local finish = self.text:find( "%*/", self.position )
		local newlines = finish and select( 2, self.text:sub( self.position, finish ):gsub( "\n", "" ) )

		if finish then
			self.line = self.line + newlines
			self.strline = getline( self.text, self.line )
			self.character = newlines > 0 and #self.text:sub( self.position, finish ):gsub( ".+\n", "" ) + 2 or self.character + finish - self.position + 2
			self.position = finish + 2

		else
			return error( self:formatError( "missing end of comment '*/'", false, self:getPosition() ), 0 )
		end
	end
end

function Lexer:consumeWhitespace()
	local len = #self.text:match( "[^%S\n]+", self.position )
	self.character = self.character + len
	self.position = self.position + len
end

function Lexer:consumeBackticks()
	local content = self.text:match( "`([^\n]-)`", self.position )

	if not content then
		return error( self:formatError( "missing end of '`'", false, self:getPosition() ) )
	end

	self.position = self.position + #content + 2

	return Token( "Backtick", content, self:getPosition( #content + 2 ) )
end

function Lexer:consume()
	if self.text:find( "^\n", self.position ) then
		self:consumeNewline()
		return self:consume()

	elseif self.text:find( "^/[%*/]", self.position ) then
		self:consumeComment()
		return self:consume()

	elseif self.text:find( "^%s", self.position ) then
		self:consumeWhitespace()
		return self:consume()

	end

	local token

	if self.position > #self.text then
		token = Token.EOF( self:getPosition() )

	elseif self.text:find( "^'\\?.'", self.position ) then
		token = self:consumeCharacter()

	elseif self.text:find( "^\\[%w_]", self.position ) then
		token = self:consumePattern( "String", "\\[%w_]+" )

	elseif self.text:find( "^[\"']", self.position ) then
		token = self:consumeString()

	elseif self.text:find( "^0x%x+", self.position ) then
		token = self:consumeHexNumber()

	elseif self.text:find( "^0b[01]+", self.position ) then
		token = self:consumeBinaryNumber()

	elseif self.text:find( "^%d+b?%W", self.position ) or self.text:find( "^%d+b?$", self.position )
		or self.text:find( "^%d+e[%+%-]?%d+b?%W", self.position ) or self.text:find( "^%d+e[%+%-]?%d+b?$", self.position )
		or self.text:find( "^%d*%.%d+%W", self.position ) or self.text:find( "^%d*%.%d+$", self.position )
		or self.text:find( "^%d*%.%d+e[%+%-]?%d+%W", self.position ) or self.text:find( "^%d*%.%d+e[%+%-]?%d+$", self.position )
	then
		token = self:consumeNumber()

	elseif self.text:find( "^[%w_]+", self.position ) then
		token = self:consumeIdentifier()

	elseif self.text:sub( self.position, self.position ) == "`" then
		token = self:consumeBackticks()

	else
		token = self:consumeSymbol()

	end

	self.buffer[#self.buffer + 1] = token

	return token
end

function Lexer:isEOF()
	return self:get().type == "EOF"
end

function Lexer:mark()
	return self.buffer_pos
end

function Lexer:jump( pos )
	self.buffer_pos = pos
end

function Lexer:get()
	local position = self.buffer_pos

	while not self.buffer[position] do
		self:consume()
	end

	return self.buffer[position]
end

function Lexer:peek( lookahead )
	local position = self.buffer_pos + (lookahead or 0)

	while not self.buffer[position] do
		self:consume()
	end

	return self.buffer[position]
end

function Lexer:next()
	local t = self:get()

	self.buffer_pos = self.buffer_pos + 1

	return t
end

function Lexer:back()
	self.buffer_pos = self.buffer_pos - 1
end

function Lexer:test( type, value, lookahead )
	local t = self:peek( lookahead )
	return t.type == type and ( value == nil or t.value == value ) and t
end

function Lexer:skip( type, value )
	if self:test( type, value ) then
		return self:next()
	end
end

function Lexer:testValue( type, lookahead )
	local t = self:peek( lookahead )
	return t.type == type and t.value
end

function Lexer:skipValue( type, value )
	local t = self:skip( type, value )
	return t and t.value or nil
end
