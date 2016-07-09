
local letters = {}
local n = 0

for i = string.byte "a", string.byte "z" do
	letters[n] = string.char( i )
	n = n + 1
end

for i = string.byte "A", string.byte "Z" do
	letters[n] = string.char( i )
	n = n + 1
end

local function getword( n )
	local s = letters[n % #letters]

	while n > #letters do
		n = math.floor( n / #letters ) - 1
		s = letters[n % #letters] .. s
	end

	return s
end

class "Emitter" {
	output = "";
	spacingRequired = false;
	indentation = 0;
	newlines = true;
	operator_spacing = true;
	words = {};
	word = 0;
}

function Emitter:push( text )
	self.output = self.output .. tostring(text)
end

function Emitter:pushWord( word )
	if self.spacingRequired then
		self:push " "
	end

	self:push( word )

	self.spacingRequired = true
end

function Emitter:pushBlockText( text )
	if self.spacingRequired and text:find "^%w" then
		self:push " "
	end

	self:push( text:gsub( "\n", "\n" .. ("\t"):rep( self.indentation ) ) )

	self.spacingRequired = text:find "%w$"
end

function Emitter:pushOperator( operator )
	self:push( self.operator_spacing and " " .. operator .. " " or operator )
	self.spacingRequired = false
end

function Emitter:pushDelimiter( delimiter )
	self:push( self.operator_spacing and delimiter .. " " or delimiter )
	self.spacingRequired = false
end

function Emitter:pushSymbol( symbol )
	self:push( symbol )
	self.spacingRequired = false
end

function Emitter:pushLineBreak()
	if self.newlines then
		self:push( "\n" .. ("\t"):rep( self.indentation ) )
		self.spacingRequired = false
	end
end

function Emitter:indent( n )
	self.indentation = self.indentation + n
end

function Emitter:pushString( s )
	self:push( ("%q"):format( s ) )
	self.spacingRequired = false
end

function Emitter:define( name )
	self.words[name] = true
end

function Emitter:getName()
	local word = getword( self.word )
	
	while self.words[word] do
		self.word = self.word + 1
		word = getword( self.word )
	end

	self.words[word] = true

	return word
end
