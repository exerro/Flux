
class "Token" {
	position = Position;
	type = "TOKEN";
	value = "";
}

function Token.EOF(...)
	return Token( "EOF", "EOF", ... )
end

function Token:Token(type, value, position)
	self.type = type
	self.value = value
	self.position = position
	self.mt.__tostring = self.tostring
end

function Token:tostring()
	return self.type .. "<" .. tostring(self.value) .. ">"
end

function Token:getNearString()
	return self.type == "String" and "<string>"
	    or self.type == "EOF" and "<eof>"
	    or self.type:lower() .. " '" .. tostring(self.value) .. "'"
end
