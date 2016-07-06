
class "Position" {
	source = "";
	line = 1;
	character = 1;
	strline = "";
}

function Position:Position( source, line, character, strline )
	self.source = source
	self.line = line
	self.character = character
	self.strline = strline
end
