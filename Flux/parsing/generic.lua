
-- TODO
--! type X {} in parameter list
--! change detectDoubleWord to allow for a type at first
--!! possibly change syntax of definitions to reduce backtracking here

function throw( lexer, err, pos )
	return error( lexer:formatError( err, false, pos or lexer:get().position ), 0 )
end

function expectSemicolon( lexer )
	return lexer:test( "Symbol", ";", -1 ) or lexer:skip( "Symbol", ";" )
end

function detectDoubleWord( source )
	local lexer = source.lexer
	local start = lexer:mark()
	
	if parseName( source ) and lexer:test "Identifier" then
		lexer:jump( start )
		return true
	end

	lexer:jump( start )
	return false
end

function isFormOfReference( expr )
	return expr.type == "Reference" or expr.type == "Index" or expr.type == "DotIndex"
end

function parseName( source )
	local lexer = source.lexer
	local word = lexer:skipValue "Identifier"

	while word and lexer:skip( "Symbol", "::" ) do
		word = word .. "::" .. (lexer:skipValue "Identifier" or throw( lexer, "expected name after '::'" ))
	end

	return word
end

function logicalNotExpression( expr )
	if expr.type == "LeftUnaryExpression" and expr.operator == "!" then
		return expr.value
	else
		return { type = "LeftUnaryExpression", value = expr, operator = "!", position = expr.position }
	end
end

function wrapStringAsReference( str, pos )
	return { type = "Reference", name = str, position = pos }
end

function wrapStringAsString( str, pos )
	return { type = "StringConstant", value = str, position = pos }
end

function wrapReturnStatement( value )
	return {
		type = "ReturnStatement";
		value = value;
		position = value.position;
	}
end
