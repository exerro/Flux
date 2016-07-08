
function throw( lexer, err, pos )
	return error( lexer:formatError( err, false, pos or lexer:get().position ), 0 )
end

function assertType( v, e )
	return v or error( e, 0 )
end

function expectSemicolon( lexer )
	return lexer:test( "Symbol", ";", -1 ) or lexer:skip( "Symbol", ";" )
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

function wrapExpressionStatement( expr, pos )
	return {
		type = "ExpressionStatement";
		value = expr;
		position = pos or expr.position;
	}
end

function wrapStringAsReference( str, pos )
	return { type = "Reference", name = str, position = pos }
end

function wrapStringAsString( str, pos )
	return { type = "StringConstant", value = str, position = pos }
end

function wrapSetExpression( lvalue, rvalue, position )
	return {
		type = "BinaryExpression";
		lvalue = lvalue;
		rvalue = rvalue;
		operator = "=";
		position = position or lvalue.position;
	}
end

function wrapEqualityCheck( lvalue, rvalue, position )
	return {
		type = "BinaryExpression";
		lvalue = lvalue;
		rvalue = rvalue;
		operator = "==";
		position = position or lvalue.position;
	}
end

function wrapIfStatement( condition, block, elseblock, position )
	return {
		type = "IfStatement";
		condition = condition;
		block = block;
		elseblock = elseblock;
		position = position or condition.position;
	}
end

function wrapDotIndex( value, index, position )
	return { type = "DotIndex", value = value, index = index, position = position }
end

function wrapReturnStatement( value )
	return {
		type = "ReturnStatement";
		value = value;
		position = value.position;
	}
end

function wrapNewExpression( class, parameters, position )
	return {
		type = "NewExpression";
		class = class;
		parameters = parameters;
		position = position;
	}
end

function wrapFunctionCall( value, parameters, position )
	return {
		type = "FunctionCall";
		value = value;
		parameters = parameters;
		position = position or value.position;
	}
end

function nullExpression( position )
	return { type = "NullExpression", position = position }
end

function wrapFunction( returns, parameters, body, position )
	return {
		type = "FunctionExpression";
		returns = returns;
		parameters = parameters;
		body = body;
		position = position;
	}
end
