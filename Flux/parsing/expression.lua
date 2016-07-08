
local lang = require "Flux.lang"
local types = require "common.types"

local function isEqualityOperator( operator )
	return lang.operators[operator] == 1
end

function parseIndex( source, allowMany )
	local lexer = source.lexer
	local expr = parseExpression( source ) or throw( lexer, "expected index after '['" )
	local indexes = { expr }

	while allowMany and lexer:skip( "Symbol", "," ) do
		indexes[#indexes + 1] = parseExpression( source ) or throw( lexer, "expected index after ','" )
	end

	if not lexer:skip( "Symbol", "]" ) then
		throw( lexer, "expected ']' to close index" )
	end

	return allowMany and indexes or indexes[1]
end

types.parseMany [[

ConstantExpressionType: "CharacterConstant" | "StringConstant" | "FloatConstant" | "IntegerConstant" | "ByteConstant" | "HexadecimalConstant" | "BinaryConstant" | "BooleanConstant"

ConstantExpression: { "type" = ConstantExpressionType, "value" = string } & HasPosition
NullExpression: { "type" = "NullExpression" } & HasPosition
ThrowExpression: { "type" = "ThrowExpression", "value" = Expression } & HasPosition
FunctionExpression: { "type" = "FunctionExpression", "returns" = Type, "parameters" = FunctionDefinitionParameters, "body" = Block } & HasPosition
BracketExpression: { "type" = "BracketExpression", "value" = Expression }
TableExpression: { "type" = "TableExpression", "value" = { number = { "index" = Expression, "value" = Expression } } | {} } & HasPosition
ArrayExpression: { "type" = "ArrayExpression", "value" = { number = Expression } | {} } & HasPosition
Reference: { "type" = "Reference", "name" = string } & HasPosition
MatchExpression: { "type" = "MatchExpression", "condition" = Expression, "cases" = { number = { "matches" = { number = Expression }, "value" = Expression } }, "default" = Expression | nil } & HasPosition
NewExpression: { "type" = "NewExpression", "class" = string } & HasPosition

PrimaryExpression:
	  ConstantExpression
	| NullExpression
	| ThrowExpression
	| FunctionExpression
	| BracketExpression
	| TableExpression 
	| ArrayExpression
	| MatchExpression
	| NewExpression
	| Reference
]]

--[[

let x = match input {
	"hello", "hi" => "Hello to you too!";
	"goodbye" => "Sad to see you go :(";
	default => "What was that?";
}

]]

local function parseBracketExpression( source, token )
	local expr = parseExpression( source )
	local lexer = source.lexer

	if not lexer:skip( "Symbol", ")" ) then
		return throw( lexer, "expected ')'" )
	end

	return {
		type = "BracketExpression";
		value = expr;
		position = token.position;
	}
end

local function parseTableExpression( source, token )
	local elements = {}
	local lexer = source.lexer
	local n = 1

	if not lexer:skip( "Symbol", "}" ) then
		while true do
			local index, value

			if lexer:skip( "Symbol", "[" ) then
				index = parseIndex( source )

				if not lexer:skip( "Symbol", "=" ) then
					throw( lexer, "expected '=' after [] table index" )
				end

			elseif lexer:test "Identifier" then
				local name = lexer:next()

				index = { type = "StringConstant", value = name.value, position = name.position }

				if not lexer:skip( "Symbol", "=" ) then
					index = nil
					lexer:back()
				end

			end

			if not index then
				index = { type = "IntegerConstant", value = tostring( n ), position = lexer:get().position }
				n = n + 1
			end

			value = parseExpression( source ) or throw( lexer, "expected table element" )
			elements[#elements + 1] = { index = index, value = value }

			if lexer:skip( "Symbol", ";" ) then
				if lexer:test( "Symbol", "}" ) then
					break
				end
			elseif not lexer:skip( "Symbol", "," ) then
				break
			end
		end

		if not lexer:skip( "Symbol", "}" ) then
			throw( lexer, "expected '}' to close table" )
		end
	end

	return {
		type = "TableExpression";
		value = elements;
		position = token.position;
	}
end

local function parseArrayExpression( source, token )
	local elements = {}
	local lexer = source.lexer

	if not lexer:skip( "Symbol", "]" ) then
		while true do
			elements[#elements + 1] = parseExpression( source )

			if not lexer:skip( "Symbol", "," ) then
				break
			end
		end

		if not lexer:skip( "Symbol", "]" ) then
			throw( lexer, "expected ']' to close array" )
		end
	end

	return {
		type = "ArrayExpression";
		value = elements;
		position = token.position;
	}
end

local function parseMatchExpression( source, token )
	local lexer = source.lexer
	local condition = parseExpression( source ) or throw( lexer, "expected value to match" )
	local cases = {}
	local default = nil

	if not lexer:skip( "Symbol", "{" ) then
		throw( lexer, "expected '{' to open match cases" )
	end

	while not lexer:skip( "Symbol", "}" ) do
		if lexer:isEOF() then
			throw( lexer, "expected '}' to close match cases" )
		end

		if default then
			throw( lexer, "unexpected case after 'default'" )
		end

		local cases_local
		local isDefault = false

		if lexer:skip( "Keyword", "default" ) then
			isDefault = true
		else
			cases_local = { parseExpression( source ) or throw( lexer, "expected case expression" ) }

			while lexer:skip( "Symbol", "," ) do
				cases_local[#cases_local + 1] = parseExpression( source ) or throw( lexer, "expected case expression" )
			end
		end

		local value = lexer:skip( "Symbol", "=>" ) and (parseExpression( source ) or throw( lexer, "expected value after '=>'" )) or throw( lexer, "expected '=>' after case" )

		if not lexer:skip( "Symbol", ";" ) then
			throw( lexer, "expected ';' after expression" )
		end

		if isDefault then
			default = value
		else
			cases[#cases + 1] = {
				matches = cases_local;
				value = value;
			}
		end
	end

	return {
		type = "MatchExpression";
		condition = condition;
		cases = cases;
		position = token.position;
		default = default;
	}
end

local function parsePrimaryExpression( source )
	local lexer = source.lexer
	local token = lexer:get()

	if lexer:test "Character" or lexer:test "String" or lexer:test "Float" or lexer:test "Integer" or lexer:test "Byte" or lexer:test "Hexadecimal" or lexer:test "Binary" or lexer:test "Boolean" then
		lexer:next()

		return {
			type = token.type .. "Constant";
			value = token.value;
			position = token.position;
		}

	elseif lexer:skip( "Keyword", "null" ) then
		return nullExpression( token.position )

	elseif lexer:test "Identifier" then
		return wrapStringAsReference( parseName( source ), token.position )

	elseif lexer:skip( "Keyword", "throw" ) then
		return {
			type = "ThrowExpression";
			value = parseExpression( source );
			position = token.position;
		}

	elseif lexer:skip( "Keyword", "function" ) then
		local returns = lexer:skip( "Symbol", "->" ) and assertType( parseType( source ) ) or "auto"
		local parameters = parseFunctionDefinitionParameters( source )
		local body = parseFunctionBody( source )

		return {
			type = "FunctionExpression";
			returns = returns;
			parameters = parameters;
			body = body;
			position = token.position;
		}

	elseif lexer:skip( "Keyword", "lambda" ) then
		local word = lexer:skipValue "Identifier"
		local parameters = {}

		while word do
			local class = lexer:skip( "Symbol", "->" ) and assertType( parseType( source ) ) or "auto"

			parameters[#parameters + 1] = { name = word, class = class }
			word = lexer:skipValue "Identifier"
		end

		local value = lexer:skip( "Symbol", "=>" ) and (parseExpression( source ) or throw( lexer, "expected expression after '=>'" )) or throw( lexer, "expected '=>' after lambda parameters" )

		return {
			type = "FunctionExpression";
			returns = "auto";
			parameters = parameters;
			body = { wrapReturnStatement( value ) };
			position = token.position;
		}

	elseif lexer:skip( "Keyword", "match" ) then
		return parseMatchExpression( source, token )

	elseif lexer:skip( "Keyword", "new" ) then
		local class = assertType( parseType( source ) )
		local parameters = {}

		if lexer:skip( "Symbol", "(" ) then
			parameters = parseFunctionCallParameters( source )

		elseif lexer:test "String" then
			local token = lexer:next()
			parameters = { wrapStringAsString( token.value, token.position ) }

		end

		return wrapNewExpression( class, parameters, token.position )

	elseif lexer:skip( "Symbol", "(" ) then
		return parseBracketExpression( source, token )

	elseif lexer:skip( "Symbol", "{" ) then
		return parseTableExpression( source, token )

	elseif lexer:skip( "Symbol", "[" ) then
		return parseArrayExpression( source, token )

	end
end

types.parseMany [[

LeftUnaryOperator: "++" | "--" | "-" | "~" | "#" | "!"
RightUnaryOperator: "++" | "--"

FunctionCall: { "type" = "FunctionCall", "value" = Expression, "parameters" = { number = Expression } | {} } & HasPosition
MethodCall: { "type" = "MethodCall", "value" = Expression, "name" = string, "parameters" = { number = Expression } | {} } & HasPosition
Index: { "type" = "Index", "value" = Expression, "index" = { number = Expression } } & HasPosition
DotIndex: { "type" = "DotIndex", "value" = Expression, "index" = string } & HasPosition
Cast: { "type" = "Cast", "value" = Expression, "class" = Type } & HasPosition
OperatorImplements: { "type" = "OperatorImplements", "lvalue" = string, "rvalue" = Expression } & HasPosition
OperatorTypeOf: { "type" = "OperatorTypeOf", "class" = Type | nil, "value" = Expression } & HasPosition
OperatorExtends: { "type" = "OperatorExtends", "lvalue" = string, "rvalue" = Expression } & HasPosition

RightUnaryExpression: { "type" = "RightUnaryExpression", "value" = Expression, "operator" = RightUnaryOperator } & HasPosition
LeftUnaryExpression: { "type" = "LeftUnaryExpression", "value" = Expression, "operator" = LeftUnaryOperator } & HasPosition

UnaryExpression:
	  LeftUnaryExpression | RightUnaryExpression
	| FunctionCall | MethodCall
	| Index | DotIndex
	| Cast
	| OperatorImplements | OperatorTypeOf | OperatorExtends

]]

function parseLeftUnaryOperator( source )
	local lexer = source.lexer

	return lexer:skipValue( "Symbol", "++" ) or lexer:skipValue( "Symbol", "--" )
		or lexer:skipValue( "Symbol", "-" )
		or lexer:skipValue( "Symbol", "~" )
		or lexer:skipValue( "Symbol", "#" )
		or lexer:skipValue( "Symbol", "!" )
end

function parseRightUnaryOperator( source )
	local lexer = source.lexer
	return lexer:skipValue( "Symbol", "++" ) or lexer:skipValue( "Symbol", "--" )
end

function parseFunctionCallParameters( source )
	local lexer = source.lexer
	local parameters = {}

	if not lexer:skip( "Symbol", ")" ) then
		while true do
			parameters[#parameters + 1] = parseExpression( source ) or throw( lexer, "expected value" )

			if not lexer:skip( "Symbol", "," ) then
				break
			end
		end

		if not lexer:skip( "Symbol", ")" ) then
			throw( lexer, "expected ')' to close function call" )
		end
	end

	return parameters
end

function parseRightUnaryExpression( source )
	local lexer = source.lexer
	local expr = parsePrimaryExpression( source )

	while expr do
		local token = lexer:get()

		if lexer:test( "Symbol", "++" ) or lexer:test( "Symbol", "--" ) then
			if isFormOfReference( expr ) then
				expr = { type = "RightUnaryExpression", operator = parseRightUnaryOperator( source ), value = expr, position = token.position }
			else
				throw( lexer, "invalid lvalue to operator '" .. parseRightUnaryOperator( source ) .. "': expected reference or index", expr.position )
			end

		elseif lexer:skip( "Symbol", "(" ) then
			local parameters = parseFunctionCallParameters( source )

			expr = { type = "FunctionCall", value = expr, parameters = parameters, position = token.position }

		elseif lexer:test "String" then
			local token = lexer:next()
			local parameters = { wrapStringAsString( token.value, token.position ) }

			expr = { type = "FunctionCall", value = expr, parameters = parameters }

		elseif lexer:skip( "Symbol", ":" ) then
			local name = lexer:skipValue "Identifier" or throw( lexer, "expected name after ':'" )

			if lexer:skip( "Symbol", "(" ) then
				local parameters = parseFunctionCallParameters( source )

				expr = { type = "MethodCall", value = expr, name = name, parameters = parameters, position = token.position }

			elseif lexer:test "String" then
				local token = lexer:next()
				local parameters = { { type = "StringConstant", value = token.value, position = token.position } }

				expr = { type = "MethodCall", value = expr, name = name, parameters = parameters }

			else
				throw( lexer, "expected '(' after method name" )
			end

		elseif lexer:skip( "Symbol", "[" ) then
			local index = parseIndex( source, true )

			expr = { type = "Index", value = expr, index = index, position = token.position }

		elseif lexer:skip( "Symbol", "." ) then
			expr = wrapDotIndex( expr, lexer:skipValue "Identifier" or throw( lexer, "expected name after '.'" ), token.position )

		elseif lexer:skip( "Symbol", "->" ) then
			local class = assertType( parseType( source ) )

			expr = { type = "Cast", value = expr, class = class, position = token.position }

		elseif lexer:skip( "Keyword", "implements" ) then
			local name = lexer:skipValue "Identifier" or throw( lexer, "expected interface name after 'implements'" )

			expr = { type = "OperatorImplements", lvalue = expr, rvalue = name, position = token.position }

		elseif lexer:skip( "Keyword", "typeof" ) then
			local class = assertType( parseType( source ) )

			expr = { type = "OperatorTypeOf", value = expr, class = class, position = token.position }

		elseif lexer:skip( "Keyword", "extends" ) then
			local name = lexer:skipValue "Identifier" or throw( lexer, "expected class name after 'extends'" )

			expr = { type = "OperatorExtends", lvalue = expr, rvalue = name, position = token.position }

		else
			break

		end
	end

	return expr
end

function parseLeftUnaryExpression( source )
	local lexer = source.lexer
	local token = lexer:get()
	local operator = parseLeftUnaryOperator( source )

	if operator then
		local rvalue = parseLeftUnaryExpression( source )

		if rvalue and (operator == "++" or operator == "--") and not isFormOfReference( rvalue ) then
			throw( lexer, "invalid rvalue to operator '" .. operator .. "': expected reference or index", rvalue.position )
		end

		return { type = "LeftUnaryExpression", operator = operator, value = rvalue or throw( lexer, "expected expression after operator '" .. operator .. "'" ), position = token.position }
	elseif lexer:skip( "Keyword", "typeof" ) then
		local position = lexer:peek( -1 ).position
		local value = parseLeftUnaryExpression( source )

		return { type = "OperatorTypeOf", value = value, position = position }
	else
		return parseRightUnaryExpression( source )
	end
end

types.parseMany [[

BinaryOperator:
	  "+" | "-" | "*" | "/" | "%" | "**"
 	| "&" | "|" | "^" | "<<" | ">>"
 	| "==" | "!=" | "<" | ">" | "<=" | ">="
 	| "||" | "&&"
 	| ".."
 	| "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "**=" | "&=" | "|=" | "^=" | "<<=" | ">>="
 	| "and" | "or"

BinaryExpression: { "type" = "BinaryExpression", "lvalue" = Expression, "rvalue" = Expression, "operator" = BinaryOperator } & HasPosition
BacktickExpression: { "type" = "BacktickExpression", "lvalue" = Expression | nil, "rvalue" = Expression | nil, "operator" = string } & HasPosition

Expression: PrimaryExpression | BinaryExpression | UnaryExpression

]]

local function parseUnaryTerm( source )
	local lexer = source.lexer
	local position = source.lexer:get().position
	local start = lexer:mark()
	local backtick = lexer:skipValue "Backtick"
	local term = backtick and parseUnaryTerm( source ) or parseLeftUnaryExpression( source )

	if term and backtick then
		term = {
			type = "BacktickExpression";
			rvalue = term;
			operator = backtick;
			position = position;
		}
	elseif not term then
		lexer:jump( start )
	end

	return term
end

local function parseTerm( source )
	local lexer = source.lexer
	local term = parseUnaryTerm( source )

	if not term then
		return nil
	end

	local position = source.lexer:get().position
	local backtick = lexer:skipValue "Backtick"

	while backtick do
		term = {
			type = "BacktickExpression";
			lvalue = term;
			rvalue = parseUnaryTerm( source );
			operator = backtick;
			position = position;
		}

		position = source.lexer:get().position
		backtick = lexer:skipValue "Backtick"
	end

	return term
end

function parseBinaryOperator( source )
	local lexer = source.lexer
	local op = lexer:testValue "Symbol" or lexer:testValue "Keyword"

	if op and lang.binary_operators[op] then
		return lexer:next()
	end
end

function parseBinaryExpression( source )
	local lexer = source.lexer
	local term = parseTerm( source )

	if not term then
		return
	end

	local operands = { term }
	local operators = {}

	local function collapse()
		local lvalue = operands[#operands - 1]
		local rvalue = operands[#operands]
		local operator = operators[#operators]

		operands[#operands] = nil
		operators[#operators] = nil

		operands[#operands] = {
			type = "BinaryExpression";
			lvalue = lvalue;
			rvalue = rvalue;
			operator = operator.symbol;
			position = operator.position;
		}

		if isEqualityOperator( operator.symbol ) and not isFormOfReference( lvalue ) then
			throw( lexer, "invalid lvalue to operator '" .. operator.symbol .. "'", lvalue.position )
		end
	end

	while true do
		local operator = parseBinaryOperator( source )
		local op = operator and operator.value

		if op then
			while operators[#operators] and (
				   operators[#operators].precedence > lang.binary_operators[op]
				or operators[#operators].precedence == lang.binary_operators[op] and lang.binary_operators[op] > 1 -- if it's greater than 1, it's left associative
			) do
				collapse()
			end

			operands[#operands + 1] = parseTerm( source ) or throw( lexer, "expected term after operator '" .. op .. "'" )
			operators[#operators + 1] = {
				precedence = lang.binary_operators[op];
				symbol = op;
				position = operator.position;
			}
		else
			break
		end
	end

	while #operands > 1 do
		collapse()
	end

	return operands[1]
end

function parseExpression( source )
	return parseBinaryExpression( source )
end

function serializeExpression( t )

	if t.type == "StringConstant" then
		return ("%q"):format( t.value )

	elseif t.type == "CharacterConstant" then
		return "'" .. t.value .. "'"

	elseif t.type == "FloatConstant" or t.type == "IntegerConstant" or t.type == "ByteConstant" or t.type == "HexadecimalConstant" or t.type == "BinaryConstant" or t.type == "BooleanConstant" then
		return t.value

	elseif t.type == "Reference" then
		return t.name

	elseif t.type == "NullExpression" then
		return "null"

	elseif t.type == "ThrowExpression" then
		return "throw " .. serializeExpression( t.value )

	elseif t.type == "BracketExpression" then
		return "(" .. serializeExpression( t.value ) .. ")"

	elseif t.type == "FunctionExpression" then
		local p = {}

		for i = 1, #t.parameters do
			p[i] = serializeType( t.parameters[i].class ) .. " " .. t.parameters[i].name
		end

		return "function ->" .. serializeType( t.returns ) .. " (" .. table.concat( p, ", " ) .. ")"
			.. " " .. serializeBlock( t.body )

	elseif t.type == "ArrayExpression" then
		local s = {}

		for i = 1, #t.value do
			s[i] = serializeExpression( t.value[i] )
		end

		return "[" .. table.concat( s, ", " ) .. "]"

	elseif t.type == "TableExpression" then
		local s = {}

		for i = 1, #t.value do
			s[i] = ("[" .. serializeExpression( t.value[i].index ) .. "] = " .. serializeExpression( t.value[i].value )):gsub( "\n", "\n\t" )
		end

		return #s == 0 and "{}" or "{\n\t" .. table.concat( s, ";\n\t" ) .. ";\n}"

	elseif t.type == "LeftUnaryExpression" then
		return t.operator .. serializeExpression( t.value )

	elseif t.type == "RightUnaryExpression" then
		return serializeExpression( t.value ) .. t.operator

	elseif t.type == "FunctionCall" then
		local s = {}

		for i = 1, #t.parameters do
			s[i] = serializeExpression( t.parameters[i] )
		end

		return serializeExpression( t.value ) .. "(" .. table.concat( s, ", " ) .. ")"

	elseif t.type == "MethodCall" then
		local s = {}

		for i = 1, #t.parameters do
			s[i] = serializeExpression( t.parameters[i] )
		end

		return serializeExpression( t.value ) .. ":" .. t.name .. "(" .. table.concat( s, ", " ) .. ")"

	elseif t.type == "Index" then
		local s = {}

		for i = 1, #t.index do
			s[i] = serializeExpression( t.index[i] )
		end

		return serializeExpression( t.value ) .. "[" .. table.concat( s, ", " ) .. "]"

	elseif t.type == "DotIndex" then
		return serializeExpression( t.value ) .. "." .. t.index

	elseif t.type == "MatchExpression" then
		local c = {}

		for i = 1, #t.cases do
			local m = {}
			for n = 1, #t.cases[i].matches do
				m[n] = serializeExpression( t.cases[i].matches[n] ):gsub( "\n", "\n\t" )
			end
			c[i] = table.concat( m, ", " ) .. " => " .. serializeExpression( t.cases[i].value ):gsub( "\n", "\n\t" )
		end

		if t.default then
			c[#c + 1] = "default => " .. serializeExpression( t.default ):gsub( "\n", "\n\t" )
		end

		return "match " .. serializeExpression( t.condition ) .. " {\n\t" .. table.concat( c, ";\n\t" ) .. ";\n}"

	elseif t.type == "NewExpression" then
		local s = {}

		for i = 1, #t.parameters do
			s[i] = serializeExpression( t.parameters[i] )
		end

		return "new " .. serializeType( t.class ) .. "(" .. table.concat( s, ", " ) .. ")"

	elseif t.type == "BacktickExpression" then
		return "(" .. (t.lvalue and serializeExpression( t.lvalue ) .. " " or "") .. "`" .. t.operator .. "`" .. (t.rvalue and " " .. serializeExpression( t.rvalue ) or "") .. ")"

	elseif t.type == "BinaryExpression" then
		return "(" .. serializeExpression( t.lvalue ) .. " " .. t.operator .. " " .. serializeExpression( t.rvalue ) .. ")"

	elseif t.type == "OperatorExtends" then
		return serializeExpression( t.lvalue ) .. " extends " .. t.rvalue

	elseif t.type == "OperatorTypeOf" then
		return (t.class and serializeType( t.class ) .. " " or "") .. " typeof " .. serializeExpression( t.value )

	elseif t.type == "OperatorImplements" then
		return serializeExpression( t.lvalue ) .. " implements " .. t.rvalue

	elseif t.type == "Cast" then
		return serializeExpression( t.value ) .. " -> " .. serializeType( t.class )

	else
		return "<serialization of " .. t.type .. " isn't written>"

	end

end
