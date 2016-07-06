
local checkall, whynot, parseType
local names = {}
local escape_lookup = setmetatable( { ["n"] = "\n", ["t"] = "\t" }, { __index = function( t, v )
	return v
end } )

local old_tostring = tostring

local function tostring( v )
	if type( v ) == "string" then
		return ("%q"):format( v )
	else
		return old_tostring( v )
	end
end

local function tostr( t )
	if t.type == "luatype" then return t.value end
	if t.type == "any" then return "any" end
	if t.type == "literal" then return type( t.value ) == "string" and ("%q"):format( t.value ) or tostring( t.value ) end
	if t.type == "custom" then return "custom (" .. tostring( t.value ) .. ")" end
	if t.type == "name" then return t.value end
	if t.type == "table" then
		local s = {}
		for i = 1, #t.value do
			s[#s + 1] = tostr( t.value[i].index ) .. " = " .. tostr( t.value[i].value )
		end
		return "{" .. table.concat( s, ", " ) .. "}"
	end
	if t.type == "union" then
		local tt = {}
		for i = 1, #t.value do
			tt[i] = tostr( t.value[i] )
		end
		return table.concat( tt, " | " )
	end
	if t.type == "intersection" then
		local tt = {}
		for i = 1, #t.value do
			tt[i] = tostr( t.value[i] )
		end
		return table.concat( tt, " & " )
	end
	return "wat?!"
end

local function rawcheck( o, t )
	return type( o ) == t
end

local function anycheck()
	return true
end

local function literalcheck( o, t )
	return o == t
end

local function unioncheck( o, t )
	for i, v in ipairs( t ) do
		if checkall( o, v ) then
			return true
		end
	end
	return false
end

local function intersectioncheck( o, t )
	for i, v in ipairs( t ) do
		if not checkall( o, v ) then
			return false
		end
	end
	return true
end

local function customcheck( o, t )
	return t( o )
end

local function tablecheck( o, t )
	if #t == 0 then
		return not next( o )
	end

	if type( o ) == "table" then
		for i = 1, #t do
			local ok = checkall( nil, t[i].value )

			if not ok then
				for k, v in pairs( o ) do
					if checkall( k, t[i].index ) and checkall( v, t[i].value ) then
						ok = true
						break
					end
				end
			end

			if not ok then
				return false
			end
		end
	else
		return false
	end

	return true
end

local function namedcheck( o, t )
	return checkall( o, names[t] )
end

function checkall( o, t )
	local f = 
		   t.type == "luatype" and rawcheck
		or t.type == "any" and anycheck
		or t.type == "literal" and literalcheck
		or t.type == "union" and unioncheck
		or t.type == "intersection" and intersectioncheck
		or t.type == "custom" and customcheck
		or t.type == "table" and tablecheck
		or t.type == "name" and namedcheck
		or error( "unexpected type '" .. tostring( type.type ) .. "'", 2 )

	return f( o, t.value )
end

local function whynot_raw( o, t )
	return type( o ) ~= t and "type of object " .. tostring( o ) .. " has type " .. type( o ) .. " not " .. tostring( t ) or nil
end

local function whynot_literal( o, t )
	return o ~= t and  "object " .. tostring( o ) .. " isn't equal to " .. tostring( t ) or nil
end

local function whynot_union( o, t )
	local s = {}
	for i, v in ipairs( t ) do
		local r = whynot( o, v )
		if r then
			s[i] = tostr( t[i] ) .. " => " .. r
		else
			return nil end
	end
	return "object " .. tostring( o ) .. " failed union test due to:\n\t" .. table.concat( s, "\n" ):gsub( "\n", "\n\t" )
end

local function whynot_intersection( o, t )
	local s = {}
	for i, v in ipairs( t ) do
		s[#s + 1] = whynot( o, v )
	end
	if #s > 0 then
		return "object " .. tostring( o ) .. " failed " .. (#s == #t and "all of" or "intersection test due to") .. ":\n\t" .. table.concat( s, "\n" ):gsub( "\n", "\n\t" )
	end
	return nil
end

local function whynot_custom( o, t )
	return not t( o ) and "object " .. tostring( o ) .. " failed to match a custom check"
end

local function whynot_table( o, t )
	if #t == 0 then
		return next( o ) and "match of non-empty table against empty table failed"
	end

	if type( o ) == "table" then
		for i = 1, #t do
			local ok = checkall( nil, t[i].value )
			local r = { not ok and whynot( nil, t[i].value ) }

			if not ok then
				for k, v in pairs( o ) do
					if checkall( k, t[i].index ) and checkall( v, t[i].value ) then
						ok = true
						break
					else
						local r1 = whynot( k, t[i].index )
						local r2 = whynot( v, t[i].value )

						r[#r + 1] = (r1 and r2 and "index: " .. r1 .. ", value: " .. r2) or (r1 and "index: " .. r1) or (r2 and "value: " .. r2)
					end
				end
			end

			if not ok then
				return "index " .. tostr( t[i].index ) .. " or value " .. tostr( t[i].value ) .. " failed to match due to the following:\n\t" .. table.concat( r, "\n" ):gsub( "\n", "\n\t" )
			end
		end
	else
		return "object " .. tostring( o ) .. " is not a table"
	end

	return nil
end

local function whynot_named( o, t )
	return whynot( o, names[t] )
end

function whynot( o, t )
	local f = 
		   t.type == "luatype" and whynot_raw
		or t.type == "any" and whynot_any
		or t.type == "literal" and whynot_literal
		or t.type == "union" and whynot_union
		or t.type == "intersection" and whynot_intersection
		or t.type == "custom" and whynot_custom
		or t.type == "table" and whynot_table
		or t.type == "name" and whynot_named
		or error( "unexpected type '" .. tostring( t.type ) .. "'", 2 )

	return f( o, t.value )
end

local function lex( str )
	local i = 1
	local t = {}

	while i <= #str do
		if str:sub( i, i ) == "\"" or str:sub( i, i ) == "'" then
			local escaped = false
			local close = str:sub( i, i )
			local s = ""

			for n = i + 1, #str do
				if escaped then
					escaped = false
					s = s .. escape_lookup[str:sub( n, n )]
				elseif str:sub( n, n ) == "\\" then
					escaped = true
				elseif str:sub( n, n ) == close then
					i = n + 1
					close = false
					break
				else
					s = s .. str:sub( n, n )
				end
			end

			if close then
				i = #str + 1
			end

			t[#t + 1] = { type = "string", value = s }

		elseif str:find( "^%s", i ) then
			i = i + #str:match( "^%s+", i )

		elseif str:find( "^%d*%.?%d", i ) then
			local number = str:match( "^%d*%.?%d+", i )
			t[#t + 1] = { type = "number", value = tonumber( number ) }
			i = i + #number

		elseif str:find( "^[%w_]", i ) then
			local word = str:match( "^[%w_ \t]+", i )
			local tword = word:gsub( "%s+$", "" )

			if tword == "true" or tword == "false" then
				t[#t + 1] = { type = "boolean", value = tword == "true" }
			elseif tword == "nil" then
				t[#t + 1] = { type = "nil", value = nil }
			else
				t[#t + 1] = { type = "word", value = tword }
			end

			i = i + #word

		else
			t[#t + 1] = { type = "symbol", value = str:sub( i, i ) }
			i = i + 1

		end
	end

	return setmetatable( t, { __index = function() return { type = "eof" } end } )
end

local function next( pos )
	pos[1] = pos[1] + 1
	return pos
end

local function parseTableObj( tokens, pos )
	local n = 1
	local t = {}

	repeat
		local index, value = { type = "literal", value = n }, parseType( tokens, pos )

		if tokens[pos[1]].type == "symbol" and tokens[pos[1]].value == "=" then
			index = value
			value = parseType( tokens, next( pos ) )
		else
			n = n + 1
		end

		t[#t + 1] = { index = index, value = value }
	until tokens[pos[1]].type ~= "symbol" or tokens[pos[1]].value ~= "," or not next( pos )

	pos[1] = pos[1] + ( tokens[pos[1]].type == "symbol" and tokens[pos[1]].value == "}" and 1 or 0 )

	return { type = "table", value = t }
end

local function parseTypeObj( tokens, pos )
	if tokens[pos[1]].type == "symbol" and tokens[pos[1]].value == "{" then
		return parseTableObj( tokens, next( pos ) )
	elseif tokens[pos[1]].type == "word" then
		local name = tokens[next( pos )[1] - 1].value

		if not names[name] then
			error( "type '" .. name .. "' is not defined", 0 )
		end

		if name == "any" then
			return { type = "any" }
		elseif name == "nil" then
			return { type = "literal", value = nil }
		end

		return { type = "name", value = name }
	elseif tokens[pos[1]].type == "number" or tokens[pos[1]].type == "boolean" or tokens[pos[1]].type == "string" or tokens[pos[1]].type == "nil" then
		return { type = "literal", value = tokens[next( pos )[1] - 1].value }
	else
		return { type = "literal", value = nil }
	end
end

function parseTypeIntersections( tokens, pos )
	local v = { parseTypeObj( tokens, pos ) }

	while tokens[pos[1]].type == "symbol" and tokens[pos[1]].value == "&" do
		v[#v + 1] = parseTypeObj( tokens, next( pos ) )
	end

	return #v == 1 and v[1] or { type = "intersection", value = v }
end

function parseType( tokens, pos )
	local v = { parseTypeIntersections( tokens, pos ) }

	while tokens[pos[1]].type == "symbol" and tokens[pos[1]].value == "|" do
		v[#v + 1] = parseTypeIntersections( tokens, next( pos ) )
	end

	return #v == 1 and v[1] or { type = "union", value = v }
end

local types = {}

types.names = names

function types.whynot( object, t )
	return whynot( object, type( t ) == "string" and types.parse( t ) or t )
end

-- table( string )
function types.parse( stype )
	local tokens = lex( stype )
	-- print( stype, tostr( parseType( tokens, { 1 } ) ) )
	return parseType( tokens, { 1 } )
end

-- void( string )
function types.parseMany( stypes )
	local tokens = lex( stypes )
	local pos = { 1 }

	repeat
		local name = tokens[pos[1]].type == "word" and tokens[next( pos )[1] - 1].value
		local p = tokens[pos[1]]

		if not (name and tokens[next( pos )[1] - 1].type == "symbol" and tokens[pos[1] - 1].value == ":") then
			return error( "unexpected " .. tostring(tokens[p].type) .. " '" .. tostring(tokens[p].value) .. "'" )
		end

		names[name] = parseType( tokens, pos )
	until tokens[pos[1]].type == "eof"
end

-- void( string, table )
-- void( string, string )
-- void( string )
function types.define( name, t )
	names[name] = type( t ) == "string" and types.parse( t ) or t or { type = "literal", value = nil }
end

-- bool( any, table )
types.rawcheck = checkall

-- bool( any, table )
-- bool( any, string )
function types.check( object, t )
	return checkall( object, type( t ) == "string" and types.parse( t ) or t )
end

-- function( table )
-- function( string )
function types.struct()

end

-- function( string )
-- function( string, table, function )
-- function( string, string, function )
function types.func( name, params, body )

end

-- void( string, table, function )
-- void( string, string, function )
function types.overload( string, params, body )

end

names["number"] = { type = "luatype", value = "number" }
names["string"] = { type = "luatype", value = "string" }
names["boolean"] = { type = "luatype", value = "boolean" }
names["function"] = { type = "luatype", value = "function" }
names["table"] = { type = "luatype", value = "table" }

names["optional number"] = { type = "union", value = { { type = "literal", value = nil }, { type = "luatype", value = "number" } } }
names["optional string"] = { type = "union", value = { { type = "literal", value = nil }, { type = "luatype", value = "string" } } }
names["optional boolean"] = { type = "union", value = { { type = "literal", value = nil }, { type = "luatype", value = "boolean" } } }
names["optional function"] = { type = "union", value = { { type = "literal", value = nil }, { type = "luatype", value = "function" } } }
names["optional table"] = { type = "union", value = { { type = "literal", value = nil }, { type = "luatype", value = "table" } } }

names["any"] = { type = "any" }

return types
