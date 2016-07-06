
function class( name )
	local mt = {}
	local class = setmetatable( { name = name, mt = mt }, mt )

	function class:extends( super )
		self.mt.__index = super
	end

	function mt:__call( ... )
		local mt = { __index = self }
		local obj = setmetatable( { class = self, mt = mt }, mt )

		if obj[name] then
			obj[name]( obj, ... )
		end

		return obj
	end

	getfenv( 2 )[name] = class

	return function(t)
		for k, v in pairs( t ) do
			class[k] = v
		end
	end
end
