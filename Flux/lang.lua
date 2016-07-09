
local types = require "common.types"

local lang = {}

lang.keywords = {}
lang.operators = {}
lang.symbols = {}
lang.binary_operators = {}
lang.unary_operators = {}

for word in ([[

import ...

namespace ... { ... }
using ...
class ... [extends ...] [implements ...[, ...] and ...] [( ..., ... )] ...
class ... = ...
interface ... [implements ...[, ...] and ...] ...
enum ... { ... }

if ... else ...
while ...
repeat ... until ...
repeat ... while ...
do ... then ...
for ...
foreach ... in ...
switch ... case ... default ...
try ... catch ... default ...
new ...
return ...
break
continue
let [const] ... = ...

const [auto] ... = ...
[template <...>] [const] [auto | void] ... (...) ...

throw ...
... typeof ...
function ...
lambda
match { ... }

and or

null

where

public private static
setter getter
operator +

template
]]):gmatch "%w+" do
	lang.keywords[word] = true;
end

local precedence = 0

for line in ([[

++ -- # ! ~

= += -= *= /= %= **= &= |= ^= <<= >>=

||
&&
> < >= <= == !=
..
|
&
^
<< >>
+ -
* / %
**
->

]]):gmatch "[^\n]+" do
	for operator in line:gmatch "%S+" do
		lang.operators[operator] = precedence
	end
	precedence = precedence + 1
end

for operator in pairs( lang.operators ) do
	lang.symbols[operator] = true
end

for symbol in ([[

( )
{ }
[ ]

=>

. :
,
;

...
::
@

]]):gmatch "%S+" do
	lang.symbols[symbol] = true
end

for operator, p in pairs( lang.operators ) do
	if p == 0 then
		lang.unary_operators[operator] = true
	else
		lang.binary_operators[operator] = p
	end
end

lang.unary_operators["-"] = true

types.define "Expression"
types.define "Block"
types.define "Type"
types.define "ExpressionStatement"

types.parseMany [[

Position: { "source" = string, "line" = number, "character" = number, "strline" = string }
HasPosition: { "position" = Position }
]]

return lang
