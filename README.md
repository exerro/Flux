# Flux

Flux is a statically typed Java-like language, that will initially compile to Lua.
More compilation targets are planned (e.g. Java, JavaScript, C, LLVM).

Flux is in its early stages right now, having recently been rewritten in Java to use ANTLR as the parsing backend.

### Syntax examples

Enums (tagged unions) and pattern matching
```
enum AST {
	enum Statement {
		IfStatement(Expression, Block, Block?)
		ReturnStatement(Expression)
		Call(string, Expression[])
	}
	enum Expression {
		enum Primary {
			Integer(int)
			Boolean(bool)
			String(string)
		}
		Binary(string, Expression, Expression)
	}
	Block(Statement[])
}

AST prog = Statement.IfStatement(
	Binary(">", Primary.Integer(5), Primary.Integer(2)),
	Block([
		Call("print", [Primary.String "It works!"])
	]),
	null
)

match (prog) {
	Statement.IfStatement(condition, block, elseBlock) -> print "IfStatement"
	                                                   -> print "Another AST type"
}

let (x, y) = (1, 2)
let {x = i} = { x = x, y = y }
print( i )
```

Classes
```
class Button(int x, int y, int w, int h) extends Element {
	private string text = ""

	Button(string text) { // note that x, y, w and h are handled automatically due to the class declaration
		self.text = text
	}

	string get text() {
		return self.text
	}

	void set text(string value) {
		return (self as Element).setProperty( "text", self.text, value )
	}

	void draw(Canvas);
}

void Button.draw(Canvas canvas) {
	canvas.drawRectangle(0, 0, w, h)
	canvas.drawText(text, 0, 0, w, h)
}
```

Operator overloading and templates
```
let string str * int rep = str.repeat(rep)

struct Vec2<`T>(T x, T y) {
	Vec2<T> operator+(Vec2<T> other)
		= new Vec2<T>(x + other.x, y + other.y)

	string tostring()
		= "(" + x + ", " + y + ")"
}

Vec2<T> Vec2<T>::origin<`T>()
	= new Vec2<T>(0, 0)

let v1 = new Vec2(5, 3)
let v2 = new Vec2(3, 6)
let v3 = Vec2<int>::origin()
print(v1 + v2 + v3) // (8, 9)
```
