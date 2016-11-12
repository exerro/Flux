# Flux

Flux is a statically typed C-like language, that will initially compile to Lua, although more compilation targets are planned.

### Development

Right now, a Lua-based Flux parser exists that can do a basic conversion from Flux to Lua.
This has allowed a Flux parser written in Flux to be started (in development now), that will also do the type checking, optimisation, and proper compilation.

### Syntax examples

Distance between two vectors.
```
float distance(Vec2 a, b = new Vec2( 0, 0 )) = math::sqrt( dx * dx + dy * dy )
	where dx = a.x - b.x
	where dy = a.y - b.y;
```

Factorial calculation.
```
let x `!` = match x {
	0, 1 => 1;
	default => x * (x - 1) `!`;
};
```

Two dimensional array class.
```
template<T>
class 2DArray<T> {
	private T[][] elements;

	2DArray(int dim1 = 0, dim2 = 0, T value = new T) {
    	foreach (i in 0 .. dim2-1) { // note the brackets are optional
			elements[i] = [];
			for (int n = 0, n < dim1, n++)
				elements[i][n] = value;
		}
 	}

	getter elements() {
		return table::deep_copy( elements );
	}

	T operator[](int i1, i2)
		= elements[i2] && elements[i2][i1];

	void operator[]=(int i1, i2, T value) {
		elements[i2] = elements[i2] || [];
		elements[i2][i1] = value;
	}
}

new 2DArray<int> myArray(5, 5);
print( myArray[3, 4] );
```

```
template<T>
void add_one_to(T& n)
	n++;

int& x = 0;
add_one_to( x );
print( x ); // 1
```
