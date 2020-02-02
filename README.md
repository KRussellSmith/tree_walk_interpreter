A Tree-Walk Interpreter
=======================
This is a tree-walk interpreter I wrote in early November 2019. It parses and executes a custom scripting language that is similar to ES6 JavaScript and CoffeeScript.
You can enter the command `exc demo` to run the included demonstration example. You can enter code into the command line and use the `run` command. Use `clear` to delete previously entered code, and `quit` to exit back to Python.
__Importantly__, do not delete the `array` file, the interpreter requires it for some library functions.


Disclaimer:
----------
This interpreter was written quickly to serve as a proof of concept, and at the expense of performance and code quality.
A new, higher quality interpreter with a custom, stack-based virtual machine is in the works, and I am writing it in Go for performance. The Go implementation will offer more features such as string interpolation (e.g. "2 + 2 = #{2 + 2}"), which this implementation, provided as-is, lacks.
