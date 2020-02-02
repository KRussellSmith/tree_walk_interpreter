'''

*{===========================================================================}*
 {                                                                           } 
 {              An interpreter for a custom scripting language               } 
 {                         By K. Russell Smith                               } 
 {                                                                           }
*{===========================================================================}*
                                     '''
import cmath
import re
import os
import sys
import curses
import random


'''

*{===================================}*
 {                                   } 
 {         APPLICATION SETUP         } 
 {                                   } 
*{===================================}*
                  '''

openFile = (lambda f:
	str(open(f, 'r').read()))

'''

*{===================================}*
 {                                   } 
 {         BEGIN INTERPRETER         } 
 {                                   } 
*{===================-===============}*
                  '''

def addition(x, y):
	if type(x) == int:
		if type(y) in (int, float):
			return x + y
		elif type(y) == str:
			return ('%i' % x) + y
	elif type(x) == float:
		if type(y) in (int, float):
			return x + y
		elif type(y) == str:
			return ('%g' % x) + y
	elif type(x) == str:
		if type(y) == str:
			return x + y
		elif type(y) == int:
			return x + ('%i' % y)
		elif type(y) == float:
			return x + ('%g' % y)
	raise Exception("Invalid addition types: {} and {}".format(type(x), type(y)))

def modulation(x, y):
	if type(x) == str:
		if type(y) == Array:
			return x % tuple(y.elements)
	return x % y

class Range():
	def __init__(self, begin, end):
		self.begin = begin
		self.end = end
		if begin > end:
			self.max = begin
			self.min = end
		else:
			self.max = end
			self.min = begin

binaryOperators = {
	'+' : addition,
	'-' : lambda x, y: x - y,
	'*' : lambda x, y: x * y,
	'/' : lambda x, y: x / y,
	'^' : lambda x, y: x ** y,
	'%' : modulation,
	'<' : lambda x, y: x < y,
	'>' : lambda x, y: x > y,
	'<=': lambda x, y: x <= y,
	'>=': lambda x, y: x >= y,
	'==': lambda x, y: x == y,
	'!=': lambda x, y: x != y,
	'&' : lambda x, y: int(x) & int(y),
	'|' : lambda x, y: int(x) | int(y),
	'<<': lambda x, y: x << y,
	'>>': lambda x, y: x >> y,
	'to': lambda x, y: Range(x, y)
}
unaryOperators =	{
	'+': lambda x: x,
	'-': lambda x: -x,
	'!': lambda x: not x,
	'~': lambda x: ~x,
}

assignmentOperators = {
	'='  : lambda x, y: y,
	'+=' : addition,
	'-=' : lambda x, y: x - y,
	'*=' : lambda x, y: x * y,
	'/=' : lambda x, y: x / y,
	'^=' : lambda x, y: x ** y,
	'%=' : lambda x, y: x % y,
	'&=' : lambda x, y: int(x) & int(y),
	'|=' : lambda x, y: int(x) | int(y),
	'<<=': lambda x, y: x << y,
	'>>=': lambda x, y: x >> y,
}

LANGUAGE = 'en'
RESERVED = (
	'else', 'var', 'datatype'
	'break', 'continue',
)
RESERVED = {
	'en': {
		'null',
		'true',
		'false',
		'else',
		'if',
		'break',
		'continue',
		'let',
		'dec',
		'return',
		'use',
		'switch',
		'while',
		'redo',
		'for',
		'in',
		'to',
		'of',
		
		# Reserved, but not currently in use:
		'import',
		'async',
		'await',
		'try',
		'except',
		'throw',
		'finally',
		'goto'
	}
}
#TODO: multilangual support:


'''

*{===================================}*
 {                                   } 
 {               LEXER               } 
 {                                   } 
*{===================================}*
                  '''

class InvalidCharacter (BaseException): pass
class MalformedNumeral (BaseException): pass
class InvalidSyntax (BaseException): pass

class Lexer:
	i = 0
	input = ''
	c = ''
	row = 1
	col = 0
	fin = lambda self: self.i >= len(self.input)
	tokens = []
	bases = {
		'b': [2, '[0-1]'], 't': [3, '[0-2]'],
		'q': [4, '[0-3]'], 'o': [8, '[0-7]'],
		'd': [12, '[0-9a-bA-B]'], 'x': [16, '[0-9a-fA-F]']
	}
	def advance(self):
		if self.fin(): return '\n'
		self.i += 1
		self.c = self.input[self.i - 1]
		return self.c
		
	def __init__(self, input):
		self.i = 0
		self.input = input
		self.c = self.input[self.i]
		self.tokens = []
	
	def add(self, type, value = None):
		self.tokens.append({'type': type, 'value': value})
	
	def skipLineBreaks(self):
		while re.search('[\s\n]', self.c):
			if self.fin():
				self.add('fin.')

	def lex(self):
		base = 10
		start = 0
		def peek():
			if self.i >= len(self.input):
				return ''
			return self.input[self.i]
		
		def peekNext():
			if self.i >= len(self.input) - 1:
				return ''
			return self.input[self.i + 1]
		match        = lambda x: not self.fin() and (x == self.advance())
		isIdentifier = lambda char: re.search('[a-zA-Z_α-ωΑ-Ωа-яА-Яá-žÁ-Ź]', char)
		numbers = '[0-9a-fA-F]'
		maxIndent = 0;
		indent = 0;
		ignore = 0;
		while not self.fin():
			self.c = self.input[self.i]
			start = self.i
			if re.search('\n', self.c):
				indent = 0
				self.add('endl')
				self.advance()
			elif re.search('\s', self.c):
				self.advance()
			elif re.search(';', self.c):
				while self.i < len(self.input) - 1 and peek() != '\n':
					self.advance()
				self.advance()
			elif re.search('`', self.c):
				scope = 1
				self.advance()
				while scope > 0:
					self.advance()
					if re.search('`', self.c):
						scope += 1
					elif (peek() + peekNext() == '.`'):
						scope -= 1
						self.advance()
						self.advance()
					if self.fin():
						break
				self.advance()
			elif re.search('\'', self.c):
				self.advance()
				while not self.fin() and not re.search('\'', peek()):
					self.advance()
				self.add('string', self.input[start + 1 : self.i])
				self.advance()
			elif re.search('"', self.c):
				self.advance()
				while not self.fin() and not re.search('"', peek()):
					self.advance()
				self.add('string', self.input[start + 1 : self.i])
				self.advance()
			elif re.search('\?', self.c):
				if peekNext() == ':':
					self.add('?:')
					self.advance()
				elif peekNext() == '!':
					self.add('?!')
					self.advance()
				else:
					self.add(self.c)
				self.advance()
			elif re.search(':', self.c):
				if peekNext() == self.c:
					self.add('::')
					self.advance()
				elif peekNext() == '>':
					self.add(':>')
					self.advance()
				else: self.add(self.c)
				self.advance()
			elif re.search('[.\{\}\\\),(\[\]\~]', self.c):
				self.add(self.c)
				self.advance()
			elif self.c == '!':
				if peekNext() == '=':
					self.add('!=')
					self.advance()
				else:
					self.add('!')
				self.advance()
			elif self.c == '=':
				if peekNext() == '=':
					self.add('==')
					self.advance()
				elif peekNext() == '>':
					self.add('=>')
					self.advance()
				else:
					self.add(self.c)
				self.advance()
			elif self.c == '-':
				if peekNext() == '-':
					self.add('--')
					self.advance()
				if peekNext() == '>':
					self.add('->')
					self.advance()
				elif peekNext() == '=':
					self.add('-=')
					self.advance()
				else:
					self.add(self.c)
				self.advance()
			elif re.search('[+\&\|]', self.c):
				if peekNext() == self.c:
					self.add(self.c + self.c)
					self.advance()
				elif peekNext() == '=':
					self.add(self.c + '=')
					self.advance()
				else:
					self.add(self.c)
				self.advance()
			elif re.search('[\>]', self.c):
				if peekNext() == self.c:
					self.advance()
					if peekNext() == '=':
						self.add(self.c + self.c + '=')
						self.advance()
					else:
						self.add(self.c + self.c)
				elif peekNext() == '=':
					self.add('>=')
					self.advance()
				else:
					self.add('>')
				self.advance()
			elif re.search('[\<]', self.c):
				if peekNext() == self.c:
					self.advance()
					if peekNext() == '=':
						self.add(self.c + self.c + '=')
						self.advance()
					else:
						self.add(self.c + self.c)
				elif peekNext() == '=':
					self.add('<=')
					self.advance()
				elif peekNext() == '-':
					self.add('<-')
					self.advance()
				else:
					self.add(self.c)
				self.advance()
			elif re.search('[*/%^]', self.c):
				if peekNext() == '=':
					self.add(self.c + '=')
					self.advance()
				else:
					self.add(self.c)
				self.advance()
			elif self.c == '#':
				if re.search(numbers, peekNext()):
					self.advance()
					start = self.i
					while not self.fin() and re.search(numbers, peek()):
						self.advance()
					number = int(self.input[start:self.i], 16)
					self.add('number', number)
				else:
					if peekNext() == '{':
						self.add('#{')
						self.advance()
					self.advance()
			elif self.c.isalpha() or self.c == '_':
				identifier = ''
				while not self.fin() and (peek().isalpha() or ord(peek()) == 0x5f):
					self.advance()
				identifier = self.input[start:self.i]
				if identifier in RESERVED[LANGUAGE]:
					self.add(identifier)
				else:
					self.add('identifier', identifier)
			elif re.search('[0-9]', self.c):
				number = ''
				digits = '[0-9]'
				if base == 10 and self.c == '0' and peekNext() in self.bases:
					self.advance()
					base = self.bases[peek()][0]
					digits = self.bases[self.advance()][1]
					start = self.i
				while not self.fin() and re.search(digits, peek()):
					self.advance()
				if base == 10:
					if not self.fin() and peek() == '.':
						self.advance()
						while not self.fin() and re.search(digits, peek()):
							self.advance()
							
						if self.c == '.':
							raise MalformedNumeral(
								'Only one radix point permitted')
						elif re.search('[eE]', peek()):
							self.advance()
							while not self.fin() and re.search(digits, peek()):
								self.advance()
						number = self.input[start:self.i]
						self.add("float", float(number))
						continue
						
					elif re.search('[eE]', peek()):
						self.advance()
						while not self.fin() and re.search(digits, peek()):
							self.advance()
							
						if self.c == '.':
							raise MalformedNumeral(
								'Radix points are not applicable here.')				
				elif self.c == '.':
					raise MalformedNumeral(
						'Radix points only applicable for base-10 numbers')
				elif not re.search('\s', self.c) and not re.search(digits, self.c):
					raise MalformedNumeral(
						'"%s" is not a valid digit for a base-%d number'
						% (self.c, base))
				number = self.input[start:self.i]
				if base == 10:
					number = int(number)
				else:
					number = int(number, base)
				self.add('int', number)
				base = 10
				
			else: raise InvalidCharacter(
				'"%s" (ASCII %s) not recognized'
				% (self.c, hex(ord(self.c))))
		for i in range(maxIndent):
			self.add('}')
			self.add('endl')
		self.add('fin.')
		
		return self.tokens




'''

*{===================================}*
 {                                   }  
 {              PARSER               } 
 {                                   } 
*{===================================}*
                  '''

class AST (object): pass

class Unary (AST):
	def __init__(self, operator, expr):
		self.operator, self.expr = operator, expr
		self.token = operator

class Binary (AST):
	def __init__(self, left, operator, right, mutable = False):
		self.left, self.operator, self.right, self.mutable = left, operator, right, mutable
		self.token = operator

class Ternary (AST):
	def __init__(self, cond, then_expr, else_expr):
		self.cond, self.then_expr, self.else_expr = cond, then_expr, else_expr
		
class IntNum (AST):
	def	__init__(self, value = 0):
		self.value = value
		
class FloatNum (AST):
	def	__init__(self, value = 0.0):
		self.value = value

class String (AST):
	def	__init__(self, token):
		self.token, self.value = token, token['value']
		
class Arr (AST):
	def	__init__(self, value = [], mutable = True):
		self.value = value
		self.mutable = mutable

class Null_ptr (AST): pass
class Bool_val (AST):
	def __init__(self, value = False):
		self.value = value

class Identifier (AST):
	def __init__(self, token, mutable = False):
		self.token, self.value, self.mutable = token, token['value'], mutable

class Key (AST):
	def __init__(self, value, mutable = False):
		self.value, self.mutable = value, mutable
	
class Arg (AST):
	def __init__(self, value, mutable = False):
		self.value, self.mutable = value, mutable

class Assign (AST):
	keys = []
	def __init__(self, mutable = False):
		self.mutable = mutable
		self.keys = []
	def addKey(self, key, value):
		self.keys.append({'key': key, 'value': value})

class Mutate (AST):
	def __init__(self, left, operator, right):
		self.left, self.operator, self.right = left, operator, right
		self.token = operator
		
class Get(AST):
	def __init__(self, expr, name, optional = True):
		self.expr = expr
		self.name = name
		self.optional = optional

class Set(AST):
	def __init__(self, obj, operator, expr):
		self.obj, self.operator, self.expr = obj, operator, expr

class Call (AST):
	def __init__(self, left, args, optional, mutable):
		self.left = left
		self.args = args
		self.optional = optional
		self.mutable = mutable
		
class Defun (AST):
	def __init__(self, left, args, right):
		self.value, self.args, self.right = left.value, args, right

class Fun (AST):
	def __init__(self, args, right):
		self.args, self.right = args, right

class AsyncFun (Fun): pass

class Block (AST):
	def __init__(self):
		self.ware = {}
		self.content = []
		self.declarations = []

class Return (AST):
	def __init__(self, value):
		self.value = value

class Break (AST): pass
class Continue (AST): pass

class ReturnCall (BaseException):
	value = None
	def __init__(self):
		super(BaseException, None)

class BreakCall (BaseException):
	def __init__(self):
		super(BaseException, None)
		
class ContinueCall (BaseException):
	def __init__(self):
		super(BaseException, None)

class Switch (AST):
	def __init__(self, expr, then_arm, else_arm):
		self.expr, self.then_arm, self.else_arm = expr, then_arm, else_arm

class Case (AST):
	def __init__(self, values, then_arm):
		self.values = values
		self.then_arm = then_arm

class If (AST):
	def __init__(self, cond, then_arm, else_arm = None):
		self.cond, self.then_arm, self.else_arm = cond, then_arm, else_arm

class While (AST):
	def __init__(self, cond, then_arm, else_arm = None):
		self.cond, self.then_arm, self.else_arm = cond, then_arm, else_arm

class Do_While (While): pass
		
class For (AST):
	def __init__(self, iter, init, comp, then_arm, else_arm):
		self.iter = iter
		self.init = init
		self.comp = comp
		self.then_arm = then_arm
		self.else_arm = else_arm
		
class ForEach (AST):
	def __init__(self, iter, arr, then_arm, else_arm):
		self.iter = iter
		self.arr = arr
		self.then_arm = then_arm
		self.else_arm = else_arm

class Mixin (AST):
	def __init__(self, expr):
		self.expr = expr

class Hash (AST):
	def __init__(self, hash = {}, borrows = [], operators = {}, mutable = True):
		self.hash = hash
		self.borrows = borrows
		self.mutable = mutable
		self.operators = operators
		
class Scope (AST):
	vars = {}
	is_async = False
	def __init__(self, parent = None, declarations = []):
		self.vars = {}
		self.parent = parent
		self.declarations = declarations
		if parent != None:
			self.is_async = parent.is_async
	
	def getVar(self, x):
		if x in self.vars:
			return self.vars[x]
		elif self.parent != None:
			return self.parent.getVar(x)
		raise Exception('Value "%s" not found' % x)
	
	def defineVar(self, key, value):
		self.vars[key] = value
		if key in self.declarations:
			self.declarations.remove(key)
	
	def assignVar(self, x, y):
		if x in self.vars or self.getVar(x) == None:
			self.vars[x] = y
			return self.vars[x]
		elif self.parent != None:
			return self.parent.assignVar(x, y)

class Empty (AST): pass

class Function():
	closure = None
	def __init__(self, parems, body, evaldoer, closure):
		self.parems, self.body, self.evaldoer, self.closure = parems, body, evaldoer, closure
	def call(self, *args):
		scope = Scope(self.closure, self.closure.declarations)
		for i in range(len(args)):
			scope.defineVar(self.parems[i].value, args[i])
		try:
			self.evaldoer.execute_Block(self.body, scope)
		except ReturnCall:
			return ReturnCall.value
		return None

class AsyncFunction(Function):
	async def call(self, *args):
		scope = Scope(self.closure, self.closure.declarations)
		scope.is_async = True
		for i in range(len(args)):
			scope.defineVar(self.parems[i].value, args[i])
		try:
			self.evaldoer.execute_Block(self.body, scope)
		except ReturnCall:
			return ReturnCall.value
		return None

class Array():
	def __init__(self, elements = [], mutable = True):
		self.elements = elements
		#self.mutable = mutable
		self.mutable = True
	
	def push(self, *elements):
		if not self.mutable:
			raise Exception("Set immutable")
		self.elements.extend(elements)
		return self
		
	def pop(self, *args):
		if not self.mutable:
			raise Exception("Set immutable")
		return self.elements.pop(*args)
		
	def length(self):
		return len(self.elements)
		
	def get(self, i):
		return self.elements[i]
		
	def put(self, x, y):
		if not self.mutable:
			raise Exception("Set immutable")
		self.elements[x] = y
		
	def swap_mut(self, mutable):
		self.mutable = mutable
		for index in range(len(self.elements)):
			if type(self.elements[index]) == HashSet:
				self.elements[index].swap_mut(mutable)
			if type(self.elements[index]) == Array:
				self.elements[index].swap_mut(mutable)
		
class HashSet():
	def __init__(self, keys = {}, operators = {}, mutable = True):
		self.keys = keys
		self.operators = operators
		#self.mutable = mutable
		self.mutable = True

	def length(self):
		return len(self.keys)
		
	def get(self, i):
		return self.keys.get(i)
		
	def put(self, x, y):
		if not self.mutable:
			raise Exception("Set immutable")
		self.keys[x] = y
		
	def swap_mut(self, 	mutable):
		self.mutable = mutable
		for key in self.keys:
			if type(self.keys[key]) == HashSet:
				self.keys[key].swap_mut(mutable)
			if type(self.keys[key]) == Array:
				self.keys[key].swap_mut(mutable)

class NativeFunction(Function):
	def __init__(self, body):
		self.body = body
	def call(self, *args):
		return self.body(*args)

class Parser (object):
	tokens = []
	current = {'type': 'fin.'}
	tree = []
	i = 0
	global_declarations = []
	this_pointer = None
	
	def __init__(self, tokens):
		self.tokens = tokens
		self.current = self.tokens[self.i]
	
	def eat(self, *types):
		if self.current['type'] in types:
			self.current = self.get_next()
		else: raise InvalidSyntax('expected "%s" got "%s"' % (type, self.current['type']))

	last = lambda self: self.tokens[self.i - 1]
	eye = lambda self, *types: self.tokens[self.i + 1]['type'] in types
	sniff = lambda self, *types: self.tokens[self.i]['type'] in types
	
	def taste(self, *types):
		if self.current['type'] in types:
			self.eat(self.current['type'])
			return True
		return False
	
	def reach(self, *types):
		j = self.i
		while self.tokens[j]['type'] == 'endl':
			j += 1
		if self.tokens[j]['type'] in types:
			self.i = j
			self.current = self.tokens[self.i]
			self.eat(self.current['type'])
			return True
		return False
	
	def block_statement(self, mutable = True):
		self.eat('{')
		statements = self.block(mutable)
		self.eat('}')
		block = Block()
		for stmt in statements:
			if type (stmt) == Assign:
				keys = map(lambda x: x['key'].value, stmt.keys)
				keys = filter(lambda x: not (x in block.declarations), keys)
				block.declarations.extend(keys)
			block.content.append(stmt)
		return block
	
	def hash_block(self, mutable = True):
		self.eat('#{')
		self.skip_breaks()
		if self.taste('}'):
			return Hash({}, [], {}, mutable)
		borrows = []
		hash = {}
		operators = {}
		i = 0
		def addKey():
			if self.taste(':>'):
				key = 'mix in: %s' % i
				hash[key] = Mixin(self.expr())
			elif self.taste('\\'):
				operator = self.current['type']
				self.eat(operator)
				self.eat('\\')
				if operator in operators:
					raise Exception("Operator already overloaded")
				operators[operator] = self.expr()
			else:
				node = self.identifier()
				if type(node) == Defun:
					key = node.value
					hash[key] = Fun(node.args, node.right)
				else:
					key = node.value
					self.eat('=')
					hash[key] = self.expr(mutable)
					
		addKey()
		i += 1
		while self.taste(','):
			self.skip_breaks()
			if self.sniff('}'):
				break
			addKey()
			i += 1
		self.skip_breaks()
		self.eat('}')
		return Hash(hash, borrows, operators, mutable)
		
	def func_body(self, mutable = True):
		return self.no_assignments(mutable)
	
	def block(self, mutable = True):
		serving = self.statement(mutable)
		table = [serving]
		while self.taste('endl'):
			table.append(self.statement(mutable))
		return table
		
	def no_assignments(self, mutable = True):
		node = self.statement(mutable)
		if type(node) in (Assign, Defun):
			raise Exception("Unexpected assignment")
		return node
	
	def statement(self, mutable = True):
		stmt = None
		def expression_statement(mutable = True):
			stmt = self.expr(mutable)
			else_arm = None
			if self.taste('if'):
				expr = self.expr()
				if self.reach('else'):
					self.skip_breaks()
					else_arm = self.no_assignments(mutable)
				return If(expr, stmt, else_arm)
			elif self.taste('while'):
				expr = self.expr()
				if self.reach('else'):
					self.skip_breaks()
					else_arm = self.no_assignments(mutable)
				return While(expr, stmt, else_arm)
			elif self.taste('->'):
				self.skip_breaks()
				then_arm = self.no_assignments(mutable)
			return stmt
		
		if self.sniff('{'):
			stmt = self.block_statement()

		elif self.sniff('if'):
			self.eat('if')
			expr = self.expr()
			if not self.taste('endl', '{'):
				self.eat(',')
			self.skip_breaks()
			then_arm = self.no_assignments()
			else_arm = None
			if self.reach('else'):
				self.skip_breaks()
				else_arm = self.no_assignments()
			return If(expr, then_arm, else_arm)
		elif self.sniff('switch'):
			self.eat('switch')
			expr = self.expr()
			self.skip_breaks()
			self.eat('{')
			statements = self.block(mutable)
			self.eat('}')
			else_arm = None
			if self.reach('else'):
				self.skip_breaks()
				else_arm = self.no_assignments(mutable)
			block = Block()
			for stmt in statements:
				if type(stmt) == Fun:
					then_arm = stmt.right
					block.content.append(Case(stmt.args, then_arm))
				elif type(stmt) != Empty:
					raise Exception('Invalid statement, only case statements allowed in switch blocks')
			return Switch(expr, block, else_arm)
		elif self.sniff('while'):
			self.eat('while')
			expr = self.expr()
			self.eat('endl', '{')
			self.skip_breaks()
			then_arm = self.no_assignments()
			else_arm = None
			if self.reach('else'):
				self.skip_breaks()
				else_arm = self.no_assignments()
			return While(expr, then_arm, else_arm)
		elif self.sniff('redo'):
			self.eat('redo')
			self.skip_breaks()
			then_arm = self.no_assignments()
			self.eat('while')
			expr = self.expr()
			else_arm = None
			if self.reach('else'):
				self.skip_breaks()
				else_arm = self.no_assignments()
			return Do_While(expr, then_arm, else_arm)
			
		elif self.sniff('return'):
			self.eat('return')
			node = self.expr(mutable)
			stmt = Return(node)
		elif self.taste('break'):
			stmt = Break()
		elif self.taste('continue'):
			stmt = Continue()
		elif self.sniff('for'):
			return self.for_loop()
		elif self.sniff('let', 'dec'):
			return self.assign()
		elif self.taste(':>'):
			return Mixin(self.expr())
		elif self.sniff('endl', '}', 'fin.'):
			return Empty()
		else:
			return expression_statement(mutable)
		if self.sniff('if'):
			if type(stmt) in (Assign, Block):
				return stmt
			self.eat('if')
			expr = self.expr()
			return If(expr, stmt, None)
		if self.sniff('while'):
			if type(stmt) in (Assign, Block):
				return stmt
			self.eat('while')
			expr = self.expr()
			return While(expr, stmt, None)
		return stmt
	
	def assign(self):
		mutable = True
		if self.taste('let'):
			mutable = True
		elif self.taste('dec'):
			mutable = False
			
		ret = Assign(mutable)
		
		left = self.identifier()
		if self.reach('='):
			self.skip_breaks()
			ret.addKey(left, self.expr(mutable))
			while self.taste(','):
				self.skip_breaks()
				left = self.identifier()
				if self.reach('='):
					ret.addKey(left, self.expr(mutable))
				else:
					ret.addKey(left, Null_ptr())
		else:
			ret.addKey(left, Null_ptr)
		return ret
		
	def mutate(self):
		left = self.identifier()
		token = self.current
		self.eat(self.current['type'])
		right = self.expr()
		node = Mutate(left, token, right)
		return node
	
	def for_loop(self):
		self.eat('for')
		if self.sniff('endl', '{'):
			self.skip_breaks()
			then_arm = self.no_assignments()
			return While(Bool_val(True), then_arm)
			
		iter = self.expr()
		if self.taste('in'):
			init = self.expr()
			self.skip_breaks()
			then_arm = self.no_assignments()
			else_arm = None
			if self.reach('else'):
				self.skip_breaks()
				else_arm = self.no_assignments()
			return For(iter, init, None, then_arm, else_arm)
		elif self.taste('of'):
			arr = self.expr()
			if not self.taste('endl', '{'):
				self.eat(',')
			self.skip_breaks()
			then_arm = self.no_assignments()
			else_arm = None
			if self.reach('else'):
				self.skip_breaks()
				else_arm = self.no_assignments()
			return ForEach(iter, arr, then_arm, else_arm)
		else:
			if not self.taste('endl', '{'):
				self.eat(',')
			self.skip_breaks()
			then_arm = self.no_assignments()
			else_arm = None
			if self.reach('else'):
				self.skip_breaks()
				else_arm = self.no_assignments()
			return For(iter, None, None, then_arm, else_arm)
			
	def skip_breaks(self):
		while self.taste('endl'): 
			continue
	
	def call(self, left):
		self.eat('(')
		self.skip_breaks()
		args = []
		if not self.taste(')'):
			args.append(self.expr())
			while self.taste(','):
				self.skip_breaks()
				args.append(self.expr())
			self.skip_breaks()
			self.eat(')')
		node = Call(left, args, False, True)
		if self.taste('->'):
			self.skip_breaks()
			node = Defun(left, args)
		elif self.current['type'] == '(':
			return self.call(node)
		return node
		
	def identifier(self, mutable = True):
		node = Identifier(self.current)
		self.eat('identifier')
		if self.sniff('(', ':', '?:'):
			return self.callTo(node, mutable)
		return node
	
	def get_next(self):
		self.i += 1
		return self.tokens[self.i]
	
	def ternary(self, mutable = True):
		expr = self.assignment(mutable)
		if self.taste('?'):
			then_expr = self.ternary(mutable)
			self.eat('else')
			else_expr = self.ternary(mutable)
			return Ternary(expr, then_expr, else_expr)
		return expr
		
	def assignment(self, mutable = True):
		expr = self.null_coalesce(mutable)
		if self.taste(*assignmentOperators):
			equals = self.last()['type']
			value = self.assignment(mutable)
			if type(expr) == Identifier:
				return Mutate(expr, equals, value)
			elif type(expr) == Get:
				return Set(expr, equals, value)
		return expr
   
	def null_coalesce(self, mutable = True):
		expr = self.ex_or(mutable)
		if self.taste('?!'):
			right = self.ex_or(mutable)
			return Binary(expr, '?!', right)
		return expr
	def ex_or(self, mutable = True):
		expr = self.ex_and(mutable)
		while self.taste('||'):
			operator = self.last()['type']
			right = self.ex_and(mutable)
			expr = Binary(expr, operator, right)
		return expr

	def ex_and(self, mutable = True):
		expr = self.equality(mutable)
		while self.taste('&&'):
			operator = self.last()['type']
			right = self.equality(mutable)
			expr = Binary(expr, operator, right)
		return expr

	def equality(self, mutable = True):
		expr = self.comparison(mutable)
		while self.taste('!=', '=='):
			operator = self.last()['type']
			right = self.comparison(mutable)
			expr = Binary(expr, operator, right)
		return expr

	def comparison(self, mutable = True):
		expr = self.to(mutable)
		while self.taste('>', '>=', '<',  '<='):
			operator = self.last()['type']
			right = self.addition(mutable)
			expr = Binary(expr, operator, right)
		return expr

	def to(self, mutable = True):
		expr = self.addition(mutable)
		while self.taste('to'):
			expr = Binary(expr, 'to', self.addition(mutable))
		return expr
		
	def addition(self, mutable = True):
		expr = self.multiplication(mutable)
		while self.taste('-', '+'):
			operator = self.last()['type']
			right = self.multiplication(mutable)
			expr = Binary(expr, operator, right)
		return expr

	def multiplication(self, mutable = True):
		expr = self.exponent(mutable)
		while self.taste('/', '*', '%', '<<', '>>'):
			operator = self.last()['type']
			right = self.exponent(mutable)
			expr = Binary(expr, operator, right) 
		return expr
		
	def exponent(self, mutable = True):
		expr = self.callTo(None, mutable)
		while self.taste('^'):
			operator = self.last()['type']
			right = self.exponent(mutable)
			expr = Binary(expr, operator, right)
		return expr
	
	def finishCall(self, expr, name, optional = False, mutable = True):
		args = []
		if not self.taste(')'):
			self.skip_breaks()
			args.append(self.expr())
			while self.taste(','):
				self.skip_breaks()
				args.append(self.expr())
				self.skip_breaks()
			self.eat(')')
		if name != None:
			if self.taste('->'):
				self.skip_breaks()
				if self.current['type'] == '{':
					right = self.func_body(mutable)
				else:
					block = Block()
					block.content.append(Return(self.expr(mutable)))
					right = block
				return Defun(name, args, right)
		return Call(expr, args, optional, mutable)
	
	def callTo(self, name = None, mutable = True):
		if name == None:
			expr = self.factor(mutable)
		else:
			expr = name
		while True:
			optional = False
			expect_key = False
			if self.taste('?:'):
				optional = True
				expect_key = True
			elif self.taste(':'):
				expect_key = True
			if self.taste('('):
				expr = self.finishCall(expr, name, optional, mutable)
			elif self.taste('['):
				name = self.expr()
				self.eat(']')
				expr = Get(expr, name, optional)
			elif expect_key:
				self.eat('identifier')
				name = String(self.last())
				expr = Get(expr, name, optional)
			else: break
		return expr
  
	def factor(self, mutable = True):
		token = self.current
		if token['type'] in unaryOperators:
			self.eat(token['type'])
			node = Unary(token['type'], self.factor())
		elif token['type'] == 'int':
			self.eat(token['type'])
			node = IntNum(token['value'])
		elif token['type'] == 'float':
			self.eat(token['type'])
			node = FloatNum(token['value'])
		elif token['type'] == 'string':
			self.eat(token['type'])
			node = String(token)
		elif token['type'] == 'null':
			self.eat(token['type'])
			node = Null_ptr()
		elif token['type'] == 'true':
			self.eat(token['type'])
			node = Bool_val(True)
		elif token['type'] == 'false':
			self.eat(token['type'])
			node = Bool_val(False)
		elif token['type'] == '#{':
			node = self.hash_block(mutable)
		elif token['type'] == '[':
			self.eat('[')
			self.skip_breaks()
			if self.taste(']'):
				return Arr(mutable = mutable)
			arr = [self.expr(mutable)]
			while self.taste(','):
				self.skip_breaks()
				arr.append(self.expr(mutable))
			self.skip_breaks()
			self.eat(']')
			node = Arr(arr, mutable)
		elif token['type'] == 'use':
			self.eat('use')
			self.skip_breaks()
			expectClose = self.taste('(')
			args = [self.identifier()]
			while self.taste(','):
				self.skip_breaks()
				args.append(self.identifier())
			if expectClose:
				self.eat(')')
			self.skip_breaks()
			return Call(Fun(args, self.func_body(mutable)), args, False, mutable)
		elif token['type'] == '(':
			self.eat('(')
			if not self.sniff(')'):
				node = self.expr()
			else:
				node = Empty()
			args = [node]
			while self.reach(','):
				args.append(self.expr())
			self.eat(')')
			if self.taste('->'):
				self.skip_breaks()
				lookForCall = self.sniff('{')
				right = self.func_body(mutable)
				
				if lookForCall and self.sniff('('):
					return self.callTo(Fun(args, right))
				return Fun(args, right)
		else:
			node = self.identifier(mutable)
		if self.taste('->'):
			self.skip_breaks()
			lookForCall = self.sniff('{')
			if lookForCall and self.sniff('('):
				return self.callTo(Fun(args, right))
			return Fun([node], self.func_body(mutable))
		else: return node

	def expr(self, mutable = True):
		node = self.ternary(mutable)
		return node
		
	def parse(self):
		statements = self.block()
		block = Block()
		for stmt in statements:
			if type (stmt) == Assign:
				keys = map(lambda x: x['key'].value, stmt.keys)
				keys = filter(lambda x: not (x in block.declarations), keys)
				block.declarations.extend(keys)
			block.content.append(stmt)
		return block

'''

*{===================================}*
 {                                   } 
 {             EVALUATOR             } 
 {                                   } 
*{===================================}*
                  '''
pre = Parser(Lexer(openFile('array')).lex()).parse()
class Visitor (object):
	def visit(self, node):
		method_name = 'visit_' + type(node).__name__
		visitor = getattr(self, method_name, self.generic_visit)
		return visitor(node)
	
	def generic_visit(self, node):
		raise Exception('Invalid expression')

class Evaluator (Visitor):
	globals = Scope()
	scope = globals
	def __init__(self, tree = Block()):
		self.tree = pre
		self.tree.content.extend(tree.content)
		self.scope = self.globals
		
		def _pop(x, *args):
			argv = map(lambda x: x.value, args)
			return x.value.pop(*argv)
			
		def push(x, *argv):
			args = map(lambda x: x.value, argv)
			x.value.push(*args)
			return x.value

		def char(x):
			return chr(int(x.value))
			
		def length(x):
			if type(x.value) in (Array, HashSet):
				return x.value.length()
			return len(x.value)
			
		def _print(*args):
			arguments = map(lambda x: x.value, args)
			def convertArray(x):
				if type(x) == Array:
					return list(map(convertArray, x.elements))
				else:
					return x
			arguments = map(convertArray, arguments)
			return print(*arguments)
		def randomInt(x, y):
			return random.randint(x.value, y.value)
		
		def _type(x):
			if type(x.value) == Array:
				return 'array'
			elif type(x.value) == HashSet:
				return 'map'
			elif type(x.value) == int:
				return 'int'
			elif type(x.value) == float:
				return 'real'
			elif type(x.value) == str:
				return 'string'
			elif type(x.value) == bool:
				return 'Boolean'
			return 'unknown'
		
		def is_mutable(x):
			return x.mutable
		
		def _input(i):
			return input(i.value)
		
		def _int(x, y = None):
			if y != None:
				return int(x.value, y.value)
			return int(x.value)
			
		def real(x):
			return float(x.value)
		
		def char_code(char_set):
			x = char_set.value.lower()
			func = None
			
			if x == 'ascii':
				func = NativeFunction(
					lambda x: ord(x.value))
			return func
		
		def Rect(x, y, w, h):
			return HashSet({
				'x': float(x.value),
				'y': float(y.value),
				'w': float(w.value),
				'h': float(h.value)
			}, {}, False)
			
		self.globals.vars['print'] = Key(NativeFunction(_print))
		self.globals.vars['type'] = Key(NativeFunction(_type))
		self.globals.vars['input'] = Key(NativeFunction(_input))
		self.globals.vars['randomInt'] = Key(NativeFunction(randomInt))
		self.globals.vars['pop'] = Key(NativeFunction(_pop))
		self.globals.vars['push'] = Key(NativeFunction(push))
		self.globals.vars['length'] = Key(NativeFunction(length))
		self.globals.vars['char'] = Key(NativeFunction(char))
		self.globals.vars['mutable'] = Key(NativeFunction(is_mutable))
		#self.globals.vars['sqrt'] = Key(NativeFunction(lambda x: x.value ** 0.5))
		self.globals.vars['real'] = Key(NativeFunction(real))
		self.globals.vars['int'] = Key(NativeFunction(_int))
		self.globals.vars['run'] = Key(NativeFunction(lambda x: asyncio.run(x.value)))
		self.globals.vars['char_code'] = Key(NativeFunction(char_code))
		self.globals.vars['Rect'] = Key(NativeFunction(Rect))
		self.globals.vars['Math'] = Key(HashSet({
			'PI': 3.14159,
			'sqrt': NativeFunction(lambda x: x.value ** 0.5),
			'hypot': NativeFunction(lambda x, y: (x.value ** 2 + y.value ** 2) ** 0.5),
			'max': NativeFunction(lambda x, y: max(x.value, y.value)),
			'min': NativeFunction(lambda x, y: min(x.value, y.value)),
			'abs': NativeFunction(lambda x: abs(x.value)),
		}, {}, False))
	
	def visit_Unary (self, node):
		operator = node.operator
		if operator in unaryOperators:
			return unaryOperators[operator](self.visit(node.expr))
	
	def visit_Binary(self, node):
		operator = node.operator
		left = self.visit(node.left)
		if type(left) == HashSet:
			if operator in left.operators:
				self.visit(node.right)
				return left.operators[operator].call(node.mutable, Key(self.visit(node.right), True))
		if operator in binaryOperators:
			return binaryOperators[operator](left, self.visit(node.right))
		elif operator == '&&':
			return self.visit(node.left) and self.visit(node.right)
		elif operator == '||':
			return self.visit(node.left) or self.visit(node.right)
		elif operator == '?!':
			x = self.visit(node.left)
			if x != None:
				return x
			else:
				return self.visit(node.right)
	
	def visit_Ternary (self, node):
		if self.visit(node.cond):
			return self.visit(node.then_expr)
		else:
			return self.visit(node.else_expr)
	
	def visit_Identifier(self, node):
		if node.value in self.scope.declarations:
			raise Exception('Premature reference')
		#print(node.value)
		x = self.scope.getVar(node.value)
		if type(x) == Key:
			return x.value
		else:
			return self.visit(x)
	
	def visit_Return(self, node):
		ReturnCall.value = self.visit(node.value)
		raise ReturnCall()
		
	def visit_Break(self, node):
		raise BreakCall()
		
	def visit_Continue(self, node):
		raise ContinueCall()
	
	def visit_Assign(self, node):
		for key in node.keys:
			if key['key'].value in self.scope.vars:
				raise Exception('Identifier "%s" already defined in scope.' % key['key'].value)
			self.scope.defineVar(key['key'].value, Key(self.visit(key['value']), node.mutable))
		
	def visit_Mutate(self, node):
		left = self.visit(node.left)
		if type(left) == HashSet:
			if node.operator in left.operators:
				return left.operators[node.operator].call(True, Key(self.visit(node.right), True))
		curr = self.scope
		while not (node.left.value in curr.vars):
			if curr.parent != None:
				curr = curr.parent
			else: raise Exception('Variable "%s" was not declared' % node.left.value)
		if not curr.vars[node.left.value].mutable:
			raise Exception('"%s" is a constant' % node.left.value)
		curr.vars[node.left.value].value = assignmentOperators[node.operator](curr.vars[node.left.value].value, self.visit(node.right))
		return curr.vars[node.left.value].value
	
	def visit_Get(self, node):
		set = self.visit(node.expr)
		if node.optional and set in (None, False):
			return None
		index = self.visit(node.name)
		if type(index) == float:
			index = int(index)
		if type(index) == Range:
			if type(set) == Array:
				return Array(set.elements[index.min : index.max], True)
			return set[index.min : index.max]
		if type(set) == str:
			return set[index]
		if type(set.get(index)) == Key:
			return set.get(index).value
		else:
			return set.get(index)
	
	def visit_Set(self, node):
		set = self.visit(node.obj.expr)
		index = self.visit(node.obj.name)
		
		if type(index) == float:
			index = int(index)
		if type(set.get(index)) == Key:
			set.get(index).value = assignmentOperators[node.operator](set.get(index).value, self.visit(node.expr))
			return set.get(index).value
		else:
			set.put(index, assignmentOperators[node.operator](set.get(index), self.visit(node.expr)))
			return set.get(index)
	
	def visit_Call(self, node):
		func = self.visit(node.left)
		if node.optional and func in (None, False):
			return None
		args = list(map(lambda arg: Key(self.visit(arg), True), node.args))
		return func.call(*args)

	def visit_Defun(self, node):
		body = node.right
		if type(node.right) != Block:
			body = Block()
			body.content.append(Return(node.right))
		func = Function(node.args, body, self, self.scope)
		self.scope.vars[node.value] = Key(func)
	
	def visit_Fun(self, node):
		body = node.right
		if type(node.right) != Block:
			body = Block()
			body.content.append(Return(node.right))
		func = Function(node.args, body, self, self.scope)
		return func
		
	def visit_AsyncFun(self, node):
		body = node.right
		if type(node.right) != Block:
			body = Block()
			body.content.append(Return(node.right))
		func = AsyncFunction(node.args, body, self, self.scope)
		return func
			
	visit_IntNum   = lambda self, node: node.value
	visit_FloatNum = lambda self, node: node.value
	visit_String   = lambda self, node: node.value
	visit_Null_ptr = lambda self, node: None
	visit_Bool_val = lambda self, node: node.value
	
	def visit_Arr(self, node):
		return Array(list(map(lambda x: self.visit(x), node.value)), node.mutable)
	
	def execute_Block(self, block, scope):
		parent = self.scope
		try:
			self.scope = scope
			for every_bite in block.content:
				self.visit(every_bite)
		finally:
			self.scope = parent
			
	def visit_Block(self, block):
		self.execute_Block(block, Scope(self.scope, block.declarations))
	
	def visit_Switch(self, node):
		comp = self.visit(node.expr)
		def check_Case(comp, node):
			values = map(lambda x: self.visit(x), node.values)
			return comp in values
		for case in node.then_arm.content:
			if check_Case(comp, case):
				self.visit(case.then_arm)
				return
		if node.else_arm != None:
			self.visit(node.else_arm)
	
	def visit_If(self, node):
		if self.visit(node.cond):
			self.visit(node.then_arm)
		elif node.else_arm != None:
			self.visit(node.else_arm)
			
	def visit_While(self, node):
		if self.visit(node.cond):
			while self.visit(node.cond):
				try:
					self.visit(node.then_arm)
				except ContinueCall:
					continue
				except BreakCall:
					break
		elif node.else_arm != None:
			self.visit(node.else_arm)
	
	def visit_Do_While(self, node):
		try:
			self.visit(node.then_arm)
		except ContinueCall:
			self.visit_While(node)
		except BreakCall:
			return
		self.visit_While(node)
				
	def visit_For(self, node):
		if node.iter == None:
			while True:
				try:
					self.visit(node.then_arm)
				except ContinueCall:
					continue
				except BreakCall:
					break
			return
		if node.init == None:
			for i in range(int(self.visit(node.iter))):
				try:
					self.visit(node.then_arm)
				except ContinueCall:
					continue
				except BreakCall:
					break
			return
		
		init = self.visit(node.init)
		if type(init) == Range:
			self.scope.vars[node.iter.value] = Key(init.begin, True)
			if init.begin == init.max:
				def move():
					self.scope.vars[node.iter.value].value -= 1
			else:
				def move():
					self.scope.vars[node.iter.value].value += 1
			if init.begin != init.end:
				for i in range(int(init.max - init.min)):
					try:
						self.visit(node.then_arm)
						move()
					except ContinueCall:
						continue
					except BreakCall:
						break
				return
			else:
				return self.visit(node.else_arm)
		if type(init) == int:
			if init != 0:
				if init < 0:
					def move():
						self.scope.vars[node.iter.value].value -= 1
				else:
					def move():
						self.scope.vars[node.iter.value].value += 1
				self.scope.vars[node.iter.value] = Key(0, True)
				for i in range(abs(init)):
					try:
						self.visit(node.then_arm)
						move()
					except ContinueCall:
						continue
					except BreakCall:
						break
				return
			else:
				return self.visit(node.else_arm)
			
		if type(init) == float:
			if init != 0.0:
				if init < 0:
					def move():
						self.scope.vars[node.iter.value].value -= 1
				else:
					def move():
						self.scope.vars[node.iter.value].value += 1
				self.scope.vars[node.iter.value] = Key(0, True)
				for i in range(abs(int(init))):
					try:
						self.visit(node.then_arm)
						move()
					except ContinueCall:
						continue
					except BreakCall:
						break
				return
			else:
				return self.visit(node.else_arm)
			
		if type(init) == str:
			for i in init:
				try:
					self.scope.vars[node.iter.value] = Key(i, True)
					self.visit(node.then_arm)
				except ContinueCall:
					continue
				except BreakCall:
					break
			return
			
		if type(init) in (Array, HashSet):
			for i in range(init.length()):
				try:
					self.scope.vars[node.iter.value] = Key(i, True)
					self.visit(node.then_arm)
				except ContinueCall:
					continue
				except BreakCall:
					break
			return
	
	def visit_ForEach(self, node):
		arr = self.visit(node.arr)
		if type(arr) == Array:
			arr = arr.elements
		elif type(arr) == HashSet:
			arr = arr.keys
		if len(arr) > 0:
			self.scope = Scope(self.scope)
			for i in arr:
				self.scope.vars[node.iter.value] = Key(i, False)
				try:
					self.visit(node.then_arm)
				except ContinueCall:
					continue
				except BreakCall:
					break
		elif node.then_arm != False:
			self.visit(node.else_arm)
	
	def visit_Hash(self, block):
		parent = self.scope
		self.scope = Scope(parent)
		operators = {}
		for op in block.operators:
			operators[op] = self.visit(block.operators[op])
		hashed = HashSet({}, operators, block.mutable)
		def resolve_mutable(set):
			if type(set) == HashSet:
				set = HashSet({**set.keys}, {**set.operators}, block.mutable)
				for key in set.keys:
					if type(set.keys[key]) in (HashSet, Array):
							set.keys[key] = resolve_mutable(set.keys[key])
				return set
			else:
				result = Array(set.elements, block.mutable)
				for index in result.elements:
					if type(result.elements[index]) in (HashSet, Array):
							result.elements[index] = resolve_mutable(result.elements[index])
				return result
		try:
			set = {}
			set['type'] = 'Generic'
			for key in block.hash:
				if type(block.hash[key]) == Mixin:
					subset = self.visit(block.hash[key].expr)
					for sub in subset.keys:
						set[sub] = subset.get(sub)
						if type(set[sub]) == Function:
							closure = Scope(self.scope)
							closure.vars = {**set[sub].closure.vars}
							set[sub] = Function(set[sub].parems, set[sub].body, self, closure)
							closure.defineVar('this', Key(hashed, False))
						elif type(set[sub]) in (HashSet, Array):
							set[sub] = resolve_mutable(set[sub])
				else:
					set[key] = self.visit(block.hash[key])
			borrows = list(map(lambda x: self.visit(x), block.borrows))
			for i in borrows:
				for j in i.keys:
					set[j] = i.get(j)
			hashed.keys = set
		finally:
			self.scope.defineVar('this', Key(hashed, False))
			self.scope = self.scope.parent
		return hashed
	
	def visit_Empty(self, node): pass
	
	def evaluate(self):
		return self.visit(self.tree)

'''

*{===================================}*
 {                                   } 
 {          END INTERPRETER          } 
 {                                   } 
*{===================================}*
                  '''

program = []

def main(*argv):
	while True:
		command = input('CMD >> ')
		if command.upper() == 'QUIT':
			del program[:]
			break
		elif command.upper() == 'CLEAR':
			del program[:]
			os.system('clear')
		elif re.split('\s', command)[0].upper() == 'LOAD':
			fileName = re.split('\s', command)[1]
			try:
				file = openFile(fileName)
			except Exception:
				print(
					'Unable to open "%s", please make sure your file-name is valid.'
					% fileName)
			else:
				program.extend(re.split('\n', file))
				print('/LOADED LINES\n   (%s)' % fileName)
				for line in program:
					print(line)
				print(' LOADED LINES/')
		elif re.split('\s', command)[0].upper() == 'RUN':
			args = re.split('\s', command)
			_input = []
			if len(args) > 1:
				args = re.split('%', ' '.join(args[1:]))
				_input.extend(args)
			lexer = Lexer('\n'.join(program))
			parser = Parser(lexer.lex())
			interpreter = Evaluator(parser.parse())
			interpreter.evaluate()
		elif re.split('\s', command)[0].upper() == 'EXC':
			fileName = re.split('\s', command)[1]
			try:
				file = openFile(fileName)
			except Exception:
				print(
					'Unable to open "%s", please make sure your file-name is valid.'
					% fileName)
			else:
				del program[:]
				os.system('clear')
				lexer = Lexer(file)
				parser = Parser(lexer.lex())
				interpreter = Evaluator(parser.parse())
				interpreter.evaluate()
				del lexer
				del parser
				del interpreter
		else:
			program.append(command)
	return 0

if __name__ == '__main__':
	main()
