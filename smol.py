
# -----------------------------------------------------------------------------
# CS 150 Machine Problem
#
# Smol 
# by Mikayla Lopez and Alyssa Senatin
# -----------------------------------------------------------------------------

# ------- #
#  Lexer  #
# ------- #

# Reserved Words
# - words that are part of the language and
#   must not be used by the user for identifiers
reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'endif' : 'ENDIF',
    'while' : 'WHILE',
    'for' : 'FOR',
    'endfor' : 'ENDFOR',
    'endwhile' : 'ENDWHILE',
    'True' : 'TRUE',
    'False' : 'FALSE',
    'input' : 'INPUT',
    'print' : 'PRINT',
    'append' : 'APPEND',
}

# List of Tokens
# - names used by the lexer
tokens = [
    # atoms
   'IDENTIFIER','INTEGER','FLOAT','STRING','COMMA','COLON','PERIOD',
    # operators
   'AND','OR','NOT',
   'PLUS','MINUS','TIMES','DIVIDE','POWER','MOD',
   'GT','LT','GTEQ','LTEQ', 
   'EQ','NEQ',
   # assignment
   'EQUALS',
   # grouping
   'LPAREN','RPAREN', 'LBRACKET', 'RBRACKET',
    ] + list(reserved.values())

# Token Specifications
# atoms
t_STRING = r'\"([^\\\n]|(\\.))*?\"'
t_COMMA = r'\,'
t_COLON = r':'
t_PERIOD = r'\.'
# operators
t_AND = r'\&\&'
t_OR = r'\|\|'
t_NOT = r'\!'
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'\/'
t_POWER = r'\^'
t_MOD = r'%'
t_GT = r'>'
t_LT = r'<'
t_GTEQ = r'>='
t_LTEQ = r'<='
t_EQ = r'=='
t_NEQ = r'\!='
# assignment
t_EQUALS  = r'='
# grouping
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'

# atoms
def t_IDENTIFIER(t):
    r'[a-zA-Z][a-zA-Z0-9]*'
    t.type = reserved.get(t.value, 'IDENTIFIER') # check if identifier is in reserved words
    return t

def t_FLOAT(t):
    r'\d+\.\d+'
    try:
        t.value = float(t.value) # try to convert to float
    except ValueError:
        print("(Lexical) Error at line %d: Cannot convert %s to float" % (t.lexer.lineno, t.value))
        t.value = 0.0
    return t

def t_INTEGER(t):
    r'\d+'
    try:
        t.value = int(t.value) # try to convert to integer
    except ValueError:
        print("(Lexical) Error at line %d: %d is not a valid integer" % (t.lexer.lineno, t.value))
        t.value = 0
    return t

# Ignored characters
t_ignore = ' \t' # white spaces

# New line
def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")
    
# Error for illegal characters in atoms
def t_error(t):
    print("(Lexical) Error at line %d: Illegal character '%s'" % (t.lexer.lineno, t.value[0]))
    t.lexer.skip(1)
    
# Build the lexer
import ply.lex as lex
lexer = lex.lex()

# -------- #
#  Parser  #
# -------- #

# Precedence values
precedence = (
    ('left','PLUS','MINUS','TIMES','DIVIDE','ELSE','COLON'),
    ('right','POWER','NOT'),
    )

# Dictionary of names and data types of each name
names = { }
types = { }

# Grammar rules

''' start
    <start> ::= <code-entity>* '''

def p_start1(p):
   'start : '
   pass

def p_start2(p):
   'start : code_entity start'
   p[0] = (p.lineno(1), 'start', p[1], p[2])
   
''' code entity
    <code-entity> ::= <iterative-statement>
                    | <conditional-statement>
                    | <expression>
                    | <input-function>
                    | <output-function>
                    | <array-append> '''

def p_code_entity(p):
   '''code_entity : iterative_statement
                  | conditional_statement
                  | expression
                  | input_function
                  | output_function
                  | array_append'''
   p[0] = p[1]
   
''' iterative statements
    <iterative-statement> ::= while <expression> : <start> endwhile
                            | for <expression> , <expression> , <expression> : <start> endfor '''

def p_iterative_statement_1(p):
   'iterative_statement : WHILE expression COLON start ENDWHILE'
   p[0] = (p.lineno(1), 'while', p[2], p[4])
    
def p_iterative_statement_2(p):
   'iterative_statement : FOR expression COMMA expression COMMA expression COLON start ENDFOR'
   p[0] = (p.lineno(1), 'for', p[2], p[4], p[6], p[8])
    
''' conditional statements
    <conditional-statement> ::= if <expression> : <start> else <start> endif
                              | if <expression> : <start> endif '''

def p_conditional_statement_1(p):
   'conditional_statement : IF expression COLON start ELSE start ENDIF'
   p[0] = (p.lineno(1), 'if', p[2], p[4], p[6])

def p_conditional_statement_2(p):
   'conditional_statement : IF expression COLON start ENDIF'
   p[0] = (p.lineno(1), 'if', p[2], p[4])

''' expression
    <expression> ::= <assignment-statement> '''

def p_expression(p):
    'expression : assignment_statement'
    p[0] = p[1]
    
''' assignment statement
    <assignment-statement> ::= <or-statement>
                             | IDENTIFIER = <or-statement>
                             | IDENTIFIER [ ( INTEGER | IDENTIFIER ) ] = ( INTEGER | FLOAT )'''

def p_assignment_statement_1(p):
    'assignment_statement : or_statement'
    p[0] = p[1]
    
def p_assignment_statement_2(p):
    'assignment_statement : IDENTIFIER EQUALS or_statement'
    p[0] = (p.lineno(2), 'assign-var', p[1], p[3])

def p_assignment_statement_3(p):
    '''assignment_statement : IDENTIFIER LBRACKET INTEGER RBRACKET EQUALS INTEGER
                            | IDENTIFIER LBRACKET INTEGER RBRACKET EQUALS FLOAT
                            | IDENTIFIER LBRACKET IDENTIFIER RBRACKET EQUALS INTEGER
                            | IDENTIFIER LBRACKET IDENTIFIER RBRACKET EQUALS FLOAT'''
    p[0] = (p.lineno(2), 'assign-arr', p[1], p[3], p[6])

''' or statement
    <or-statement> ::= <and-statement>
                     | <or-statement> || <and-statement> '''

def p_or_statement_1(p):
    'or_statement : and_statement'
    p[0] = p[1]
    
def p_or_statement_2(p):
    'or_statement : or_statement OR and_statement'
    p[0] = (p.lineno(2), 'or', p[1], p[3])

''' and statement
    <and-statement> ::= <equality-statement>
                      | <and-statement> && <equality-statement> '''

def p_and_statement_1(p):
    'and_statement : equality_statement'
    p[0] = p[1]
    
def p_and_statement_2(p):
    'and_statement : and_statement AND equality_statement'
    
    p[0] = (p.lineno(2), 'and', p[1], p[3])
    
''' equality statement
    <equality-statement> ::= <relational-statement>
                           | <equality-statement> == <relational-statement>
                           | <equality-statement> != <relational-statement> '''

def p_equality_statement_1(p):
    'equality_statement : relational_statement'
    p[0] = p[1]
    
def p_equality_statement_2(p):
    '''equality_statement : equality_statement EQ relational_statement
                          | equality_statement NEQ relational_statement '''
    p[0] = (p.lineno(2), 'equality', p[2], p[1], p[3])
    
''' relational statement
    <relational-statement> ::= <add-statement>
                             | <relational-statement> < <add-statement>
                             | <relational-statement> > <add-statement>
                             | <relational-statement> <= <add-statement>
                             | <relational-statement> >= <add-statement> '''

def p_relational_statement_1(p):
    'relational_statement : add_statement'
    p[0] = p[1]
    
def p_relational_statement_2(p):
    '''relational_statement : relational_statement LT add_statement
                            | relational_statement GT add_statement
                            | relational_statement LTEQ add_statement
                            | relational_statement GTEQ add_statement'''
    p[0] = (p.lineno(2), 'relational', p[2], p[1], p[3])

''' add statement
    <add-statement> ::= <multiply-statement>
                      | <add-statement> + <multiply-statement>
                      | <add-statement> - <multiply-statement> '''

def p_add_statement_1(p):
    'add_statement : multiply_statement'
    p[0] = p[1]
    
def p_add_statement_2(p):
    '''add_statement : add_statement PLUS multiply_statement
                     | add_statement MINUS multiply_statement'''
    p[0] = (p.lineno(2), 'add', p[2], p[1], p[3])

''' multiply statement
    <multiply-statement> ::= <unary-statement>
                           | <multiply-statement> * <unary-statement>
                           | <multiply-statement> / <unary-statement>
                           | <multiply-statement> % <unary-statement> '''

def p_multiply_statement_1(p):
    'multiply_statement : unary_statement'
    p[0] = p[1]
    
def p_multiply_statement_2(p):
    '''multiply_statement : multiply_statement TIMES unary_statement
                          | multiply_statement DIVIDE unary_statement
                          | multiply_statement MOD unary_statement'''
    p[0] = (p.lineno(2), 'multiply', p[2], p[1], p[3])
 
''' unary statement
    <unary-statement> ::= <exponent>
                        | - <unary-statement>
                        | ! <unary-statement> '''

def p_unary_statement_1(p):
    'unary_statement : exponent'
    p[0] = p[1]

def p_unary_statement_2(p):
    '''unary_statement : MINUS unary_statement
                       | NOT unary_statement'''
    p[0] = (p.lineno(1), 'unary', p[1], p[2])

''' exponent
    <exponent> ::= <term>
                 | <term> ^ <unary-statement> '''

def p_exponent_1(p):
    'exponent : term'
    p[0] = p[1]
    
def p_exponent_2(p):
    'exponent : term POWER unary_statement'
    p[0] = (p.lineno(2), 'exponent', p[1], p[3])

''' term
    <term> ::= IDENTIFIER
             | <atom>
             | ( <expression> ) '''

def p_term_1(p):
    'term : IDENTIFIER'
    p[0] = (p.lineno(1), 'identifier', p[1])
    
def p_term_2(p):
    'term : atom'
    p[0] = p[1]
    
def p_term_3(p):
    'term : LPAREN expression RPAREN'
    p[0] = p[2]

''' atom
    <atom> ::= INTEGER
             | FLOAT
             | STRING
             | TRUE
             | FALSE
             | [ (INTEGER | FLOAT)* ]
             | IDENTIFIER [ ( INTEGER | IDENTIFIER ) ] ''' 

def p_atom1(p):
   '''atom : INTEGER
           | FLOAT
           | STRING
           | TRUE
           | FALSE'''
   p[0] = (p.lineno(1), 'atom', p[1])

def p_atom2(p):
   'atom : LBRACKET elements RBRACKET'
   p[0] = (p.lineno(1), 'atom-array', p[2])

def p_atom3(p):
   '''atom : IDENTIFIER LBRACKET INTEGER RBRACKET
           | IDENTIFIER LBRACKET IDENTIFIER RBRACKET'''
   p[0] = (p.lineno(1), 'atom-indexing', p[1], p[3])

# elements in array
def p_elements1(p):
    'elements : elements INTEGER'
    p[0] = (p[1], p[2])

def p_elements2(p):
    'elements : elements FLOAT'
    p[0] = (p[1], p[2])

def p_elements3(p):
    'elements : '
    pass

''' input function
    <input-function> ::= input( IDENTIFIER ) '''

def p_input_function(p):
    'input_function : INPUT LPAREN IDENTIFIER RPAREN'
    p[0] = (p.lineno(1), 'input', p[3])  

''' output function
    <output-function> ::= print( ("<atom>" | IDENTIFIER) ) '''

def p_output_function(p):
    'output_function : PRINT LPAREN term RPAREN'
    p[0] = (p.lineno(1), 'output', p[3])

def p_array_append(p):
    '''array_append : IDENTIFIER PERIOD APPEND LPAREN INTEGER RPAREN
                    | IDENTIFIER PERIOD APPEND LPAREN FLOAT RPAREN
                    | IDENTIFIER PERIOD APPEND LPAREN IDENTIFIER RPAREN'''
    p[0] = (p.lineno(1), 'append-arr', p[1], p[5])

# error in syntax
def p_error(p):
    if p:
      print("(Syntax) Error at line %d: Invalid symbol at '%s'" % (p.lineno, p.value))
    else:
      print("(Syntax) Error at EOF")

# ------------- #
#  Interpreter  #
# ------------- #

# called after parsing a code entity to interpret what it means
# and includes actions to take for each grammar rule stated above

def interpreter(result):

    # start
    # result = (p.lineno(1), 'start', p[1], p[2])

    def docommands(tuple):
      interpreter(tuple[2])
      if tuple[3] != None:
        docommands(tuple[3])

    if result[1] == 'start':
      docommands(result)

    # while statement
    # result = (p.lineno(1), 'while', p[2], p[4])

    if result[1] == 'while':
      if type(interpreter(result[2])).__name__ == 'bool' or type(interpreter(result[2])).__name__ == 'int' or type(interpreter(result[2])).__name__ == 'float':
        while (interpreter(result[2])):
          interpreter(result[3])
      else:
        print("(Syntax) Error at line %d: While expression should be of type 'bool', 'int', 'float'" % result[0])

    # for statement
    # result = (p.lineno(1), 'for', p[2], p[4], p[6], p[8])

    if result[1] == 'for':
      interpreter(result[2])
      if type(interpreter(result[3])).__name__ == 'bool' or type(interpreter(result[3])).__name__ == 'int' or type(interpreter(result[3])).__name__ == 'float':
        while (interpreter(result[3])):
          interpreter(result[5])
          interpreter(result[4])
      else: print("(Syntax) Error at line %d: Second for expression should be of type 'bool', 'int', 'float'" % result[0])

    # if-then-else statement
    # result = (p.lineno(1), 'if', p[2], p[4], p[6])
    # if-then statement
    # result = (p.lineno(1), 'if', p[2], p[4])

    if result[1] == 'if':
      value = interpreter(result[2])
      if type(value).__name__ == 'bool' or type(value).__name__ == 'int' or type(value).__name__ == 'float':
        if value == True:
          interpreter(result[3])
        else:
          if len(result) == 5:
            interpreter(result[4])
      else:
        print("(Syntax) Error at line %d: While expression should be of type 'bool', 'int', 'float'" % result[0])

    # assignment to variables
    # result = (p.lineno(2), 'assign-var', p[1], p[3])

    if result[1] == 'assign-var':
      var = result[2]
      value = interpreter(result[3])
      names[var] = value
      types[var] = type(value).__name__

    # replace an array element
    # result = (p.lineno(2), 'assign-arr', p[1], p[3], p[6])

    if result[1] == 'assign-arr': # fix errors here
      if result[3] in names:
        index = names[result[3]]
      else:
        index = result[3] # check if index is int
      if type(index).__name__ == 'int':
        var = result[2]
        if var in names:
          value = names[var]
          if index < len(value) and index >= 0:
            value[index] = result[4]
            names[var] = value
          else:
            print("(Runtime) Error at line %d: Index out of bounds" % result[0])
        else:
          print("(Syntax) Error at line %d: '%s' not declared" % (result[0], result[2]))
      else:
        print("(Syntax) Error at line %d: Index not of type 'int'" % result[0])

    # or statement
    # result = (p.lineno(2), 'or', p[1], p[3])

    if result[1] == 'or':
      value1 = interpreter(result[2])
      value2 = intepreter(result[3])
      if type(value1).__name__ != 'list' and type(value1).__name__ != 'NoneType' and type(value2).__name__ != 'list' and type(value2).__name__ != 'NoneType':
        return value1 or value2
      else:
        print("(Runtime) Error at line %d: Incompatible types '%s' and '%s'" % (result[0], type(value1).__name__, type(value2).__name__))

    # and statement
    # result = (p.lineno(2), 'and', p[1], p[3])

    if result[1] == 'and':
      value1 = interpreter(result[2])
      value2 = intepreter(result[3])
      if type(value1).__name__ != 'list' and type(value1).__name__ != 'NoneType' and type(value2).__name__ != 'list' and type(value2).__name__ != 'NoneType':
        return value1 and value2
      else:
        print("(Runtime) Error at line %d: Incompatible types '%s' and '%s'" % (result[0], type(value1).__name__, type(value2).__name__))

    # equality statement
    # result = (p.lineno(2), 'equality', p[2], p[1], p[3])

    if result[1] == 'equality':
      value1 = interpreter(result[3])
      value2 = interpreter(result[4])
      if type(value1).__name__ != 'list' and type(value1).__name__ != 'NoneType' and type(value2).__name__ != 'list' and type(value2).__name__ != 'NoneType':
        if result[2] == '==':
          return value1 == value2
        if result[2] == '!=':
          return value1 != value2
      else:
        print("(Runtime) Error at line %d: Incompatible types '%s' and '%s'" % (result[0], type(value1).__name__, type(value2).__name__))

    # relational statement
    # result = (p.lineno(2), 'relational', p[2], p[1], p[3])

    if result[1] == 'relational':
      value1 = interpreter(result[3])
      value2 = interpreter(result[4])
      if type(value1).__name__ != 'list' and type(value1).__name__ != 'NoneType' and type(value2).__name__ != 'list' and type(value2).__name__ != 'NoneType':
        if result[2] == '<':
          return value1 < value2
        if result[2] == '>':
          return value1 > value2
        if result[2] == '<=':
          return value1 <= value2
        if result[2] == '>=':
          return value1 >= value2
      else:
        print("(Runtime) Error at line %d: Incompatible types '%s' and '%s'" % (result[0], type(value1).__name__, type(value2).__name__))

    # add statement
    # result = (p.lineno(2), 'add', p[2], p[1], p[3])

    if result[1] == 'add':
      value1 = interpreter(result[3])
      value2 = interpreter(result[4])
      if (type(value1).__name__ == 'int' or type(value1).__name__ == 'float') and (type(value2).__name__ == 'int' or type(value2).__name__ == 'float'):
        if result[2] == '+':
          return value1 + value2
        if result[2] == '-':
          return value1 - value2
      else:
        print("(Runtime) Error at line %d: Incompatible types '%s' and '%s'" % (result[0], type(value1).__name__, type(value2).__name__))

    # multiply statement
    # result = (p.lineno(2), 'multiply', p[2], p[1], p[3])

    if result[1] == 'multiply':
      value1 = interpreter(result[3])
      value2 = interpreter(result[4])
      if (type(value1).__name__ == 'int' or type(value1).__name__ == 'float') and (type(value2).__name__ == 'int' or type(value2).__name__ == 'float'):
        if result[2] == '*':
          return value1 * value2
        if result[2] == '/':
          return value1 / value2
        if result[2] == '%':
          return value1 % value2
      else:
        print("(Runtime) Error at line %d: Incompatible types '%s' and '%s'" % (result[0], type(value1).__name__, type(value2).__name__))

    # unary statement
    # result = (p.lineno(1), 'unary', p[1], p[2])

    if result[1] == 'unary':
      value = interpreter(result[3])
      if result[2] == '-':
        if type(value).__name__ == 'int':
          return -value
        else:
          print("(Runtime) Error at line %d: Incompatible type '%s'" % (result[0], type(value).__name__))
      if result[2] == '!':
        if type(value).__name__ == 'bool' or type(value).__name__ == 'int' or type(value).__name__ == 'float':
          return not value
        else:
          print("(Runtime) Error at line %d: Incompatible type '%s'" % (result[0], type(value).__name__))

    # exponents
    # result = (p.lineno(2), 'exponent', p[1], p[3])

    if result[1] == 'exponent':
      value1 = interpreter(result[3])
      value2 = interpreter(result[4])
      if (type(value1).__name__ == 'int' or type(value1).__name__ == 'float') and (type(value2).__name__ == 'int' or type(value2).__name__ == 'float'):
        return value1 ** value2
      else:
        print("(Runtime) Error at line %d: Incompatible types '%s' and '%s'" % (result[0], type(value1).__name__, type(value2).__name__))

    # identifier
    # result = (p.lineno(1), 'identifier', p[1])

    if result[1] == 'identifier':
      if result[2] in names:
        return names[result[2]]
      else:
        print("(Syntax) Error at line %d: '%s' not declared" % (result[0], result[2]))

    # atom
    # result = (p.lineno(1), 'atom', p[1])

    if result[1] == 'atom':
      return result[2]

    # array elements
    # result = (p.lineno(1), 'atom-array', p[2])

    # adding elements to the array
    def addelements(tuple, line):
      if type(tuple[1]).__name__ == 'int' or type(tuple[1]).__name__ =='float':
        array.insert(0, tuple[1])
        if tuple[0] != None:
          addelements(tuple[0], line)
      else:
        print("(Runtime) Error at %d: Array values cannot be of type '%s'" %(line, type(tuple[1].__name__)))

    if result[1] == 'atom-array':
      array = []
      if result[2] == None:
        return None
      else:
        addelements(result[2], result[0])
        return array

    # getting element at array index
    # result = (p.lineno(1), 'atom-indexing', p[1], p[3])

    if result[1] == 'atom-indexing':
      if result[3] in names:
        index = names[result[3]]
      else:
        index = result[3]
      if type(index).__name__ == 'int':
        if result[2] in names:
          array = names[result[2]]
          if index < len(array) and index >= 0:
            return array[index]
          else:
            print("(Runtime) Error at line %d: Index out of bounds" % result[0])
        else:
          print("(Syntax) Error at line %d: '%s' not declared" % (result[0], result[2]))
      else:
        print("(Syntax) Error at line %d: Index not of type 'int'" % result[0])

    # input function
    # result = (p.lineno(1), 'input', p[3]) 

    if result[1] == 'input':
      if result[2] in types:
        inp = input()
        inp = interpreter(parser.parse(inp))
        if type(inp).__name__ == types[result[2]]:
          if result[2] in names:
            del names[result[2]]
          names[result[2]] = inp
        else:
          print("(Runtime) Error at line %d: '%s' is not of type '%s'" % (result[0], inp, types[result[2]]))
      else:
        print("(Syntax) Error at line %d: '%s' not declared" % (result[0], result[2]))

    # output function
    # result = (p.lineno(1), 'output', p[3])

    if result[1] == 'output':
      value = interpreter(result[2])
      if value:
        print(value)

    # operation to append elements at end of array
    # result = (p.lineno(1), 'append-arr', p[1], p[4])

    if result[1] == 'append-arr':
      if result[2] in names:
        if names[result[2]] != None:
          array = names[result[2]]
        else:
          array = []
        if result[3] in names:
          value = names[result[3]]
        else:
          value = result[3]
        if type(value).__name__ == 'int' or type(value).__name__ == 'float':
          array.append(value)
          names[result[2]] = array
        else:
          print("(Runtime) Error at line %d: Value not of type 'int' or 'float'" % result[0])
      else:
        print("(Syntax) Error at line %d: '%s' not declared" % (result[0], result[2]))

# Build the parser
import ply.yacc as yacc
parser = yacc.yacc()

# for file input
import sys as filein

# check if user specified a file to read
# else do command line interpreter
if len(filein.argv) == 2:
  file = open(filein.argv[1])
  stream = line = file.readline()
  while line != 'end':
    while line != '\n': # allow multiple lines for code entity
      line = file.readline()
      stream = stream + line
    stream = parser.parse(stream)
    interpreter(stream)
    stream = ''
    stream = line = file.readline()
else: # command line interpreter if no file is entered
  while True:
    try:
        # check for python version run by user
        # if python version is 3, use input()
        if filein.version_info[0] == 3:
            s = input(':: ')
        # else if python version is 2, use raw_input()
        elif filein.version_info[0] == 2:
            s = raw_input(':: ')
    except EOFError:
        break
    result = parser.parse(s)
    if result != None:
      interpreter(result)
      if result[2][1] == 'identifier': # print the value of an identifier
        if result[2][2] in names:
          if names[result[2][2]] != None:
            print(names[result[2][2]])
          else:
            print("[]")