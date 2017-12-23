
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
# words that are part of the language and
# must not be used by the user for identifiers
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
# names used by the lexer
tokens = [
    # atoms
   'IDENTIFIER','INTEGER','FLOAT','STRING','COMMA','COLON','PERIOD',
    # operators
   'AND','OR','NOT',
   'PLUS','MINUS','TIMES','DIVIDE','FLRDIV','POWER','MOD',
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
t_FLRDIV  = r'\/\/'
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
    except LexicalError:
        print("(LexicalError) Line %d: Cannot convert %s to float" % (t.lexer.lineno, t.value))
        t.value = 0.0
    return t

def t_INTEGER(t):
    r'\d+'
    try:
        t.value = int(t.value) # try to convert to integer
    except LexicalError:
        print("(LexicalError) Line %d: %d is not a valid integer" % (t.lexer.lineno, t.value))
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
    print("(LexicalError) Line %d Position %d: Illegal character '%s'" % (t.lexer.lineno, t.lexer.lexpos, t.value[0]))
    t.lexer.skip(1)
    
# Build the lexer
import ply.lex as lex
lexer = lex.lex()

# -------- #
#  Parser  #
# -------- #

# Precedence values
precedence = (
    ('left','PLUS','MINUS','TIMES','DIVIDE','FLRDIV'),
    ('right','POWER','NOT','LBRACKET','UMINUS'),
    )

# Symbol tables
# dictionary of names and data types of each name
names = { }
types = { }

# Stack
# storage of return values of every interpreter call
stack = []

# Grammar rules
# each function (or group of functions with the same name)
# represents 1 grammar rule
# return value is a node of an abstract syntax tree

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
    <iterative-statement> ::= while <or-statement> : <start> endwhile
                            | for <assignment-statement> , <or-statement> , <assignment-statement> : <start> endfor '''

def p_iterative_statement_1(p):
   'iterative_statement : WHILE or_statement COLON start ENDWHILE'
   p[0] = (p.lineno(1), 'while', p[2], p[4])
    
def p_iterative_statement_2(p):
   'iterative_statement : FOR assignment_statement COMMA or_statement COMMA assignment_statement COLON start ENDFOR'
   p[0] = (p.lineno(1), 'for', p[2], p[4], p[6], p[8])
    
''' conditional statements
    <conditional-statement> ::= if <or-statement> : <start> else : <start> endif
                              | if <or-statement> : <start> endif '''

def p_conditional_statement_1(p):
   'conditional_statement : IF or_statement COLON start ELSE COLON start ENDIF'
   p[0] = (p.lineno(1), 'if', p[2], p[4], p[7])

def p_conditional_statement_2(p):
   'conditional_statement : IF or_statement COLON start ENDIF'
   p[0] = (p.lineno(1), 'if', p[2], p[4])

''' expression
    <expression> ::= <assignment-statement> '''

def p_expression(p):
    'expression : assignment_statement'
    p[0] = p[1]
    
''' assignment statement
    <assignment-statement> ::= <or-statement>
                             | IDENTIFIER = <or-statement>
                             | IDENTIFIER [ ( INTEGER | IDENTIFIER ) ] = <or-statement> '''

def p_assignment_statement_1(p):
    'assignment_statement : or_statement'
    p[0] = p[1]
    
def p_assignment_statement_2(p):
    'assignment_statement : IDENTIFIER EQUALS or_statement'
    p[0] = (p.lineno(2), 'assign-var', p[1], p[3])

def p_assignment_statement_3(p):
    '''assignment_statement : IDENTIFIER LBRACKET INTEGER RBRACKET EQUALS or_statement
                            | IDENTIFIER LBRACKET IDENTIFIER RBRACKET EQUALS or_statement'''
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
                           | <multiply-statement> // <unary-statement>
                           | <multiply-statement> % <unary-statement> '''

def p_multiply_statement_1(p):
    'multiply_statement : unary_statement'
    p[0] = p[1]
    
def p_multiply_statement_2(p):
    '''multiply_statement : multiply_statement TIMES unary_statement
                          | multiply_statement DIVIDE unary_statement
                          | multiply_statement FLRDIV unary_statement
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
    'unary_statement : MINUS unary_statement %prec UMINUS'
    p[0] = (p.lineno(1), 'unary', p[1], p[2])

def p_unary_statement3(p):
    'unary_statement : NOT unary_statement'
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
             | ( <or-statement> ) '''

def p_term_1(p):
    'term : IDENTIFIER'
    p[0] = (p.lineno(1), 'identifier', p[1])
    
def p_term_2(p):
    'term : atom'
    p[0] = p[1]
    
def p_term_3(p):
    'term : LPAREN or_statement RPAREN'
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
    'elements : elements unary_statement'
    p[0] = (p[1], p[2])

def p_elements2(p):
    'elements : '
    pass

''' input function
    <input-function> ::= input( IDENTIFIER ) '''

def p_input_function(p):
    'input_function : INPUT LPAREN IDENTIFIER RPAREN'
    p[0] = (p.lineno(1), 'input', p[3])  

''' output function
    <output-function> ::= print( <term> ) '''

def p_output_function(p):
    'output_function : PRINT LPAREN term RPAREN'
    p[0] = (p.lineno(1), 'output', p[3])

''' array append
    <array-append> ::= IDENTIFIER .append( (INTEGER | FLOAT | IDENTIFIER) ) '''

def p_array_append(p):
    '''array_append : IDENTIFIER PERIOD APPEND LPAREN INTEGER RPAREN
                    | IDENTIFIER PERIOD APPEND LPAREN FLOAT RPAREN
                    | IDENTIFIER PERIOD APPEND LPAREN IDENTIFIER RPAREN'''
    p[0] = (p.lineno(1), 'append-arr', p[1], p[5])

# error in syntax
def p_error(p):
    if p:
      print("(SyntaxError) Line %d Position %d: Invalid symbol '%s'" % (p.lineno, p.lexpos, p.value))
      stack.append("ERROR")
    else:
      print("(SyntaxError) At EOF")
      stack.append("ERROR")

# ------------- #
#  Interpreter  #
# ------------- #

# called after parsing a code entity to interpret what it means
# and includes actions to take for each grammar rule stated above
# each action is defined by a tag also passed by the parser
# traverses the abstract syntax tree in postorder

def interpreter(result):
  if result: # don't do anything if empty node is passed

    # start
    # result = (p.lineno(1), 'start', p[1], p[2])

    # interpreting multiple code entities by order
    def docommands(tuple):
      interpreter(tuple[2])
      if tuple[3] != None: # interpret code entities recursively until there are no code entities left
        docommands(tuple[3])

    if result[1] == 'start':
      docommands(result)

    # while statement
    # result = (p.lineno(1), 'while', p[2], p[4])

    if result[1] == 'while': 
      interpreter(result[2])
      value = stack.pop()
      if value == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      # check if while expression is of type 'bool', 'int', 'float'
      if type(value).__name__ == 'bool' or type(value).__name__ == 'int' or type(value).__name__ == 'float':
        while (value): # while loop until while expression is not true
          interpreter(result[3])
          interpreter(result[2])
          value = stack.pop()
          if value == "ERROR": # check if error was returned by one of the interpreter calls
            stack.append("ERROR")
            return
      else:
        print("(SyntaxError) Line %d: While expression should be of type 'bool', 'int', 'float'" % result[0])
        stack.append("ERROR")

    # for statement
    # result = (p.lineno(1), 'for', p[2], p[4], p[6], p[8])

    if result[1] == 'for':
      if result[2][1] == 'assign-var' or result[2][1] == 'assign-arr': # check if first expression assignment statement
        interpreter(result[2]) 
        interpreter(result[3])
        value = stack.pop()
        if value == "ERROR": # check if error was returned by one of the interpreter calls
          stack.append("ERROR")
          return
        # check if second expression is of type 'bool', 'int', 'float'
        if type(value).__name__ == 'bool' or type(value).__name__ == 'int' or type(value).__name__ == 'float':
          while (value): # while loop until second expression is not true
            interpreter(result[5])
            interpreter(result[4]) # update assigned variable
            interpreter(result[3])
            value = stack.pop()
            if value == "ERROR": # check if error was returned by one of the interpreter calls
              stack.append("ERROR")
              return
        else:
          print("(SyntaxError) Line %d: Second 'for' expression should be of type 'bool', 'int', 'float'" % result[0])
          stack.append("ERROR")
      else:
        print("(SyntaxError) Line %d: First 'for' expression should be an assignment expression" % result[0])
        stack.append("ERROR")

    # if-then-else statement
    # result = (p.lineno(1), 'if', p[2], p[4], p[6])
    # if-then statement
    # result = (p.lineno(1), 'if', p[2], p[4])

    if result[1] == 'if':
      interpreter(result[2])
      value = stack.pop()
      if value == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      # check if if expression is of type 'bool', 'int', 'float'
      if type(value).__name__ == 'bool' or type(value).__name__ == 'int' or type(value).__name__ == 'float':
        if value == True: # execute if if expression is true
          interpreter(result[3])
        else:
          if len(result) == 5: # do nothing if there is no else statement, otherwise execute else statement
            interpreter(result[4])
      else:
        print("(SyntaxError) Line %d: If expression should be of type 'bool', 'int', 'float'" % result[0])
        stack.append("ERROR")

    # assignment to variables
    # result = (p.lineno(2), 'assign-var', p[1], p[3])

    if result[1] == 'assign-var':
      var = result[2]
      interpreter(result[3])
      value = stack.pop()
      if value == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      names[var] = value
      types[var] = type(value).__name__ # update symbol tables

    # replace an array element
    # result = (p.lineno(2), 'assign-arr', p[1], p[3], p[6])

    if result[1] == 'assign-arr':
      if result[3] in names: # check if index is variable or int
        index = names[result[3]]
      else:
        index = result[3]
      if type(index).__name__ == 'int': # check if index is type int
        var = result[2]
        if var in names: # check if array is declared
          value = names[var]
          if index < len(value) and index >= 0: # check if 0 < index < arraylength
            interpreter(result[4])
            toassign = stack.pop()
            if toassign == "ERROR": # check if error was returned by one of the interpreter calls
              stack.append("ERROR")
              return
            if type(toassign).__name__ == 'int' or type(toassign).__name__ == 'float': # check if new value is type int or float
              value[index] = toassign
              names[var] = value
            else:
              print("(TypeError) Line %d: Value not of type 'int' or 'float'" % result[0])
              stack.append("ERROR")
          else:
            print("(IndexError) Line %d: Index out of bounds" % result[0])
            stack.append("ERROR")
        else:
          print("(IdentifierError) Line %d: '%s' not declared" % (result[0], result[2]))
          stack.append("ERROR")
      else:
        print("(SyntaxError) Line %d: Index not of type 'int'" % result[0])
        stack.append("ERROR")

    # or statement
    # result = (p.lineno(2), 'or', p[1], p[3])

    if result[1] == 'or':
      interpreter(result[2])
      intepreter(result[3])
      value2 = stack.pop()
      if value2 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      value1 = stack.pop()
      if value1 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      # check if operands are not invalid types (list and none)
      if type(value1).__name__ != 'list' and type(value1).__name__ != 'NoneType' and type(value2).__name__ != 'list' and type(value2).__name__ != 'NoneType':
        stack.append(value1 or value2)
      else:
        print("(TypeError) Line %d: Incompatible types '%s' and '%s' for '||' operation" % (result[0], type(value1).__name__, type(value2).__name__))
        stack.append("ERROR")

    # and statement
    # result = (p.lineno(2), 'and', p[1], p[3])

    if result[1] == 'and':
      interpreter(result[2])
      interpreter(result[3])
      value2 = stack.pop()
      if value2 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      value1 = stack.pop()
      if value1 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      # check if operands are not invalid types (list and none)
      if type(value1).__name__ != 'list' and type(value1).__name__ != 'NoneType' and type(value2).__name__ != 'list' and type(value2).__name__ != 'NoneType':
        stack.append(value1 and value2)
      else:
        print("(TypeError) Line %d: Incompatible types '%s' and '%s' for '&&' operation" % (result[0], type(value1).__name__, type(value2).__name__))
        stack.append("ERROR")

    # equality statement
    # result = (p.lineno(2), 'equality', p[2], p[1], p[3])

    if result[1] == 'equality':
      interpreter(result[3])
      interpreter(result[4]) 
      value2 = stack.pop()
      if value2 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      value1 = stack.pop()
      if value1 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      # check if operands are not invalid types (list and none)
      if type(value1).__name__ != 'list' and type(value1).__name__ != 'NoneType' and type(value2).__name__ != 'list' and type(value2).__name__ != 'NoneType':
        if result[2] == '==': # check which operation to execute
          stack.append(value1 == value2)
        if result[2] == '!=':
          stack.append(value1 != value2)
      else:
        print("(TypeError) Line %d: Incompatible types '%s' and '%s' for '%s' operation" % (result[0], type(value1).__name__, type(value2).__name__, result[2]))
        stack.append("ERROR")

    # relational statement
    # result = (p.lineno(2), 'relational', p[2], p[1], p[3])

    if result[1] == 'relational':
      interpreter(result[3])
      interpreter(result[4])
      value2 = stack.pop()
      if value2 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      value1 = stack.pop()
      if value1 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      # check if operands are not invalid types (list and none)
      if type(value1).__name__ != 'list' and type(value1).__name__ != 'NoneType' and type(value2).__name__ != 'list' and type(value2).__name__ != 'NoneType':
        if result[2] == '<': # check which operation to execute
          stack.append(value1 < value2)
        if result[2] == '>':
          stack.append(value1 > value2)
        if result[2] == '<=':
          stack.append(value1 <= value2)
        if result[2] == '>=':
          stack.append(value1 >= value2)
      else:
        print("(TypeError) Line %d: Incompatible types '%s' and '%s' for '%s' operation" % (result[0], type(value1).__name__, type(value2).__name__, result[2]))
        stack.append("ERROR")

    # add statement
    # result = (p.lineno(2), 'add', p[2], p[1], p[3])

    if result[1] == 'add':
      interpreter(result[3])
      interpreter(result[4])
      value2 = stack.pop()
      if value2 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      value1 = stack.pop()
      if value1 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      # check if operands are not invalid types (list and none)
      if (type(value1).__name__ == 'int' or type(value1).__name__ == 'float') and (type(value2).__name__ == 'int' or type(value2).__name__ == 'float'):
        if result[2] == '+': # check which operation to execute
          stack.append(value1 + value2)
        if result[2] == '-':
          stack.append(value1 - value2)
      else:
        print("(TypeError) Line %d: Incompatible types '%s' and '%s' for '%s' operation" % (result[0], type(value1).__name__, type(value2).__name__, result[2]))
        stack.append("ERROR")

    # multiply statement
    # result = (p.lineno(2), 'multiply', p[2], p[1], p[3])

    if result[1] == 'multiply':
      interpreter(result[3])
      interpreter(result[4])
      value2 = stack.pop()
      if value2 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      value1 = stack.pop()
      if value1 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      # check if operands are valid types (int and float)
      if (type(value1).__name__ == 'int' or type(value1).__name__ == 'float') and (type(value2).__name__ == 'int' or type(value2).__name__ == 'float'):
        if result[2] == '*': # check which operation to execute
          stack.append(value1 * value2)
        if result[2] == '/':
          stack.append(value1 / value2)
        if result[2] == '//':
            stack.append(value1 // value2)
        if result[2] == '%':
          stack.append(value1 % value2)
      else:
        print("TypeError) Line %d: Incompatible types '%s' and '%s' for '%s' operation" % (result[0], type(value1).__name__, type(value2).__name__, result[2]))
        stack.append("ERROR")

    # unary statement
    # result = (p.lineno(1), 'unary', p[1], p[2])

    if result[1] == 'unary':
      interpreter(result[3])
      value = stack.pop()
      if value == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      if result[2] == '-': # check which operation to execute
        if type(value).__name__ == 'int' or type(value).__name__ == 'float': # check if operand is of valid type (int and float)
          stack.append(-value)
        else:
          print("TypeError) Line %d: Incompatible type '%s' for '%s' operation" % (result[0], type(value).__name__, result[2]))
          stack.append("ERROR")
      if result[2] == '!':
        # check if operand is of valid type (bool, int, and float)
        if type(value).__name__ == 'bool' or type(value).__name__ == 'int' or type(value).__name__ == 'float':
          stack.append(not value)
        else:
          print("TypeError) Line at line %d: Incompatible type '%s' for '%s' operation" % (result[0], type(value).__name__, result[2]))
          stack.append("ERROR")

    # exponents
    # result = (p.lineno(2), 'exponent', p[1], p[3])

    if result[1] == 'exponent':
      interpreter(result[2])
      interpreter(result[3])
      value2 = stack.pop()
      if value2 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      value1 = stack.pop()
      if value1 == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      # check if operands are valid types (int and float)
      if (type(value1).__name__ == 'int' or type(value1).__name__ == 'float') and (type(value2).__name__ == 'int' or type(value2).__name__ == 'float'):
        stack.append(value1 ** value2)
      else:
        print("(TypeError) Line %d: Incompatible types '%s' and '%s' for '^' operation" % (result[0], type(value1).__name__, type(value2).__name__))
        stack.append("ERROR")

    # identifier
    # result = (p.lineno(1), 'identifier', p[1])

    if result[1] == 'identifier':
      if result[2] in names: # check if variable is declared
        stack.append(names[result[2]])
      else:
        print("(IdentifierError) Line %d: '%s' not declared" % (result[0], result[2]))
        stack.append("ERROR")

    # atom
    # result = (p.lineno(1), 'atom', p[1])

    # atoms are stored in reserved variable to bypass NoneType return values from calling 'start'
    if result[1] == 'atom':
      stack.append(result[2])

    # array elements
    # result = (p.lineno(1), 'atom-array', p[2])

    # adding elements to the array
    def addelements(tuple, line):
      interpreter(tuple[1])
      value = stack.pop()
      if value == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      if type(value).__name__ == 'int' or type(value).__name__ =='float': # check if element is of valid type (int and float)
        array.insert(0, value)
        if tuple[0] != None: # add array elements recursively until there are no elements left
          addelements(tuple[0], line)
      else:
        print("(TypeError) Line %d: Array values cannot be of type '%s'" %(line, type(tuple[1].__name__)))
        stack.append("ERROR")

    # lists are stored in reserved variable to bypass NoneType return values from calling 'start'
    if result[1] == 'atom-array':
      array = [] # add elements here, then push to stack
      if result[2] == None: # check if empty list is passed
        stack.append(array)
      else:
        addelements(result[2], result[0])
        stack.append(array)

    # getting element at array index
    # result = (p.lineno(1), 'atom-indexing', p[1], p[3])

    if result[1] == 'atom-indexing':
      if result[3] in names: # check if index is variable or int
        index = names[result[3]]
      else:
        index = result[3]
      if type(index).__name__ == 'int': # check if index is of type int
        if result[2] in names: # check if variable is declared
          array = names[result[2]]
          if index < len(array) and index >= 0: # check if 0 < index < arraylength
            stack.append(array[index])
          else:
            print("(IndexError) Line %d: Index out of bounds" % result[0])
            stack.append("ERROR")
        else:
          print("(IdentifierError) Line %d: '%s' not declared" % (result[0], result[2]))
          stack.append("ERROR")
      else:
        print("(SyntaxError) Line %d: Index not of type 'int'" % result[0])
        stack.append("ERROR")

    # input function
    # result = (p.lineno(1), 'input', p[3]) 

    if result[1] == 'input':
      if result[2] in types: # check if variable is declared
        inp = input()
        interpreter(parser.parse(inp)) # parse the user input
        value = stack.pop()
        if value == "ERROR": # check if error was returned by one of the interpreter calls
          stack.append("ERROR")
          return
        if type(value).__name__ == types[result[2]]: # check if input has same type as variable
          if result[2] in names: # replace value if already assigned
            del names[result[2]]
          names[result[2]] = value
        else:
          print("(TypeError) Line %d: '%s' is not of type '%s'" % (result[0], value, types[result[2]]))
          stack.append("ERROR")
      else:
        print("(IdentifierError) Line %d: '%s' not declared" % (result[0], result[2]))
        stack.append("ERROR")

    # output function
    # result = (p.lineno(1), 'output', p[3])

    if result[1] == 'output':
      interpreter(result[2])
      value = stack.pop()
      if value == "ERROR": # check if error was returned by one of the interpreter calls
        stack.append("ERROR")
        return
      value = str(value) # convert output value to string
      if value: # check if string is not empty
        print(value)

    # operation to append elements at end of array
    # result = (p.lineno(1), 'append-arr', p[1], p[4])

    if result[1] == 'append-arr':
      if result[2] in names: # check if array is declared
        if names[result[2]] != None: # check if array has elements
          array = names[result[2]]
        else:
          array = []
        if result[3] in names: # check if element to append is identifier
          value = names[result[3]]
        else:
          value = result[3]
        if type(value).__name__ == 'int' or type(value).__name__ == 'float': # check if element is of valid type (int and float)
          array.append(value) # append element to array
          names[result[2]] = array
        else:
          print("(TypeError) Line %d: Value not of type 'int' or 'float'" % result[0])
          stack.append("ERROR")
      else:
        print("(IdentifierError) Line %d: '%s' not declared" % (result[0], result[2]))
        stack.append("ERROR")

# Build the parser
import ply.yacc as yacc
parser = yacc.yacc()

# for file input
import sys as filein

# check if user specified a file to read
# else do command line interpreter
if len(filein.argv) == 2:
  file = open(filein.argv[1]) # get file from terminal
  stream = line = file.readline()
  while line != 'end':
    while line != '\n': # allow multiple lines for code entity
      line = file.readline()
      stream = stream + line
    stream = parser.parse(stream) # lex
    interpreter(stream) # interpreter
    stream = ''
    stream = line = file.readline() # reset stream
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
    if result != None: # interpret if parser returned an AST
      interpreter(result)
      if stack: # empty the stack
        if stack[0] != "ERROR":
          print(stack.pop())
        else:
          stack.pop()