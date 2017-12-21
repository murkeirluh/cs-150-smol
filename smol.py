
# -----------------------------------------------------------------------------
# CS 150 MP 
#
# Smol 
# -----------------------------------------------------------------------------

# Reserved Words
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
}

# List of tokens
tokens = [
    # atoms
   'IDENTIFIER','INTEGER','FLOAT','CHARACTER','STRING','COMMA','COLON',
    # operations
   'AND','OR','NOT',
   'PLUS','MINUS','TIMES','DIVIDE','EQUALS','POWER','MOD',
   'LPAREN','RPAREN', 'LBRACKET', 'RBRACKET',
   'GT','LT','GTEQ','LTEQ', 
   'EQ','NEQ',
    ] + list(reserved.values())

###  Tokens  ###
# atoms
t_CHARACTER = r'(L)?\'([^\\\n]|(\\.))*?\''
t_STRING = r'\"([^\\\n]|(\\.))*?\"'
t_COMMA = r'\,'
t_COLON = r':'
# operations
t_AND = r'\&\&'
t_OR = r'\|\|'
t_NOT = r'\!'
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'\/'
t_EQUALS  = r'='
t_POWER = r'\^'
t_MOD = r'%'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_GT = r'>'
t_LT = r'<'
t_GTEQ = r'>='
t_LTEQ = r'<='
t_EQ = r'=='
t_NEQ = r'\!='

# atoms
def t_IDENTIFIER(t):
    r'[a-zA-Z][a-zA-Z0-9]*'
    t.type = reserved.get(t.value, 'IDENTIFIER') # check if identifier is in reserved words
    #if t.value in names:
    #  print(names[t.value])
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
'''
# for debugging purposes
data = "-1"
lexer.input(data)

while True:
  tok = lexer.token()
  if not tok:
    break
  print(tok)
'''

# Parsing rules
precedence = (
    ('left','PLUS','MINUS','TIMES','DIVIDE','ELSE','COLON'),
    ('right','POWER'),
    )

# Dictionary of names and data types of each name
names = { }
types = { }

global then_flag
then_flag = 1
global else_flag
else_flag = 1

# start
# <start> ::= <code-entity>*
def p_start(p):
   'start : code_entity'
   p[0] = p[1]
   
# code entity
# <code-entity> ::= <iterative-statement>
#                 | <conditional-statement>
#                 | <expression>
#                 | <input-function>
#                 | <output-function>
def p_code_entity(p):
   '''code_entity : iterative_statement
                  | conditional_statement
                  | expression
                  | input_function
                  | output_function'''
   p[0] = p[1]
   
# iterative statements
# <iterative-statement> ::= while <expression> : <start> endwhile
#                         | for <expression> , <expression> , <expression> : <start> endfor
def p_iterative_statement_1(p):
   'iterative_statement : WHILE expression COLON start ENDWHILE'
   while(p[2]):
     p[4]
    
def p_iterative_statement_2(p):
   'iterative_statement : FOR expression COMMA expression COMMA expression COLON start ENDFOR'
    
# conditional statements
# <conditional-statement> ::= if <expression> : <start> else <start> endif
#                           | if <expression> : <start> endif
def p_conditional_statement_1(p):
   'conditional_statement : IF expression seen_if COLON start seen_start ELSE seen_else start seen_start ENDIF'

def p_conditional_statement_2(p):
   'conditional_statement : IF expression seen_if COLON start seen_start ENDIF'

# check if expression is true
# if false, set flag to 0 (prevents symbol table from updating during then statement)
def p_seen_if(p):
   'seen_if : '
   global then_flag
   if p[-1] == False:
     then_flag = 0

# check if expression is true for if-then-else statements
# if true, set flag to 0 (prevents symbol table from updating during else statement)
def p_seen_else(p):
   'seen_else : '
   global then_flag
   if p[-5] == True:
     then_flag = 0

# after then and else statements, set flag to 1
def p_seen_start(p):
   'seen_start : '
   global then_flag
   then_flag = 1
      
# expression
# <expression> ::= <assignment-statement>
def p_expression(p):
    'expression : assignment_statement'
    p[0] = p[1]
    
# assignment statement
# <assignment-statement> ::= <or-statement>
#                          | IDENTIFIER = <or-statement>
def p_assignment_statement_1(p):
    'assignment_statement : or_statement'
    p[0] = p[1]
    
def p_assignment_statement_2(p):
    'assignment_statement : IDENTIFIER EQUALS or_statement'
    if then_flag == 1 and else_flag == 1:
      if type(p[3]).__name__ != 'NoneType':
        types[p[1]] = type(p[3]).__name__
        names[p[1]] = p[3]

# or statement
# <or-statement> ::= <and-statement>
#                  | <or-statement> || <and-statement>
def p_or_statement_1(p):
    'or_statement : and_statement'
    p[0] = p[1]
    
def p_or_statement_2(p):
    'or_statement : or_statement OR and_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] = p[1] or p[3]
    else:
      print("(Runtime) Error at line %d: Incompatible types '%s' and '%s'" % (p.lineno(2), type(p[1]).__name__, type(p[3]).__name__))

# and statement
# <and-statement> ::= <equality-statement>
#                   | <and-statement> && <equality-statement>
def p_and_statement_1(p):
    'and_statement : equality_statement'
    p[0] = p[1]
    
def p_and_statement_2(p):
    'and_statement : and_statement AND equality_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] = p[1] and p[3]
    else:
      print("(Runtime) Error at line %d: Incompatible types '%s' and '%s'" % (p.lineno(2), type(p[1]).__name__, type(p[3]).__name__))
    
# equality statement
# <equality-statement> ::= <relational-statement>
#                        | <equality-statement> == <relational-statement>
#                        | <equality-statement> != <relational-statement>
def p_equality_statement_1(p):
    'equality_statement : relational_statement'
    p[0] = p[1]
    
def p_equality_statement_2(p):
    '''equality_statement : equality_statement EQ relational_statement
                          | equality_statement NEQ relational_statement'''
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      if p[2] == '==':
        p[0] = (p[1] == p[3])
      elif p[2] == '!=':
        p[0] = not (p[1] == p[3])
    else:
      print("(Runtime) Error at line %d: Incompatible types '%s' and '%s'" % (p.lineno(2), type(p[1]).__name__, type(p[3]).__name__))
    
# relational statement
# <relational-statement> ::= <add-statement>
#                          | <relational-statement> < <add-statement>
#                          | <relational-statement> > <add-statement>
#                          | <relational-statement> <= <add-statement>
#                          | <relational-statement> >= <add-statement>
def p_relational_statement_1(p):
    'relational_statement : add_statement'
    p[0] = p[1]
    
def p_relational_statement_2(p):
    '''relational_statement : relational_statement LT add_statement
                            | relational_statement GT add_statement
                            | relational_statement LTEQ add_statement
                            | relational_statement GTEQ add_statement'''
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      if p[2] == '<':
        p[0] = (p[1] < p[3])
      elif p[2] == '>':
        p[0] = (p[1] > p[3])
      elif p[2] == '<=':
        p[0] = (p[1] <= p[3])
      elif p[2] == '>=':
        p[0] = (p[1] >= p[3])
    else:
      print("(Runtime) Error at line %d: Incompatible types '%s' and '%s'" % (p.lineno(2), type(p[1]).__name__, type(p[3]).__name__))

# add statement
# <add-statement> ::= <multiply-statement>
#                   | <add-statement> + <multiply-statement>
#                   | <add-statement> - <multiply-statement>
def p_add_statement_1(p):
    'add_statement : multiply_statement'
    p[0] = p[1]
    
def p_add_statement_2(p):
    '''add_statement : add_statement PLUS multiply_statement
                     | add_statement MINUS multiply_statement'''
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      if p[2] == '+':
        p[0] = p[1] + p[3]
      elif p[2] == '-':
        p[0] = p[1] - p[3]
    else:
      print("(Runtime) Error at line %d: Incompatible types '%s' and '%s'" % (p.lineno(2), type(p[1]).__name__, type(p[3]).__name__))

# multiply statement
# <multiply-statement> ::= <unary-statement>
#                        | <multiply-statement> * <unary-statement>
#                        | <multiply-statement> / <unary-statement>
#                        | <multiply-statement> % <unary-statement> 
def p_multiply_statement_1(p):
    'multiply_statement : unary_statement'
    p[0] = p[1]
    
def p_multiply_statement_2(p):
    '''multiply_statement : multiply_statement TIMES unary_statement
                          | multiply_statement DIVIDE unary_statement
                          | multiply_statement MOD unary_statement'''
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      if p[2] == '*':
        p[0] = p[1] * p[3]
      elif p[2] == '/':
        p[0] = p[1] / p[3]
      elif p[2] == '%':
        p[0] = p[1] % p[3]
    else:
      print("(Runtime) Error at line %d: Incompatible types '%s' and '%s'" % (p.lineno(2), type(p[1]).__name__, type(p[3]).__name__))
 
# unary statement
# <unary-statement> ::= <exponent>
#                     | - <unary-statement>
#                     | ! <unary-statement>   
def p_unary_statement_1(p):
    'unary_statement : exponent'
    p[0] = p[1]

def p_unary_statement_2(p):
    '''unary_statement : MINUS unary_statement
                       | NOT unary_statement'''
    if p[1] == '-':
      p[0] = -p[2]
    elif p[1] == '!':
      p[0] = not p[2]

# exponent
# <exponent> ::= <term>
#              | <term> ^ <unary-statement>   
def p_exponent_1(p):
    'exponent : term'
    p[0] = p[1]
    
def p_exponent_2(p):
    'exponent : term POWER unary_statement'
    p[0] = p[1] ** p[3]

# term
# <term> ::= IDENTIFIER
#          | <atom>
#          | ( <expression> )    
def p_term_1(p):
    'term : IDENTIFIER'
    if p[1] in types:
        p[0] = names[p[1]]
    else:
      print("(Runtime) Error at line %d: '%s' not defined" % (p.lineno(1), p[1]))
    
def p_term_2(p):
    'term : atom'
    p[0] = p[1]
    
def p_term_3(p):
    'term : LPAREN expression RPAREN'
    p[0] = p[2]

# atom
# <atom> ::= INTEGER
#          | FLOAT
#          | STRING
#          | TRUE
#          | FALSE
#          | [ (INTEGER | FLOAT)* ]
#          | IDENTIFIER [ INTEGER ]   
def p_atom1(p):
   'atom : INTEGER'
   p[0] = p[1]

def p_atom2(p):
   'atom : FLOAT'
   p[0] = p[1]

def p_atom3(p):
   'atom : CHARACTER'
   p[0] = p[1]

def p_atom4(p):
   'atom : STRING'
   p[0] = p[1]

def p_atom5(p):
   'atom : TRUE'
   p[0] = True

def p_atom6(p):
   'atom : FALSE'
   p[0] = False

global array
array = []

def p_atom7(p):
   'atom : LBRACKET elements RBRACKET'
   global array
   array = []
   p[0] = p[2]

def p_atom8(p):
   'atom : IDENTIFIER LBRACKET INTEGER RBRACKET'
   getarray = []
   if p[1] in names:
     getarray = names[p[1]]
     if p[3] < len(getarray):
       p[0] = getarray[p[3]]
     else:
       print("(Runtime) Error at line %d: Index is out of range" % p.lineno(3))
   else:
     print("(Runtime) Error at line %d: '%s' not declared" % (p.lineno(1), p[1]))

# elements in array
def p_elements1(p):
    'elements : elements INTEGER'
    array.append(p[2])
    p[0] = array

def p_elements2(p):
    'elements : elements FLOAT'
    array.append(p[2])
    p[0] = array

def p_elements3(p):
    'elements : '
    pass

# input function
# <input-function> ::= input( IDENTIFIER )    
def p_input_function(p):
    'input_function : INPUT LPAREN IDENTIFIER RPAREN' # input(variable)
    inp = input()
    if p[3] in types:
      inp = parser.parse(inp)
      if type(inp).__name__ == types[p[3]]:
        if p[3] in names:
          del names[p[3]]

        names[p[3]] = inp
      else:
        print("(Runtime) Error at line %d: '%s' is not of type '%s'" % (p.lineno(3), inp, types[p[3]]));
    else:
      print("(Runtime) Error at line %d: '%s' not declared" % (p.lineno(3), p[3]))    

# output function
# <output-function> ::= print( (“<atom>” | IDENTIFIER) {+ (“<atom>” | IDENTIFIER)+} )
def p_output_function(p):
    'output_function : PRINT LPAREN term RPAREN'
    print(p[3])

# error in syntax
def p_error(p):
    if p:
      print("(Syntax) Error at %s" % p.value)    

import ply.yacc as yacc
parser = yacc.yacc()

while True:
    try:
        s = input(':: ')   # Use raw_input on Python 2
    except EOFError:
        break
    if not s: continue
    result = parser.parse(s) #, tracking=True)
    if type(result).__name__ != 'NoneType':
      print(result)