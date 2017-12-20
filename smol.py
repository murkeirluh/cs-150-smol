
# -----------------------------------------------------------------------------
# CS 150 MP 
#
# Smol 
# -----------------------------------------------------------------------------

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

tokens = [
    # atoms
   'IDENTIFIER','INTEGER','FLOAT','STRING','COMMA','COLON',
    # operations
   'AND','OR','NOT',
   'PLUS','MINUS','TIMES','DIVIDE','EQUALS','POWER','MOD',
   'LPAREN','RPAREN', 'LBRACKET', 'RBRACKET',
   'GT','LT','GTEQ','LTEQ', 
   'EQ','NEQ',
    ] + list(reserved.values())

###  Tokens  ###
# atoms
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

def t_IDENTIFIER(t):
    r'[a-zA-Z][a-zA-Z0-9]*'
    t.type = reserved.get(t.value, 'IDENTIFIER')
    #if t.value in names:
    #  print(names[t.value])
    return t

def t_FLOAT(t):
    r'\d+\.\d+'
    try:
        t.value = float(t.value)
    except ValueError:
        print("Error at line %d: Cannot convert %s to float" % (t.lexer.lineno, t.value))
        t.value = 0.0
    return t

def t_INTEGER(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Error: %d is not a valid integer" % t.value)
        t.value = 0
    return t

# Ignored characters
t_ignore = ' \t'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")
    
def t_error(t):
    print("Illegal character'%s' at line %d" % (t.value[0], t.lexer.lineno))
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
    ('left','PLUS','MINUS','TIMES','DIVIDE'),
    ('right','POWER'),
    )

# dictionary of names
names = { }
types = { }

# start
def p_start(p):
   'start : code_entity'
   p[0] = p[1]
   
# code entity
def p_code_entity_1(p):
   'code_entity : iterative_statement'
   p[0] = p[1]

def p_code_entity_2(p):
   'code_entity : conditional_statement'
   p[0] = p[1]
  
def p_code_entity_3(p):
   'code_entity : expression'
   p[0] = p[1]

def p_code_entity_4(p):
   'code_entity : input_function'
   p[0] = p[1]
    
def p_code_entity_5(p):
   'code_entity : output_function'
   p[0] = p[1]
    
# iterative statements
def p_iterative_statement_1(p):
   'iterative_statement : WHILE expression COLON start ENDWHILE'
    
def p_iterative_statement_2(p):
   'iterative_statement : FOR EQUALS expression COMMA expression COMMA expression COLON start ENDFOR'
    
# conditional statements
def p_conditional_statement_1(p):
   'conditional_statement : IF expression COLON start ELSE start ENDIF'

def p_conditional_statement_2(p):
   'conditional_statement : IF expression COLON start ENDIF'
  
# expression
def p_expression(p):
    'expression : assignment_statement'
    p[0] = p[1]
    
# assignment statement
def p_assignment_statement_1(p):
    'assignment_statement : or_statement'
    p[0] = p[1]
    
def p_assignment_statement_2(p):
    'assignment_statement : IDENTIFIER EQUALS or_statement'
    if type(p[3]).__name__ != 'NoneType':
      types[p[1]] = type(p[3]).__name__
      names[p[1]] = p[3]

# or statement
def p_or_statement_1(p):
    'or_statement : and_statement'
    p[0] = p[1]
    
def p_or_statement_2(p):
    'or_statement : or_statement OR and_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] = p[1] or p[3]
    else:
      print("Error: Incompatible types", type(p[1]).__name__, "and", type(p[3]).__name__)
    
def p_and_statement_1(p):
    'and_statement : equality_statement'
    p[0] = p[1]
    
def p_and_statement_2(p):
    'and_statement : and_statement AND equality_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] = p[1] and p[3]
    else:
      print("Error: Incompatible types", type(p[1]).__name__, "and", type(p[3]).__name__)
    
# equality statement
def p_equality_statement_1(p):
    'equality_statement : relational_statement'
    p[0] = p[1]
    
def p_equality_statement_2(p):
    'equality_statement : equality_statement EQ relational_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] = (p[1] == p[3])
    else:
      print("Error: Incompatible types", type(p[1]).__name__, "and", type(p[3]).__name__)
    
def p_equality_statement_3(p):
    'equality_statement : equality_statement NEQ relational_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] = not (p[1] == p[3])
    else:
      print("Error: Incompatible types", type(p[1]).__name__, "and", type(p[3]).__name__)
    
# relational statement
def p_relational_statement_1(p):
    'relational_statement : add_statement'
    p[0] = p[1]
    
def p_relational_statement_2(p):
    'relational_statement : relational_statement LT add_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] = (p[1] < p[3])
    else:
      print("Error: Incompatible types", type(p[1]).__name__, "and", type(p[3]).__name__)
    
def p_relational_statement_3(p):
    'relational_statement : relational_statement GT add_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] =  (p[1] > p[3])
    else:
      print("Error: Incompatible types", type(p[1]).__name__, "and", type(p[3]).__name__)
    
def p_relational_statement_4(p):
    'relational_statement : relational_statement LTEQ add_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] = (p[1] <= p[3])
    else:
      print("Error: Incompatible types", type(p[1]).__name__, "and", type(p[3]).__name__)
    
def p_relational_statement_5(p):
    'relational_statement : relational_statement GTEQ add_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] = (p[1] >= p[3])
    else:
      print("Error: Incompatible types", type(p[1]).__name__, "and", type(p[3]).__name__)
   
def p_add_statement_1(p):
    'add_statement : multiply_statement'
    p[0] = p[1]
    
def p_add_statement_2(p):
    'add_statement : add_statement PLUS multiply_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] = p[1] + p[3]
    else:
      print("Error: Incompatible types", type(p[1]).__name__, "and", type(p[3]).__name__)
    
def p_add_statement_3(p):
    'add_statement : add_statement MINUS multiply_statement'
    if type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType' and type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType':
      p[0] = p[1] - p[3]
    else:
      print("Error: Incompatible types", type(p[1]).__name__, "and", type(p[3]).__name__)
    
def p_multiply_statement_1(p):
    'multiply_statement : unary_statement'
    p[0] = p[1]
    
def p_multiply_statement_2(p):
    'multiply_statement : multiply_statement TIMES unary_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] = p[1] * p[3]
    else:
      print("Error: Incompatible types", type(p[1]).__name__, "and", type(p[3]).__name__)
    
def p_multiply_statement_3(p):
    'multiply_statement : multiply_statement DIVIDE unary_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] = p[1] / p[3]
    else:
      print("Error: Incompatible types", type(p[1]).__name__, "and", type(p[3]).__name__)
    
def p_multiply_statement_4(p):
    'multiply_statement : multiply_statement MOD unary_statement'
    if (type(p[1]).__name__ != 'list' and type(p[1]).__name__ != 'NoneType') and (type(p[3]).__name__ != 'list' and type(p[3]).__name__ != 'NoneType'):
      p[0] = p[1] % p[3]
    else:
      print("Error: Incompatible types", type(p[1]).__name__, "and", type(p[3]).__name__)
    
def p_unary_statement_1(p):
    'unary_statement : exponent'
    p[0] = p[1]

def p_unary_statement_2(p):
    'unary_statement : MINUS unary_statement'
    p[0] = -p[2]
    
def p_unary_statement_3(p):
    'unary_statement : NOT unary_statement'
    p[0] = not p[1]
   
def p_exponent_1(p):
    'exponent : term'
    p[0] = p[1]
    
def p_exponent_2(p):
    'exponent : term POWER unary_statement'
    p[0] = p[1] ** p[3]
    
def p_term_1(p):
    'term : IDENTIFIER'
    if p[1] in types:
        p[0] = names[p[1]]
    else:
      print("Error:", p[1], "not defined")
    
def p_term_2(p):
    'term : atom'
    p[0] = p[1]
    
def p_term_3(p):
    'term : LPAREN expression RPAREN'
    p[0] = p[2]
    
def p_atom1(p):
   'atom : INTEGER'
   p[0] = p[1]

def p_atom2(p):
   'atom : FLOAT'
   p[0] = p[1]

def p_atom3(p):
   'atom : STRING'
   p[0] = p[1]

def p_atom4(p):
   'atom : TRUE'
   p[0] = True

def p_atom5(p):
   'atom : FALSE'
   p[0] = False

global array
array = []

def p_atom6(p):
   'atom : LBRACKET elements RBRACKET'
   global array
   array = []
   p[0] = p[2]

def p_atom7(p):
   'atom : IDENTIFIER LBRACKET INTEGER RBRACKET'
   getarray = []
   getarray = names[p[1]]
   if p[3] < len(getarray):
     p[0] = getarray[p[3]]
   else:
     print("Error: Index is out of range")

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
        print("Error:", inp, "is not of type", types[p[3]]);
    else:
      print("Error:", p[3], "not declared")    

def p_output_function(p):
    'output_function : PRINT LPAREN term RPAREN'
    print(p[3])

def p_error(p):
    print("Syntax error!\n")    

import ply.yacc as yacc
parser = yacc.yacc()

while True:
    try:
        s = input(':: ')   # Use raw_input on Python 2
    except EOFError:
        break
    if not s: continue
    result = parser.parse(s) #, tracking=True)
    if result:
      print(result)