
# -----------------------------------------------------------------------------
# CS 150 MP 
#
# Smol 
# -----------------------------------------------------------------------------

reserved = {
  'if' :'IF',
  'else' :'ELSE',
  'endif' :'ENDIF',
  'while' :'WHILE',
  'for' :'FOR',
  'endfor' :'ENDFOR',
  'endwhile' :'ENDWHILE',
  'int' :'TYP_INT',
  'char' :'TYP_CHAR',
  'float' :'TYP_FLOAT',
  'str' :'TYP_STR',
  'input' : 'INPUT',
  'print' : 'PRINT',
}

tokens = [
    # atoms
   'IDENTIFIER','INTEGER','FLOAT','CHARACTER','STRING','COMMA','COLON',
    # iterations
   'WHILE','FOR','ENDFOR','ENDWHILE',
   'IF','ELSE','ENDIF', 
    # operations
   'AND','OR','NOT',
   'PLUS','MINUS','TIMES','DIVIDE','EQUALS','POWER','MOD',
   'LPAREN','RPAREN',
   'GT','LT','GTEQ','LTEQ', 
   'EQ','NEQ',
    ] + list(reserved.values())

###  Tokens  ###
# atoms
t_IDENTIFIER = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_CHARACTER = r'[ -~]'
t_STRING = r'(\'\w+\'|"\w+")'
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
t_GT = r'>'
t_LT = r'<'
t_GTEQ = r'>='
t_LTEQ = r'<='
t_EQ = r'=='
t_NEQ = r'\!='
# types
t_TYP_INT = r'int'
t_TYP_CHAR = r'char'
t_TYP_FLOAT = r'float'
t_TYP_STRING = r'str'

def t_INTEGER(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Error: %d is not a valid integer" % t.value)
        t.value = 0
    return t

def t_FLOAT(t):
    r'\d+\.\d+'
    try:
        t.value = float(t.value)
    except ValueError:
        print("Error at line %d: Cannot convert %s to float" % (t.lexer.lineno, t.value))
        t.value = 0.0
    return t


# Ignored characters
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")
    
def t_error(t):
    print("Illegal character'%s' at line %d" % (t.value[0], t.lexer.lineno))
    t.lexer.skip(1)
    
# Build the lexer
import ply.lex as lex
lexer = lex.lex()

# Parsing rules

precedence = (
    ('left','PLUS','MINUS','TIMES','DIVIDE'),
    ('right','UMINUS'),
    )

# dictionary of names
names = { }

# start
def p_start_1(t):
   'start : code_entity'
    pass

def p_start_2(t):
   'start : start code_entity'
    pass

# code entity
def p_code_entity_1(t):
   'code_entity : iterative_statement'
    pass

def p_code_entity_2(t):
   'code_entity : declarative_statement'
    pass 

def p_code_entity_3(t):
   'code_entity : expression'
    pass

def p_code_entity_4(t):
   'code_entity : input_function'
    pass

def p_code_entity_5(t):
   'code_entity : output_function'
    pass

# iterative statements
def p_iterative_statement_1(t):
   'iterative_statement : WHILE expression COLON code_entity ENDWHILE'
    pass

def p_iterative_statement_2(t):
   'iterative_statement : FOR EQUALS expression COMMA expression COMMA expression COLON code_entity ENDFOR'
    pass

def p_iterative_statement_3(t):
   'iterative_statement : WHILE expression COLON ENDWHILE'
    pass

def p_iterative_statement_4(t):
   'iterative_statement : FOR EQUALS expression COMMA expression COMMA expression COLON ENDFOR'
    pass

# conditional statements
def p_conditional_statement_1(t):
   'conditional_statement : IF expression COLON code_entity ENDIF'
    pass

def p_conditional_statement_2(t):
   'conditional_statement : IF expression COLON code_entity ELSE code_entity ENDIF'
    pass

def p_conditional_statement_3(t):
   'conditional_statement : IF expression COLON ENDIF'
    pass

# declarative statements
def p_declarative_statement(t):
   'declarative_statement : variable_type declarator'
    pass

# variable types
def p_variable_type(t):
   'variable_type : TYP_INT | TYP_CHAR | TYP_FLOAT | TYP_STR'
   pass

# declarator
def p_declarator_1(t):
    'declarator : IDENTIFIER'
    pass

def p_declarator_2(t):
    'declarator : IDENTIFIER EQUALS atom'
    pass

# expression
def p_expression(p):
    'expression : assignment_statement'
    pass

# assignment statement
def p_assignment_statement_1(p):
    'assignment_statement : or_statement'
    pass

def p_assignment_statement_2(p):
    'assignment_statement : IDENTIFIER EQUALS or_statement'
    pass

# or statement
def p_or_statement_1(p):
    'or_statement: and_statement'
    pass

def p_or_statement_2(p):
    'or_statement : or_statement OR and_statement'
    pass

def p_and_statement_1(p):
    'and_statement : equality_statement'
    pass

def p_and_statement_2(p):
    'and_statement : and_statement AND equality_statement'
    pass

# equality statement
def p_equality_statement_1(p):
    'equality_statement : relational_statement'
    pass

def p_equality_statement_2(p):
    'equality_statement : equality_statement EQ relational_statement'
    pass

def p_equality_statement_3(p):
    'equality_statement : equality_statement NEQ relational_statement'
    pass

# relational statement
def p_relational_statement_1(p):
    'relational_statement : add_statement'
    pass

def p_relational_statement_2(p):
    'relational_statement : relational_statement LT add_statement'
    pass

def p_relational_statement_3(p):
    'relational_statement : relational_statement GT add_statement'
    pass

def p_relational_statement_4(p):
    'relational_statement : relational_statement LTEQ add_statement'
    pass

def p_relational_statement_5(p):
    'relational_statement : relational_statement GTEQ add_statement'
    pass

def p_add_statement_1(p):
    'add_statement : multiply_statement'
    pass

def p_add_statement_2(p):
    'add_statement : add_statement PLUS multiply_statement'
    pass

def p_add_statement_3(p):
    'add_statement : add_statement MINUS multiply_statement'
    pass

def p_multiply_statement_1(p):
    'multiply_statement : unary_statement'
    pass

def p_multiply_statement_2(p):
    'multiply_statement : multiply_statement TIMES unary_statement'
    pass

def p_multiply_statement_3(p):
    'multiply_statement : multiply_statement DIVIDE unary_statement'
    pass

def p_multiply_statement_4(p):
    'multiply_statement : multiply_statement MOD unary_statement'
    pass

def p_unary_statement_1(p):
    'unary_statement : exponent'
    pass

def p_unary_statement_2(p):
    'unary_statement : MINUS unary_statement'
    pass

def p_unary_statement_3(p):
    'unary_statement : NOT unary_statement'
    pass

def p_exponent_1(p):
    'exponent : term'
    pass

def p_exponent_2(p):
    'exponent : term POWER unary_statement'
    pass

def p_term_1(p):
    'term : IDENTIFIER'
    pass

def p_term_2(p):
    'term : atom'
    pass

def p_term_3(p):
    'term : LPAREN expression RPAREN'
    pass

def p_atom(p):
   'atom : INTEGER | FLOAT | CHARACTER | STRING'
    pass

def p_input_function(p):
    'input_function : INPUT LPAREN IDENTIFIER RPAREN'
    pass

def p_output_function(p):
    'output_function : PRINT LPAREN ATOM RPAREN'
    ##### MODIFY THIS RULE
    pass

import ply.yacc as yacc
parser = yacc.yacc()

while True:
    try:
        s = input('>>> ')   # Use raw_input on Python 2
    except EOFError:
        break
    parser.parse(s, tracking=True)

