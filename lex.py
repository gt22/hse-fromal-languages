from ply import lex
from sys import argv
from os.path import exists


tokens = [
    "ID",
    "DERIVE",
    "COMMA",
    "SEMI",
    "DOT",
    "LBRACK",
    "RBRACK",
]


def t_ID(t):
    r"""[a-zA-Z_][a-zA-Z_0-9]*"""
    return t


t_DERIVE = ':-'
t_COMMA = ','
t_SEMI = ';'
t_DOT = r'\.'
t_LBRACK = r'\('
t_RBRACK = r'\)'

t_ignore = ' \t'


def t_newline(t):
    r"""\n+"""
    t.lexer.lineno += len(t.value)


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()