from lex import tokens
from ply import yacc
from tree import *
from sys import argv
from os.path import exists

"""

S : DEF S | eps
DEF : ATOM :- BODY. | ATOM.
BODY : CON;BODY | CON
CON : TERM,CON | TERM
TERM : ATOM | (BODY)
ATOM : ID ABODY
ABODY : (PARATOM) ABODY | ID ABODY | eps
PARATOM : (PARATOM) | ATOM


"""


def p_start_def(p):
    """start : def start"""
    p[0] = [p[1], *p[2]]


def p_start_eps(p):
    """start : eps"""
    p[0] = []


def p_def(p):
    """def : atom DERIVE body DOT
           | atom DOT"""
    p[0] = Definition(p[1], None)
    if len(p) >= 4:
        p[0].body = p[3]


def p_body_dis(p):
    """body : con SEMI body"""
    p[0] = Disjunction(p[1], p[3])


def p_body_con(p):
    """body : con"""
    p[0] = p[1]


def p_con_con(p):
    """con : term COMMA con"""
    p[0] = Conjunction(p[1], p[3])


def p_con_term(p):
    """con : term"""
    p[0] = p[1]


def p_term_atom(p):
    """term : atom"""
    p[0] = p[1]


def p_term_body(p):
    """term : LBRACK body RBRACK"""
    p[0] = p[2]


def p_atom(p):
    """atom : ID abody"""
    p[0] = Atom(p[1], p[2])


def p_abody_par(p):
    """abody : LBRACK paratom RBRACK abody"""
    p[0] = [p[2], *p[4]]


def p_abody_id(p):
    """abody : ID abody"""
    p[0] = [Atom(p[1]), *p[2]]


def p_paratom_par(p):
    """paratom : LBRACK paratom RBRACK"""
    p[0] = p[2]


def p_paramtom_atom(p):
    """paratom : atom"""
    p[0] = p[1]


def p_abody_eps(p):
    """abody : eps"""
    p[0] = []


def p_eps(p):
    """eps : """
    pass


# Что-то не нашёл как сказать ply бросить все при ошибке...
error = None


def p_error(p):
    global error
    error = f"Syntax error: {p if p is not None else 'EOF'}"


parser = yacc.yacc()


def main():
    global error
    if len(argv) < 2:
        fname = input("Enter filename: ")
    else:
        fname = argv[1]
    if not exists(fname):
        print(f"File {fname} not found")
        return
    with open(fname) as f:
        res = parser.parse(f.read())
    with open(f"{fname}.out", "w") as f:
        if error is not None:
            f.write(error)
        else:
            f.write('\n\n'.join(map(str, res)))


if __name__ == '__main__':
    main()
