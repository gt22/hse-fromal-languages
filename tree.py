from __future__ import annotations
from typing import List, Optional


class Node:
    pass


class Atom(Node):

    name: str
    args: List[Atom]

    def __init__(self, name: str, args: List[Atom] = []):
        self.name = name
        self.args = args

    def __str__(self):
        return f"Atom({self.name}, [{', '.join(map(str, self.args))}])"

    def __eq__(self, other):
        return isinstance(other, type(self))\
               and self.name == other.name \
               and len(self.args) == len(other.args) \
               and all(x == y for (x, y) in zip(self.args, other.args))


class Disjunction(Node):

    a: Node
    b: Node

    def __init__(self, a: Node, b: Node):
        self.a = a
        self.b = b

    def __str__(self):
        return f"OR({self.a}, {self.b})"

    def __eq__(self, other):
        return isinstance(other, type(self)) and self.a == other.a and self.b == other.b


class Conjunction(Node):

    a: Node
    b: Node

    def __init__(self, a: Node, b: Node):
        self.a = a
        self.b = b

    def __str__(self):
        return f"AND({self.a}, {self.b})"

    def __eq__(self, other):
        return isinstance(other, type(self)) and self.a == other.a and self.b == other.b


class Definition(Node):

    head: Atom
    body: Optional[Node]

    def __init__(self, head: Atom, body: Optional[Node]):
        self.head = head
        self.body = body

    def __str__(self):
        return f"{self.head} :- {self.body}"

    def __eq__(self, other):
        return isinstance(other, type(self)) and self.head == other.head and self.body == other.body
