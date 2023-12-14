"""
Just for fun, a snake has come to play.
"""
from itertools import count


# Problem 1
def dia(n):
    """
    Returns a sequence (read generator) of pairs
      (0, n), (1, n - 1), ..., (n - 1, 1), (n, 0)
    """
    return zip(range(n + 1), range(n, -1, -1))


# Problem 2
def flip(seq):
    """
    Given a sequence of pairs (a0, b0), (a1, b1), ..., (an, bn)
      it returns (b0, a0), (b1, a1), ..., (bn, an)
    """
    for a, b in seq:
        yield b, a


# Alternatively, this would do the same:
def flip2(seq):
    yield from ((b, a) for a, b in seq)


# Problem 3
def all_coordinates():
    """
    Generates an infinite sequence of coordinates.
    """
    def f(n):
        yield from dia(n) if n % 2 == 0 else flip(dia(n))
        yield from f(n + 1)

    return f(0)

# Alternative version, using a good ol' for-loop
def all_coordinates2():
    for n in count():
        yield from n % 2 == 0 and dia(n) or flip(dia(n))
