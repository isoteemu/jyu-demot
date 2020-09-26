from tokenize import tokenize, NUMBER, OP, ENCODING, NEWLINE, ENDMARKER, TokenInfo
from io import BytesIO

from operator import attrgetter
from collections import namedtuple

from typing import Union

import logging
logger = logging.getLogger(__name__)


def laske(laskutoimitus: str) -> float:
    # Tokenize string
    bytestream = BytesIO(laskutoimitus.encode("utf-8"))
    tokens = tokenize(bytestream.readline)

    # Remove uninteresting things.
    harsittu = [x for x in tokens if x.type not in [ENCODING, NEWLINE, ENDMARKER]]

    try:
        evaluated = evaluate_branch(list(harsittu))
        return float(evaluated.string)
    except ZeroDivisionError:
        # As per requirement:
        #    2/(5-5) → palautetaan +∞
        return float('inf')


def evaluate_branch(haara):
    """
        Evaluate branch until solution has been found.

    :param haara: list of tokenized expression.
    :type haara: list of TokenInfo

    :raises RuntimeError: If no solution can be found.
    :raises TypeError: Expression contains illegal characters.
    :raises ValueError: Expression can't be evaluated due to logic errors.
    :raises ZeroDivisionError: In case of zero division.

    :return: Evaluation result.
    :rtype: TokenInfo
    """

    n = len(haara)
    if n == 1:
        # Only one leaf. Return result.
        logger.debug("Lehti: %s", haara[0])
        return haara[0]
    elif n == 0:
        raise RuntimeError("Can't evaluate empty expression")

    for op in sorted(OPERATIONS, key=attrgetter("weight"))[::-1]:
        # Enumerate OPERATIONS in reverse order, and look for matching tokens.

        operand, weight, stem, callback = op

        # Branch can start with operation. Sanitize it.
        if haara[0].type == OP and haara[0].string in ['+', '-']:
            logger.info("Expression starts with operation.")
            haara.insert(0, TokenInfo(type=NUMBER, string='0', start=-1, end=0, line=haara[0].line))

        left = right = []
        for i, tok in enumerate(haara):
            # Enumerate all tokens. Only evaluates first found token. Rest
            # are evaluated recursively - as recommended by λ gods.
            tyyppi, arvo, *_ = tok

            if tyyppi == OP and arvo == operand:
                # First operand found. Split tokens into two, and perform
                # evaluation operation.

                logger.debug("Found operand %s", repr(operand))

                left = haara[:max(i, 0)]
                right = haara[i + 1:]
                try:
                    if stem == "infix":
                        r = infix(left, right, callback)
                    elif stem == "prefix":
                        r = prefix(left, right, callback)
                    elif stem == "postfix":
                        r = postfix(left, right, callback)
                except Exception as e:
                    logger.error("Error on evaluation token %s: %s", haara[i], e)
                    raise e

                return evaluate_branch(r)

    raise RuntimeError("Did not find parseable solution")


def infix(left, right, callback):
    a, b = left.pop(), right.pop(0)
    supistettu = TokenInfo(type=NUMBER,
                           string=callback(literal(a), literal(b)),
                           start=a.start, end=b.end, line=a.line)
    return left + [supistettu] + right


def prefix(left, right, callback):
    b = callback(right)
    return left + b


def postfix(left, right, callback):
    a = callback(left)
    return a + right


def parenthesis(haara):
    dbg = [repr(x.string) for x in haara]
    logger.debug("(%s)", dbg)
    return [evaluate_branch(haara)]


def literal(token: Union[float, TokenInfo]) -> float:
    """
    Convert - if necessary - TokenInfo into float.
    """
    if type(token) == float:
        return token
    elif token.type == NUMBER:
        return float(token.string)
    else:
        raise TypeError("Expected type to be either float or TokenInfo of type NUMBER")


def plus(left, right) -> float:
    logger.debug("+ %s %s", repr(left), repr(right))
    return left + right


def minus(left, right) -> float:
    logger.debug("%s - %s", repr(left), repr(right))
    return float(left - right)


def multiply(left, right) -> float:
    logger.debug("%s * %s", repr(left), repr(right))
    return float(left * right)


def div(left, right) -> float:
    logger.debug("%s / %s", repr(left), repr(right))
    return float(left / right)


# Structure for operation
operation = namedtuple("operation", ['token', 'weight', 'stem', 'callback'])

# List of supported operations
OPERATIONS = [
    operation('+', 1, 'infix', plus),
    operation('-', 2, 'infix', minus),

    operation('*', 10, 'infix', multiply),
    operation('/', 11, 'infix', div),

    operation('(', 1000, 'prefix', parenthesis),
    operation(')', 999, 'postfix', parenthesis),
]

if __name__ == '__main__':
    import unittest

    class Expressions(unittest.TestCase):
        def test_plussaa(self):
            """
                Test basic math operations.

                Assigment:
                    1+1 → palautetaan 2
            """
            maths = [
                ("1+1", 2),
                ("1+2+3", 6),
                ("1+2-3", 0),
                ("-3+2+1", 0)
            ]
            for s, v in maths:
                assert laske(s) == float(v), s

        def test_multiplication(self):
            """
                Test multiplication being prioritized before.

                Assigment:
                    2+3*4 → palautetaan 14
            """
            odotusarvo = float(14)
            assert laske("2+3*4") == odotusarvo, "2+3*4"

        def test_enclosure(self):
            """
                Test for correct parenthesis handling

                Assigment:
                    (2+3)*4 → palautetaan 20
            """
            odotusarvo = 20
            assert laske("(2+3)*4") == odotusarvo

        def test_syntaxerror(self):
            """
                Test illogical statement handling.

                Assigment:
                    )( → heitetään poikkeus
            """
            self.assertRaises(RuntimeError, laske, ")(")

        def test_type_error(self):
            """
                Test unaccepted values for arithmetic operations.

                Assigment:
                    x/2 → heitetään poikkeus
            """
            self.assertRaises(TypeError, laske, "x/2")

        def test_zerodivision(self):
            """
                Divide by zero. Assigment requires it to return infinite.

                Assigment:
                    2/(5-5) → palautetaan +∞
            """
            assert laske("2/(5-5)") == float("inf"), "Divizion by zero sould return infinite"

    class Ositus(unittest.TestCase):
        def test_ositus_plus(self):
            r = plus(257, 149)
            assert r == 406

        def test_ositus_minus(self):
            r = minus(257, 149)
            assert r == 108

        def test_ositus_multiply(self):
            r = multiply(257, 149)
            assert r == 38293

        def test_ositus_div(self):
            r = div(257, 149)
            assert r == 1.72483221476510067114

    # logging.basicConfig(level=logging.DEBUG)
    unittest.main()
