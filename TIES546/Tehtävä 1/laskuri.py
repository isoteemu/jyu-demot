from tokenize import tokenize, NUMBER, OP, ENCODING, NEWLINE, ENDMARKER, TokenInfo
from io import BytesIO

from operator import attrgetter
from collections import namedtuple

from typing import Union

import logging
logger = logging.getLogger(__name__)

"""
    Tehtävä 1
    ~~~~~~~~~

    Testauksen kohteena on aliohjelma (staattinen metodi, funktio), jolla on yksi parametri
    (tyypiltään merkkijono) ja joka palauttaa liukuluvun. Se voi myös heittää poikkeuksen.
    Aliohjelmalta edellytetään seuraavanlaista toiminnallisuutta:

    Aliohjelma laskee parametrinaan saamansa laskutehtävän tuloksen.

    Aliohjelman tulee tukea lukuvakioita sekä yhteen-, vähennys-, kerto- ja jakolaskuja.
    Lisäksi sen tulee tukea sulkeiden käyttöä laskutehtävän ryhmittelyyn. Laskutoimitusten
    laskujärjestyksessä sen tulee noudattaa tavanomaisia laskusääntöjä (esimerkiksi
    kertolaskut lasketaan ennen yhteenlaskuja).

    Mikäli aliohjelman parametri ei ole aliohjelman tukema laskutehtävä, aliohjelman tulee
    heittää poikkeus.

    Nollalla jako ei saa heittää poikkeusta, vaan sen tulee tuottaa erityisliukuluku
    (positiivinen tai negatiivinen äärettömyys tai NaN).

    Esimerkkejä aliohjelman syötteestä ja edellytetystä vasteesta:

        1+1 → palautetaan 2
        2+3*4 → palautetaan 14
        (2+3)*4 → palautetaan 20
        )( → heitetään poikkeus
        x/2 → heitetään poikkeus
        2/(5-5) → palautetaan +∞

    Laadi aliohjelman syötealueelle toiminnallinen ositus. Anna jokaisesta osituksen
    ekvivalenssiluokasta esimerkkisyöte ja sille ennakoimasi vaste.

    Perustele vastauksesi huolellisesti. Erityisesti kerro, miten olet huomioinut
    erillisyyden ja kattavuuden (engl. disjointness ja completeness) kriteerit. Huomaa,
    että perustelu on olennainen osa tehtävän vastausta.

    Pohdi lisäksi vastauksessasi, mikä tehtävässä oli hankalaa ja mitä siitä opit.
"""


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
        # As per assigment:
        #    2/(5-5) → palautetaan +∞
        return float('inf')


def evaluate_branch(branch):
    """
        Evaluate branch until solution has been found.

    :param branch: list of tokenized expression.
    :type branch: list of TokenInfo

    :raises RuntimeError: If no solution can be found.
    :raises TypeError: Expression contains illegal characters.
    :raises ValueError: Expression can't be evaluated due to logic errors.
    :raises ZeroDivisionError: In case of zero division.

    :return: Evaluation result.
    :rtype: TokenInfo
    """

    n = len(branch)
    if n == 1:
        # Only one leaf. Return result.
        logger.debug("Lehti: %s", branch[0])
        return branch[0]
    elif n == 0:
        raise RuntimeError("Can't evaluate empty expression")

    for op in reversed(sorted(OPERATIONS, key=attrgetter("weight"))):
        # Enumerate OPERATIONS in reverse order, and look for matching tokens.

        operand, weight, stem, callback = op

        # Branch can start with operation. Sanitize it.
        if branch[0].type == OP and branch[0].string in ['+', '-']:
            logger.info("Expression starts with operation.")
            branch.insert(0, TokenInfo(type=NUMBER, string='0', start=-1, end=0, line=branch[0].line))

        left = right = []
        for i, tok in enumerate(branch):
            # Enumerate all tokens. Only evaluates first found token. Rest
            # are evaluated recursively - as recommended by λ gods.
            tyyppi, arvo, *_ = tok

            if tyyppi == OP and arvo == operand:
                # First operand found. Split tokens into two, and perform
                # evaluation operation.

                logger.debug("Found operand %s", repr(operand))

                left = branch[:max(i, 0)]
                right = branch[i + 1:]
                try:
                    if stem == "infix":
                        r = infix(left, right, callback)
                    elif stem == "prefix":
                        r = prefix(left, right, callback)
                    elif stem == "postfix":
                        r = postfix(left, right, callback)
                except Exception as e:
                    logger.error("Error on evaluation token %s: %s", branch[i], e)
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
    logger.debug("EVAL: %s + %s", repr(left), repr(right))
    return left + right


def minus(left, right) -> float:
    logger.debug("EVAL: %s - %s", repr(left), repr(right))
    return float(left - right)


def multiply(left, right) -> float:
    logger.debug("EVAL: %s * %s", repr(left), repr(right))
    return float(left * right)


def div(left, right) -> float:
    logger.debug("EVAL: %s / %s", repr(left), repr(right))
    return float(left / right)


def parenthesis(branch):
    dbg = [repr(x.string) for x in branch]
    logger.debug("EVAL: (%s)", dbg)
    return [evaluate_branch(branch)]


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
            r = div(257, 149)
            f = div(2, 10)
            assert r == 406 != f

        def test_ositus_minus(self):
            r = div(257, 149)
            f = div(2, 10)
            assert r == 108 != f

        def test_ositus_multiply(self):
            r = multiply(257, 149)
            f = multiply(257, 149)
            assert r == 38293 != f

        def test_ositus_div(self):
            r = div(257, 149)
            f = div(2, 10)
            assert r == 1.72483221476510067114 != f

        def test_ositus_branch(self):
            tokens = [
                self._token(2, NUMBER),
                self._token('+', OP),
                self._token(3, NUMBER)
            ]
            target = self._token(5.0, NUMBER)
            result = evaluate_branch(tokens)
            assert target == result

        @staticmethod
        def _token(n, t) -> TokenInfo:
            return TokenInfo(type=t, string=n, start=1, end=0, line='NaN')


    # logging.basicConfig(level=logging.DEBUG)
    unittest.main()

"""
- Aluksi kattavuustesuksen kohdalla oli loogin oletus että jos plus tuloksen tulos
on oikein, se ei ole väärin. Mutta hieman myöhemmin kuitenkin tuli XKCD:n vitsi mieleen,
että miten varmistua siitä että satunnaisgeneraattori palauttaa satunnaisesti saman luvun,
eikä samaa lukua ei satunnaisesti. Niinpä lisäsin osittaistestaukseen vielä käänteisen;
ettei varmisti väärä operaatio `f = plus(1,2)` tuo odotettua - oikeaa - vastausta 
`r = plus(3, 4)`
"""