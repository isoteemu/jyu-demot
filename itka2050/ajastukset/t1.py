#!/usr/bin/python3

import timeit
import random, string
import math

eka_lause = ''.join([random.choice(string.ascii_letters + string.digits) for n in range(50)])
toka_lause = ''.join([random.choice(string.ascii_letters + string.digits) for n in range(50)])

assert eka_lause != toka_lause, "Aika lotota, tai tarkastaa koneen random"

koodi = r"""
if "{:50.50s}" == "{:50.50s}":
    pass
else:
    pass
"""

koodi_secure = r"""
import hmac
if hmac.compare_digest("{:50.50s}", "{:50.50s}"):
    pass
else:
    pass
"""


def ajastuksia(koodi):
    i = 0
    while True:
        i += 1
        looppeja = int(math.exp(i))

        samikset = timeit.timeit(koodi.format(eka_lause, eka_lause), number=looppeja)
        erikset = timeit.timeit(koodi.format(eka_lause, toka_lause), number=looppeja)

        # NOT funktionaalista
        print(format.format(looppeja, samikset, erikset))
        if erikset > 10.0:
            break


format = "{:11d} {:10.10f} {:10.10f}"
print("Suoritetaan normaali vertailu:")
print(" #          Sama:        Eri:")

ajastuksia(koodi)

print("")
print("Suoritetaan HMAC vertailu:")
print(" #          Sama:        Eri:")
ajastuksia(koodi_secure)
