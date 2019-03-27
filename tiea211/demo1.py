#!/usr/bin/python3
# -*- coding: utf-8 -*-
r"""
    Heap pythonilla. Demo 1 ohjelmointitehtävä ``lajittele_keko()``.

    :class:`AssertError` jos :func:`lajittele_keko()` eroaa :func:`sorted()`
    lajittelusta.

 """


def lisaa_kekoon(alkio, keko):
    i = len(keko)
    keko = keko[:] + [alkio]

    while i > 0 and alkio < keko[i//2]:
        keko[i] = keko[i//2]
        i = i // 2

    print("Lisätään elementti", alkio, "kohtaan", i)
    keko[i] = alkio
    return keko


def korjaa_keko(keko, i):
    keko = keko[:]
    koko = len(keko)

    alkio = keko[i]
    idx = i * 2

    while idx < koko:
        if idx + 1 < koko and keko[idx] > keko[idx+1]:
            idx += 1
        if alkio <= keko[idx]:
            break

        keko[idx//2] = keko[idx]
        idx = idx * 2

    keko[idx//2] = alkio
    return keko


def tee_keko(keko):
    keko = keko[:]
    i = len(keko) // 2
    while i >= 0:
        keko = korjaa_keko(keko, i)
        i = i - 1
    return keko


def lajittele_keko(lista):
    r""" Kekolajittelu, Demo 1 ohjelmointitehtävä """

    jarjestetty_lista = []

    keko = tee_keko(lista)

    while True:
        try:
            jarjestetty_lista.append(keko[0])
            keko[0] = keko.pop(-1)
            keko = korjaa_keko(keko, 0)
        except IndexError:
            return jarjestetty_lista


if __name__ == "__main__":
    # Luodaan satunnaista dataa.
    import random
    satunnaista = random.sample(range(1000), k=100)

    demo1 = lajittele_keko(satunnaista)
    vertailu = sorted(satunnaista)

    assert(demo1 == vertailu), "Demo1 Epäonnistui; Järjestetty data ei vastaa `sorted()` näkemystä!"

    print("kekojärjestetty:", demo1)
