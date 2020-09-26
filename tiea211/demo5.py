#!/usr/bin/python
# -*- coding: utf-8 -*-

r"""
Algoritmit 2 Ohjelmointitehtävä 5

..O5:
    Likiarvo piille voidaan laskea seuraavasti: Piirretään neliö
    ja sen sisään mah-dollisimman suuri ympyrä. Generoidaan neliön
    sisään joukko satunnaisia pisteitä (koordinaatit tasaisesti
    jakautuneita satunnaislukuja). Lasketaan, kuinka moni pisteistä
    osuu myös ympyrän sisään. Likiarvo saadaan kaavasta``4k/n``, missä
    ``k`` on ympyrän sisällä olevien pisteiden lukumäärä ja ``n``
    kaikkien pisteiden lukumäärä. Kirjoita ohjelma, joka laskee piin
    likiarvon tällä tavalla. Laske myös likiarvon virhe oikeaan piin
    arvoon verrattuna. Testaa ohjelmaa usealla eri pisteiden lukumäärällä.

"""

__author__ = "tearautt"

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

import matplotlib

def laske_pi(sample_size):

    # Yksinkertaisuuden vuoksi neliö on 1x1
    neliö = (1.0, 1.0)
    osumia = 0

    max_rad = min(neliö) / 2.0
    keski_x, keski_y = map(lambda x: x / 2.0, neliö)

    pisteet = zip(np.random.random(sample_size), np.random.random(sample_size))
    for x, y in pisteet:
        etäisyys = np.hypot(keski_x - x, keski_y - y)
        if etäisyys <= max_rad:
            osumia += 1

    return 4 * osumia / sample_size

if __name__ == "__main__":
    otoksia = 5
    näytteitä = [int(np.exp(k)) for k in range(2, 13)]

    data = pd.DataFrame(columns=näytteitä)

    for c in range(otoksia):
        data_rivi = [laske_pi(k) for k in näytteitä]
        data.loc[c] = data_rivi

    mean = data.mean()

    [print(u"Näytteitä: {:6d} * {:d}\tMean: {:f}\tVirhe: {:f}%".format(
           s, otoksia, mean.loc[s], abs(1.0 - mean.loc[s] / np.pi) * 100))
        for s in mean.index]

    try:
        matplotlib.use('Qt5Agg')
        ax = data.plot.box()
        ax.set_title("Hajonta %d otannalla" % otoksia)
        ax.set_xlabel("Näytteitä")
        line, = plt.plot([np.pi] * (len(data.columns) + 2))
        plt.setp(line, linestyle='--')
        plt.show()
    except Exception as e:
        print("Ei voitu esittää graaffia: %s" % e )
        pass

    #data = [(k, laske_pi(k)) for k in otoksia]
    #[print(u"Sample: {:6d}\tResult: {:f}\tError {:f}%".format(s, d, abs(1.0 - d / np.pi) * 100)) for s, d in data]
