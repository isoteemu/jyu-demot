#!/usr/bin/python3

r"""
Tehtävä: Polkujen käsittely / Tehtävä 5

> Millä keinoin voisit tehdä aliohjelman, jota voi kutsua vain absoluuttisilla poluilla eikä
> suhteellisilla ollenkaan? Pystytkö tekemään käännösaikaisen eston? 
"""

import sys
import os


def aliohjelma(path):
    if path[0] != os.path.sep:
        raise ValueError("Polkukuvaaja ei ole absoluuttinen")

    if not os.path.exists(path):
        raise FileNotFoundError("Tiedostoa ei ole olemassa")

    pass


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Käyttö: %s <polku>" % sys.argv[0])
    
    polku = sys.argv[1]
    if aliohjelma(polku):
        sys.exit(0)
    else:
        sys.exit(1)
