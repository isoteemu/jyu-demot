#!/usr/bin/python3

r"""
TIEA211 Algoritmit 2 Viikkotehtävä 3

..
    Perusjoukossa on n kappaletta erillisiä merkkijonoja, esimerkiksi:
    {"banaani", "luumu", "omena", "persikka", "sitruuna"}

    Kirjoita ohjelma, joka toteuttaa union-find-operaatiot tämän tyyppiselle
    perusjoukolle. Testaa operaatioiden toimivuus.

"""

from random import choice

perusjoukko = ["banaani", "luumu", "omena", "persikka", "sitruuna"]


class TIEA211_Demo3():

    def __init__(self, joukko):
        for x in joukko:
            # Sisäinen rakenne:
            # Ensimmäinen alkio on lasten määrä + self, toinen parent
            self.__dict__[x] = [1, None]

    def yhdistä(self, a, b):
        a_juuri = self.etsi(a)
        b_juuri = self.etsi(b)

        if a_juuri == b_juuri:
            # Ovat jo yhdistetty
            return

        # Asetetaan osoittimet juurielementteihin
        a_ = self.__dict__[a_juuri]
        b_ = self.__dict__[b_juuri]

        k = a_[0] + b_[0]

        if a_[0] > b_[0]:
            a_[0] = k
            b_[1] = a_juuri
        else:
            b_[0] = k
            a_[1] = b_juuri

    def etsi(self, alkio):
        r"""
        Etsii juurisolmun.

        OBS: Ei sisällä tiivistystä, tehtävä ei sitä halunnut.
        """
        vanhempi = self.__dict__[alkio]

        if vanhempi[1] is not None:
            return self.etsi(vanhempi[1])
        else:
            return alkio

    def __repr__(self):
        return str(self.__dict__)


if __name__ == "__main__":
    demo = TIEA211_Demo3(perusjoukko)

    print("Testataan:")
    for i in range(0, 3):
        a = None
        b = None
        while a == b:
            a = choice(perusjoukko)
            b = choice(perusjoukko)

        print("> Yhtistetään", a, "sekä", b)
        demo.yhdistä(a, b)

        # Varmistetaan elementtien kuuluvan saman juuren alle
        assert demo.etsi(a) == demo.etsi(b), "Elementtejä ei ole yhdistetty"

    print("Lopullinen rakenne:", demo)
