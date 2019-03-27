#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import cgi
import cgitb

import simplejson

from jinja2 import Environment, FileSystemLoader

def luo_ruudukko(koko=8):
    """Luo taulukko.

    :return: tyhjän Matriisin
    """

    return [[0 for x in range(koko)] for y in range(koko)] 


def ruudukko_oletus(ruudukko):
    """Sijoitetaan nappuloita kuvan osoittamiin paikkoihin"""
    kopio = ruudukko[:]
    riveja = len(ruudukko) - 1

    """ Muokkaa ruudukkoa taso 3:sen mukaisesti """
    for rivi_idx, rivi in enumerate(kopio):
        for sarake_idx, sarake in enumerate(rivi):
            if sarake_idx == rivi_idx:
                kopio[rivi_idx][sarake_idx] = 1
            elif (riveja - rivi_idx) == sarake_idx:
                kopio[rivi_idx][sarake_idx] = 2

    return kopio


def yhdista_ruudukko(kohde, lahde):
    """Yhdistä ruudukko ``lahde`` -arvot ``kohde`` -ruudukkoon"""
    kopio = kohde[:]

    for rivi_idx, rivi in enumerate(kopio):
        for sarake_idx, sarake in enumerate(rivi):
            if lahde[rivi_idx][sarake_idx] in [0,1,2]:
                kopio[rivi_idx][sarake_idx] = lahde[rivi_idx][sarake_idx]

    return kopio


if __name__ == "__main__":
    CHARSET = u"utf-8"

    # Sivun tila tallennetaan tähän
    sivu = {
        "koko": 8,
        "koko_min": 8,
        "koko_max": 16,

        "ruudukko": [],

        "toimintotila": "poista"
    }

    print(u"Content-Type: text/html; charset={0}\n\n".format(CHARSET).encode(CHARSET))

    tila = cgi.FieldStorage()

    jinja = Environment(loader=FileSystemLoader('tpl'))

    # Jinjan yhteensopivuusfunctio
    if "tojson" not in jinja.filters:
        jinja.filters['tojson'] = lambda x: u"%s" % simplejson.dumps(x)


    # Taso1: Koon syöttö, ja sen tarkistukset
    try:
        _koko = int(tila.getfirst("x", sivu['koko']))
        if(_koko < sivu["koko_min"]):
            sivu['koko_virhe'] = "Koon tulee olla suurempi kuin 8"
        elif(_koko > sivu["koko_max"]):
            sivu['koko_virhe'] = "Koon tulee olla pienempi kuin 16"
        else:
            sivu['koko'] = _koko

    except ValueError:
        sivu['koko_virhe'] = "Koon tulee olla numero."


    # Taso 1: teksti
    sivu['teksti'] = u"%s" % tila.getfirst("teksti", "").decode(CHARSET)

    # Taso 1: Lataa ruudukko
    # Jos taulukon kooksi annetaan epäkelpo arvo (liian suuri, liian pieni tai ei numero)
    # niin taulukkoa ei luoda ollenkaan
    if "koko_virhe" not in sivu:
        ruudukko = luo_ruudukko(sivu['koko'])

        try:
            if "ruudukko" in tila:
                # Lataa ruudukko jos sellainen on
                cgi_ruudukko = simplejson.loads(tila.getfirst("ruudukko", []))
                # Yhdistää ladatun ruudukon tyhjään. Näin ulottuvuudet ovat varmasti oikein.
                ruudukko = yhdista_ruudukko(ruudukko, cgi_ruudukko)
            else:
                ruudukko = ruudukko_oletus(ruudukko)

        except Exception as e:
            print(u"Virhe yhdistettäessä ruudukoita: %s" % e)
            ruudukko = ruudukko_oletus(ruudukko)

        # Taso 3: Poistetaan nappula jos sellaista haluttu
        toiminto = tila.getfirst("toiminto", None)
        if toiminto == "poista":
            try:
                poistettavan_rivi = int(tila.getfirst("rivi", None))
                poistettavan_sarake = int(tila.getfirst("sarake", None))

                ruudukko[poistettavan_rivi][poistettavan_sarake] = 0
            except Exception as e:
                print(u"Virhe poistettaessa nappulaa: %s" % e)

        sivu["ruudukko"] = ruudukko

    # Renderöi sivu
    template = jinja.get_template("index.tpl.html")
    print(template.render(sivu).encode(CHARSET))
