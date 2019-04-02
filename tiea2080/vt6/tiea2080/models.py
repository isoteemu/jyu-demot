#!/usr/bin/python2
# -*- coding: utf-8 -*-
from google.appengine.ext import ndb
from datetime import datetime

import string
from transliterate import translit


def nimistin(nimi):
    r"""
    Muuttaa nimen yksinkertaiseen muotoon, jota vastaan voidaan tehdä kyselyitä.

    Canonisoi nimen transliteroimalla ():class:`transliterate`), poistamalla
    erikoismerkit (control-characters yms.) sekä tietysti pienentämällä merkistön.
    """
    if not isinstance(nimi, unicode):
        nimi = nimi.decode("utf-8")

    # Käyttää translittiä canonisoimiseen, ei olisi oikeasti hyvä.
    nimi = translit(nimi, reversed=True)
    nimi = u"".join([x for x in nimi if x in string.printable])
    nimi = nimi.strip().lower()

    return u"%s" % nimi


class AikaProperty(ndb.DateTimeProperty):
    def _to_base_type(self, value):
        """
        Muunna merkkijono :class:`datetime` olioksi.
        """
        if isinstance(value, (str, unicode)):
            r = None
            for f in ["%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S"]:
                try:
                    r = datetime.strptime(value, f)
                    break
                except ValueError:
                    # Jos ei pystytä muuttamaan, seuraava taso heittää sitten virheen.
                    pass

            return r


class Kilpailu(ndb.Model):
    def validate_kesto(prop, value):
        if not value > 0:
            raise ValueError(u"Keston on oltava suurempi kuin nolla")

    nimi = ndb.StringProperty(required=True)
    alkuaika = AikaProperty(required=True)
    loppuaika = AikaProperty(required=True)
    kesto = ndb.IntegerProperty(required=True, default=1, validator=validate_kesto)

    nimi_lower = ndb.ComputedProperty(lambda self: nimistin(self.nimi))

    # ylimääritellään put-metodi. Lisätään tarkistus alku- ja loppuajalle
    def put(self, *args, **kwargs):
        if self.alkuaika > self.loppuaika:
            raise ValueError(u"Alkuajan on oltava pienempi kuin loppuajan")

        return super(Kilpailu, self).put(*args, **kwargs)

    @property
    def sarjat(self):
        return Sarja.query(Sarja.kilpailu == self.key)


class Sarja(ndb.Model):

    def validate_kesto(prop, value):
        if not value > 0:
            raise ValueError(u"Keston on oltava suurempi kuin nolla")

    nimi = ndb.StringProperty(required=True)
    kesto = ndb.IntegerProperty(required=True, validator=validate_kesto, default=4)
    kilpailu = ndb.KeyProperty(kind=Kilpailu, required=True)

    nimi_lower = ndb.ComputedProperty(lambda self: nimistin(self.nimi))

    def __init__(self, *args, **kwargs):
        if "kilpailu" in kwargs and isinstance(kwargs['kilpailu'], (str, unicode)):
            # jos kilpailu on merkkijono, muuta se relaatioavaimeksi.
            kwargs['kilpailu'] = Kilpailu.query(Kilpailu.nimi == kwargs['kilpailu']).get().key

        return super(Sarja, self).__init__(*args, **kwargs)


class Joukkue(ndb.Model):
    def validate_jasenet(prop, value):
        jasenet = value
        if not len(jasenet) >= 2:
            raise ValueError(u"Jäseniä on oltava vähintään kaksi : %s" % jasenet)

    nimi = ndb.StringProperty(required=True)

    # json-muotoon ja siitä purkaminen hoituu automaattisesti eli tähän kenttään voi suoraan tallentaa
    # melkein minkä tahansa pythonin listan, joka sisältää perustietotyyppejä
    #
    # HUOM: Tämä olisi parempi olla ``ndb.StringProperty(repeated=True)``, mutta
    # tehtävä vaatii pitämään sen tällaisena.
    jasenet = ndb.JsonProperty(required=True, validator=validate_jasenet)
    jasenet_lower = ndb.ComputedProperty(lambda self: map(nimistin, self.jasenet))

    sarja = ndb.KeyProperty(required=True, kind=Sarja)
