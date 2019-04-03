#!/usr/bin/python2
# -*- coding: utf-8 -*-
from google.appengine.ext import ndb
from datetime import datetime

import string
from transliterate import translit
from transliterate.exceptions import LanguageDetectionError


def nimistin(nimi):
    r"""
    Muuttaa nimen yksinkertaiseen muotoon, jota vastaan voidaan tehdä kyselyitä.

    Normalisoi nimen transliteroimalla (:class:`transliterate`), poistamalla
    erikoismerkit (control-characters yms.) sekä tietysti pienentämällä koon.
    """
    if not isinstance(nimi, unicode):
        nimi = u"%s" % nimi.decode("utf-8")

    try:
        nimi = translit(nimi, reversed=True)
    except LanguageDetectionError:
        # jos erikoismerkkejä ei havaittu, translit valittaa siitä.
        pass

    nimi = u"".join([x for x in nimi if x in string.printable])
    nimi = nimi.strip().lower()

    return u"%s" % nimi


def pura_aika(value):
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


class AikaProperty(ndb.DateTimeProperty):
    def _to_base_type(self, value):
        return pura_aika(value)


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
        return list(Sarja.query(Sarja.kilpailu == self.key))

    @property
    def rastit(self):
        return list(Rasti.query(Rasti.kilpailu == self.key))


class Sarja(ndb.Model):

    def validate_kesto(prop, value):
        if not value > 0:
            raise ValueError(u"Keston on oltava suurempi kuin nolla")

    nimi = ndb.StringProperty(required=True)
    kesto = ndb.IntegerProperty(required=True, validator=validate_kesto, default=4)
    kilpailu = ndb.KeyProperty(kind=Kilpailu, required=True)

    nimi_lower = ndb.ComputedProperty(lambda self: nimistin(self.nimi))

    def __init__(self, *args, **kwargs):
        if "kilpailu" in kwargs and isinstance(kwargs['kilpailu'], basestring):
            # jos kilpailu on merkkijono, muuta se relaatioavaimeksi.
            kwargs['kilpailu'] = Kilpailu.query(Kilpailu.nimi == kwargs['kilpailu']).get().key

        return super(Sarja, self).__init__(*args, **kwargs)

    @property
    def joukkueet(self):
        return Joukkue.query(Joukkue.sarja == self.key)


class Joukkue(ndb.Model):
    def validate_jasenet(prop, value):
        jasenet = list(set(map(lambda x: x.strip(), value)))
        if not len(jasenet) >= 2:
            raise ValueError(u"Jäseniä on oltava vähintään kaksi : %s" % jasenet)

        return jasenet

    nimi = ndb.StringProperty(required=True)
    nimi_lower = ndb.ComputedProperty(lambda self: nimistin(self.nimi))

    # json-muotoon ja siitä purkaminen hoituu automaattisesti eli tähän kenttään voi suoraan tallentaa
    # melkein minkä tahansa pythonin listan, joka sisältää perustietotyyppejä
    #
    # HUOM: Tämä olisi parempi olla ``ndb.StringProperty(repeated=True)``, mutta
    # tehtävä vaatii pitämään sen tällaisena.
    jasenet = ndb.JsonProperty(required=True, validator=validate_jasenet)
    jasenet_lower = ndb.ComputedProperty(lambda self: map(nimistin, self.jasenet), repeated=True)

    sarja = ndb.KeyProperty(required=True, kind=Sarja)

    rastit = ndb.JsonProperty(required=False, default=[])


class Rasti(ndb.Model):
    def validate_lat(prop, value):
        r"""
        Latituden on oltava liukuluku väliltä -90 - 90
        """

        return max(-90, min(90, value))

    def validate_lon(prop, value):
        r"""
        Longituden on oltava liukuluku väliltä -180 - 180
        """

        return max(-180, min(180, value))

    koodi = ndb.StringProperty(required=True)
    kilpailu = ndb.KeyProperty(kind=Kilpailu, required=True)

    lat = ndb.FloatProperty(required=True, validator=validate_lat)
    lon = ndb.FloatProperty(required=True, validator=validate_lon)

    nimi = ndb.ComputedProperty(lambda self: self.koodi)

