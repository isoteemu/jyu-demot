#!/usr/bin/python2
# -*- coding: utf-8 -*-

r"""sporttidataluokka json tiedostolle"""

import io
import simplejson

import logging
logger = logging.getLogger(__name__)
logger.addHandler(logging.NullHandler())


class SuunnistusdataJson():
    """ Datatyyppi suunistusdatalle. """

    def __init__(self, tiedosto, **kwargs):
        self.tiedosto = tiedosto
        fp = io.open(self.tiedosto, encoding=kwargs.get("encoding", "utf-8"), mode="r")
        self.data = simplejson.load(fp)

    def listaa_lajit(self):
        """ Palauttaa lajit listana """
        r = []
        for laji in self.data:
            r.append(laji['nimi'])

        return r

    def etsi_laji(self, laji):
        logger.debug("Etsi laji: %s -> %s", laji, self.apu_etsi(self.data, u"nimi", laji))
        return self.data[self.apu_etsi(self.data, u"nimi", laji)]

    def etsi_sarja(self, sarja, laji):
        if type(laji) in [str, unicode]:
            laji = self.etsi_laji(laji)

        return laji['sarjat'][self.apu_etsi(laji['sarjat'], u"nimi", sarja)]

    def etsi_joukkue(self, joukkue, sarja, laji):
        if type(sarja) in [str, unicode]:
            sarja = self.etsi_sarja(sarja, laji)

        return sarja['joukkueet'][self.apu_etsi(sarja["joukkueet"], u"nimi", joukkue)]

    def listaa_joukkueet(self):
        """ Yksiulotteinen lista joukkueista """
        r = []
        for laji_idx, laji in enumerate(self.data):
            laji['idx'] = laji_idx
            for sarja_idx, sarja in enumerate(laji['sarjat']):
                sarja['idx'] = sarja_idx
                for joukkue_idx, joukkue in enumerate(sarja['joukkueet']):
                    joukkue['idx'] = joukkue_idx
                    r.append([joukkue, sarja, laji])
        return r

    def poista_joukkue(self, joukkue, sarja, laji):
        logger.debug("Poistetaan joukkue: %s, %s, %s", joukkue, sarja, laji)
        """ Ruma HAX poistamaan joukkuetta """
        for j, s, l in self.listaa_joukkueet():
            if j['nimi'] == joukkue and s['nimi'] == sarja and l['nimi'] == laji:
                del self.data[l['idx']]['sarjat'][s['idx']]['joukkueet'][j['idx']]

    def tallenna(self):
        with io.open(self.tiedosto, 'w', encoding='utf-8') as f:
            f.write(simplejson.dumps(self.data, ensure_ascii=False))

    def apu_etsi(self, lista, avain, arvo):
        """Palauttaa ensimmäisen sopivan avaimen.

        Heittää StopIteration exceptionin jos arvoa ei löydy.
        """
        # OBS: Arvo sekä etsittävän avaimen arvo muutetaan unicode -merkkijonoksi
        return next(i for i, x in enumerate(lista) if u"%s" % x.get(avain) == u"%s" % arvo)
