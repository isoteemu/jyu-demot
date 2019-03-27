#!/usr/bin/env python2
# -*- coding: utf-8 -*-
1
""" Viikkotehtävä 1. Tasot 1 sekä 2. """

import cgi
import cgitb
import urllib2, simplejson

cgitb.enable(format="text")

# Muutama vakio
EOL = u"\n"
CHARSET = u"UTF-8"

# Alustetaan muutama muuttuja. `headers` sisältää headerit, `body` sisällön
headers = [u"Content-type: text/plain; charset="+CHARSET]
body = [u""]

##
## * Functiot
##


def t1_lataa_json(url):
    """Lataa jsondata `url` osoitteesta
    
    Ohjelma osaa ladata osoitteessa http://appro.mit.jyu.fi/tiea2120/vt/vt1/2019/data.json määritellyn
    json-tietorakenteen ja muuntaa sen pythonin tietorakenteeksi.
    """

    r = urllib2.urlopen(url)
    return simplejson.load(r, encoding=CHARSET)


def t1_tulosta_lajit_ja_joukkueet(data):
    """
    Kirjoita funktio, joka tulostaa kaikkien tietorakenteen sisältämien kilpailujen ja niiden sisältämien
    joukkueiden nimet. Kilpailujen nimet on tulostettava kukin omalle rivilleen. Kunkin joukkueen nimi on
    tulostettava omalle rivilleen neljän välilyönnin verran sisennettynä. Älä tulosta mitään ylimääräistä.
    Funktion on toimittava riippumatta tietorakenteessa olevien kilpailujen ja joukkueiden määrästä.
    """
    print(t1_lajit_ja_joukkueet(data))


def t1_lajit_ja_joukkueet(data):
    """Kerää lajit, ja niiden sarjojen joukkueet.
    """
    r = []
    for laji in data:
        r.append(t1_muotoile_lajin_joukkueet(laji))

    return EOL.join(r)


def t1_muotoile_lajin_joukkueet(laji):
    """Tulostaa tietueen.
    """

    joukkueet = t1_niputa_lajin_joukkueet(laji) 

    if joukkueet:
        joukkueet_str = u"".join(map(lambda s: "    %s%s" % (s, EOL), joukkueet)) + EOL
    else:
        joukkueet_str = ""

    return u"{laji}{joukkueet}".format(
        laji=laji.get("nimi", ""),
        joukkueet=joukkueet_str
    )


def t1_niputa_lajin_joukkueet(laji):
    """Kerää lajin joukkueiden nimet"""
    joukkueet = []

    for sarja in laji.get("sarjat", []):
        for joukkue in sarja.get("joukkueet"):
            try:
                joukkueet.append(joukkue["nimi"])
            except KeyError:
                """Joukkueella ei ollut nimeä"""
                pass

    return set(joukkueet)


def t1_lisaa_joukkue(data, joukkue, laji, sarja):
    """Lisää uusi joukkue lajin sarjaan.
    
    Jos parametrina annettua kilpailua (sama kilpailun nimi) tai sarjaa (sama nimi) ei löydy annetusta datasta niin funktio ei tee mitään.
    Funktion pitää toimia millä tahansa sarjoilla eikä vain niillä, jotka löytyvät nyt annetusta rakenteesta.
    """

    try:
        laji_idx = apu_etsi(data, "nimi", laji)
        sarja_idx = apu_etsi(data[laji_idx]['sarjat'], "nimi", sarja)

        try:
            """Jos joukkue on jo, älä tee mitään """
            apu_etsi(data[laji_idx]['sarjat'][sarja_idx]['joukkueet'], "nimi", joukkue['nimi'])

        except StopIteration:
            """ Joukkuetta ei löytynyt, lisää se."""
            data[laji_idx]['sarjat'][sarja_idx]['joukkueet'].append(joukkue)

    except StopIteration:
        """ Sopivaa lajia tai sarjaa ei löytynyt """

    return data


def t1_joukkueen_pseudodata():
    """Palauttaa tehtävä 1:sen vaatiman pseudodatan.
    
    Lisää edellä luomallasi funktiolla data-tietorakenteeseen yksi uusi joukkue Jäärogainingin
    4h-sarjaan seuraavilla tiedoilla [--]

    * Ohjelmalle voidaan antaa parametrina rakenteeseen lisättävän joukkueen nimi.
      Esim. http://users.jyu.fi/~omatunnus/cgi-bin/tiea2080/vt1/vt1.cgi?nimi=Pällit
    * Jos nimiparametria ei anneta, niin ohjelma käyttää joukkuetta lisättäessä nimeä "Pällit".
    * Ohjelma ei saa kaatua vaikka parametreina lukisi mitä tahansa
    """

    try:
        data = cgi.FieldStorage()
        nimi = data.getfirst(u"nimi", u"Pällit").decode(CHARSET)
    except:
        nimi = u"Pällit"

    return {
        "nimi": u"%s" % nimi,
        "jasenet": [
            u"Tommi Lahtonen",
            u"Matti Meikäläinen"
        ],
        "id": 99999,
        "leimaustapa": [
            u"GPS"
        ],
        "rastit": []
    }


def t3_poista_joukkue(data, laji, sarja, joukkue):
    """Poistaa joukkueen `data` rakenteesta.

    Lisää ohjelmaan funktio, joka osaa poistaa annetusta kilpailusta nimen perusteella joukkueen.
    Funktio ottaa parametrina datan, kilpailun nimen, sarjan nimen ja poistettavan joukkueen nimen.
    Esim. poistaJoukkue(data, u"Jäärogaining", u"8h", u"Vara 1");. Jos annettua kilpailua, sarjaa
    tai joukkuetta ei löydy niin funktio ei tee mitään. 
    """

    try:
        laji_idx = apu_etsi(data, "nimi", laji)
        sarja_idx = apu_etsi(data[laji_idx]['sarjat'], u"nimi", sarja)
        joukkue_idx = apu_etsi(data[laji_idx]['sarjat'][sarja_idx]['joukkueet'], u"nimi", joukkue)

        del data[laji_idx]['sarjat'][sarja_idx]['joukkueet'][joukkue_idx]
    except StopIteration:
        """Joukkuetta, sarjaa tai lajia ei löytynyt, älä tee mitään."""
        pass

    return data


def t3_kisat_pisteittain(data):
    """Listaa kisojen joukkueet pisteittäin.
   
    Ohjelma tulostaa kilpailuttain joukkueiden nimet ja pisteet pisteiden mukaisessa laskevassa
    suuruusjärjestyksessä. Ts. Eniten pisteitä saanut joukkue tulostetaan ensimmäisenä.

    :return: list Lista kisoista, ja tuple joukkueista pisteittäin järjestettynä.
    """

    kisat = []
    for laji in data:
        for sarja in laji['sarjat']:
            kisat.append(t3_jarjesta_pisteittain(laji, sarja))

    return kisat


def t3_jarjesta_pisteittain(laji, sarja):
    """ Järjestä sarjan jokkuuet pisteittäin.

        :return: list Tuplelista, joissa joukkueen nimi ja pisteet
    """

    pisteet = []

    for joukkue in sarja['joukkueet']:
        pisteet.append((joukkue['nimi'], t3_laske_rastin_pisteet(laji['rastit'], joukkue)))

    return sorted(pisteet, key=lambda x: x[1], reverse=True)


def t3_laske_rastin_pisteet(rastit, joukkue):
    """Laskee joukkueen saamat pisteet rasteista
    
    Tietorakenteessa on listattu kaikki joukkueiden käymät rastit.
    Rakenteesta löytyy aika jolloin rastilla on käyty ja rastin tunniste (id).
    """
    pisteet = 0
    piste = 0
    rasti_idx = 0

    try:
        """Käy läpi tietorakenne ja etsi kunkin joukkueen käymät rastit"""
        for joukkueen_rasti in joukkue['rastit']:

            try:
                rasti_idx = apu_etsi(rastit, u"id", joukkueen_rasti.get("rasti", 0))

                """Joukkue saa kustakin rastista pisteitä rastin koodin ensimmäisen merkin verran.
                Esim. jos rastin koodi on 9A niin joukkue saa yhdeksän (9) pistettä. Jos rastin
                koodin ensimmäinen merkki ei ole kokonaisluku niin kyseisestä rastista saa nolla (0)
                pistettä. Esim. rasteista LÄHTÖ ja F saa 0 pistettä.
                """
                piste = rastit[rasti_idx].get("koodi", "0")[0]

                if piste.isdigit():
                    pisteet += int(piste)
                else:
                    pisteet += 0

            except StopIteration:
                """Data ei ole "tasalaatuista" eli siinä esiintyy rasteja joille ei ole vastinetta rastit-rakenteessa"""

    except KeyError:
        return 0

    return pisteet


def t3_muokka_joukkuetta_urlista(datarakenne, joukkue):
    """Muokkaa tietoja urlista.

    Mahdollista Jäärogainingiin lisättävän joukkueen kaikkien muiden
    tietojen paitsi rastien antaminen osoiterivin parametreina.
    Esim. http://users.jyu.fi/~omatunnus/cgi-bin/tiea2080/vt1/vt1.cgi?nimi=Pällit&id=99999&leimaustapa=GPS&jasenet=Tommi%20Lahtonen&jasenet=Matti%20Meikäläinen 

     * Huom. Myös leimaustapoja täytyy voida antaa useampia kuin yksi samaan tapaan kuin jäseniäkin.
     * Jos nimeä tai id:tä ei anneta niin lisäämistä ei tehdä vaan annetaan virheilmoitus, että joukkuetta ei voida lisätä
     * Jos leimaustapaa tai jäseniä ei anneta niin näihin tallennetaan tyhjät listat. Rasteihin tallennetaan aina tyhjä lista.
     * id on tarkistettava eli kahdella joukkueella ei saa olla samaa id:tä. id täytyy olla kokonaisluku. Jos annetaan jo olemassa oleva id niin joukkuetta ei lisätä vaan annetaan virheilmoitus
    """
    data = cgi.FieldStorage()

    nimi = joukkue['nimi']
    jasenet = list()
    joukkue_id = joukkue['id']
    leimaustapa = list()

    if not data.has_key(u"nimi") and not data.has_key(u"id"):
        raise ValueError("Joko nimi tai ID on annettava")

    if data.has_key(u"nimi"):
        nimi = data.getfirst(u"nimi").decode(CHARSET)

    if data.has_key(u"jasenet"):
        jasenet = map(lambda x: u"%s" % x.decode(CHARSET), data.getlist(u"jasenet"))

    if data.has_key(u"id"):
        joukkue_id = int(data.getfirst(u"id").decode(CHARSET))
        if joukkue_id != joukkue['id'] and t3_kaksoisid(datarakenne, joukkue_id):
            raise ValueError("Kaksinkertainen joukkueen ID %i" % joukkue_id)

    if data.has_key(u"leimaustapa"):
        leimaustapa = map(lambda x: u"%s" % x.decode(CHARSET), data.getlist(u"leimaustapa"))

    return {
        "nimi": u"%s" % nimi,
        "jasenet": jasenet,
        "id": joukkue_id,
        "leimaustapa": leimaustapa,
        "rastit": []
    }


def t3_kaksoisid(datarakenne, joukkue_id):
    for laji in datarakenne:
        for sarja in laji['sarjat']:
            try:
                apu_etsi(sarja['joukkueet'], u"id", joukkue_id)
                return True
            except StopIteration:
                pass
    return False


def apu_etsi(lista, avain, arvo):
    """Palauttaa ensimmäisen sopivan avaimen.

    Heittää StopIteration exceptionin jos arvoa ei löydy.
    """
    # OBS: Arvo sekä etsittävän avaimen arvo muutetaan unicode -merkkijonoksi
    return next(i for i, x in enumerate(lista) if u"%s" % x.get(avain) == u"%s" % arvo)


if __name__ == "__main__":
    datarakenne = []

    print EOL.join(headers).encode(CHARSET) + EOL

    try:
        datarakenne = t1_lataa_json("http://appro.mit.jyu.fi/tiea2120/vt/vt1/2019/data.json")
        #datarakenne = simplejson.load(open("data.json"))
    except Exception as e:
        print(u"Virhe ladattaessa JSON dataa: %s" % e)
        exit()

    ##
    ## * Taso 1
    ##

    pseudodata = t1_joukkueen_pseudodata()

    datarakenne = t1_lisaa_joukkue(datarakenne,
                                   pseudodata,
                                   u"Jäärogaining",
                                   u"4h")

    """ Ohjelma tulostaa aakkosjärjestyksessä jäärogainingin kaikkien joukkueiden nimet.
    Mukana on oltava myös itse lisätty joukkue """

    laji_idx = apu_etsi(datarakenne, u"nimi", u"Jäärogaining")
    joukkueet = sorted(t1_niputa_lajin_joukkueet(datarakenne[laji_idx]))

    body += joukkueet

    ##
    ## * Taso 3
    ##

    """ Poista funktiollasi joukkueet joiden nimet ovat: "Vara 1", "Vara 2" ja "Vapaat". """
    datarakenne = t3_poista_joukkue(datarakenne, u"Jäärogaining", u"8h", u"Vara 1")
    datarakenne = t3_poista_joukkue(datarakenne, u"Jäärogaining", u"8h", u"Vara 2")
    datarakenne = t3_poista_joukkue(datarakenne, u"Jäärogaining", u"8h", u"Vapaat")

    """ Esim. Dynamic Duon oikea pistemäärä on 206. """
    dd_idx = apu_etsi(datarakenne[2]['sarjat'][2]['joukkueet'], u"nimi", "Dynamic Duo ")
    assert t3_laske_rastin_pisteet(datarakenne[2]['rastit'], datarakenne[2]['sarjat'][2]['joukkueet'][dd_idx]) == 206

    """ Ohjelma tulostaa kilpailuttain joukkueiden nimet ja pisteet pisteiden mukaisessa
    laskevassa suuruusjärjestyksessä. Ts. Eniten pisteitä saanut joukkue tulostetaan ensimmäisenä."""
    for kisa in t3_kisat_pisteittain(datarakenne):
        body += map(lambda x: u"%s (%i p)" % x, kisa)

    """ Muokataan "Pällit" joukkuetta urlin parametrien perusteella. """
    pallit_idx = apu_etsi(datarakenne[2]['sarjat'][0]['joukkueet'], u"id", "99999")

    try:
        datarakenne[2]['sarjat'][0]['joukkueet'][pallit_idx] = t3_muokka_joukkuetta_urlista(datarakenne, datarakenne[2]['sarjat'][0]['joukkueet'][pallit_idx])
    except ValueError as e:
        """Jos virhe annettujen parametrien perusteella, tulostetaan virheilmoitus
        ja joukkuetta ei lisätä => poistetaan """
        body.append(u"Virhe: Joukkuetta ei voitu lisätä: %s" % e)
        del datarakenne[2]['sarjat'][0]['joukkueet'][pallit_idx]

    # Tulostetaan sisältö
    print EOL.join(map(unicode, body)).encode(CHARSET)

