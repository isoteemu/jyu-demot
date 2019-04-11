#!/usr/bin/python2
# -*- coding: utf-8 -*-

import os

from flask import render_template, abort, flash, request, redirect, url_for
from flask_wtf import CSRFProtect, FlaskForm
from flask_babel import _

from google.appengine.ext import ndb
from google.appengine.api import users

from tiea2080 import alusta, app_init_virhe as level_select_screen, Virhe
from tiea2080.models import Kilpailu, Sarja, Joukkue, Rasti, pura_aika
from tiea2080.kaava import KilpailuKaava, SarjaKaava, JoukkueKaava, RastiKaava, RastiLeimaus

try:
    from urllib import urlencode
    from urlparse import urlparse, urlunparse, urljoin, parse_qsl
except:
    # Python 3
    from urllib.parse import urlencode, urlparse, urlunparse, urljoin, parse_qsl

app = alusta()

app.config['DEBUG'] = app.debug = not os.getenv('SERVER_SOFTWARE', '').startswith('Google App Engine/')

# Sanakirja joka määrittelee miten eri mallit linkittyvät eri ``WTForms`` luokkiin.
# Näin koska en käytä ``wtforms-appengine```:ä
kaava_map = {
    "Kilpailu": KilpailuKaava,
    "Sarja": SarjaKaava,
    "Joukkue": JoukkueKaava,
    "Rasti": RastiKaava
}

# Alustetaan flaskiin lisättävät moduulit
with app.app_context():
    if not app.config['DEBUG']:
        # Estä kaatumiset.
        level_select_screen(app)

    # Suojausta. Tehtävä ei vaadi, mutta eh.
    csrf = CSRFProtect(app)


def apu_setattr(objekti, nimi, data):
    r"""
    Aseta mallin arvot vastaamaan kaavan arvoja.
    """

    prop = getattr(type(objekti), nimi)

    if not hasattr(objekti, nimi) or nimi[0] == "_":
        return objekti
    if isinstance(prop, ndb.KeyProperty):
        # ^^ Vertaillaan onko mallin arvo avainarvo[n perillinen]
        # Haetaan objektin luokka, ja luokasta property, ja propertyn edustama luokka.
        setattr(objekti, nimi, ndb.Key(urlsafe=data))
    elif isinstance(data, list):
        # Jos on lista, poistetaan tyhjät ja kaksoisarvot.
        setattr(objekti, nimi, list(set(filter(None, data))))
    else:
        setattr(objekti, nimi, data)

    return objekti


def crud_kaava_render(kaava, objekti, **kwargs):
    r"""
    Renderöi kaavan. Valitsee mahdollisesti sopivan templaten.

    :param objekti: :class:`ndb.Model` luokan instanssi.
    :param tyyppi: Mahdollinen edellä mainitun luokan tyyppimäärite.
    """
    if "tyyppi" not in kwargs:
        kwargs['tyyppi'] = objekti._get_kind()

    templatet = [u"wtforms-%s.html.j2" % kwargs['tyyppi'], "wtforms.html.j2"]
    return render_template(templatet, objekti=objekti, form=kaava, **kwargs)


def is_safe_url(target):
    r"""
    Tarkistaa että paluu url on oma url

    Kopioitu suoraan http://flask.pocoo.org/snippets/62/
    """
    ref_url = urlparse(request.host_url)
    test_url = urlparse(urljoin(request.host_url, target))
    return test_url.scheme in ('http', 'https') and \
        ref_url.netloc == test_url.netloc


def paluu_url(fallback="sivu_kilpailut", **kwargs):
    """
    Jos on ``return=`` arvo urlissa, palauta siihen, muutoin :param:`fallback`
    """

    r = request.args.get('return')
    if r:
        if not is_safe_url(r):
            return abort(400)
        # Puretaan url, päivitetään query string, ja koostetaan uudestaan.
        parser = urlparse(r)
        parts = list(parser)
        qs = dict(parse_qsl(parts[4]))
        qs.update(kwargs)
        parts[4] = urlencode(qs)
        return urlunparse(parts)

    else:
        # Ei return osoitetta, palataan fallbackiin.
        return url_for(fallback, **kwargs)


# ROUTET
# ======

@app.route("/")
def root():
    return redirect(url_for("sivu_kilpailut"))


@app.route("/kilpailut")
def sivu_kilpailut():
    uusin = None

    if "uusi" in request.args:
        # Varmistaa että juuri lisätty on sivulla ja uusin
        try:
            uusin = ndb.key.Key(urlsafe=request.args['uusi']).get()
        except TypeError as e:
            app.logger.info(u"Objektin avain virheellinen: %s" % e)

    kilpailut = list(Kilpailu.query())

    if uusin:
        if uusin._get_kind() == "Kilpailu":
            if uusin not in kilpailut:
                kilpailut.append(uusin)

    return render_template("kilpailut.html.j2", kilpailut=kilpailut)


@app.route("/rastit/<kilpailu>")
def sivu_rastit(kilpailu):
    try:
        kisa = ndb.Key(urlsafe=kilpailu).get()
        if not isinstance(kisa, Kilpailu):
            raise ValueError(u"Pyydetty objekti ei ole Kilpailu.")
    except Exception as e:
        app.logger.info(e)
        raise Virhe(u"Kilpailua ei löydetty.")

    if not kisa:
        abort(404)

    uusin = None
    if "uusi" in request.args:
        # Varmistaa että juuri lisätty on sivulla ja uusin
        try:
            uusin = ndb.key.Key(urlsafe=request.args['uusi']).get()
        except TypeError as e:
            app.logger.info(u"Rastin avain virheellinen: %s" % e)

    rastit = list(Rasti.query(Rasti.kilpailu == kisa.key))

    if uusin:
        if uusin._get_kind() == "Rasti":
            if uusin not in rastit and uusin.kilpailu == kisa.key:
                rastit.append(uusin)

    return render_template("rastit.html.j2", rastit=rastit, kilpailu=kisa)


@app.route("/leimaa/<joukkue>", methods=('GET', 'POST'))
def sivu_leimaa(joukkue):
    """
    Joukkueen rastien leimauksen sivu
    """
    try:
        joukkue_obj = ndb.Key(urlsafe=joukkue).get()
        if not isinstance(joukkue_obj, Joukkue):
            raise ValueError(u"Pyydetty objekti ei ole Rasti.")
    except Exception as e:
        app.logger.info(e)
        raise Virhe(u"Kilpailua ei löydetty.")

    kaava = RastiLeimaus()
    kaava.rasti.choices = [(e.key.urlsafe(), u"%s" % e.nimi) for e in Rasti.query(Rasti.kilpailu == joukkue_obj.sarja.get().kilpailu)]
    _rastit = list(joukkue_obj.rastit)

    if kaava.validate_on_submit():

        # Poistetaan vanha leima
        _rastit = filter(lambda x: x['rasti'] != kaava.rasti.data, _rastit)

        joukkue_obj.rastit = _rastit

        if "poista" in request.form:
            flash(_(u"Rasti %(rasti)s poistettu leimatuista", rasti=dict(kaava.rasti.choices)[kaava.rasti.data]))
        else:
            joukkue_obj.rastit = [{
                u"aika": u"%s" % kaava.aika.data,
                u"rasti": kaava.rasti.data
            }] + _rastit

            flash(_(u"Rasti %(rasti)s leimattu", rasti=dict(kaava.rasti.choices)[kaava.rasti.data]))

        joukkue_obj.put()

    rastit = []
    for rasti in joukkue_obj.rastit:
        # Kerää kaikki pätevät rastit
        try:
            # Huom kaksoissulut, on :type:`Tuple`.
            rastit.append((
                ndb.Key(urlsafe=rasti['rasti']).get(),
                pura_aika(rasti['aika'])
            ))
        except Exception as e:
            app.logger.debug(u"Joukkueelle ei löytynyt rastia: %s" % e)
            # Elementti poistettu.
            pass

    return render_template("leimaa.html.j2", form=kaava, joukkue=joukkue_obj, rastit=rastit)


@app.route("/test/<test>")
def sivu_testi(test, foo=None):

    flash(u"Tämä oli virhe", "error")

    if app.config["DEBUG"]:
        return render_template("virhe.html.j2", virhe=unicode("TESTI VIRHE"), routet=app.url_map.iter_rules())

    raise Virhe("Testivirhe")


@app.route('/populate')
#@ndb.transactional(xg=True)
def populate():
    r"""
    Täytä datastore oletusdatalla.

    Ei vaivaudu tarkistamaan, onko data jo lisätty.

    Ei myöskään tehdä transactionissa, liikaa tapahtumia.
    """

    kilpailut = [{"nimi":u"Jäärogaining", "loppuaika": "2015-03-17 20:00:00", "alkuaika": "2015-03-15 09:00:00", "kesto": 1}, {"nimi":u"Fillarirogaining", "loppuaika": "2016-03-17 20:00:00", "alkuaika": "2016-03-15 09:00:00", "kesto": 1}, {"nimi":u"Kintturogaining", "loppuaika": "2017-03-18 20:00:00", "alkuaika": "2017-03-18 09:00:00", "kesto": 1}]
    sarjat = [{"nimi":u"4 h", "kilpailu": u"Kintturogaining", "kesto": 4}, {"nimi":u"2 h", "kilpailu": u"Kintturogaining", "kesto": 2}, {"nimi":u"8 h", "kilpailu": u"Kintturogaining", "kesto": 8}]
    joukkueet = [{"sarja": "8 h", "nimi": u"Onnenonkijat", "jasenet": ["Antero Paununen", "Pekka Paununen", "Raimo Laine"]}, {"sarja": "8 h", "nimi": u"Mudan Ystävät", "jasenet": ["Kaija Kinnunen", "Teija Kinnunen"]}, {"sarja": "8 h", "nimi": u"Vara 3", "jasenet": ["barbar"]}, {"sarja": "8 h", "nimi": u"Tollot", "jasenet": ["Juju", "Tappi"]}, {"sarja": "8 h", "nimi": u"Kahden joukkue", "jasenet": ["Matti Humppa", "Miikka Talvinen"]}, {"sarja": "8 h", "nimi": u"Siskokset", "jasenet": ["Sanna Haavikko", "Seija Kallio"]}, {"sarja": "8 h", "nimi": u"Dynamic Duo", "jasenet": ["Karhusolan Rentukka", "Kutajoen Tiukunen"]}, {"sarja": "8 h", "nimi": u"Toipilas", "jasenet": ["Leena Annila", "Satu Lehtonen"]}, {"sarja": "8 h", "nimi": u"Sopupeli", "jasenet": ["Antti Haukio", "Janne Hautanen", "Taina Pekkanen", "Venla Kujala"]}, {"sarja": "4 h", "nimi": u"Retkellä v 13", "jasenet": ["Henna Venäläinen", "Katja Vitikka"]}, {"sarja": "4 h", "nimi": u"Pelättimet", "jasenet": ["Kari Vaara", "Katja Vaara"]}, {"sarja": "8 h", "nimi": u"Kaakelin putsaajat", "jasenet": ["Jaana Kaajanen", "Mikko Kaajanen", "Timo Ruonanen"]}, {"sarja": "8 h", "nimi": u"Vara 1", "jasenet": []}, {"sarja": "2 h", "nimi": u"Hullut fillaristit", "jasenet": ["Hannele Saari", "Paula Kujala"]}, {"sarja": "2 h", "nimi": u"Kotilot", "jasenet": ["Jaana Meikäläinen", "Kaisa Konttinen", "Maija Meikäläinen", "Niina Salonen"]}, {"sarja": "8 h", "nimi": u"Rennot 1", "jasenet": ["Anja Huttunen", "Siru Kananen"]}, {"sarja": "8 h", "nimi": u"Vara 2", "jasenet": []}, {"sarja": "4 h", "nimi": u"Vapaat", "jasenet": ["Juha Vapaa", "Matti Vapaa"]}, {"sarja": "8 h", "nimi": u"Susi jo syntyessään", "jasenet": ["Janne Pannunen", "Riku Aarnio"]}, {"sarja": "8 h", "nimi": u"Vara 4", "jasenet": []}, {"sarja": "4 h", "nimi": u"Rennot 2", "jasenet": ["Heikki Häkkinen", "Pia Virtanen", "Sari Maaninka"]}, {"sarja": "4 h", "nimi": u"Tähdenlento", "jasenet": ["Anu", "Virva"]}, {"sarja": "8 h", "nimi": u"RogRog", "jasenet": ["Antti Kaakkuri", "Mikko Meikäläinen", "Pekka Kosonen", "Samuli Paavola"]}]
    rastit = [{"lat": 62.13028, "lon": 25.666688, "koodi": "Tuntematon"}, {"lat": 62.120776, "lon": 25.542413, "koodi": "66"}, {"lat": 62.156532, "lon": 25.496872, "koodi": "6D"}, {"lat": 62.112172, "lon": 25.714338, "koodi": "91"}, {"lat": 62.099795, "lon": 25.544984, "koodi": "48"}, {"lat": 62.133029, "lon": 25.737019, "koodi": "31"}, {"lat": 62.110562, "lon": 25.518665, "koodi": "85"}, {"lat": 62.115047, "lon": 25.615203, "koodi": "69"}, {"lat": 62.088183, "lon": 25.729848, "koodi": "99"}, {"lat": 62.11183, "lon": 25.644512, "koodi": "60"}, {"lat": 62.148123, "lon": 25.618079, "koodi": "63"}, {"lat": 62.134681, "lon": 25.605762, "koodi": "70"}, {"lat": 62.13028, "lon": 25.666688, "koodi": "LAHTO"}, {"lat": 62.10393, "lon": 25.63595, "koodi": "90"}, {"lat": 62.122986, "lon": 25.573049, "koodi": "34"}, {"lat": 62.11906, "lon": 25.628228, "koodi": "37"}, {"lat": 62.089674, "lon": 25.652877, "koodi": "5C"}, {"lat": 62.129767, "lon": 25.626533, "koodi": "44"}, {"lat": 62.086189, "lon": 25.695688, "koodi": "79"}, {"lat": 62.127323, "lon": 25.597278, "koodi": "82"}, {"lat": 62.095187, "lon": 25.628236, "koodi": "64"}, {"lat": 62.141243, "lon": 25.509358, "koodi": "6F"}, {"lat": 62.136462, "lon": 25.668097, "koodi": "41"}, {"lat": 62.153864, "lon": 25.540227, "koodi": "40"}, {"lat": 62.102194, "lon": 25.673997, "koodi": "5A"}, {"lat": 62.144852, "lon": 25.493141, "koodi": "92"}, {"lat": 62.118784, "lon": 25.718561, "koodi": "5B"}, {"lat": 62.121247, "lon": 25.678314, "koodi": "49"}, {"lat": 62.111294, "lon": 25.553191, "koodi": "78"}, {"lat": 62.098636, "lon": 25.691051, "koodi": "56"}, {"lat": 62.078212, "lon": 25.733259, "koodi": "42"}, {"lat": 62.139918, "lon": 25.535011, "koodi": "67"}, {"lat": 62.138397, "lon": 25.56252, "koodi": "7C"}, {"lat": 62.091567, "lon": 25.680401, "koodi": "96"}, {"lat": 62.13232, "lon": 25.498431, "koodi": "53"}, {"lat": 62.132964, "lon": 25.57761, "koodi": "95"}, {"lat": 62.142319, "lon": 25.590916, "koodi": "76"}, {"lat": 62.15146, "lon": 25.50711, "koodi": "46"}, {"lat": 62.126591, "lon": 25.704639, "koodi": "58"}, {"lat": 62.147298, "lon": 25.665822, "koodi": "83"}, {"lat": 62.125561, "lon": 25.558017, "koodi": "51"}, {"lat": 62.087827, "lon": 25.671071, "koodi": "97"}, {"lat": 62.147942, "lon": 25.563169, "koodi": "5E"}, {"lat": 62.124222, "lon": 25.649234, "koodi": "94"}, {"lat": 62.100104, "lon": 25.586932, "koodi": "47"}, {"lat": 62.153364, "lon": 25.52873, "koodi": "74"}, {"lat": 62.099512, "lon": 25.522034, "koodi": "73"}, {"lat": 62.126639, "lon": 25.750133, "koodi": "7B"}, {"lat": 62.141674, "lon": 25.718473, "koodi": "6A"}, {"lat": 62.107914, "lon": 25.61344, "koodi": "43"}, {"lat": 62.093545, "lon": 25.716227, "koodi": "71"}, {"lat": 62.101185, "lon": 25.565572, "koodi": "77"}, {"lat": 62.153435, "lon": 25.560594, "koodi": "33"}, {"lat": 62.09468, "lon": 25.647515, "koodi": "6E"}, {"lat": 62.100413, "lon": 25.728135, "koodi": "80"}, {"lat": 62.131251, "lon": 25.540316, "koodi": "7E"}, {"lat": 62.149572, "lon": 25.597308, "koodi": "68"}, {"lat": 62.134123, "lon": 25.682473, "koodi": "7A"}, {"lat": 62.109962, "lon": 25.7288, "koodi": "89"}, {"lat": 62.115924, "lon": 25.569589, "koodi": "45"}, {"lat": 62.135094, "lon": 25.523811, "koodi": "57"}, {"lat": 62.147825, "lon": 25.513792, "koodi": "38"}, {"lat": 62.113906, "lon": 25.668757, "koodi": "81"}, {"lat": 62.141654, "lon": 25.628636, "koodi": "50"}, {"lat": 62.081466, "lon": 25.686679, "koodi": "7D"}, {"lat": 62.108717, "lon": 25.589347, "koodi": "54"}, {"lat": 62.146315, "lon": 25.645642, "koodi": "72"}, {"lat": 62.095246, "lon": 25.732937, "koodi": "62"}, {"lat": 62.149229, "lon": 25.576022, "koodi": "86"}, {"lat": 62.123662, "lon": 25.531059, "koodi": "5D"}, {"lat": 62.142258, "lon": 25.526039, "koodi": "88"}, {"lat": 62.144101, "lon": 25.694017, "koodi": "32"}, {"lat": 62.125632, "lon": 25.49602, "koodi": "6B"}, {"lat": 62.131769, "lon": 25.669574, "koodi": "MAALI"}, {"lat": 62.115241, "lon": 25.743788, "koodi": "65"}, {"lat": 62.093203, "lon": 25.606234, "koodi": "55"}, {"lat": 62.117266, "lon": 25.694911, "koodi": "75"}, {"lat": 62.156431, "lon": 25.519131, "koodi": "93"}, {"lat": 62.147942, "lon": 25.531926, "koodi": "61"}, {"lat": 62.128162, "lon": 25.724837, "koodi": "36"}, {"lat": 62.118778, "lon": 25.524245, "koodi": "39"}, {"lat": 62.115914, "lon": 25.503483, "koodi": "59"}, {"lat": 62.140919, "lon": 25.648821, "koodi": "35"}, {"lat": 62.094023, "lon": 25.661916, "koodi": "84"}, {"lat": 62.120424, "lon": 25.599044, "koodi": "52"}, {"lat": 62.131207, "lon": 25.650436, "koodi": "98"}, {"lat": 62.127514, "lon": 25.674748, "koodi": "5F"}, {"lat": 62.10758, "lon": 25.687644, "koodi": "6C"}]

    kisat_map = {}
    sarjat_map = {}
    rastit_map = {}

    virheet = []

    for kisa in kilpailut:
        kisat_map[kisa['nimi']] = Kilpailu(**kisa)

    ndb.put_multi(kisat_map.values())

    for sarja in sarjat:
        sarja['kilpailu'] = kisat_map[sarja['kilpailu']].key
        sarjat_map[sarja['nimi']] = Sarja(**sarja)

    ndb.put_multi(sarjat_map.values())

    for joukkue in joukkueet:
        try:
            joukkue['sarja'] = sarjat_map[joukkue['sarja']].key
            Joukkue(**joukkue).put()
        except Exception as e:
            virheet.append(u"Joukkue: %s: %s" % (joukkue['nimi'], e))

    for rasti in rastit:
        rasti['kilpailu'] = kisat_map[u"Kintturogaining"].key
        rastit_map[rasti['koodi']] = Rasti(**rasti)

    ndb.put_multi(rastit_map.values())

    virheet.append(u"<strong>Populoitu!</strong>");
    u"<br />".join(virheet)

    return u"<br />".join(virheet), 201


@app.route("/lopeta", methods=('POST',))
def sivu_uloskirjaudu():
    r"""
    Ohjaa googlen uloskirjautumissivulle.
    """
    url = paluu_url("sivu_root")

    return redirect(users.create_logout_url(url))


# CRUD
# ----

@app.route("/luo/<tyyppi>", methods=('GET', 'POST'))
def sivu_luo(tyyppi):
    r"""
    Luo uuden olion, ja muokkaa sitä.

    :param tyyppi: Muokattavan mallin luokan nimi.

    """

    if tyyppi not in kaava_map.keys():
        flash(_(u"Tuntematon malli."), "warning")
        app.logger.warning(u"Tuntematon malli pyydetty luotavaksi: %s" % tyyppi)

        return redirect(paluu_url("sivu_kilpailut"))

    # Ouh nouh, globals! Mutta ihan OK tässä tapauksessa.
    objekti = globals()[tyyppi]()

    if request.method == "GET":
        # Täytä mahdollisilla esi-tiedoilla.
        for arg in request.args:
            if hasattr(objekti, arg):
                apu_setattr(objekti, arg, request.args.get(arg))

    try:
        kaava = crud_magic_happens_here(objekti)

        if kaava.Meta.tila is True:
            return redirect(paluu_url("sivu_kilpailut", uusi=objekti.key.urlsafe()))
    except Exception as e:
        flash(_(u"Tapahtui virhe: %(virhe)s", virhe=e))

    return crud_kaava_render(kaava, objekti, tyyppi=tyyppi)


@app.route("/muokkaa/<avain>", methods=('GET', 'POST'))
def sivu_muokkaa(avain):
    """
    Lataa mallin avaimella, ja muokkaa sitä.

    :param avain: class:`ndb.key` :func:`urlsafe()` muodossa muokattavalle
                  mallille. Jos avain on nolla - kuten pääteohjauksessa -
                  oletetaan haluttavan uutta.
    """

    try:
        objekti = ndb.Key(urlsafe=avain).get()
    except Exception as e:
        flash(_(u"Tietoja ei löydetty"), "warning")
        app.logger.info(u"Mallia ei voitu ladata: %s" % e)
        return abort(404)

    tyyppi = objekti._get_kind()

    try:
        kaava = crud_magic_happens_here(objekti)

        if kaava.Meta.tila is True:
            return redirect(paluu_url("sivu_kilpailut", uusi=objekti.key.urlsafe()))
    except Exception as e:
        flash(_(u"Tapahtui virhe: %(virhe)s", virhe=e))

    return crud_kaava_render(kaava, objekti, tyyppi=tyyppi)

def crud_magic_happens_here(objekti):
    """
    Functio joka vastaa mallien muokkauskaavoista

    Palautetussa kaavassa on :param:`Meta.tila` muuttuja sen mukaan määritelty,
    onko kaavan koettu prosessoiduksi.

    Alkuperäinen idea oli käyttää WTForm-appengineä, mutta sen
    bugisuus aiheutti epäluottamusta, ja nyt käyttää ``kaava_map``
    rakennetta määrittelemään eri ndb malleille (:file:`tiea2080/kaava.py`)
    omat kaavat.
    """

    tyyppi = objekti._get_kind()

    if tyyppi not in kaava_map:
        # Rikkoo tehtävää, kaataa sovelluksen jos funnybussinessia havaittavissa.
        raise Virhe(u"Tuntematon objekti pyydetty muokattavaksi: %s" % tyyppi)

    crud = kaava_map[tyyppi](obj=objekti)

    # Tallenna muokattavan objektin avain kaavaan.
    crud.Meta.avain = None
    if objekti.key:
        crud.Meta.avain = objekti.key

    # Toggle muuttuja, onko kaavan tila ok.
    crud.Meta.tila = False

    if crud.is_submitted() and "poista" in request.form:
        # Elementti on pyydetty poistettavaksi, joten skipataan
        # validoinnit.
        try:
            objekti.key.delete()
            flash(_(u"%(tyyppi)s %(nimi)s poistettiin.", tyyppi=tyyppi, nimi=objekti.nimi))
            crud.Meta.tila = True
        except Exception as e:
            flash(_(u"Poisto epäonnistui. Tarkemmat tiedot lokissa."), "warning")
            app.logger.info(u"Tietueen poisto epäonnistui: %s" % e)

    elif crud.validate_on_submit():
        try:
            for f in crud:
                # Lue kaavasta data tietueeseen.
                if hasattr(objekti, f.name):
                    apu_setattr(objekti, f.name, f.data)

            objekti.put()

            # Vesan ohjeiden mukaan pitäisi toimia, mutta ei toimi. Tosin se
            # taitaa olla BASEn kanssa eläessä siedettävä.
            objekti.key.get()

            flash(_(u"%(tyyppi)s %(nimi)s tallennettu", tyyppi=tyyppi, nimi=objekti.nimi))
            crud.Meta.tila = True
        except Exception as e:
            flash(_(u"Virhe tallennettaessa"), "error")
            app.logger.error(u"Virhe tallennettaessa kaavaa: %s" % e)

    return crud
