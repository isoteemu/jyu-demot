#!/usr/bin/python2
# -*- coding: utf-8 -*-

import os

from flask import render_template, flash, request, redirect, url_for
from flask_wtf import CSRFProtect, FlaskForm
from werkzeug.exceptions import InternalServerError
from flask_babel import _

from google.appengine.ext import ndb
from google.appengine.api import users

from tiea2080 import alusta, app_init_virhe as level_select, Virhe
from tiea2080.models import Kilpailu, Sarja
from tiea2080.kaava import KilpailuKaava, SarjaKaava

app = alusta()

app.config['DEBUG'] = app.debug = not os.getenv('SERVER_SOFTWARE', '').startswith('Google App Engine/')

# Sanakirja joka määrittelee miten eri mallit linkittyvät eri ``WTForms`` luokkiin.
# En käytä ``wtforms-appengine:ä``.
kaava_map = {
    "Kilpailu": KilpailuKaava,
    "Sarja": SarjaKaava,
}

with app.app_context():
    level_select(app)
    # Suojausta. Tehtävä ei vaadi, mutta eh.
    csrf = CSRFProtect(app)


def apu_setattr(objekti, nimi, data):
    r"""
    Aseta mallin arvot vastaamaan kaavan arvoja.
    """

    if not hasattr(objekti, nimi) or nimi[0] == "_":
        return objekti
    if issubclass(type(getattr(type(objekti), nimi)), ndb.KeyProperty):
        # ^^ Vertaillaan onko mallin arvo avainarvo[n perillinen]
        # Haetaan objektin luokka, ja luokasta property, ja propertyn edustama luokka.
        setattr(objekti, nimi, ndb.Key(urlsafe=data))
    else:
        setattr(objekti, nimi, data)

    return objekti


##
## ROUTET
##

@app.route("/")
def root():
    return redirect(url_for("sivu_kilpailut"))


@app.route("/kilpailut")
def sivu_kilpailut():
    kilpailut = Kilpailu.query()
    return render_template("kilpailut.html.j2", kilpailut=kilpailut)

@app.route("/test/<test>")
def sivu_testi(test, foo=None):

    flash(u"Tämä oli virhe", "error")
    raise Virhe("Testivirhe")


@app.route("/lopeta", methods=['POST'])
def sivu_uloskirjaudu():
    r""" Ohjaa googlen uloskirjautumissivulle. """
    return redirect(users.create_logout_url('/'))


@app.route("/luo/<tyyppi>", methods=('GET', 'POST'))
def sivu_luo(tyyppi):
    r"""
    Luo uuden olion, ja muokkaa sitä.

    :param malli: Muokattavan mallin luokan nimi.

    TODO: Ohjaa pois sivulta onnistuttaessa.
    """

    if tyyppi not in kaava_map.keys():
        flash(_(u"Tuntematon malli."), "warning")
        app.logger.warning(u"Tuntematon malli pyydetty luotavaksi: %s" % tyyppi)
        return redirect(url_for("sivu_kilpailut"))

    # Ouh nouh, globals! Mutta ihan OK tässä tapauksessa.
    objekti = globals()[tyyppi]()

    if request.method == "GET":
        # Täytä mahdollisilla esi-tiedoilla.
        for arg in request.args:
            apu_setattr(objekti, arg, request.args.get(arg))

    return crud_magic_happens_here(objekti)


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
        app.logger.info(u"Tietoja ei voitu ladata: %s" % e)
        return redirect(url_for("sivu_kilpailut"))

    return crud_magic_happens_here(objekti)


def crud_magic_happens_here(objekti):
    """
    Functio joka vastaa mallien muokkauskaavoista

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

    crud.Meta.avain = None
    if objekti.key:
        crud.Meta.avain = objekti.key

    if crud.is_submitted() and "poista" in request.form:
        # Elementti on pyydetty poistettavaksi, joten skipataan
        # validoinnit.
        try:
            objekti.key.delete()
            flash(u"Tietue %s poistettiin." % crud.data.get('nimi'))
            return redirect(url_for("sivu_kilpailut"))
        except Exception as e:
            flash(_(u"Poisto epäonnistui. Tarkemmat tiedot lokissa."), "warning")
            app.logger.info(u"Tietueen poisto epäonnistui: %s" % e)

    elif crud.validate_on_submit():
        try:
            for f in crud:
                # Lue kaavasta data tietueeseen.
                apu_setattr(objekti, f.name, f.data)

            objekti.put()

            # Vesan ohjeiden mukaan pitäisi toimia, mutta ei toimi. Tosin se
            # taitaa olla BASEn kanssa eläessä siedettävä.
            objekti.key.get()

            flash(_(u"Muutokset tallennettu"))
            return redirect(url_for("sivu_kilpailut"))
        except Exception as e:
            flash(_(u"Virhe tallennettaessa"), "error")
            app.logger.error(u"Virhe tallennettaessa kaavaa: %s" % e)

    templatet = [u"wtforms-%s.html.j2" % objekti._get_kind(), "wtforms.html.j2"]

    return render_template(templatet, form=crud, tyyppi=tyyppi, objekti=objekti)


@app.route('/populate')
@ndb.transactional(xg=True)
def populate():
    r"""
    Täytä datastore oletusdatalla.

    Ei vaivaudu tarkistamaan, onko data jo lisätty.
    """

    kilpailut = [{"nimi":u"Jäärogaining", "loppuaika": "2015-03-17 20:00:00", "alkuaika": "2015-03-15 09:00:00", "kesto": 1}, {"nimi":u"Fillarirogaining", "loppuaika": "2016-03-17 20:00:00", "alkuaika": "2016-03-15 09:00:00", "kesto": 1}, {"nimi":u"Kintturogaining", "loppuaika": "2017-03-18 20:00:00", "alkuaika": "2017-03-18 09:00:00", "kesto": 1}]
    sarjat = [{"nimi":u"4 .h", "kilpailu": u"Kintturogaining", "kesto": 4}, {"nimi":u"2 h", "kilpailu": u"Kintturogaining", "kesto": 2}, {"nimi":u"8 h", "kilpailu": u"Kintturogaining", "kesto": 8}]
    # joukkueet = [{"sarja": "8 h", "nimi": u"Onnenonkijat", "jasenet": ["Antero Paununen", "Pekka Paununen", "Raimo Laine"]}, {"sarja": "8 h", "nimi": u"Mudan Ystävät", "jasenet": ["Kaija Kinnunen", "Teija Kinnunen"]}, {"sarja": "8 h", "nimi": u"Vara 3", "jasenet": ["barbar"]}, {"sarja": "8 h", "nimi": u"Tollot", "jasenet": ["Juju", "Tappi"]}, {"sarja": "8 h", "nimi": u"Kahden joukkue", "jasenet": ["Matti Humppa", "Miikka Talvinen"]}, {"sarja": "8 h", "nimi": u"Siskokset", "jasenet": ["Sanna Haavikko", "Seija Kallio"]}, {"sarja": "8 h", "nimi": u"Dynamic Duo", "jasenet": ["Karhusolan Rentukka", "Kutajoen Tiukunen"]}, {"sarja": "8 h", "nimi": u"Toipilas", "jasenet": ["Leena Annila", "Satu Lehtonen"]}, {"sarja": "8 h", "nimi": u"Sopupeli", "jasenet": ["Antti Haukio", "Janne Hautanen", "Taina Pekkanen", "Venla Kujala"]}, {"sarja": "4 h", "nimi": u"Retkellä v 13", "jasenet": ["Henna Venäläinen", "Katja Vitikka"]}, {"sarja": "4 h", "nimi": u"Pelättimet", "jasenet": ["Kari Vaara", "Katja Vaara"]}, {"sarja": "8 h", "nimi": u"Kaakelin putsaajat", "jasenet": ["Jaana Kaajanen", "Mikko Kaajanen", "Timo Ruonanen"]}, {"sarja": "8 h", "nimi": u"Vara 1", "jasenet": []}, {"sarja": "2 h", "nimi": u"Hullut fillaristit", "jasenet": ["Hannele Saari", "Paula Kujala"]}, {"sarja": "2 h", "nimi": u"Kotilot", "jasenet": ["Jaana Meikäläinen", "Kaisa Konttinen", "Maija Meikäläinen", "Niina Salonen"]}, {"sarja": "8 h", "nimi": u"Rennot 1", "jasenet": ["Anja Huttunen", "Siru Kananen"]}, {"sarja": "8 h", "nimi": u"Vara 2", "jasenet": []}, {"sarja": "4 h", "nimi": u"Vapaat", "jasenet": ["Juha Vapaa", "Matti Vapaa"]}, {"sarja": "8 h", "nimi": u"Susi jo syntyessään", "jasenet": ["Janne Pannunen", "Riku Aarnio"]}, {"sarja": "8 h", "nimi": u"Vara 4", "jasenet": []}, {"sarja": "4 h", "nimi": u"Rennot 2", "jasenet": ["Heikki Häkkinen", "Pia Virtanen", "Sari Maaninka"]}, {"sarja": "4 h", "nimi": u"Tähdenlento", "jasenet": ["Anu", "Virva"]}, {"sarja": "8 h", "nimi": u"RogRog", "jasenet": ["Antti Kaakkuri", "Mikko Meikäläinen", "Pekka Kosonen", "Samuli Paavola"]}]

    kisat_map = {}
    sarjat_map = {}

    for kisa in kilpailut:
        kisat_map[kisa['nimi']] = Kilpailu(**kisa).put()

    for sarja in sarjat:
        sarja['kilpailu'] = kisat_map[sarja['kilpailu']]
        sarjat_map[sarja['nimi']] = Sarja(**sarja).put()

    # for joukkue in joukkueet:
    #     joukkue['sarja'] = sarjat_map[joukkue['sarja']]
    #     Joukkue(**joukkue).put()

    return u"Populoitu!", 201
