#!/usr/bin/python2
# -*- coding: utf-8 -*-

""" Pro-tip: Kuka ikinä vastaakaan wtformista, voi vetää vitun päähänsä. """

from flask import (
    Blueprint, g, abort, flash, redirect, render_template, request, session, url_for
)

from flask_wtf import FlaskForm
from wtforms import StringField, RadioField, SubmitField, HiddenField
from wtforms.validators import DataRequired, ValidationError, Optional

from . import _
from .kirjautuminen import kirjautuminen, varmista_joukkue

import logging
logger = logging.getLogger(__name__)
logger.addHandler(logging.NullHandler())

bp = Blueprint('joukkueet', __name__)


def vt3_vertaa(a, b):
    """ Auttaa vertailemaan elementtejä tehtävän puitteissa.

    Vertaa sarjan nimen mukaan ja toissijaisesti joukkueen nimen mukaan.
    """
    r = nimi_vertaa(a[0], b[0])
    return r if r == 0 else nimi_vertaa(a[1], b[1])


def nimi_vertaa(a, b):
    nimi_a = u"%s" % a['nimi'].strip().lower()
    nimi_b = u"%s" % b['nimi'].strip().lower()
    if nimi_a > nimi_b:
        return 1
    elif nimi_a < nimi_b:
        return -1

    return 0


def varmista_sarja(form, field):
    try:
        g.data.etsi_sarja(field.data, g.laji)
    except StopIteration:
        logger.warning("Sarjaa ei löytynyt: %s", field.data)
        raise ValidationError("Data corruption; Sarjaa ei löytynyt")
    return true


class JoukkueKaava(FlaskForm):
    nimi = StringField(_(u"Joukkue"), validators=[DataRequired()], default=lambda: g.joukkue)
    sarja = RadioField(_(u"Sarja"), choices=[], validators=[DataRequired()])

    #vanha_sarja = HiddenField(validators=[varmista_sarja])
    vanha_sarja = HiddenField()




@bp.route('/')
@kirjautuminen
def index():
    lista = []
    joukkueet = sorted(g.data.listaa_joukkueet(), vt3_vertaa)
    for joukkue in joukkueet:
        if joukkue[2]['nimi'] != g.laji:
            continue

        lista.append(joukkue)

    return render_template("joukkueet.html.j2", joukkueet=lista)

@bp.route('/<joukkue>/muokkaa', methods=('GET', 'POST'))
@kirjautuminen
def muokkaa(joukkue):
    if session['joukkue'] != joukkue:
        abort(403)

    sarjat = g.data.etsi_laji(session['laji'])['sarjat']
    joukkueen_sarja = None
    joukkueen_jasenet = []

    # Etsi joukkue sarjoista
    for sarja in sarjat:
        try:
            j = g.data.etsi_joukkue(joukkue, sarja, session['laji'])
            if j:
                joukkueen_sarja = sarja
                joukkueen_jasenet = j['jasenet'] + ([""] * 5)
                break
        except StopIteration:
            pass

    if not joukkueen_sarja:
        abort(500, "Joukkueen sarjaa ei löytynyt")

    for i in range(0, 5):
        _validators = [] 
        if i < 2:
            _validators += [DataRequired()]
        else:
            _validators += [Optional()]

        setattr(JoukkueKaava, "jasen_%d" % i,  StringField(u"Jäsen #%d" % i, validators=_validators, default=joukkueen_jasenet[i]))

    kaava = JoukkueKaava()

    for s in sarjat:
        kaava.sarja.choices.append((s['nimi'], s['nimi']))

    if kaava['sarja'].data == "None":
        # Tietysti se on merkkijono None. Miksei se olisi merkkijono None
        logger.debug("Asetataan sarja: " + joukkueen_sarja['nimi'])
        kaava.sarja.data = joukkueen_sarja['nimi']

    kaava.vanha_sarja.data = joukkueen_sarja['nimi']

    if kaava.validate_on_submit():
        logger.debug("Tallennetaan kaava")
        logger.debug("S: %s, %s, %s", joukkue, kaava['vanha_sarja'].data, session['laji'])
        # HAXHAXHAX - Kopioi, poista ja lisää.
        cc = g.data.etsi_joukkue(joukkue, kaava['vanha_sarja'].data, session['laji'])
        g.data.poista_joukkue(joukkue, kaava['vanha_sarja'].data, session['laji'])
        cc['nimi'] = kaava.nimi.data

        cc['jasenet'] = [x for x in list(set([
            kaava['jasen_0'].data,
            kaava['jasen_1'].data,
            kaava['jasen_2'].data,
            kaava['jasen_3'].data,
            kaava['jasen_4'].data,
        ])) if x != ""]
        logger.debug("Jäsenet: %s", cc['jasenet'] )


        g.data.etsi_sarja(kaava.vanha_sarja.data, session['laji'])['joukkueet'].append(cc)
        g.data.tallenna()

        session['joukkue'] = cc['nimi']
        g.joukkue = session['joukkue']

        flash("Joukkueen tiedot tallennettu")
        return redirect(url_for('joukkueet.muokkaa', joukkue=session['joukkue']))

    return render_template("joukkue-muokkaa.html.j2", form=kaava)
