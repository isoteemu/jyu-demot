#!/usr/bin/python2
# -*- coding: utf-8 -*-

from flask import (
    Blueprint, g, abort, flash, redirect, render_template, session, url_for
)
from flask_login import login_required, current_user
from flask_babel import _

from flask_wtf import FlaskForm
from wtforms import StringField, RadioField, SubmitField, HiddenField
from wtforms.validators import DataRequired, ValidationError, Optional

import simplejson as json

from .kirjautuminen import varmista_joukkue, login_manager
from .data import Data

import logging
logger = logging.getLogger(__name__)

bp = Blueprint('joukkueet', __name__)

CHARSET = "utf-8"


def app_init(app):
    global CHARSET
    app.config.get("CHARSET", CHARSET)
    with app.app_context():
        app.register_blueprint(bp)


def validaattori_nimi(form, field):
    r""" Varmistaa nimen yksilöllisyyden """
    valid = True
    nimi = u"%s" % field.data.strip()

    try:
        joukkue = Data().etsi_joukkue(nimi=nimi)
        if joukkue['id'] != form.Meta.joukkue_id:
            valid = False
    except TypeError:
        # Odotettu virhe jos nimeä ei löydy 
        pass

    if valid is not True:
        raise ValidationError("Joukkueen nimi on jo käytössä.")


@bp.route('/')
@login_required
def index():
    """ Toteuta joukkuelistaussivu jossa on listaus kaikista valitun kilpailun joukkueista.
    Listauksessa näkyvät myös sarjan nimi sekä joukkueiden jäsenet. """

    lista = []
    for joukkue in Data().listaa_joukkueet():
        if joukkue['kilpailu'] != current_user.kilpailu:
            continue

        _joukkue = dict(joukkue)
        _joukkue['jasenet_lista'] = json.loads(joukkue['jasenet'])

        lista.append(_joukkue)

    return render_template("joukkueet.html.j2", joukkueet=lista)



@bp.route('/<int:joukkue_id>/muokkaa', methods=('GET', 'POST'))
@login_required
def muokkaa(joukkue_id):
    if joukkue_id != current_user.id:
        abort(403)

    nr_jasenia = 5

    joukkue = Data().etsi_joukkue(id=joukkue_id)
    sarjat = [(unicode(e['id']), e['nimi']) for e in Data().listaa_sarjat()]
    joukkueen_jasenet = json.loads(joukkue['jasenet'], encoding=CHARSET) + ([u""] * nr_jasenia)

    class F(FlaskForm):
        pass

    setattr(F, "nimi", StringField(_(u"Joukkue"), validators=[DataRequired(), validaattori_nimi], default=unicode(joukkue['nimi'])))
    setattr(F, "sarja", RadioField(_(u"Sarja"), choices=sarjat, validators=[DataRequired()], default=joukkue['sarja']))
    setattr(F.Meta, "joukkue_id", joukkue_id)

    for i in range(0, nr_jasenia):
        _validators = []
        if i < 2:
            _validators += [DataRequired()]
        else:
            _validators += [Optional()]

        setattr(F, "jasen_%d" % i, StringField(u"Jäsen #%d" % (i + 1), validators=_validators, default=joukkueen_jasenet[i]))

    kaava = F()
    #kaava.joukkue.data = joukkue_id

    if kaava.validate_on_submit():
        try:
            uusi_tietue = dict(joukkue)
            uusi_tietue['nimi'] = kaava.nimi.data
            uusi_tietue['sarja'] = int(kaava.sarja.data)

            jasenet = []
            for i in range(0, nr_jasenia):
                jasen = kaava['jasen_%d' % i].data.strip()
                if jasen != u"":
                    jasenet.append(jasen)

            uusi_tietue['jasenet'] = json.dumps(jasenet, encoding=CHARSET)

            Data().paivita_joukkue(uusi_tietue)

            Data().tallenna()
            flash(_("Joukkueen muutokset tallennettu"))
            return redirect(url_for('joukkueet.index'))

        except Exception as e:

            Data().kumoa()
            logger.error(e)

            flash(_("Tietoja ei voitu tallentaa"))

    return render_template("joukkue-muokkaa.html.j2", form=kaava)
