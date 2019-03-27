#!/usr/bin/python2
# -*- coding: utf-8 -*-

from flask import (
    Blueprint, g, flash, redirect, render_template, request, session, url_for
)
from flask_wtf import FlaskForm
from wtforms import StringField, PasswordField, SelectField, SubmitField
from wtforms.validators import DataRequired, ValidationError
from tiea2080 import _

import hashlib
import logging
import functools

logger = logging.getLogger(__name__)
logger.addHandler(logging.NullHandler())

bp = Blueprint('kirjautuminen', __name__)


def kirjautuminen(view):
    """ Kirjautumisen decorator """

    @functools.wraps(view)
    def wrapped_view(**kwargs):
        if session.get('kirjautunut', False) is not True:
            return redirect(url_for('kirjautuminen.kirjautumissivu'))

        g.joukkue = session['joukkue']
        g.laji = session['laji']

        return view(**kwargs)

    return wrapped_view

def varmista_joukkue(form, field):
    """ Validaattori joukkueen nimelle """
    for joukkue, sarja, laji in g.data.listaa_joukkueet():
        if(field.data.strip().lower() == joukkue['nimi'].strip().lower() and
        form.laji.data == laji['nimi']):
            return True

    raise ValidationError(u"Virheellinen joukkue")

def varmista_salasana(form, field):
    """ Validaattori salasanalle """
    m = hashlib.sha512()
    m.update(field.data.encode("utf-8"))

    if m.hexdigest() == g.salasana:
        return True

    raise ValidationError(u"Virheellinen salasana")

class KirjautumisKaava(FlaskForm):
    """WTForms kaava"""
    laji = SelectField(_(u"Laji"))
    joukkue = StringField(_(u"Joukkue"),  validators=[DataRequired(), varmista_joukkue])
    salasana = PasswordField(_(u"Salasana"), validators=[DataRequired(), varmista_salasana])
    kirjaudu = SubmitField(_(u"Kirjaudu sisään"))


@bp.route('/kirjaudu', methods=('GET', 'POST'))
def kirjautumissivu():
    kaava = KirjautumisKaava()
    kaava.laji.choices = [(l, l) for l in g.data.listaa_lajit()]

    if kaava.validate_on_submit():
        flash("Kirjautuminen onnistui.")
        session['kirjautunut'] = True
        session['laji'] = kaava.laji.data
        session['joukkue'] = kaava.joukkue.data

        logger.info("Joukkue '%s' kirjautui" % kaava.joukkue.data)

        return redirect(url_for('joukkueet.index'))

    return render_template("kirjaudu.html.j2", form=kaava)


@bp.route('/logout', methods=('GET', 'POST'))
@kirjautuminen
def uloskirjautumissivu():
    r""" Uloskirjautumissivu.
    """

    if request.method == 'POST':
        session['kirjautunut'] = False
        flash(_(u"Olet kirjautunut ulos"))
        return redirect(url_for('kirjautuminen.kirjautumissivu'))

    else:
        return render_template("uloskirjaudu.html.j2")

