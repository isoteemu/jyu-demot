#!/usr/bin/python2
# -*- coding: utf-8 -*-

from flask import (
    Blueprint, flash, redirect, render_template, request, session, url_for
)

from flask_login import LoginManager, UserMixin, login_user, logout_user, login_required
from flask_wtf import FlaskForm
from flask_babel import _

from wtforms import StringField, PasswordField, SelectField, SubmitField
from wtforms.validators import DataRequired, ValidationError

import hashlib

from .data import Data
import logging

logger = logging.getLogger(__name__)
logger.addHandler(logging.NullHandler())


bp = Blueprint('kirjautuminen', __name__)
login_manager = LoginManager()

CHARSET = "utf-8"


def app_init(app):
    global CHARSET
    r""" Alustaa kirjautumiseen tarvittavaa shittiä """
    login_manager.init_app(app)
    login_manager.login_view = "kirjautuminen.kirjautumissivu"
    CHARSET = app.config.get("CHARSET", CHARSET)
    with app.app_context():
        app.register_blueprint(bp)


class Joukkue(UserMixin):
    r""" Käyttäjäolio loginmanagerille - tässä tapauksessa joukkue """

    def __init__(self, *args, **kwargs):
        for k, v in kwargs.iteritems():
            setattr(self, k, v)

    @staticmethod
    def hae(id):
        """ Etsi ja luo Joukkue -olion id:n perusteella """
        joukkue_data = Data().etsi_joukkue(id=id)
        return Joukkue(**joukkue_data)

@login_manager.user_loader
def user_loader(id):
    j = Joukkue.hae(id)
    return j


def varmista_joukkue(form, field):
    """ Validaattori joukkueen nimelle """

    joukkueen_nimi = field.data.strip().lower()
    kilpailu = int(form.kilpailu.data)

    try:
        joukkue = Data().etsi_joukkue(
            nimi=joukkueen_nimi,
            kilpailu=kilpailu
        )
        return joukkue
    except Exception as e:
        logging.error("Virhe joukkueen tarkistuksessa: %s", e)

    raise ValidationError(u"Virheellinen joukkue")


def varmista_salasana(form, field):
    """ Validaattori salasanalle """
    m = hashlib.sha512()

    try:
        joukkueen_nimi = form.joukkue.data.strip().lower()
        kilpailu = int(form.kilpailu.data)

        joukkue = Data().etsi_joukkue(
            nimi=joukkueen_nimi,
            kilpailu=kilpailu
        )
        m.update(str(joukkue['id']))
        m.update(unicode(field.data))

        if m.hexdigest() == joukkue['salasana']:
            return True

    except TypeError:
        pass


    raise ValidationError(u"Virheellinen salasana")


class KirjautumisKaava(FlaskForm):
    """WTForms kaava"""
    kilpailu = SelectField(_(u"Kilpailu"), choices=[], validators=[DataRequired()])
    joukkue = StringField(_(u"Joukkue"), validators=[DataRequired(), varmista_joukkue])
    salasana = PasswordField(_(u"Salasana"), validators=[DataRequired(), varmista_salasana])
    kirjaudu = SubmitField(_(u"Kirjaudu sisään"))


@bp.route('/kirjaudu', methods=('GET', 'POST'))
def kirjautumissivu():

    kaava = KirjautumisKaava()
    # de fakken thing ei osaa välittää numeroita.
    kaava.kilpailu.choices = [(unicode(i), l) for i, l in Data().listaa_lajit()]

    if kaava.validate_on_submit():
        flash(_("Kirjautuminen onnistui."))

        joukkueen_nimi = kaava.joukkue.data.strip().lower()

        joukkue = Joukkue(**Data().etsi_joukkue(nimi=joukkueen_nimi))
        login_user(joukkue)

        logger.info("Joukkue '%s' kirjautui" % kaava.joukkue.data)

        return redirect(url_for('joukkueet.index'))

    return render_template("kirjaudu.html.j2", form=kaava)


@bp.route('/logout', methods=('GET', 'POST'))
@login_required
def uloskirjautumissivu():
    r""" Uloskirjautumissivu.
    """

    if request.method == 'POST':
        logout_user()
        flash(_(u"Olet kirjautunut ulos"))
        return redirect(url_for('kirjautuminen.kirjautumissivu'))

    else:
        return render_template("uloskirjaudu.html.j2")
