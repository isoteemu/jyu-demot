#!/usr/bin/python2
# -*- coding: utf-8 -*-

""" Joukkueen kirjautumisen luokka.

    Viikkotehtävä 5: Lopussa on KirjautumisRajapinta -luokka, joka on se kiinnostava.
 """

from flask import (
    Blueprint, current_app as app, flash, redirect, render_template, request, url_for, json
)

from flask_login import LoginManager, UserMixin, login_user, logout_user, login_required, current_user
from flask_wtf import FlaskForm
from flask_babel import _

from wtforms import StringField, PasswordField, SelectField, SubmitField
from wtforms.validators import DataRequired, ValidationError

import hashlib

from .data import Data
from .rajapinta import lisaa_rajapinta, Rajapinta, rajapinta_nakyma

bp = Blueprint('kirjautuminen', __name__)
login_manager = LoginManager()


def app_init(app):
    r""" Alustaa kirjautumiseen tarvittavaa shittiä """
    login_manager.init_app(app)
    login_manager.login_view = "kirjautuminen.kirjautumissivu"
    lisaa_rajapinta(KirjautumisApi, "/kirjaudu")

    with app.app_context():
        app.register_blueprint(bp)


class Joukkue(UserMixin):
    r""" Käyttäjäolio loginmanagerille - tässä tapauksessa joukkue """

    def __init__(self, *args, **kwargs):
        for k, v in kwargs.iteritems():
            if k == "jasenet":
                v = json.loads(v)
            setattr(self, k, v)


    @staticmethod
    def hae(id):
        """ Etsi ja luo Joukkue -olion id:n perusteella """
        joukkue_data = Data().etsi_joukkue(id=id)
        return Joukkue(**joukkue_data)


@login_manager.user_loader
def user_loader(id):
    j = Joukkue.hae(int(id))
    return j


def varmista_joukkue(form, field):
    """ Validaattori joukkueen nimelle """

    joukkueen_nimi = field.data.strip().lower()
    kilpailu = int(form.kilpailu.data)

    app.logger.debug(u"Etsitään joukkuetta %s", field.data)

    try:
        joukkue = Data().etsi_joukkue(
            nimi=joukkueen_nimi,
            kilpailu=kilpailu
        )

        app.logger.debug(u"... Löydetty joukkue")
        if joukkue is not None:
            return joukkue

    except Exception as e:
        app.logger.error(u"Virhe joukkueen tarkistuksessa: %s", e)

    raise ValidationError(u"Virheellinen joukkue")


def varmista_salasana(form, field):
    """ Validaattori salasanalle """
    m = hashlib.sha512()

    try:
        joukkueen_nimi = form.joukkue.data.strip().lower()
        kilpailu = form.kilpailu.data

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
    kilpailu = SelectField(_(u"Kilpailu"), choices=[], validators=[], coerce=int)
    joukkue = StringField(_(u"Tunnus"), validators=[DataRequired(), varmista_joukkue])
    salasana = PasswordField(_(u"Salasana"), validators=[DataRequired(), varmista_salasana])
    kirjaudu = SubmitField(_(u"Kirjaudu sisään"))


def kirjautumissivu_kaava():
    kaava = KirjautumisKaava()
    kaava.kilpailu.choices = Data().listaa_lajit()

    if kaava.validate_on_submit():

        app.logger.info("Kirjautumisyritys: %s", kaava.joukkue.data)

        flash(_("Kirjautuminen onnistui."))

        joukkueen_nimi = kaava.joukkue.data.strip().lower()

        joukkue = Joukkue(**Data().etsi_joukkue(nimi=joukkueen_nimi))
        login_user(joukkue)

        app.logger.info(u"Joukkue '%s' kirjautui" % kaava.joukkue.data)

    return kaava


@bp.route('/kirjaudu', methods=('GET', 'POST'))
def kirjautumissivu():
    kaava = kirjautumissivu_kaava()

    if kaava.validate_on_submit() is True:
        return redirect(url_for('joukkueet.index'))
    else:
        return render_template("kirjaudu.html.j2", form=kaava)


@bp.route('/logout', methods=("GET", "POST"))
@login_required
def uloskirjautumissivu():
    r""" Uloskirjautumissivu.
    """

    if request.method in ["POST", "DELETE"]:
        logout_user()
        flash(_(u"Olet kirjautunut ulos"))
        return redirect(url_for('kirjautuminen.kirjautumissivu'))

    else:
        return render_template("uloskirjaudu.html.j2")


@rajapinta_nakyma
def kirjautumiskaava_nakyma():
    kaava = kirjautumissivu_kaava()
    return render_template("kirjaudu.html.j2", form=kaava, standalone=True)


class KirjautumisApi(Rajapinta):
    def get(self):
        r""" Palauttaa kirjautuneen käyttäjän.
            Huomaa, että palauttaa 200 aina, vaikkei oltaisi kirjauduttu
        """
        if current_user.is_authenticated:
            return current_user.__dict__
        app.logger.debug("Kirjautuminen vaaditaan")
        return {}, 200

    def delete(self):
        try:
            logout_user()
            return _("Olet kirjautunut ulos"), 200
        except Exception as e:
            app.logger.error("Virhe kirjauduttaessa ulos: %s", e)
            return _("Virhe kirjauduttuessa ulos:"), 500

    def post(self):
        kaava = kirjautumissivu_kaava()
        if kaava.validate() is True:
            return current_user.__dict__

        return kaava.errors, 403

