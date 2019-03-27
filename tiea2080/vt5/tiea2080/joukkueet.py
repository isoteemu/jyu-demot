#!/usr/bin/python2
# -*- coding: utf-8 -*-

from flask import (
    Blueprint, current_app as app, abort, flash, redirect, render_template, url_for, json
)
from flask_login import login_required, current_user
from flask_babel import _

from flask_wtf import FlaskForm
from wtforms import StringField, RadioField, SubmitField
from wtforms.validators import DataRequired, ValidationError, Optional

from .data import Data
from .rajapinta import Rajapinta, lisaa_rajapinta, rajapinta_nakyma

bp = Blueprint('joukkueet', __name__)

CHARSET = "utf-8"


def app_init(app):
    lisaa_rajapinta(JoukkueLista, "/joukkue")
    lisaa_rajapinta(JoukkueApi, "/joukkue/<int:joukkue_id>")

    with app.app_context():
        CHARSET = app.config['CHARSET']
        app.register_blueprint(bp, url_prefix="/joukkue")


def varmista_acl(joukkue=None):
    r""" Varmistaa, onko käyttäjällä pääsy kyseisen joukkueen tietohin. """

    try:
        if not current_user.is_authenticated:
            app.logger.debug(u"ACL Violation: Käyttäjä ei ole kirjautunut")
            return False
        elif joukkue and joukkue['id'] != current_user.id:
            app.logger.debug(u"ACL Violation: Käyttäjän joukkue ei täsmää")
            return False
        return True
    except Exception as e:
        app.logger.error(u"ACL tarkituksen virhe: %s" % e)

    # Aina epäonnistu virhetilanteissa
    return False


def validaattori_nimi(form, field):
    r""" Varmistaa nimen yksilöllisyyden """
    valid = True
    nimi = u"%s" % field.data.strip()

    try:
        joukkue = Data().etsi_joukkue(nimi=nimi)
        if joukkue['id'] != form.Meta.joukkue_id:
            app.logger.debug("Joukkueen nimi on jo käytössä eri joukkueella.")
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
def muokkaa_sivu(joukkue_id):
    joukkue = Data().etsi_joukkue(id=joukkue_id)
    varmista_acl(joukkue) or abort(403)

    kaava = muokkaa_kaava(joukkue)
    if kaava is True:
        return redirect(url_for('joukkueet.index'))
    else:
        return render_template("joukkue-muokkaa.html.j2", form=kaava)


def muokkaa_kaava(joukkue):
    nr_jasenia = 5

    sarjat = [(e['id'], e['nimi']) for e in Data().listaa_sarjat()]
    joukkueen_jasenet = json.loads(joukkue['jasenet'], encoding=CHARSET) + ([u""] * nr_jasenia)

    class F(FlaskForm):
        pass

    setattr(F, "nimi", StringField(_(u"Nimi"), validators=[DataRequired(), validaattori_nimi], default=unicode(joukkue['nimi'])))
    setattr(F, "sarja", RadioField(_(u"Sarja"), choices=sarjat, validators=[DataRequired()], default=joukkue['sarja'], coerce=int))
    setattr(F.Meta, "joukkue_id", joukkue['id'])

    for i in range(0, nr_jasenia):
        _validators = []
        if i < 2:
            _validators += [DataRequired()]
        else:
            _validators += [Optional()]

        setattr(F, "jasen_%d" % i, StringField(_(u"Jäsen #%d") % (i + 1), validators=_validators, default=joukkueen_jasenet[i]))

    setattr(F, "tallenna", SubmitField(_(u"Tallenna")))

    kaava = F()

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

        except Exception as e:

            Data().kumoa()
            app.logger.error(e)

            flash(_("Tietoja ei voitu tallentaa"))

    return kaava


@rajapinta_nakyma
def muokkaa_joukkuetta():
    joukkue = {"nimi": "", "id": "", "jasenet": u"[]", "sarja": ""}
    kaava = muokkaa_kaava(joukkue)
    return render_template("joukkue-muokkaa.html.j2", form=kaava, standalone=True)

@rajapinta_nakyma
def listaa_joukkueet():
    return render_template("joukkueet.html.j2", standalone=True)


class JoukkueLista(Rajapinta):
    r""" Joukkuelistaamisesta vastaava rajapinta """
    def get(self):

        if not varmista_acl():
            return _(u"Listaus vaatii kirjautumisen"), 403

        lista = []
        for joukkue in Data().listaa_joukkueet():
            joukkue = dict(joukkue)
            joukkue['jasenet'] = json.loads(joukkue['jasenet'])

            lista.append(joukkue)
        return lista


class JoukkueApi(Rajapinta):
    """ Joukkueen muokkaukseen littyvä rajapinta """
    def get(self, joukkue_id):

        joukkue = Data().etsi_joukkue(id=joukkue_id)

        if not varmista_acl(joukkue):
            return _(u"Riittämättömät oikeudet"), 403

        kaava = muokkaa_kaava(joukkue)

        return kaava.data

    def post(self, joukkue_id):

        joukkue = Data().etsi_joukkue(id=joukkue_id)
        if not varmista_acl(joukkue):
            return _(u"Riittämättömät oikeudet"), 403

        kaava = muokkaa_kaava(joukkue)

        if kaava.validate() is True:
            return kaava.data, 201

        return kaava.errors, 400

