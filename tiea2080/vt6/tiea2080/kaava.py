#!/usr/bin/python2
# -*- coding: utf-8 -*-

from flask_babel import _
from flask_wtf import FlaskForm
from wtforms import validators as v, widgets as w
from wtforms.fields import StringField, SelectField, FormField, FieldList
from wtforms.fields.html5 import DateTimeLocalField, IntegerField
from .models import Kilpailu, Sarja, Joukkue, nimistin

from google.appengine.ext import ndb

r"""
    Nimivalidaattorit
    =================

    Validoi nimen, ja vastaa niiden yksilöllisyydestä.
    Hieman toistoa.
"""


def validate_kilpailun_nimi(form, field):
    nimi = nimistin(field.data)
    q = Kilpailu.query(Kilpailu.nimi_lower == nimi)

    for s in q:
        if form.Meta.avain is not s.key:
            raise v.ValidationError(_(u"Nimi on jo käytössä"))


def validate_sarjan_nimi(form, field):
    r"""
    Varmista ettei sarjan nimeä ole jo kilpailussa.
    """
    nimi = nimistin(field.data)
    kilpailu = ndb.Key(urlsafe=form.kilpailu.data)

    q = Sarja.query(Sarja.nimi_lower == nimi,
                    Sarja.kilpailu == kilpailu)

    for s in q:
        if form.Meta.avain is not s.key:
            raise v.ValidationError(_(u"Valitussa kilpailussa on jo sarja \"%(sarja)s\"", sarja=field.data))


def validate_joukkueen_nimi(form, field):
    r"""
    Varmista ettei joukkueen nimeä ole jo sarjassa.
    """
    nimi = nimistin(field.data)
    sarja = ndb.Key(urlsafe=form.sarja.data)

    q = Joukkue.query(Joukkue.nimi_lower == nimi,
                      Joukkue.sarja == sarja)

    for s in q:
        if form.Meta.avain is not s.key:
            raise v.ValidationError(_(u"Valitussa sarjassa on jo joukkue \"%(joukkue)s\"", joukkue=field.data))


r"""
    Avainviittaus
    =============

    Oma formin kenttä, joka muuttaa avaimen urlista :class:`ndb.Key()`
    muotoon.
"""

class Ristiviittaus(SelectField):
    r"""
    Selectfieldin korvike.

    Piempi purukumiviritelmä kuin ``wtforms-appengine``
    """
    def process_data(self, value):
        if isinstance(value, ndb.key.Key):
            value = value.urlsafe()
        return super(Ristiviittaus, self).process_data(value)


r"""
    Kaavat
    ======
"""


class KilpailuKaava(FlaskForm):
    nimi = StringField(_("Nimi"), validators=[v.InputRequired(), validate_kilpailun_nimi])
    alkuaika = DateTimeLocalField(_("Alkuaika"), format='%Y-%m-%dT%H:%M:%S', validators=[v.InputRequired()])
    loppuaika = DateTimeLocalField(_("Loppuaika"), format='%Y-%m-%dT%H:%M:%S', validators=[v.InputRequired()])
    kesto = IntegerField(_("Kesto"), validators=[v.InputRequired(), v.NumberRange(min=0, max=99)])

    def validate_alkuaika(form, field):
        if form.alkuaika.data > form.loppuaika.data:
            raise v.ValidationError(u"Ajan tulee olla kronologista")


class SarjaKaava(FlaskForm):
    # TODO: Varmista että kaavan nimi on kilpailussa yksilöllinen.
    nimi = StringField(_("Nimi"), validators=[v.InputRequired(), validate_sarjan_nimi])
    kesto = IntegerField(_("Kesto"))
    kilpailu = Ristiviittaus(_("Kilpailu"), validators=[v.InputRequired()])

    def __init__(self, *args, **kwargs):
        super(SarjaKaava, self).__init__(*args, **kwargs)
        self.kilpailu.choices = [(e.key.urlsafe(), e.nimi) for e in Kilpailu.query()]


class JoukkueKaava(FlaskForm):
    nimi = StringField(_("Nimi"), validators=[v.InputRequired(), validate_joukkueen_nimi])
    sarja = Ristiviittaus(_("Sarja"), validators=[v.InputRequired()])

    # KAIKEN TÄMÄN AJAN OLISIN VOINUT KÄYTTÄÄ :class:`FieldList`iä.
    jasenet = FieldList(StringField(u"Jäsen",
                                    validators=[v.Optional(), v.Length(min=2, max=127)]),
                        min_entries=5, max_entries=5, label=_(u"Jäsenet"))

    def __init__(self, *args, **kwargs):
        super(JoukkueKaava, self).__init__(*args, **kwargs)
        self.sarja.choices = [(e.key.urlsafe(), e.nimi) for e in Sarja.query()]

    def validate_jasenet(form, field):
        uniq_jasenet = list(set([j.strip() for j in field.data if j != ""]))
        _jasenet = list(set([nimistin(j) for j in uniq_jasenet]))
        print(_jasenet)
        if not len(_jasenet) >= 2:
            raise v.ValidationError(_(u"Yksilöllisiä nimiä vaaditaan vähintään %(nr)d", nr=2))

        return uniq_jasenet

    def validate_nimi(form, field):
        pass


class RastiKaava(FlaskForm):
    koodi = StringField(_("Koodi"), validators=[v.InputRequired()])
    kilpailu = Ristiviittaus(_("Kilpailu"), validators=[v.InputRequired()])
