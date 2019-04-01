#!/usr/bin/python2
# -*- coding: utf-8 -*-

from flask_babel import _
from flask_wtf import FlaskForm
from wtforms import validators as v
from wtforms.fields import StringField, SelectField
from wtforms.fields.html5 import DateTimeLocalField, IntegerField
from .models import Kilpailu, Sarja, nimistin

from google.appengine.ext import ndb


def validate_kilpailun_nimi(form, field):
    if not validate_nimi(form, field, Kilpailu):
        raise v.ValidationError(u"Nimi on jo käytössä. Valitse toinen.")


def validate_sarjan_nimi(form, field):
    if not validate_nimi(form, field, Sarja):
        raise v.ValidationError(u"Nimi on jo käytössä. Valitse toinen.")


def validate_nimi(form, field, malli):
    r""" Varmistaa nimen yksilöllisyyden """
    nimi = nimistin(field.data)
    q = Kilpailu.query(Kilpailu.nimi_lower == nimi).fetch(1)
    if len(q):
        e = q[0]
        if hasattr(form.Meta, 'avain') and form.Meta.avain is not e.key:
            return False

    return True


class Ristiviittaus(SelectField):
    r"""
    Selectfieldin korvike.

    Piempi purukumiviritelmä kuin ``wtforms-appengine``
    """
    def process_data(self, value):
        if isinstance(value, ndb.key.Key):
            value = value.urlsafe()
        return super(Ristiviittaus, self).process_data(value)


class KilpailuKaava(FlaskForm):
    nimi = StringField(_("Nimi"), validators=[v.InputRequired(), validate_kilpailun_nimi])
    alkuaika = DateTimeLocalField(_("Alkuaika"), format='%Y-%m-%dT%H:%M:%S', validators=[v.InputRequired()])
    loppuaika = DateTimeLocalField(_("Loppuaika"), format='%Y-%m-%dT%H:%M:%S', validators=[v.InputRequired()])
    kesto = IntegerField(_("Kesto"), validators=[v.InputRequired(), v.NumberRange(min=0, max=99)])

    def validate_alkuaika(form, field):
        if form.alkuaika.data > form.loppuaika.data:
            raise v.ValidationError(u"Ajan tulee olla kronologista")


class SarjaKaava(FlaskForm):
    nimi = StringField(_("Nimi"), validators=[v.InputRequired()])
    kesto = IntegerField(_("Kesto"))
    kilpailu = Ristiviittaus(_("Kilpailu"), validators=[v.InputRequired()])

    def __init__(self, *args, **kwargs):
        super(SarjaKaava, self).__init__(*args, **kwargs)

        self.kilpailu.choices = [(e.key.urlsafe(), e.nimi) for e in Kilpailu.query()]
