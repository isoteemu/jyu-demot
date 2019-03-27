#!/usr/bin/python2.7
# -*- coding: utf-8 -*-

from flask import (
    Blueprint, render_template
)
from flask_restful import Resource, Api
from functools import wraps

bp = Blueprint('rajapinta', __name__)

""" api on Flask-RESTfulin rajapinta. Tälle on omat abstraktiot tässä tiedostossa.
Itse json-rajapinnat ovat :file:`joukkueet.py` ja :file:`kirjautuminen.py` tiedoston
lopussa. Hyvää suunnittelua olisi laittaa ne erikseen, mutta helpompaa näin pienessä
projektissa pitää ne samassa.
"""
api = Api(bp)

""" Decoroiduista näkymistä lista """
_rajapinta_nakymat = []


class Rajapinta(Resource):
    r"""
    Abstraktiotaso :class:`flask_restful.Resource`:lle.

    Ei aja muuta virkaa kuin on hyvää tulevaisuuteen päin varautumista.
    """
    pass


def lisaa_rajapinta(rajapinta, *urls, **kwargs):
    r""" Abstractio :class:`Flask-RESTful`in ympärille """
    api.add_resource(rajapinta, *urls, **kwargs)


def app_init(app):
    r"""
    Rekisteröi rajapinta.py toiminnallisuuden flask sovellukseen.

    Rajapinta on tämän jälkeen käytettävissä `url_prefix` urlin alla,
    joka on kovakoodattu tähän olemaan `/api/1.0`.
    """
    with app.app_context():
        app.register_blueprint(bp, url_prefix="/api/1.0")


@bp.route('/')
def root():
    r"""
    Luo staattisen "etusivun".
    Käy låpi `_rajapinta_nakymat` listan, ja suorittaa siihen rekisteröidyt
    funktiot. Funktioita voi rekisteröidä `rajapinta_nakyma()` decoraattorilla.
    """
    osiot = {}

    for nakyma in _rajapinta_nakymat:
        nimi = nakyma.__name__

        if nimi in osiot:
            raise RuntimeWarning(u"Osio %s on jo rekisteröity. Käytä eri nimeä" % nimi)

        osiot[nimi] = nakyma()

    return render_template('index.html.j2', osiot=osiot)


def rajapinta_nakyma(f):
    r"""
    Decorator html näkymille, jotka integroidaan etusivulle.
    """
    _rajapinta_nakymat.append(f)

    @wraps(f)
    def decorator(f):
        return f
    return decorator
