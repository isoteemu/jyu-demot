#!/usr/bin/python2
# -*- coding: utf-8 -*-

r"""
Tiedosto joka on kiertänyt viikkotehtävästä toiseen.

Vastaa Flaskin sovellustason alustuksesta
"""

__title__ = 'TIEA2080'
__version__ = 'VT6'

from flask import Flask, Response, render_template, flash
from flask_babel import Babel
from flask.logging import default_handler
import werkzeug.exceptions

import os

import logging
logger = logging.getLogger(__name__)
logger.addHandler(logging.NullHandler())


def alusta(**kwargs):
    r"""
    Alustaa flask ympäristön.
    
    :param CHARSET:
    :param SECRET_KEY:
    :param DEBUG:
    :param BABEL_DEFAULT_LOCALE:
    :param BABEL_DEFAULT_TIMEZONE:
    :param DATABASE:
    """

    app = Flask(__name__)

    kwargs.setdefault("CHARSET", "utf-8")
    kwargs.setdefault("SECRET_KEY", b'\xb3\xc3)\x05\xf8\xdf07\t/;F\xca\xe1Z:\x95\xd9\x14\xe9\xd3\xfbx\x94\x9ek\x16w^\xd0\nC')
    kwargs.setdefault("DEBUG", True)

    kwargs.setdefault("BABEL_DEFAULT_LOCALE", "fi")
    kwargs.setdefault("BABEL_DEFAULT_TIMEZONE", "EEST")

    app.config.update(kwargs)

    try:
        os.makedirs(app.instance_path)
    except OSError:
        pass

    Babel(app)

    logger = logging.getLogger(__name__)
    logger.addHandler(default_handler)

    return app


class Virhe(werkzeug.exceptions.InternalServerError):
    code = 500
    description = ("Tapahtui virhe.")


def app_init_virhe(app):
    r"""
    Määrittelee oman virhekäsittelijän.

    Hassu fakta, MegaDriven peleissä on monesti exception handler joka väittää
    sen olevan "salainen kenttä", jotta Segan QA ei hylkäisi peliä sen
    bugisuuden vuoksi. https://www.youtube.com/watch?v=i9bkKw32dGw
    """

    @app.errorhandler(werkzeug.exceptions.InternalServerError)
    def handle_internal_server_error(e):
        virhe_sivu = u"Palvelimen virhe:\n %s" % e
        try:
            app.logger.error(u"500: Kohdattiin virhe ladattaessa sivua: %s " % e)
            flash(u"%s" % e, "error")

            virhe_sivu = render_template("virhe.html.j2", virhe=e.description, routet=app.url_map.iter_rules())
        except Exception as e:
            app.logger.error(u"Virhe käsiteltäessä aikasempaa virhettä: %s " % e)
            pass
        return Response(virhe_sivu, status=500)
