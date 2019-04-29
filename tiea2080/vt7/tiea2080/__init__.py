#!/usr/bin/python
# -*- coding: utf-8 -*-

r"""
RSS Reader
~~~~~~~~~~

"""

__title__ = "TIEA2080"
__description__ = "RSS Reader"
__author__ = "Teemu Autto"
__version__ = "VT7"
__url__ = "https://github.com/isoteemu/jyu-demot/tree/master/tiea2080/vt7"

from flask import Flask, Response, render_template, flash
from flask_caching import Cache
from flask_babel import Babel, get_locale
from flask_wtf import CSRFProtect
from flask.logging import default_handler

from werkzeug.local import LocalProxy
import werkzeug.exceptions

from google.appengine.api import memcache

import os
import io

from .utils import *

import logging
logger = logging.getLogger(__name__)
logger.addHandler(logging.NullHandler())


csrf = CSRFProtect()
cache = Cache(config={'CACHE_TYPE': 'simple'})

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
    kwargs.setdefault("DEBUG", True)
    kwargs.setdefault("BABEL_DEFAULT_LOCALE", "fi")
    kwargs.setdefault("BABEL_DEFAULT_TIMEZONE", u"EEST")

    app.config.update(kwargs)

    try:
        os.makedirs(app.instance_path)
    except OSError:
        pass

    if not app.config.get("SECRET_KEY"):
        key_file = os.path.join(app.instance_path, "secret_key")

        try:
            with io.open(key_file, 'rb') as fd:
                app.config['SECRET_KEY'] = fd.read()
        except Exception as e:
            app.logger.exception(e)
            app.logger.info("Generating new SECRET_KEY %s", key_file)
            app.config['SECRET_KEY'] = os.urandom(64)
            with io.open(key_file, 'wb') as fd:
                fd.write(app.config['SECRET_KEY'])

    Babel(app)
    app.jinja_env.globals.update(get_locale=get_locale)

    logger = logging.getLogger(__name__)
    logger.addHandler(default_handler)

    return app


class Virhe(werkzeug.exceptions.InternalServerError):
    code = 500
    description = (u"Tapahtui virhe.")


def app_init_virhe(app):
    r"""
    Määrittelee oman virhekäsittelijän.

    Hassu fakta, MegaDriven peleissä oli monesti exception handler joka väittää
    sen olevan "salainen kenttä". Näin Segan QA ei toivottavasti hylkäisi peliä
    sen bugisuuden vuoksi. https://www.youtube.com/watch?v=i9bkKw32dGw
    """

    # @app.errorhandler(werkzeug.exceptions.InternalServerError)
    @app.errorhandler(Exception)
    def handle_internal_server_error(e):
        virhe_sivu = u"Palvelimen virhe:\n %s" % e
        try:
            app.logger.error(u"Kohdattiin virhe ladattaessa sivua: %s " % e)
            app.logger.exception(e)
            if isinstance(e, Virhe):
                flash(u"%s" % e, "error")

            virhe_sivu = render_template("virhe.html.j2", virhe=unicode(e), routet=app.url_map.iter_rules())
        except Exception as e:
            app.logger.error(u"Virhe käsiteltäessä aikasempaa virhettä: %s" % e)
            app.logger.exception(e)

        status = 500
        if isinstance(e, werkzeug.exceptions.HTTPException):
            status = e.code

        return Response(virhe_sivu, status=status)


def init_app(app):
    # Alustetaan flaskiin lisättävät moduulit
    with app.app_context():
        if not app.config['DEBUG']:
            # Estä kaatumiset.
            app_init_virhe(app)

        # Suojausta. Tehtävä ei vaadi, mutta eh.
        csrf.init_app(app)
