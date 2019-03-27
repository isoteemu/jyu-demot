#!/usr/bin/python2
# -*- coding: utf-8 -*-

__title__ = 'TIEA2080'
__version__ = 'VT4'

from flask import Flask, Response
from flask_babel import Babel
from flask.logging import default_handler
import werkzeug.exceptions

import os

from . import data

import logging
logger = logging.getLogger(__name__)
logger.addHandler(logging.NullHandler())


def alusta(**kwargs):
    r"""Alustaa flask ympäristön"""

    app = Flask(__name__)

    kwargs.setdefault("CHARSET", "utf-8")
    kwargs.setdefault("SECRET_KEY", b'\xb3\xc3)\x05\xf8\xdf07\t/;F\xca\xe1Z:\x95\xd9\x14\xe9\xd3\xfbx\x94\x9ek\x16w^\xd0\nC')
    kwargs.setdefault("DEBUG", True)

    kwargs.setdefault("BABEL_DEFAULT_LOCALE", "fi")
    kwargs.setdefault("BABEL_DEFAULT_TIMEZONE", "EEST")

    kwargs.setdefault("DATABASE", os.path.join(app.instance_path, "tiea2080.sqlite3"))

    app.config.update(kwargs)

    try:
        os.makedirs(app.instance_path)
    except OSError:
        pass

    @app.errorhandler(werkzeug.exceptions.InternalServerError)
    def handle_internal_server_error(e):
        logger.error(u"Virhe ladattaessa sivua")
        return Response(u'Internal Server Error\n' + str(e), status=500)

    Babel(app)
    data.init_app(app)

    logger = logging.getLogger(__name__)
    logger.addHandler(default_handler)

    return app
