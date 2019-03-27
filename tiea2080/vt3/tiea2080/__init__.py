#!/usr/bin/python2
# -*- coding: utf-8 -*-

__title__ = 'TIEA2080'
__version__ = 'VT3'

from flask import Flask, Response
import werkzeug.exceptions

from wsgiref.handlers import CGIHandler
from werkzeug.debug import DebuggedApplication

import logging
logger = logging.getLogger(__name__)
logger.addHandler(logging.NullHandler())


def alusta(**kwargs):
    r"""Alustaa flask ympäristön"""

    kwargs.setdefault("CHARSET", "utf-8")
    kwargs.setdefault("SECRET_KEY", b'\xb3\xc3)\x05\xf8\xdf07\t/;F\xca\xe1Z:\x95\xd9\x14\xe9\xd3\xfbx\x94\x9ek\x16w^\xd0\nC')
    kwargs.setdefault("DEBUG", True)

    app = Flask(__name__)
    app.config.update(kwargs)

    @app.errorhandler(werkzeug.exceptions.InternalServerError)
    def handle_internal_server_error(e):
        logger.error(u"Virhe ladattaessa sivua")
        return Response(u'Internal Server Error\n' + str(e), status=500)

    return app


def _(string, **variables):
    """Käännösfunktion placeholder.

    Ei tee mitään
    """

    return string if not variables else string % variables

