# -*- coding: utf-8 -*-

import os
from tiea2080 import alusta, init_app, cache
from tiea2080 import models, user, assets, feed

from werkzeug.debug import DebuggedApplication

from requests_toolbelt.adapters import appengine
import warnings
import urllib3.contrib.appengine


def setup_app(app):

    app.config.from_pyfile('application.cfg')

    init_app(app)

    if app.debug:
        app.wsgi_app = DebuggedApplication(app.wsgi_app, True)

    # Set up memcached
    cache.init_app(app, config={'CACHE_TYPE': 'memcached'})

    models.init_app(app)
    user.init_app(app)
    assets.init_app(app)
    feed.init_app(app)


def setup_appengine(app):
    r"""
    App Engine setup procedures for app.
    """

    appengine.monkeypatch()
    warnings.filterwarnings('ignore', r'urllib3 is using URLFetch', urllib3.contrib.appengine.AppEnginePlatformWarning)

    app.config.setdefault('DEBUG', os.getenv('SERVER_SOFTWARE', '').startswith('Google App Engine/'))
    app.debug = app.config['DEBUG']
