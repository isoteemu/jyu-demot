#!/usr/bin/python2
# -*- coding: utf-8 -*-

import os
from tiea2080 import alusta, init_app, cache, memcache
from tiea2080 import models, user, assets, feed

from werkzeug.debug import DebuggedApplication

from requests_toolbelt.adapters import appengine
import warnings
import urllib3.contrib.appengine

app = alusta()
app.config['DEBUG'] = app.debug = not os.getenv('SERVER_SOFTWARE', '').startswith('Google App Engine/')


def setup(app):

    appengine.monkeypatch()
    warnings.filterwarnings('ignore', r'urllib3 is using URLFetch', urllib3.contrib.appengine.AppEnginePlatformWarning)

    app.config.setdefault('DEBUG', os.getenv('SERVER_SOFTWARE', '').startswith('Google App Engine/'))
    app.debug = app.config['DEBUG']

    init_app(app)

    if app.debug:
        app.wsgi_app = DebuggedApplication(app.wsgi_app, True)


    # Set up memcached
    cache.init_app(app, config={'CACHE_TYPE': 'memcached'})

    models.init_app(app)
    user.init_app(app)
    assets.init_app(app)
    feed.init_app(app)


if __name__ == "main":
    # Notice: its not ``__main__```
    setup(app)
