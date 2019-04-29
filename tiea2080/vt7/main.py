#!/usr/bin/python2
# -*- coding: utf-8 -*-

import os
from tiea2080 import alusta, init_app, cache, memcache
from tiea2080 import models, user, assets, feed

from requests_toolbelt.adapters import appengine
appengine.monkeypatch()

app = alusta()
app.config['DEBUG'] = app.debug = not os.getenv('SERVER_SOFTWARE', '').startswith('Google App Engine/')

init_app(app)

if app.debug:
    memcache.flush_all()

# Set up memcached
cache.init_app(app, config={'CACHE_TYPE': 'memcached'})

models.init_app(app)
user.init_app(app)
assets.init_app(app)
feed.init_app(app)
