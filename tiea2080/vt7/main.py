#!/usr/bin/python2
# -*- coding: utf-8 -*-

r"""
Main entry point for app engine instance.
"""

from app import (
    alusta,
    setup_app,
    setup_appengine,
)

app = alusta()

setup_appengine(app)
setup_app(app)

if app.config.get("DEBUG"):
    import time
    from flask import g

    @app.before_request
    def before_request():
        g.start_time = time.time()

    @app.teardown_request
    def teardown_request(exception=None):
        diff = time.time() - g.start_time
        app.logger.info("Page generation took %f seconds.", diff)
