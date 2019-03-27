#!/usr/bin/python2.7
# -*- coding: utf-8 -*-

from flask.logging import default_handler
from flask_wtf.csrf import CSRFProtect

import logging
import sys

from tiea2080 import alusta, kirjautuminen, joukkueet

app = alusta()

with app.app_context():
    kirjautuminen.app_init(app)
    joukkueet.app_init(app)
    csrf = CSRFProtect(app)

    app.add_url_rule('/', endpoint='index')


if __name__ == '__main__':

    logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)

    with app.app_context():

        app.add_url_rule('/', endpoint='joukkueet.index')
        app.debug = True
        app.run(debug=True)
