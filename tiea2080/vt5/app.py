#!/usr/bin/python2.7
# -*- coding: utf-8 -*-

from flask_wtf import CSRFProtect

import logging
import sys

from tiea2080 import alusta, rajapinta, kirjautuminen, joukkueet

app = alusta()

with app.app_context():
    rajapinta.app_init(app)

    kirjautuminen.app_init(app)
    joukkueet.app_init(app)
    csrf = CSRFProtect(app)

    app.add_url_rule('/', endpoint='rajapinta.root')

if __name__ == '__main__':

    logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)

    with app.app_context():

        app.debug = True
        app.run(debug=True)
