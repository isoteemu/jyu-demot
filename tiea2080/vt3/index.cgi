#!/home/tearautt/public_html/cgi-bin/tiea2080/venv/bin/python
# -*- coding: utf-8 -*-

from tiea2080 import alusta
from tiea2080 import kirjautuminen, joukkueet
from tiea2080.data import SuunnistusdataJson

from wsgiref.handlers import CGIHandler
from werkzeug.debug import DebuggedApplication
from flask import g

import logging, os

app = alusta()


@app.before_request
def lataa_data():
    g.data = SuunnistusdataJson(os.path.abspath("../../../tiea2080-vt3.json"))
    g.salasana = "4a9c1e10386291a3f7a86fff3eb585c4aacf2ffd8ee9d8d9f8b156efe53ad7c84b6b9ce81d16f11a121f9d56c542caf0313baa605418465c3f8b68f5021d16f2"


if __name__ == '__main__':

    logging.basicConfig(filename=os.path.abspath('../../../tiea2080-vt3.log'), level=logging.DEBUG)

    with app.app_context():
        # Rekisteröi polut
        app.register_blueprint(kirjautuminen.bp)
        app.register_blueprint(joukkueet.bp)

        app.add_url_rule('/', endpoint='joukkueet.index')

    handler = CGIHandler()
    handler.run(DebuggedApplication(app))


