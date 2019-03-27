#!/home/tearautt/public_html/cgi-bin/tiea2080/venv/bin/python
# -*- coding: utf-8 -*-

"""Tämä on entrypoint tiedosto www-palvelimelle"""

from wsgiref.handlers import CGIHandler
from werkzeug.debug import DebuggedApplication

import logging
import os

from app import app

if __name__ == '__main__':

    logging.basicConfig(filename=os.path.join(app.instance_path, 'tiea2080.log'), level=logging.INFO)

    handler = CGIHandler()
    handler.run(DebuggedApplication(app))
