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
