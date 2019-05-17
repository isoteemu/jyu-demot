#!/usr/bin/python2
# -*- coding: utf-8 -*-

r"""
Admin interface entry point for app engine instances
"""

from app import (
    setup_app,
    setup_appengine,
    alusta,
)
from tiea2080 import admin

admin_app = alusta()

setup_appengine(admin_app)

# Inject our own stuff.
admin.init_app(admin_app)

# Main setup
setup_app(admin_app)
