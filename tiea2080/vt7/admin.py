#!/usr/bin/python2
# -*- coding: utf-8 -*-

from main import app, setup
from tiea2080 import admin

if __name__ == "admin":
    admin.init_app(app)
    setup(app)
