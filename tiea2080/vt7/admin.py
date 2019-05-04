#!/usr/bin/python2
# -*- coding: utf-8 -*-

import os
from main import app, setup
from tiea2080 import admin

admin.init_app(app)
setup(app)
