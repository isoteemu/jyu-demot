#!/usr/bin/python2
# -*- coding: utf-8 -*-

r"""
Tiedosto Google App Engine sovellusten debuggaukseen Visual Studio Codessa.

Please ignore.
"""

import sys
import os

# Assuming that pdvsd is located in the working folder
sys.path.append(os.path.join(os.getcwd(), "lib"))

import ptvsd
# Modify the port number as desired; you're debugging locally so the values don't matter.
# However, be sure the port is not blocked on your computer.
ptvsd.enable_attach(address=('0.0.0.0', 3000), redirect_output=True)

# The debug server has started and you can now use VS Code to attach to the application for debugging
print("Google App Engine has started; ready to attach the debugger")

