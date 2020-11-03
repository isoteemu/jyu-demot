#!/usr/bin/env pytho3

""" Muunna SVG:n polku listaksi koordinaatteja

    Käyttö:
        $ svg-to-path.py <tiedosto> <g-id>

    antamalla `g-id`:n jolle löytyy vastaavalla ID:llä `<g>` -tagi (group),
    palauttaa vain sen ryhmän polut.
"""

import sys
from svg.path.parser import parse_path
from svg.path.path import Close, Line, Move
import xml.etree.ElementTree as etree
from pathlib import Path


def svg_to_path(svg_line):
    line = parse_path(svg_line)
    r = []
    for seg in line:
        if isinstance(seg, Line):
            r += [(seg.start.real, seg.start.imag)]
        elif isinstance(seg, Close):
            r += [(seg.start.real, seg.start.imag)]
            r += [(seg.end.real, seg.end.imag)]
        elif isinstance(seg, Move):
            pass
        else:
            print("TUNTEMATON", seg)

    return r


if __name__ == "__main__":
    if Path(sys.argv[1]).is_file():
        svg = etree.parse(sys.argv[1])
        if len(sys.argv) == 3:
            lines = svg.findall("*/{http://www.w3.org/2000/svg}g[@id='%s']/{http://www.w3.org/2000/svg}path" % sys.argv[2])
        else:
            lines = svg.findall("**/{http://www.w3.org/2000/svg}path")
        for path in lines:
            title = ""
            try:
                title = "-- %s" % path.find("{http://www.w3.org/2000/svg}title").text
            except AttributeError:
                pass

            print(svg_to_path(path.get("d")), ",", title)

    else:
        print(svg_to_path(" ".join(sys.argv[1:])))
