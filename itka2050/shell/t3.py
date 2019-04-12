#!/usr/bin/python3

r""" Suorittaa komennon shellissä """

import subprocess
import shlex
import sys

if __name__ == "__main__":
    if not len(sys.argv) >= 2:
        print("Tulostaa sovelluksen palauttamien rivien määrän.")
        print("Käyttö: %s <käynnistettävä komento>" % sys.argv[0])
        sys.exit(1)

    # Suoritetaan prosessi. Heittää exeptionin jos 404.

    komento = "{komento} {arg}".format(
        komento=sys.argv[1],
        arg=" ".join(map(shlex.quote, sys.argv[2:]))
    )
    prosessi = subprocess.run(komento, capture_output=True, shell=True, close_fds=True, env={})

    print("Suoritettu komento:\n\t$ %s" % prosessi.args)

    if prosessi.returncode != 0:
        print("[FAIL] Sovellus piti suoritusta erheellisenä, ja palautti %d merkkiä ongelmistaan" % len(prosessi.stderr))
    else:
        print("[SUCCESS] Sovellus palautti %d merkkiä" % len(prosessi.stdout))

    sys.exit(prosessi.returncode)

