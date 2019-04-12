#!/usr/bin/python3

r""" Käynnistää toisen ohjelman. """

import subprocess
import sys

if __name__ == "__main__":
    if not len(sys.argv) >= 2:
        print("Tulostaa sovelluksen palauttamien rivien määrän.")
        print("Käyttö: %s <käynnistettävä komento>" % sys.argv[0])
        sys.exit(1)

    # Suoritetaan prosessi. Heittää exeptionin jos 404.
    prosessi = subprocess.run(sys.argv[1:], capture_output=True)

    if prosessi.returncode != 0:
        print("[FAIL] Sovellus piti suoritusta erheellisenä, ja palautti %d merkkiä ongelmistaan" % len(prosessi.stderr))
    else:
        print("[SUCCESS] Sovellus palautti %d merkkiä" % len(prosessi.stdout))
    
    sys.exit(prosessi.returncode)

