#!/usr/bin/env python3

import Adafruit_DHT

class Demo3:

    TEMP_TYPE = 11
    PIN_TEMP = 21

    def __init__(self):
        pass

    def lampotila(self):
        sensoridata = self.sensori_temp()
        print("Lämpötila: %i Kosteus: %i" % sensoridata)

    def sensori_temp(self):
        return Adafruit_DHT.read_retry(self.TEMP_TYPE, self.PIN_TEMP)
if __name__ == "__main__":
    import argparse

    toiminnot = ["lampotila"]

    cmdline = argparse.ArgumentParser(description="TIEA345 Demo 3")
    cmdline.add_argument("toiminto", help="Suoritettava toiminto.",
                         choices=toiminnot)

    args = cmdline.parse_args()

    if args.toiminto in toiminnot:
        demo = Demo3()
        getattr(demo, args.toiminto)()
    else:
        cmdline.error("Ei toimintoa")