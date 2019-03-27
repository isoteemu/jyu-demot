#!/usr/bin/env python
# -*- coding: utf-8 -*-

""" Demo 5 Tehtävä 3.
Käyttö lähettäjälle:
$ t3.py pub [topic]
tai käyttö kuuntelijalle
$ t3.py sub [topic]
"""

import redis
from random import randint, sample
from time import sleep
import io

SANAKIRJA = "/usr/share/dict/american-english"


def yhdista():
    r = redis.Redis(host='localhost', port=6379, db=0)
    return r


def pub(aihe):
    """ Lähettele redissiin satunnaisia sanoja """
    r = yhdista()

    with io.open(SANAKIRJA, encoding="utf-8") as f:
        sanoja = f.read().splitlines()

    while(True):
        viesti = u" ".join(sample(sanoja, randint(1, 5)))

        print(u"Lähetetään: %s" % viesti)
        r.publish(aihe, viesti)

        sleep(randint(5, 30)/10)


def sub(aihe):
    r = yhdista()
    p = r.pubsub(ignore_subscribe_messages=True)

    p.subscribe(aihe)

    for message in p.listen():
        print(u"Saatiin: %s" % message['data'])


if __name__ == "__main__":
    import argparse

    cmdline = argparse.ArgumentParser(description='TIEA345 Demo 5.3 Redis pub/sub')
    cmdline.add_argument('tila', nargs='?', default="sub", help="pub tai sub")
    cmdline.add_argument('aihe', nargs='?', default="TIEA456")

    args = cmdline.parse_args()

    if args.tila == "pub":
        print("pub tila aiheelle %s" % args.aihe)
        pub(args.aihe)
    else:
        print("sub tila aiheelle %s" % args.aihe)
        sub(args.aihe)
