#!/usr/bin/python3

import sys
import logging

logging.basicConfig(stream=sys.stdout, level=logging.INFO)
logger = logging.getLogger()


def hakkuri(string, **variables):

    string = string if not variables else string.format(**{
        k: repr(v) for k, v in variables.items()
    })
    logger.info(string)


def reminder(lender):
    logger.warning("Pay 100e to %s" % repr(lender))


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Käyttö: %s Logitettava viesti" % sys.argv[0])

    hakkuri("Sanoit: {sana}", sana=sys.argv[1])
