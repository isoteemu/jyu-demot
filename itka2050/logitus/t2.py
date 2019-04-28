#!/usr/bin/python3

import sys
import logging

logging.basicConfig(stream=sys.stdout, level=logging.INFO)
logger = logging.getLogger()


def reminder(lender):
    logger.warning("Pay 100e to %s" % lender)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Käyttö: %s Logitettava viesti" % sys.argv[0])

    logger.info("Sanoit: %s" % sys.argv[1])
