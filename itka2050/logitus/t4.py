#!/usr/bin/python3

import sys
import logging

from systemd.journal import JournalHandler

logger = logging.getLogger()
logger.addHandler(JournalHandler())
logger.setLevel(logging.INFO)


def reminder(lender):
    logger.warning("Pay 100e to %s" % lender)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Käyttö: %s Logitettava viesti" % sys.argv[0])

    logger.info("Sanoit: %s" % sys.argv[1])
