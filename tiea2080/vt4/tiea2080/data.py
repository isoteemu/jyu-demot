#!/usr/bin/python2
# -*- coding: utf-8 -*-

r""" Wrapperi sqlite3 -kannan ympärille. """

import click
from flask.cli import with_appcontext
from flask import current_app, g

import sqlite3

import logging

logger = logging.getLogger(__name__)
logger.addHandler(logging.NullHandler())


def init_app(app):
    r""" Flaskin hook, jolla tietokanta yhdistetään sovellustasoon """

    app.teardown_appcontext(sulje_yhteys)
    app.cli.add_command(alusta_tietokanta)


@click.command('alusta-tietokanta')
@with_appcontext
def alusta_tietokanta(**kwargs):
    r""" Lukee tietokannan kuvauksen ja luo tietokannan rakenteen

    :param schema: Tietokannan luontiin käytettävä sql -tiedosto.
    """
    kwargs.setdefault("schema", "tietokanta.sql")

    db = avaa_yhteys()

    logger.debug("Luodaan tietokanta schemasta %s" % kwargs['schema'])
    with current_app.open_resource(kwargs['schema']) as f:
        db.executescript(f.read().decode("utf-8"))

    click.echo("Tietokanta luotu.")


def avaa_yhteys(**kwargs):
    r""" Muodostaa yhteyden tietokantaan, ja sitoo sen g.db -olioon.

    :param database: Sqlite3 resurssi johon yhdistetään.
    """
    kwargs.setdefault("database", current_app.config['DATABASE'])

    if 'db' not in g:
        try:
            g.db = sqlite3.connect(
                kwargs['database'],
                detect_types=sqlite3.PARSE_DECLTYPES
            )
        except sqlite3.OperationalError as e:
            logger.error("Ei voitu avata tietokantaa: %s", e)
            raise e

        logger.debug("Muodostettu tietokantayhteys tietokantaan %s", kwargs['database'])

    g.db.row_factory = sqlite3.Row

    # Asetetaan tietokantayhteydessä viite-eheydet (foreign keys) käyttöön
    g.db.execute("PRAGMA foreign_keys = ON")

    return g.db


def sulje_yhteys(self):
    db = g.pop('db', None)

    if db is not None:
        db.close()


class Data():

    def __init__(self):
        self.db = avaa_yhteys()

    def listaa_lajit(self):
        lajit = self.db.execute("SELECT id, nimi FROM kilpailut ORDER BY nimi").fetchall()

        return lajit

    def listaa_sarjat(self):
        sarjat = self.db.execute("SELECT * FROM sarjat ORDER BY nimi").fetchall()
        return sarjat

    def etsi_laji(self):
        raise NotImplementedError

    def etsi_sarja(self):
        raise NotImplementedError

    def etsi_joukkue(self, **kwargs):
        sql = "SELECT *, s.nimi as sarjan_nimi, k.nimi as kilpailun_nimi FROM joukkueet j, sarjat s, kilpailut k WHERE j.sarja = s.id AND s.kilpailu = k.id"

        if "id" in kwargs:
            sql += " AND j.id = :id"

        elif "nimi" in kwargs:
            sql += " AND j.nimi LIKE :nimi"
            if "sarja" in kwargs:
                sql += " AND j.sarja = :sarja"

        if "kilpailu" in kwargs:
            sql += " AND k.id = :kilpailu"

        joukkue = self.db.execute(sql, kwargs).fetchone()
        return joukkue


    def listaa_joukkueet(self):
        """ Listaa kaikki joukkueet """
        joukkueet = self.db.execute("""SELECT *, s.nimi as sarjan_nimi, k.nimi as kilpailun_nimi
            FROM joukkueet j, sarjat s, kilpailut k
            WHERE j.sarja = s.id AND s.kilpailu = k.id
        """).fetchall()
        return joukkueet


    def paivita_joukkue(self, joukkue):
        self.db.execute("UPDATE joukkueet SET nimi = :nimi, sarja = :sarja, jasenet = :jasenet WHERE id = :id", joukkue)

    def tallenna(self):
        self.db.commit()

    def kumoa(self):
        self.db.rollback()
