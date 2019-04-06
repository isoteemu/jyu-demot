#!/usr/bin/env python3

from sqlalchemy import (
    create_engine, Column, Integer, String, ForeignKey, Boolean
)
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship, sessionmaker

# Yhdistetään tietokantaan
tietokanta = create_engine('sqlite:///:memory:', echo=True)
orm_sessio = sessionmaker(bind=tietokanta, autocommit=True)()
_Base = declarative_base()


class Wrestler(_Base):
    __tablename__ = 'wrestlers'

    nr = Column(Integer, primary_key=True)
    name = Column(String)
    promotion_id = Column(Integer, ForeignKey('promotions.cm_id'))
    pwi = Column(Integer)
    gimmicks = relationship("Gimmick", backref='wrestler')

    ranks_risen = None


class Gimmick(_Base):
    __tablename__ = 'gimmicks'

    id = Column(Integer, primary_key=True)
    wrestler_nr = Column('wrestler_cm', Integer, ForeignKey('wrestlers.nr'))
    gimmick = Column(String)
    primary = Column(Boolean)
    alias_of = Column(Integer, ForeignKey('gimmicks.id'))


class Promotion(_Base):
    __tablename__ = 'promotions'

    cm_id = Column(Integer, primary_key=True)
    name = Column(String)
    abbrevation = Column(String)

    wrestlers = relationship("Wrestler", backref='promotion')


# Luodaan taulut
_Base.metadata.create_all(tietokanta)


if __name__ == "__main__":

    # Lisätään oma ORM mallisesti.
    orm_sessio.add(Promotion(
        name="World Wildlife Federation",
        abbrevation="WWF",
    ))

    promo = orm_sessio.query(Promotion).one()
    assert promo.abbrevation == "WWF"
    print(">>> Lisättiin ORM mallisesti promootio", promo.name)

    # Lisätään SQL
    sql = r"""INSERT INTO wrestlers (name, promotion_id) VALUES (:nimi, :promootio_id)"""
    nimi = "Hulk Hogan"
    orm_sessio.connection().execute(
        sql,
        nimi=nimi,
        # Typecastaan sen virheelliseksi tyypiksi, just for shits 'n giggels
        promootio_id="%s" % promo.cm_id
    )

    hahmo = orm_sessio.query(Wrestler).one()
    assert hahmo.name == nimi
    print(">>> Lisättiin Prepared stetementtina", hahmo.name, "promootioon", hahmo.promotion.name)

