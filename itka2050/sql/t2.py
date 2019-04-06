#!/usr/bin/env python3

import sys
from t1 import orm_sessio, Promotion

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(">>> Käyttö: %s <promootion nimi>" % sys.argv[0])
        sys.exit(1)

    nimi = u" ".join(sys.argv[1:])

    abbr = "".join([x[0].upper() for x in nimi.strip().split(" ")])
    sql = "INSERT INTO promotions (name, abbrevation) VALUES ('" + nimi + "', '" + abbr + "')"
    print(">>> liimattu SQL", sql)
    orm_sessio.connection().execute(sql)

    print(">>> Lisätty", orm_sessio.query(Promotion).filter(Promotion.name == nimi).one().abbrevation)
