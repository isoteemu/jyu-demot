## TIEA2080 Google App Engine ja Datastore - viikkotehtävä 6
https://web.archive.org/web/20190401122759*/http://appro.mit.jyu.fi/tiea2080/vt/vt6/

Taso 1:
 - Sovelluksessa kaksi näkymää; ``/kilpailut`` ja ``/muokkaa``. Lisäksi salaisia, kuten ``/populate``, joka lataa esitiedot.
 - Sama crud näkymä hoitaa mallien muokkauksen. Ajatuksena oli käyttää ``model_form()ia``, mutta...
 - Kaavat ``WTFormsseja``, mutta eivät käytä ``WTForms-appengineä``. Kaavat ovat ``tiea2080/kaavat.py`` tiedostossa tarkistuksineen.
 - Malleja pääsee muokkaamaan ``/kilpailut`` sivulta klikkaamalla linkkejä. Uusia voi lisätä alla olevista ``Lisää uusi ...`` napeista. => "Sovelluksessa voi luoda, poistaa ja muuttaa valittuun kilpailuun liittyviä sarjoja"
 - Sovellus ei voi kaatua - sisältää salaisen tason ``0``, johon pääsee vain pahasti glitchaamalla.
 - Javascriptillä (``tiea2080/static/tiea2080-vt6.js``) varmistetaan poisto.
 - Kilpailun valinta on toteutettu javasciptillä, jolloin sekä Taso 1 ja Taso 3 kummatkin toteutuvat samalla ratkaisulla.
 - Aikahommeli herjaa, jos ei ole muotoa "%Y-%m-%dT%H:%M:%S". Selaimissa hieman eriäväisiä toetutuksia ``datetime-local``ille, ja viikkotehtävän tuntimäärät täynnä niin siihen ei ole javascript fallbackia.

Taso 3:
 - Koska mallidatassa kaikki joukkueet eivät täytä annettuja ehtoja, ja taso 1:llä erityisesti sanotaan ettei tarkistuksia saa poistaa, skippaan kaikki ``populate()`` -kutsussa joiden ehdot eivät täyty.
 - Listauksessa asiat ovat järjestyksessä. Koska taso on nelitasoinen baabelin torni, täyttää se myös "kolmitasoisen listan" vaatimukset.
 - Sovelluksessa voi lisätä, poistaa ja muuttaa valittuun kilpailuun liittyviä joukkueita. Kaava: ``tiea2080/kaavat.py`` ``JoukkueKaava()`` *JA NYT VASTA LÖYSIN FIELDLISTIN*. Muokkaukseen pääsee klikkaamalla joukkueen nimeä.
 - Jäsenten nimien yksilöllisyys joukkueen sisällä tarkastetaan, joukkueen yksilöllinen nimi sarjassa, sarja yksilöllinen nimi kilpailussa. Ja kilpailun yksilöllinen nimi.
 - Bonusta: Kaikki nimet normalisoidaan mallien ``nimi_lower`` attribuutteihin, eikä edes samannäköisiä nimiä sallita => kyrillinen ``o`` v. amerikkalainen ``о``.
 - Validointivirheet esitetään inputin vieressä kuten aikaisemmissa viikkotehtävissä. Näyttää vähän tiiviiltä.
 - Extrana javascript generoi joukkueen jäsenille oman emoji-ikonin.

Taso 5:
 - ``Rasti(ndb.Model)`` malli, joka varmistaa lat/lon arvot.
 - Kilpailukohtaisia rasteja pääsee katselemaan ja muokkaaman kilpailulistauksesta valitsemalla ``Rastit`` kilpailun nimen vierestä.
 - Joukkueen leimojen määrä näkyy kilpailulistauksessa, ja ne tallennetaan ``Joukkue`` -tietueeseen ``ndb.JsonProperty()``:na.
 - Orvoksi jäävät leimat jäävät kummittelemaan rakenteeseen, mutteivat kaada sovellusta.
