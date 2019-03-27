Toteuttaa Taso 1:sen, jotain satunnaista Taso 3:sesta ja Taso 5:sesta. Yleistä:
 - Kirjautumisen hallinta on  ``Flask-Login``, ja json rajapinta on ``Flask-RESTful``.
 - Käyttää javascriptissä routtaukseen ``SammyJS`` -kirjastoa, joka on jQueryn plugin. Back nappi toimii.
 - CSRF suojaus on käytössä myös ajax-kutsuissa.
 - Javascriptissä minimaallinen ACL, reagoi palvelimen antamiin virheisiin.
 - Lukuunottamatta kirjaudu ulos -nappulaa, kaikki kaavat on wtformsilla toteutettu. Javascriptissä liimataso, jonka avulla uusien kaavojen luonti wtformsien avulla olisi jouhevaa. Ikävä vain miten epäpythonmainen wtfroms on, jonka vuoksi sitä ei halua käyttää.
 - Aikaisempien tasojen koodit vielä sotkemassa. Graceful degragaatio olisi suht helppoa toteuttaa, mutta asiakas ei tajunnut vaatia ennen tilausvahvistuksen allekirjoittamista.

Taso 1:
 - Toteuttaa muutoin, mutta ei kirjaa automaattisesti Kotiloita, vaan vaatii Taso 3:sen mukaisen kirjautumisen.

Taso 3:
 - Kirjautuminen on, mutta admin liittymää ei.

Taso 5:
 - Ajax-operaatioihin liitetty selkeä latausindikaattori, jota näytetään, kunnes operaatio on suoritettu.
 - Käytää ajax-kutsuissa vain JSON-muotoa.
 - Kaikki sovelluksen lomakkeilla tietokannasta tarvittavat tiedot ladataan ajax-kutsujen avulla.
 - Admin liittymää ei ole.
 