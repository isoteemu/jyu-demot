## TIEA2080 RSS Lukija - viikkotehtävä 7
https://web.archive.org/web/20190429082156/http://appro.mit.jyu.fi/tiea2080/vt/vt7/


### Muutoksia edelliseen arvioituun:
 - Muutettu käyttöliittymää, ja vaihdettu "Discover Feeds" -> "Add Feed".
 - Uloskirjautuminen siirretty yhden lisäaskeleen taakse.
 - Feedien hallinta ja järjestely siirretty tunnuksen hallinan taakse ja vaihdettu pallurat listaksi. Tilauksen poisto vaihdettu Feedin yhteydessä olevasta isosta punaisesta miinuksesta napiksi jossa lukee "Remove" parantamaan löydettävyyttä.
 - Kasvatettu minimaalista modalin kokoa, jolloin myös artikkelit, joilla ei ole kuvaa, ainoastaan tekstiä, näyttää tekstit.
 - Vähennetty oletuksena näytettävien artikkeleiden määrää.
 - Vakava CGIn ja App Enginen eroista johtuva bugi korjattu, jonka vuoksi instanssin ensimmäinen käyttäjä säilyi palvelun muistissa, ja sotki template -järjestelmää.

 - Ja feminismi -feedejä ei lisätä enää uusille arvijoijille.

### Taso 1
 - Tallentaa RSS-syötteiden osoitteita ja niille annettuja nimiä. Nimet ovat feedin määrittelemiä, mutta käyttäjä voi omaan tilaukseensa sitä muokata, ja oman tilauksensa poistaa. Epätoimivaa osoitetta tai osoitetta, joka ei ole rss-syöte, ei tallenneta, mutta jos kyseessä on HTML sivu, siltä etsitään mahdolliset RSS / Atom Feedit.
 - Sovellus näyttää yksittäisen valitun syötteen sisältämien tuoreiden juttujen otsikot (title). Otsikkoteksti, tai jutun kuva aukaisee modalin josta voi jutun lukea. Modalin otsikkoteksti vie jutun varsinaiselle sivulle.
 - Sovellus ymmärtää RSS 2.0 ja ATOM -muotoisia syötteitä. Syötteen description-kenttä voi sisältää html-koodia. description-kentän sisältö sanitoidaan ja näytetään rajoitetussa määrin. HTML on aina validia.
 - Sovellus tallentaa ja muistaa syötteet käyttäjäkohtaisesti. Sovellukseen kirjaudutaan Googlen käyttäjätunnuksella. Salasanoja ei esiinny ohjelmakoodissa tai tietokannassa. Salausavaimet generoidaan instanssikohtaisesti.
 - ``http://www.neatorama.com/feed`` hylätään, koska se ei ole validi RSS syöte, ja ei täytä tehtävän vaatimuksia.

### Taso 3
Toteuttaa taso 1 -mukaisen sovelluksen seuraavilla lisäominaisuuksilla ja muutoksilla:

 - Käyttäjälle tarjotaan esivalittu lista syötteistä. Jos käyttäjä on ``Admin``, hänelle näytetään kaikki feedit admin liittymässä.
 - Syötteitä ei tallenneta kuin kerran kantaan, ja jokaisen syötteen ja käyttäjän välille muodostetaan relaatio ``Subscription``. Käyttäjä voi muokata tilauksensa tietoja.
 - Oletuksena syötteen juttulistaus sisältää vain juttujen otsikot ja/tai kuvat, mutta cardia klikattaessa avataan näkyviin laajempi tähän otsikkoon liittyvä sisältö (description ja kuva) modalissa. Jutun otsikko on linkki, joka vie suoraan jutun sivulle.
 - Haluamansa yksittäiset jutut voi merkitä suosikeiksi ja sovellus tallentaa ja muistaa ne. Suosikeista saa erikseen oman listauksen ja suosikkeja voi halutessaan järjestää ja poistaa.
 - Tallennettuja feedejä ja suosikkiartikkeleita voi laittaa haluamaansa järjestykseen ja sovellus muistaa järjestyksen. Jos järjestystä ei ole annettu niin feedejä ja suosikkiartikkelit listataan aakkosjärjestyksessä. Järjestäminen tapahtuu raahaamalla. 
 - Sovellus toimii järkevästi myös moderneissa mobiililaitteissa ja erikokoisilla näytöillä. Sovellus toteuttaa responsive web design -periaatteita.

### TODO:
 - Jos syöttessä on useampi kuin yksi kuva per artikkeli, ne olisi hyvä esittää myös.
