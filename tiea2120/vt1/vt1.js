/* eslint-disable no-console */
// voit tutkia tarkemmin käsiteltäviä tietorakenteita konsolin kautta 
// tai json-editorin kautta osoitteessa http://jsoneditoronline.org/
// Jos käytät json-editoria niin avaa data osoitteesta:
// http://appro.mit.jyu.fi/tiea2120/vt/vt1/2019/data.json

// Seuraavilla voit tutkia selaimen konsolissa käytössäsi olevaa tietorakennetta. 
/*
console.log(data);

console.dir(data);
*/
// voit vapaasti luoda data-rakenteen pohjalta omia aputietorakenteita

// Kirjoita tästä eteenpäin oma ohjelmakoodisi

"use strict";

if(typeof(data) == undefined) var data = {};

/** 
 * Kirjoita yleiskäyttöinen Javascript-funktio, joka tulostaa selaimen konsoliin
 * console.log-komennolla data-tietorakenteen (data.json) sisältämien joukkueiden
 * nimet aakkosjärjestyksessä. Kunkin joukkueen nimi on tulostettava omalle rivilleen.
 * Älä tulosta mitään ylimääräistä. Isot ja pienet kirjaimet eivät ole merkityksellisiä
 * aakkostuksessa. 
 */
function tulostaJoukkueet(kasa=data) {
    let joukkueet = haeKaikkiJoukkueet(kasa);

    for (let joukkue of joukkueet.sort(_nimiSort)) {
        console.log(joukkue["nimi"]);
    }
}

function _nimiSort(a, b) {
    let nimi_a = a["nimi"].toUpperCase();
    let nimi_b = b["nimi"].toUpperCase();
    if(nimi_a > nimi_b) return 1;
    else if(nimi_a < nimi_b) return -1;
    else return 0;
}

/**
 * Palauttaa kaikki joukkueet listana.
 */
function haeKaikkiJoukkueet(data) {
    let joukkueet = [];
    for (let laji of data) {
        for(let sarja of laji["sarjat"]) {
            for(let joukkue of sarja["joukkueet"]) {
                joukkueet.push(joukkue);
            }
        }
    }
    return joukkueet;
}

/**
 * Kirjoita yleiskäyttöinen Javascript-funktio, joka osaa lisätä data-tietorakenteeseen
 * funktiolle parametrina tuodun joukkueen pyydettyyn sarjaan.
 * Jos parametrina annettua sarjaa (samanniminen sarja) ei löydy, niin funktio ei tee mitään. 
 */

function lisaaJoukkue(data, kilpailu, joukkue, sarja) {
    try {
        s(sarja, kilpailu)["joukkueet"].push(joukkue);
    } catch (error) {
        console.error("Ei voitu lisätä joukkuetta: ", error);
    }
    return data;
}

/**
 * Palauta feikkidata.
 */
function t1PseudoJoukkue() {
    return { 
        "nimi": "Mallijoukkue",
        "jasenet": [
            "Tommi Lahtonen",
            "Matti Meikäläinen"
        ],
        "id": 99999,
        "rastit": [],
    };
}

/**
 * Tulostaa kaikkien lajien rastit
 * 
 * Kirjoita Javascript-funktio, joka tulostaa konsoliin kaikkien kokonaisluvulla
 * alkavien rastien koodit aakkosjärjestyksessä. Jos rastin koodi on LÄHTÖ, niin 
 * kyseistä koodia ei tulosteta, mutta jos rastin koodi on 9D, niin tämä koodi 
 * tulostetaan. Tulosta kaikki rastikoodit konsoliin samalle riville pilkulla 
 * (;) eroteltuina.
 */
function tulostaRastit(data) {
    let rastit = [];
    for(let laji of data) {
        for(let rasti of laji["rastit"]) {
            if (isFinite(rasti["koodi"][0])) rastit.push(rasti["koodi"]);
        }
    }
    console.log(rastit.sort().join(";"));
}

/**
 * Poista joukkue datasta.
 * 
 * Lisää ohjelmaan yleiskäyttöinen funktio, joka osaa poistaa tietorakenteesta
 * kilpailun nimen, sarjan nimen ja joukkueen nimen perusteella joukkueen. 
 */
function poistaJoukkue(data, kilpailu, sarja, joukkue) {
    try {
        // Käydään sarjan joukkueet läpi
        let joukkueet = s(sarja, kilpailu, data)["joukkueet"];
        for(let idx in joukkueet) {
            if(joukkueet[idx]["nimi"] == joukkue)
                joukkueet.splice(idx, 1);

        }
    } catch (error) {
        console.warn("Ei voitu poistaa joukkuetta:", error);
    }
    return data;
}

function pisteytys(data) {
    let pisteet = laskePisteet(data);
    return pisteet;
}

/**
 * Käy läpi lajit, sarjat ja joukkueet laskien pisteet.
 * 
 * Rakenne ei ole "tasalaatuista" eli siinä esiintyy rasteja, joille ei
 * ole vastinetta rastit-rakenteessa. Kaikki nämä pitää osata käsitellä
 * jotenkin järkevästi eli vähintään hypätä yli eikä ohjelma saa kaatua
 * tai laskenta sekoilla.
 * 
 * Käy läpi koko tietorakenne ja etsi kunkin joukkueen käymät rastit
 * Joukkue saa kustakin rastista pisteitä rastin koodin ensimmäisen merkin
 * verran. Jos rastin koodi on 9A, niin joukkue saa yhdeksän (9) pistettä.
 * Jos rastin koodin ensimmäinen merkki ei ole kokonaisluku, niin kyseisestä
 * rastista saa nolla (0) pistettä. Esim. rasteista LÄHTÖ ja F saa 0 pistettä.
 */

function laskePisteet(data) {
    let pisteytys = [];
    let rastit = {};
    let joukkueen_pisteet = 0;
    let seen = [];
    let alkuaika = 0;
    let loppuaika = 0;

    let lahto_rasti = -1;
    let maali_rasti = -1;
    let lahto_aika = 0;
    let maali_aika = 0;
    let joukkueen_aika = 0;

    let matka = 0;
    let edellinen_rasti = 0;

    let rasti_aika = 0;

    // Ugh.....
    for(let laji of data) {
        // Lajin rastit muistiin
        rastit = lajinRastit(laji);

        // Taso 5: hae lähtö ja maalirastin koodit.
        try {
            lahto_rasti = r("LAHTO", laji, data)["id"];
            maali_rasti = r("MAALI", laji, data)["id"];

        } catch(error) {
            lahto_rasti = -1;
            maali_rasti = -1;
        }

        for(let sarja of laji["sarjat"]) {

            // Milloin kisa on ollut meneillään.
            alkuaika = (sarja["alkuaika"]) ? Date.parse(sarja["alkuaika"]) : Date.parse(laji["alkuaika"]);
            loppuaika = (sarja["loppuaika"]) ? Date.parse(sarja["loppuaika"]) : Date.parse(laji["loppuaika"]);

            for(let joukkue of sarja["joukkueet"]) {
                console.group(joukkue["nimi"], joukkue);

                let rid = 0;
                seen = [];
                joukkueen_pisteet = 0;
                joukkueen_aika = 0;

                lahto_aika = 0;
                maali_aika = 0;

                matka = 0;
                edellinen_rasti = 0;

                try {

                    for(let rasti of joukkue["rastit"].sort((a,b) => a["aika"] - b["aika"])) {
                        /*
                        Samasta rastista voi sama joukkue saada pisteitä vain yhden kerran. Jos
                        joukkue on leimannut saman rastin useampaan kertaan lasketaan kyseinen
                        rasti mukaan pisteisiin vain yhden kerran. Leimauksia, jotka tehdään
                        ennen lähtöleimausta tai maalileimauksen jälkeen, ei huomioida.
                        maalileimausta ei huomioida kuin vasta lähtöleimauksen jälkeen.
                        */

                        rid = String(rasti["rasti"]);

                        rasti_aika = Date.parse(rasti["aika"]);

                        if(lahto_aika == 0 && rid == lahto_rasti) {

                            console.debug("%s %s aloitti", joukkue["nimi"], rid, rasti["aika"], rastit[rid]);

                            lahto_aika = rasti_aika;
                            edellinen_rasti = rid;
                        } else if (lahto_aika == 0) continue; // Skippaa kaikki kunnes alku on löytynyt

                        if( seen.includes(rid)) {
                            continue;
                        }
                        seen.push(rid);

                        if(rasti_aika < alkuaika || rasti_aika > loppuaika) {
                            console.debug("Rasti on aikataulun ulkona", rastit[rid]);
                            //continue;
                        }

                        if(rid in rastit) {

                            if(edellinen_rasti != 0) {
                                let etaisyys = getDistanceFromLatLonInKm(
                                    rastit[rid]["lat"],
                                    rastit[rid]["lon"],
                                    rastit[edellinen_rasti]["lat"],
                                    rastit[edellinen_rasti]["lon"]
                                );
                                matka += etaisyys;
                            }
                            edellinen_rasti = rid;
                            joukkueen_pisteet += ("pisteet" in rastit[rid]) ? rastit[rid]["pisteet"] : 0;

                        }

                        if(rid == maali_rasti) {
                            maali_aika = rasti_aika;
                            console.debug("Joukkue %s saavutti maalin %s ajassa %s", joukkue["nimi"],  rasti["aika"], muotoileAika((maali_aika - lahto_aika) / 1000 ),rastit[rid]);
                            // Ohjeissa sanotaa "Leimauksia, jotka tehdään ennen lähtöleimausta tai maalileimauksen jälkeen, ei huomioida.",
                            // mutta on välttämätöntä huomioida jos haluaa ajan täsmäävän. 
                            //break;
                        }
                        if(maali_aika!=0 && maali_aika < rasti_aika) {
                            console.warn("Ulkopuolinen rasti:", rastit[rid]);
                        }

                    }
                } catch(error) {
                    console.warn(error);
                }

                joukkueen_aika = maali_aika - lahto_aika;
                pisteytys.push([joukkue, joukkueen_pisteet, matka, joukkueen_aika]);

                console.groupEnd();
            }
        }
    }
    return pisteytys;
}

/**
 * Palauttaa mapin rasteista ja niiden pisteistä.
 */
function lajinRastit(laji) {
    let rastit = {};
    let rid = "";
    for(let idx in laji["rastit"]) {
        rid = String(laji["rastit"][idx]["id"]);
        rastit[rid] = laji["rastit"][idx];
        rastit[rid]["pisteet"] = (isFinite(laji["rastit"][idx]["koodi"][0])) ? parseInt(laji["rastit"][idx]["koodi"][[0]]) : 0;
    }

    return rastit;
}

/**
 * Palauta laji datasta.
 */
function l(laji, kasa=data) {
    if(typeof(kasa) == "undefined") kasa = data;

    for(let idx in data) {
        if(data[idx]["nimi"] == laji) return kasa[idx];
    }
    throw "Lajia ei löytynyt";
}

/**
 * Palauta sarja datasta.
 */
function s(sarja, laji, kasa=data) {

    if(typeof(laji) == "string") laji = l(laji, kasa);

    for(let idx in laji["sarjat"]) {
        if(laji["sarjat"][idx]["nimi"] == sarja) return laji["sarjat"][idx];
    }
    throw "Sarjaa ei löytynyt";
}

/**
 * Palauta joukkue datasta.
 */
function j(joukkue, sarja, laji, kasa=data) {
    if(typeof(sarja) == "string") sarja = s(sarja, laji, kasa);
    
    for(let idx in sarja["joukkueet"]) {
        if(sarja["joukkueet"][idx]["nimi"] == joukkue)
            return sarja["joukkueet"][idx]["nimi"];
    }

    throw "Joukkuetta "+joukkue+" ei löytynyt";
}

/**
 * Palauta lajin rasti.
 */
function r(koodi, laji, kasa=data) {

    if(typeof(laji) == "string") laji = l(laji, kasa);
    for(let idx in laji["rastit"]) {
        if(laji["rastit"][idx]["koodi"] == koodi) return laji["rastit"][idx];
    }

    throw "Rastia koodilla "+koodi+" ei löytynyt";
}

function muotoileAika(timestamp) {

    let h = "00"+Math.floor(timestamp/60/60%24);
    let m = "00"+Math.floor(timestamp/60%60);
    let s = "00"+(timestamp%60);
    return h.substr(-2)+":"+m.substr(-2)+":"+s.substr(-2);
}

function getDistanceFromLatLonInKm(lat1,lon1,lat2,lon2) {
    var R = 6371; // Radius of the earth in km
    var dLat = deg2rad(lat2-lat1);  // deg2rad below
    var dLon = deg2rad(lon2-lon1); 
    var a = 
        Math.sin(dLat/2) * Math.sin(dLat/2) +
        Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) * 
        Math.sin(dLon/2) * Math.sin(dLon/2)
        ; 
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a)); 
    var d = R * c; // Distance in km
    return d;
}
  
function deg2rad(deg) {
    return deg * (Math.PI/180);
}

// Toggle switch debug tulostuksen hallintaan.
if(true) {
    console.debug = console.group = console.endGroup = function() {};

}

// Lisää edellä luomallasi funktiolla data-tietorakenteeseen yksi uusi joukkue
// Jäärogainingin 8h-sarjaan seuraavilla tiedoilla: 
data = lisaaJoukkue(data, "Jäärogaining", t1PseudoJoukkue(), "8h");

// Ohjelma tulostaa konsoliin kaikkien joukkueiden nimet aakkosjärjestyksessä.
// Mukana on oltava myös itse lisätty joukkue
tulostaJoukkueet(data);
console.log(""); // Esimerkin mukaista tulostusta varten.
tulostaRastit(data);

//
// * TASO 3
//

// Poista funktiollasi joukkueet, joiden nimet ovat: "Vara 1", "Vara 2" ja "Vapaat".
data = poistaJoukkue(data, "Jäärogaining", "8h", "Vara 1");
data = poistaJoukkue(data, "Jäärogaining", "8h", "Vara 2");
data = poistaJoukkue(data, "Jäärogaining", "4h", "Vapaat");

console.log("----------\nTaso 3\n----------\n\n");

let pisteet = pisteytys(data);
let taso3_pisteet = pisteet.sort(function(a,b) {
    if(a[1] == b[1]) return _nimiSort(a[0], b[0]);
    return b[1] - a[1];
});

for(let piste of taso3_pisteet) {
    console.log("%s (%i p)", piste[0]["nimi"], piste[1]);
}

console.log("----------\nTaso 5\n----------\n\n");


let taso5_pisteet = pisteet.sort(function(a,b) {
    if(a[1] == b[1]) return b[2] - a[2];
    return b[1] - a[1];
});

for(let piste of taso5_pisteet) {
    let aika = piste[3] / 1000;
    // Formatoinnissa täytyy käyttää matkassa roundia, %i == floor
    console.log("%s, %i p, %s km, %s", piste[0]["nimi"], piste[1], Math.round(piste[2]), muotoileAika(aika));
}

//console.dir(pisteet);