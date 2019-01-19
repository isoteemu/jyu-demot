// data-muuttuja on sama kuin viikkotehtävässä 1.
//

"use strict";

//console.log(data);

function listaaJoukkueet(kasa=data) {
    let r = [];
    for (let laji of kasa) {
        for(let sarja of laji["sarjat"]) {
            for(let joukkue of sarja["joukkueet"]) {
                r.push([
                    laji,
                    sarja,
                    joukkue
                ]);
            }
        }
    }

    return r;
}

/**
 * Järjestää joukkueet Taso 1:sen mukaisesti
 */
function t1_järjestäJoukkueet(lista) {
    return lista.sort(function(a,b) {
        let r = _nimiSort(a[1], b[1]);
        return (r != 0) ? r : _nimiSort(a[2], b[2]);
    });
}

/**
 * Järjestää joukkueet Taso 3:sen mukaisesti
 */
function t3_järjestäJoukkueet(lista) {
    return lista.sort(function(a,b) {
        let r = _nimiSort(a[1], b[1]);
        return (r != 0) ? r : (b[2]["pisteet"] - a[2]["pisteet"]);
    });
}

function _nimiSort(a, b) {
    let nimi_a = a["nimi"].toUpperCase();
    let nimi_b = b["nimi"].toUpperCase();
    if(nimi_a > nimi_b) return 1;
    else if(nimi_a < nimi_b) return -1;
    else return 0;
}

/**
 * Käy läpi lajit, sarjat ja joukkueet laskien pisteet.
 * 
 * Muutoksia VT1:seen verrattuna: Pisteet lisätään joukkue -rakenteeseen.
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
    let pisteytys = {};
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

                            lahto_aika = rasti_aika;
                            edellinen_rasti = rid;
                        } else if (lahto_aika == 0) continue; // Skippaa kaikki kunnes alku on löytynyt

                        if( seen.includes(rid)) {
                            continue;
                        }
                        seen.push(rid);

                        if(rasti_aika < alkuaika || rasti_aika > loppuaika) {
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

                joukkue["pisteet"] = joukkueen_pisteet;
                joukkue["matka"]   = matka;
                joukkue["aika"]    = maali_aika - lahto_aika;

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
 * Käy läpi lajit, ja palauttaa kaikki rastit.
 */
function kaikkiRastit(kasa=data) {
    let rastit = {};
    for(let laji of kasa) {
        Object.assign(rastit, lajinRastit(laji));
    }

    return rastit;
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

/**
 * Palauta laji datasta.
 */
function l(laji, kasa=data) {
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

/**
 * suositun kirjaston tapainen wräpperi helpottamaan dom-manipulaatiota.
 */
function H(el) {
    if(el instanceof HQ)
        return el;
    return new HQ(el);
}

function HQ(el) {
    this.node;

    if(el instanceof HQ)
        this.node = el.node;
    else if(typeof(el) == "string")
        this.node = document.createElement(el);
    else
        this.node = el;

    return this;
}

HQ.prototype = {

    /**
     * Etsi elementtiä puusta.
     */
    hae: function(elementti) {
        let n = this.node.getElementsByTagName(elementti);
        if(n.length >= 1) {
            return H(n[0]);
        } else {
            n = H(elementti);
            this.append(n);
            return n;
        }
    },

    /**
     * Luo tekstinode.
     */
    text: function(txt) {
        let tn = document.createTextNode(txt);
        return this.append(tn);
    },

    append: function(el) {
        if(el instanceof HQ) {
            this.node.append(el.node);
        } else if(el instanceof Array) {
            for(let n of el) {
                this.append(n);
            }
        } else {
            this.append(H(el));
        }
        return this;
    },

    remove: function() {
        this.node.remove();
    },

    attr: function(key,val=undefined) {
        if(typeof(key) == "string") {
            if(val == undefined) return this.node.getAttribute(key);
            else this.node.setAttribute(key,val);
        } else if(typeof(key) == Array) {
            for(let e of key) {
                this.attr(e);
            }
        } else if(typeof(key) == "object") {
            for(let e in key) {
                this.attr(e, key[e]);
            }
        } else throw "Tuntematon attribuutin tyyppi: "+typeof(key);

        return this;
    },

    click: function(callback) {
        this.node.addEventListener("click", callback);
        return this;
    },

    /**
     * Näytä elementti
     */
    show: function() {
        this.node.style.display = "initial";
        return this;
    },

    /**
     * Piilota elementti
     */
    hide: function() {
        this.node.style.display = "none";
        return this;
    }

};

/**
 * Tulostaulun luonnista vastaava luokka.
 */
function TulosTaulukko(caption) {

    this.taulu;
    this.otsikko;

    this.luoTaulu();

    if(caption)
        H(this.otsikko).text(caption);
}

TulosTaulukko.prototype = {
    luoTaulu: function () {
        this.otsikko = H("caption");
        this.taulu = H("table").append(this.otsikko);
    },

    otsikkoRivi: function(otsikko_rivi=[]) {
        let thead = this.taulu.hae("thead").node;
        let rivi;

        try {
            rivi = thead.rows.item(0);
            if(rivi == null) throw "Meh.";
        } catch(error) {
            rivi = thead.insertRow();
        }

        // TODO: Ideaalimaailmassa käy läpi olemassaolevat.
        for(let tiedot of otsikko_rivi) {
            let solu = H("th");
            H(rivi).append(this._lisää(solu, tiedot));
        }
        return thead;
    },

    lisääRivi: function(uusi_rivi) {
        let rivi = this.taulu.hae("tbody").node.insertRow();
        let solu;

        for(let tiedot of uusi_rivi) {
            solu = rivi.insertCell();
            this._lisää(solu,tiedot);
        }

        return rivi;
    },

    _lisää: function(kohde, el) {
        if(typeof(el) == "string") {
            H(kohde).text(el);
        } else {
            H(kohde).append(el);
        }
        return kohde;
    },

};

/**
 * Luo kaavan rastin lisäykseen.
 */
function RastiKaava() {
    let kaava = H("fieldset").append(
        H("legend").text("Rastin tiedot"),
    );
    for(let f of ["Lat", "Lon", "Koodi"]) {
        H(kaava).append(
            H("p").append(
                H("label").text(f).append(
                    H("input").attr({"type": "text", "name": f})
                ),
            )
        );
    }

    kaava.append(H("p").append(
        H("button").attr({
            "name": "rasti",
            "id": "rasti",
        }).text("Lisää rasti")
    ));

    return kaava.node;
}

/**
 * Kaavan käsittelijä.
 */
function rastiKaavaSubmit(kaava) {

    let rasti_id = 0;
    let lat = kaava.elements["Lat"].value;
    let lon = kaava.elements["Lon"].value;
    let koodi = kaava.elements["Koodi"].value;
    if(lat == "" || lon == "" || koodi == "") {
        console.warn("Tyhjä arvo, lisäystä ei tehdä.");
        return false;
    }

    let rastit = kaikkiRastit();

    // Arvo vapaa rastin id
    while(true) {
        rasti_id = Math.floor(Math.random() * 10000000000000000);

        if(rasti_id in rastit) continue;
        else break;
    }

    let uusi_rasti = {
        id: rasti_id,
        koodi: koodi,
        lat: lat,
        lon: lon
    };

    // Lisää rasti kaikkiin sarjoihin.
    for(let laji_idx in data) {
        data[laji_idx]["rastit"].push(uusi_rasti);
    }
    rastit[rasti_id] = uusi_rasti;

    console.dir(rastit);

    // Estä lähetys.
    return false;
}

/**
 * Kolmostasolla lisää tähän lomake jolla voi lisätä uuden joukkueen tai muokata
 * jo olemassa olevan joukkueen tietoja. Joukkueelta kysytään
 * nimi ja lista jäsenistä. Jäseniä voi syöttää n kappaletta. Jäseniä on aina syötettävä vähintään
 * kaksi kappaletta eli alussa on oltava kentät kahden jäsenen nimille. 
 * Uusia syöttökenttiä luodaan tarpeen mukaan. Jos kaikki pyydettyjä tietoja (joukkueen nimi ja väh. kaksi jäsentä ) ei ole
 * syötetty niin lisäystä ei tehdä. Lisää joukkue -painike on disabloituna kunnes pakolliset tiedot on syötetty.
 * Onnistuneen lisäämisen jälkeen lomakkeen kenttien sisältö tyhjennetään.
 * Näytä lomakkeella vain jompikumpi painikkeista.
 */
function joukkueKaava() {
    let kaava = H("fieldset").append([
        H("input").attr({
            "type": "hidden",
            "name": "id"
        }),
        H("legend").text("Uusi joukkue"),
        H("p").append(
            H("label").text("Nimi").append(
                H("input").attr({type:"text",name:"nimi"})
            )
        ),
        H("fieldset").attr("name", "jasenet").append([
            H("legend").text("Jäsenet")
        ]),
        H("p").append(
            H("button").text("Lisää joukkue").attr({
                "id": "lisaa",
                "name": "joukkue"
            })
        ),
        H("p").append(
            H("button").text("Tallenna muutokset").attr({
                "id": "muokkaa",
                "name": "muokkaa"
            })
        ),
    ]);

    kaava.node.addEventListener("change", () => joukkueKaavanTarkistus(kaava.node));
    kaava.node.addEventListener("keypress", () => joukkueKaavanTarkistus(kaava.node));
    return kaava;
}

/**
 * Tarkista että meillä on aina riittävästi - muttei liikaa - jäsenkenttiä.
 */
function joukkuJasenSyöttö(el=undefined, min=2) {
    let kaava = (el != undefined) ? el.form : document.forms["joukkue"];
    let tarvitaan = 1;
    let tyhjaa = 0;

    try {
        for(let kentta of kaava.elements["jasen"]) {
            if(kentta.value == "") {
                tyhjaa += 1;
                if(tyhjaa > 1 && el != kentta && kaava.elements["jasen"].length > min) {
                    H(kentta.parentNode.parentNode).remove();
                }
            }
        }
    } catch(error) {}

    try {
        min -= kaava.elements["jasen"].length;
    } catch(error) {}

    for(let i = 0; i < Math.max(tarvitaan - tyhjaa, min); i++) {
        H(kaava.elements["jasenet"]).append(
            H("p").append(
                H("label").text("Jäsen").append(
                    H("input").attr({
                        type:"text",
                        name:"jasen",
                        oninput:"joukkuJasenSyöttö(this)"
                    })
                )
            )
        );
    }

}


function muokkaaJoukkuettaKaavaan(joukkue) {
    let kaava = document.getElementById("joukkue");

    kaava.elements.nimi.value = joukkue.nimi;
    kaava.elements.id.value = joukkue.id;

    let jasenia = 2;
    try {
        jasenia = Math.max(jasenia, joukkue.jasenet.length + 1);
    } catch(error) {}
    
    for(let f of kaava.elements.jasen) {
        f.value = "";
    }

    joukkuJasenSyöttö(undefined, jasenia);
    for(let i in joukkue.jasenet) {
        kaava.elements.jasen[i].value = joukkue.jasenet[i];
    }
    
    H(kaava.elements["joukkue"]).hide();
    H(kaava.elements["muokkaa"]).show();

}

/**
 * Tarkista kaava
 *
 * Jos kaikkia pyydettyjä tietoja (joukkueen nimi ja väh. kaksi jäsentä ) ei ole syötetty, niin lisäystä ei tehdä.
 * Lisää joukkue -painike on disabloituna, kunnes pakolliset tiedot on syötetty. 
 * Näkyvissä saa olla joko vain Lisää joukkue -painike tai Tallenna muutokset -painike. Ei molempia.
 * Toteuta käyttäen kahta eri painiketta. Älä muuta painikkeen tekstiä tilanteen mukaan vaan piilota aina se painike,
 * jota ei käytetä.
 */
function joukkueKaavanTarkistus(kaava) {
    kaava = ("form" in kaava) ? kaava.form : kaava;
    let ok = true;

    if(kaava.elements.nimi.value == "") {
        ok = false;
    }

    let min_jasen = 2;
    for(let f of kaava.elements.jasen) {
        if(f.value != "") min_jasen -= 1;
    }

    if(min_jasen > 0) ok = false;
    
    for(let b of kaava.getElementsByTagName("button")) {
        b.disabled = (ok) ? false : true;
    }

    return ok;
}


function tallennaKaava(kaava) {
    kaava = ("form" in kaava) ? kaava.form : kaava;

    if(! joukkueKaavanTarkistus(kaava)) return false;

    if(kaava.elements.id.value != "") return muokkaaJoukkuetta(kaava);
    else return lisaaJoukkue(kaava);

    return false;
}

function lisaaJoukkue(kaava) {
    console.log("Lisää joukkue");
    kaava = ("form" in kaava) ? kaava.form : kaava;
    if(! joukkueKaavanTarkistus(kaava)) return false;

    // Vakioita tehtävän mukaan
    let sarja = "2h";
    let laji  = "Jäärogaining";
    let jid = 0;

    let joukkue = {
        "id": 0,
        "nimi": kaava.elements.nimi.value,
        "jasenet": [],
        "leimaustapa": ["GPS"],
        "rastit": [],
        "pisteet": 0,
        "aika": 0,
        "matka": 0
    };

    while(true) {
        jid = Math.floor(Math.random() * 10000000000000000);
        try {
            j(jid, sarja, laji);
        } catch(error) {
            joukkue.id = jid;
            break;
        }
    }

    for(let jasen of kaava.elements.jasen) {
        if(jasen.value != "")
            joukkue.jasenet.push(jasen.value);
    }

    s(sarja, laji).joukkueet.push(joukkue);

    // Onnistuneen lisäämisen tai muokkaamisen jälkeen lomakkeen kenttien sisältö tyhjennetään.
    resetoiKaava(kaava);
    return true;
}

function muokkaaJoukkuetta(kaava) {

    kaava = ("form" in kaava) ? kaava.form : kaava;
    if(! joukkueKaavanTarkistus(kaava)) return false;

    let a_ok = false;
    let jid = kaava.elements.id.value;

    for (let laji of data) {
        for(let sarja of laji["sarjat"]) {
            for(let joukkue of sarja["joukkueet"]) {
                if(joukkue.id == jid) {
                    a_ok = true;
                    joukkue.nimi = kaava.elements.nimi.value;
                    joukkue.jasenet = [];

                    for(let f of kaava.elements.jasen) {
                        if(f.value != "") joukkue.jasenet.push(f.value);
                    }
                }
            }
        }
    }

    if(! a_ok) return false;

    resetoiKaava(kaava);
    return true;
}

function resetoiKaava(kaava) {
    kaava = ("form" in kaava) ? kaava.form : kaava;

    kaava.elements.nimi.value = "";
    kaava.elements.id.value = "";

    try {
        for(let f of kaava.elements.jasen) {
            f.value = "";
        }
    } catch(error) {};

    joukkuJasenSyöttö(undefined, 2);

    for(let b of kaava.getElementsByTagName("button")) {
        b.disabled = true;
    }

    H(kaava.elements["joukkue"]).show();
    H(kaava.elements["muokkaa"]).hide();
       
}


/**
 * Joukkueen lisääminen rivinä tauluun.
 */
TulosTaulukko.prototype.JoukkueRivi = function(joukkuedata) {
    // Taso 3: <a href="#joukkue">Porukka</a> <br />Etunimi1 Sukunimi1, Etunimi2 Sukunimi2
    let joukkue = [
        H("a").attr({
            "href": "#joukkue",
        }).click(() => {
            return muokkaaJoukkuettaKaavaan(joukkuedata[2]);
        }).text(joukkuedata[2]["nimi"]),
        H("br"),
        document.createTextNode(joukkuedata[2]["jasenet"].join(", "))
    ];

    let aika = muotoileAika(joukkuedata[2].aika);
    return this.lisääRivi([
        joukkuedata[1]["nimi"],
        joukkue,
        joukkuedata[2].pisteet,
        H("time").attr({"datetime": aika, "data-aika": joukkuedata[2].aika}).text(aika),
        Math.round(joukkuedata[2].matka)
    ]);
}

/**
 * (Uudelleenpiirtää) taulun datasta.
 */
TulosTaulukko.prototype.reflowTaulu = function(kasa=data) {

    H(this.taulu).hae("tbody").remove();

    let joukkueet = t3_järjestäJoukkueet(listaaJoukkueet(kasa));

    for(let joukkuedata of joukkueet) {
        this.JoukkueRivi(joukkuedata);
    }

}

if(typeof(data) == undefined) var data = {};

document.addEventListener("DOMContentLoaded", function(event) { 

    var tulostaulu = new TulosTaulukko("Tulokset");

    // Lisää otsikkorivi
    // <tr><th>Sarja</th><th>Joukkue</th><th>Pisteet</th></tr>
    tulostaulu.otsikkoRivi(["Sarja", "Joukkue", "Pisteet", "Aika", "Matka"]);

    laskePisteet(data);

    tulostaulu.reflowTaulu(data);
    // Lisää taulukko DOM rakenteeseen.
    document.getElementById("tupa").append(tulostaulu.taulu.node);

    let rastikaava = H(document.forms[0]);
    rastikaava.append(RastiKaava());
    // Kaavan lähetys
    rastikaava.attr("onsubmit", "return rastiKaavaSubmit(this);");

    let joukkuekaava = H(document.forms[1]);
    joukkuekaava.append(joukkueKaava());
    joukkuekaava.node.addEventListener("submit", (e) => { tallennaKaava(joukkuekaava.node); tulostaulu.reflowTaulu(data); e.preventDefault();});

    resetoiKaava(joukkuekaava.node);
});

/// TODO: Joukkueen lisäys, näkymän päivitys