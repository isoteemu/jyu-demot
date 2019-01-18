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

function _nimiSort(a, b) {
    let nimi_a = a["nimi"].toUpperCase();
    let nimi_b = b["nimi"].toUpperCase();
    if(nimi_a > nimi_b) return 1;
    else if(nimi_a < nimi_b) return -1;
    else return 0;
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
            for(let n in el) {
                this.append(n);
            }
        } else {
            this.append(H(el));
        }
        return this;
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
    }

    // Lisää rasti kaikkiin sarjoihin.
    for(let laji_idx in data) {
        data[laji_idx]["rastit"].push(uusi_rasti);
    }
    rastit[rasti_id] = uusi_rasti;

    console.dir(rastit);

    //console.log(new FormData(kaava).keys());
    return false;
}

if(typeof(data) == undefined) var data = {};

document.addEventListener("DOMContentLoaded", function(event) { 

    var tulostaulu = new TulosTaulukko("Tulokset");

    // Lisää otsikkorivi
    // <tr><th>Sarja</th><th>Joukkue</th><th>Pisteet</th></tr>
    tulostaulu.otsikkoRivi(["Sarja", "Joukkue"]);

    let joukkueet = t1_järjestäJoukkueet(listaaJoukkueet());

    for(let joukkuedata of joukkueet) {
        tulostaulu.lisääRivi([
            joukkuedata[1]["nimi"],
            joukkuedata[2]["nimi"],
        ]);
    }

    // Lisää taulukko DOM rakenteeseen.
    document.getElementById("tupa").append(tulostaulu.taulu.node);


    let rastikaava = H(document.forms[0]);
    rastikaava.append(RastiKaava());

    // Kaavan lähetys
    rastikaava.attr("onsubmit", "return rastiKaavaSubmit(this);");
});
