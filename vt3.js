"use strict";

/**
 * HUOM:
 * - Tehtävänannossa halutaan tallettaa luontiaika kuten muissakin joukkueissa on.
 *   Datassa ei kuitenkaan ole joukkueen luontiaikaa muilla joukkueilla.
 * 
 * - Tehtävässä sarjoille halutaan luoda ID. Sellaista ei kuitenkaan sarjoilla ole.
 */

//console.log(data);

function H(el) {
    if(el instanceof HQ)
        return el;
    return new HQ(el);
}

function HQ(el) {
    this.node;

    if(el instanceof HQ) {
        this.node = el.node;
    } else if(typeof(el) == "string") {
        if(el[0] == "#") {
            this.node = document.getElementById(el.substr(1));
        } else {
            this.node = document.createElement(el);
        }
    } else {
        this.node = el;
    }

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
        let isä = H(this.node.parentNode);
        this.node.remove();
        return isä;
    },

    empty: function() {
        while (this.node.firstChild) {
            this.node.removeChild(this.node.firstChild);
        }
        return this;
    },

    attr: function(key, val=undefined) {
        if(typeof(key) == "string") {
            if(val == undefined) return this.node.getAttribute(key);
            else this.node.setAttribute(key,val);
        } else if(key instanceof Array) {
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

    on: function(event, callback) {
        if(event instanceof Array) {
           for(let e in event) {
               this.on(e, callback);
           }
        } else {
            this.node.addEventListener(event, callback);
        }
        return this;
    },

    
    click: function(callback) {
        this.on("click", callback);
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

function listaaSarjat(kasa=data) {
    let r = []
    for(let laji of kasa) {
        for(let sarja of laji['sarjat']) {
            if(r.indexOf(sarja['nimi']) == -1) r.push(sarja['nimi']);
        }
    }

    return r
}

function listaaLeimaustavat(kasa=data) {
    let r = []
    for(let laji of kasa) {
        for(let leimaustapa of laji['leimaustapa']) {
            if(r.indexOf(leimaustapa) == -1) r.push(leimaustapa);
        }
    }

    return r
}


function järjestäJoukkueet(lista) {
    return lista.sort(function(a,b) {
        return _nimiSort(a[2], b[2]);
    });
}

function _nimiSort(a, b) {
    let nimi_a = a["nimi"].toUpperCase();
    let nimi_b = b["nimi"].toUpperCase();
    if(nimi_a > nimi_b) return 1;
    else if(nimi_a < nimi_b) return -1;
    else return 0;
}

function pad(number) {
    if (number < 10) {
        return '0' + number;
    }
    return number;
}

function aika(date) {

    return date.getFullYear() +
      '-' + pad(date.getMonth() + 1) +
      '-' + pad(date.getDate()) +
      'T' + pad(date.getHours()) +
      ':' + pad(date.getMinutes());
};

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
            return sarja["joukkueet"][idx];
    }

    throw "Joukkuetta "+joukkue+" ei löytynyt";
}


/**
 * Tarkista että meillä on aina riittävästi - muttei liikaa - jäsenkenttiä.
 */
function joukkuJasenSyöttö(el, min=5) {
    let kaava = ("form" in el) ? el.form : el;
    let tarvitaan = 1;
    let tyhjaa = 0;

    try {
        for(let kentta of kaava.elements["jasen"]) {
            if(kentta.value == "") {
                tyhjaa += 1;
                if(tyhjaa > 1 && el != kentta && kaava.elements["jasen"].length > min) {
                    H(kentta.parentNode.previousSibling).remove();
                    H(kentta.parentNode).remove();
                }
            }
        }
    } catch(error) {}

    try {
        min -= kaava.elements["jasen"].length;

    } catch(error) {}

    for(let i = 0; i < Math.max(tarvitaan - tyhjaa, min); i++) {
        H(kaava.elements["jasenet"]).hae("dl").append([
            H("dt").append(
                H("label").text("Jäsen")
            ),
            H("dd").append(
                H("input").attr({
                    type:"text",
                    name:"jasen",
                    required: true
                }).on("input", (e) => joukkuJasenSyöttö(e.target))
            )
        ]);
    }


    if(el.tagName == "INPUT")
        el.setCustomValidity("")

    let vaadi = (kaava.elements.jasen.length - tyhjaa < 2 || el.tagName == "FORM") ? true : false;

    let jasennr = 0;
    for(let e of kaava.elements.jasen) {
        if(vaadi == false || e.value != "") {
            e.required = false;
        } else {
            e.required = true;
            vaadi = false;
        }

        try {
            if(el.tagName == "INPUT" && el.value != "") {
                if(el != e && e.value.trim().toUpperCase() == el.value.trim().toUpperCase()) {
                    el.setCustomValidity("Samaa nimeä ei voi syöttää kahteen kertaan.");
                }
            }
        } catch(error) {
            console.debug(error);
        }
    }
}

function muokkaaJoukkuettaKaavaan(joukkue, sarja, laji) {
    let kaava = document.getElementById("joukkue-tiedot");

    kaava.elements.nimi.value = joukkue.nimi;
    kaava.elements.id.value = joukkue.id;

    kaava.elements.sarja.value = sarja.nimi;

    for(let leimaustapa of kaava.elements.leimaustapa) {
        leimaustapa.required = false;
        leimaustapa.checked = (joukkue['leimaustapa'].indexOf(leimaustapa.value) == -1) ? false : true;
    }

    let jasenia = 2;
    try {
        jasenia = Math.max(jasenia, joukkue.jasenet.length + 1);
    } catch(error) {}
    
    for(let f of kaava.elements.jasen) {
        f.value = "";
    }

    joukkuJasenSyöttö(kaava, jasenia);
    for(let i in joukkue.jasenet) {
        kaava.elements.jasen[i].value = joukkue.jasenet[i];
    }

}

/**
 * Viikkotehtävä 3:sen omat
 */


function init_kaava_nimi() {
    let kenttä = H("#tiedot-nimi");

    kenttä.on("input", (e) => {
        if(e.target.form.id.value != "") return;

        let nimi = e.target.value.trim().toUpperCase();
        for(let joukkue of listaaJoukkueet()) {

            if(joukkue[2]['nimi'].trim().toUpperCase() == nimi) {
                e.target.setCustomValidity("Olemassa oleva joukkue");
                return false;
            }

        }
        e.target.setCustomValidity("");
    });

}

function init_kaava_leimaustavat() {
    let leimaustavat = listaaLeimaustavat().sort();

    let rakenne = H("#tiedot-leimaustapa").empty();

    for(let leima of leimaustavat) {
        rakenne.append(H("label").text(leima).append([
            H("input").attr({
                "type": "checkbox",
                "name": "leimaustapa",
                "required": "required",
                "value": leima
            }).on("change", (e) => validate_leimaustapa(e.target))
        ]));
    }
}

function validate_leimaustapa(el) {
    let valittu = false;
    for(let nappi of el.parentNode.parentNode.getElementsByTagName(el.tagName)) {
        if(nappi.checked) {
            valittu = true;
        }
    }

    for(let nappi of el.parentNode.parentNode.getElementsByTagName(el.tagName)) {
        nappi.required = !valittu;
    }
}

function init_kaava_sarjat() {
    let sarjat = listaaSarjat().sort();

    let rakenne = H("#tiedot-sarja").empty();
    
    for(let sarja of sarjat) {
        rakenne.append(H("label").text(sarja).append([
            H("input").attr({
                "type": "radio",
                "name": "sarja",
                "value": sarja,
                "required": "required"
            })
        ]));
    }
}

function init_joukkue_jäsenet() {
    joukkuJasenSyöttö(document.forms["joukkue-tiedot"], 5);
}

function init_sarja() {

    let kilpailun_alku = new Date(data[2]['alkuaika']);
    let kilpailun_loppu = new Date(data[2]['loppuaika']);

    let aseta_aika = function() {
        let aloitus = H("#sarja-alkuaika").node;

        if(aloitus.value=="" || !aloitus.validity.valid) return false;

        let sekuntteja = parseInt(H("#sarja-kesto").node.value) * 3600000;
        
        let loppumax = new Date(Date.parse(aloitus.value) + sekuntteja);
        H("#sarja-loppuaika").attr("min", aika(loppumax));

    }

    H("#sarja-nimi").on("input", (e) => {
        let el = e.target;
        el.setCustomValidity("");
        if(el.value != "" && listaaSarjat().indexOf(el.value.trim()) != -1) el.setCustomValidity("Sarja on jo olemassa.");
    })

    H("#sarja-alkuaika").on("input", (e) => {
        H("#sarja-loppuaika").attr("required", (e.target.value) ? true : false);
        aseta_aika();
    }).attr({
        "min": aika(kilpailun_alku)
    });

    H("#sarja-kesto").on("input", aseta_aika);

    H("#sarja-loppuaika").on("input", (e) => {
    }).attr({
        "max": aika(kilpailun_loppu)
    });

}

/**
 * Resetoi kaavan.
 */
function tyhjennäKaava(kaava) {
    console.log("tyhjennäKaava", kaava);
    for(let elementti of kaava.elements) {
        if(elementti.tagName != "INPUT") continue;

        switch(elementti.type.toUpperCase()) {
            case "RADIO":
            case "CHECKBOX":
                elementti.checked = false;
                break;
            default:
                console.log("Hanskaamaton kaavan elementti:", elementti);
            case "HIDDEN":
            case "DATETIME-LOCAL":
            case "NUMBER":
            case "TEXT":
                elementti.value = "";
                break;
        }
    }
}

function reset_joukkuekaava(kaava) {
    tyhjennäKaava(kaava);
    // Radiobutton-valinnassa täytyy aina olla jokin oletuksena valittuna
    kaava.elements.sarja[0].checked = true;
    for(let e of kaava.elements.leimaustapa)
        e.required = true;

}

function reload_joukkueet(kasa=data) {
    let jokkueet = H("#joukkueet").hae("ul").empty();

    for(let joukkue of järjestäJoukkueet(listaaJoukkueet(kasa))) {
        let jäsenet = H("ul");
        for(let jäsen of joukkue[2]['jasenet'].sort()) {
            jäsenet.append(H("li").text(jäsen));
        }

        jokkueet.append(H("li").append([
            H("a").attr("href", "#joukkue-tiedot").text(joukkue[2]['nimi']).on("click",(e) => {
                muokkaaJoukkuettaKaavaan(joukkue[2], joukkue[1], joukkue[0])
            }),
            jäsenet
        ]));

    }
}
/**
 * Lisää joukkue rakenteeseen. Kopioitu VT2:sesta.
 */
function lisaaJoukkue(kaava) {
    kaava = ("form" in kaava) ? kaava.form : kaava;

    // Vakioita tehtävän mukaan
    let sarja = kaava.elements.sarja.value;
    let laji  = "Jäärogaining";
    let jid = 0;
    let joukkue = {}

    if(kaava.elements.id.value != "") {
        jid = kaava.elements.id.value;
        etsi_joukkue:
        for (let laji of data) {
            for(let sarja of laji["sarjat"]) {
                for(let joukkue_idx in sarja["joukkueet"]) {
                    joukkue = sarja["joukkueet"][joukkue_idx]
                    if(joukkue.id == jid) {
                        joukkue.nimi = kaava.elements.nimi.value;
                        joukkue.jasenet = [];
                        joukkue.leimaustapa = [];
                        sarja["joukkueet"].splice(joukkue_idx,1);
                        break etsi_joukkue;
                    }
                }
            }
        }
    } else {
        while(true) {
            jid = Math.floor(Math.random() * 10000000000000000);
            try {
                j(jid, sarja, laji);
            } catch(error) {
                joukkue.id = jid;
                break;
            }
        }

        let joukkue = {
            "id": 0,
            "nimi": kaava.elements.nimi.value,
            "jasenet": [],
            "leimaustapa": [],
            "rastit": [],
            "pisteet": 0,
            "aika": 0,
            "matka": 0
        };
    }

    for(let valinta of kaava.elements.leimaustapa) {
        if(valinta.checked==true)
            joukkue['leimaustapa'].push(valinta.value)
    }


    //joukkue.jasenet.push("Foo Bar");
    //joukkue.jasenet.push("Bar Foo");

    for(let jasen of kaava.elements.jasen) {
        if(jasen.value != "")
            joukkue.jasenet.push(jasen.value);
    }

    s(sarja, laji).joukkueet.push(joukkue);

    H("#viesti-joukkue").text("Joukkue "+joukkue['nimi']+" on lisätty. ");
    return true;
}

function lisääSarja(kaava) {
    kaava = ("form" in kaava) ? kaava.form : kaava;

    let formatoi_aika = function(aika) {
        // Sieltä mistä aita on matalin.
        return aika.replace("T", " ")+":00";
    }

    let laji  = "Jäärogaining";

    // Tehtävässä sarjalle halutaan luotavan ID. Sellaista ei kuitenkaan
    // sarjoilla ole.
    let sarja = {
        alkuaika: null,
        loppuaika: null,
        kesto: parseInt(kaava.elements.kesto.value),
        nimi: kaava.elements.nimi.value,
        joukkueet: []
    };
    
    if(kaava.elements["alkuaika"].value)
        sarja['alkuaika'] = formatoi_aika(kaava.elements["alkuaika"].value)

    if(kaava.elements["loppuaika"].value)
        sarja['loppuaika'] = formatoi_aika(kaava.elements["loppuaika"].value)

    for(let kisa of data) {
        kisa.sarjat.push(sarja);
    }

    console.debug("Lisättiin sarja", s(sarja['nimi'], laji));

    H("#viesti-sarja").text("Sarja "+sarja['nimi']+" on lisätty. ");

}

document.addEventListener("DOMContentLoaded", function(event) { 
    reload_joukkueet();

    init_kaava_nimi();
    init_kaava_sarjat();
    init_kaava_leimaustavat();
    init_joukkue_jäsenet();

    init_sarja();

    reset_joukkuekaava(document.getElementById("joukkue-tiedot"));

    H("#joukkue-tiedot").on("submit", (e) => {
        e.preventDefault();

        lisaaJoukkue(e.target);
        reload_joukkueet(data);
        reset_joukkuekaava(e.target);
    });

    H("#lisää-sarja").on("submit", (e) => {
        e.preventDefault();
        lisääSarja(e.target);

        init_kaava_sarjat();

        tyhjennäKaava(e.target);
    });

});
