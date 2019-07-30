// kirjoita tänne oma ohjelmakoodisi

'use strict';
if(typeof(data) == undefined) var data = {};
var ui_joukkuelista = $, ui_joukkuevalinta = $;
var ui_kartta = $;

var kartta;

var raahattava;

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

function ui_lisää_joukkue(joukkue) {
    let li = $('<li class="joukkue" draggable="true" />');
    li.text(joukkue['nimi']);
    li.css("backgroundColor", joukkue['väri']);

    li.data("joukkue", joukkue);
    ui_joukkuelista.append(li);

    li.on("dragstart", function(e) {
        // Nimeä EI käytetä id:nä, kuten tehtävä määrittelee.
        e.originalEvent.dataTransfer.setData("text/plain", $(e.target).data("joukkue")['nimi']);
        e.originalEvent.dataTransfer.setData("application/tiea2120.joukkue", $(e.target).data("joukkue")['id']);
        raahattava = e.target;
    }).on("dragstop", function(e) {
        raahattava = null;
    });
}

// Lisää värit joukkueille
function bootsrap_värit() {
    let joukkueet = listaaJoukkueet();

    let i = 0;
    for(let joukkue of joukkueet) {
        let väri = rainbow(joukkueet.length, i++);
        joukkue[2]['väri'] = väri;
    }
}

/**
 * Luo joukkuelista, ja aseta drop handlerit.
 */
function bootsrap_joukkuelista() {
    let joukkueet = listaaJoukkueet();
    for(let joukkue of joukkueet) {
        ui_lisää_joukkue(joukkue[2]);
    }

    ui_joukkuevalinta.on("dragover", function(e) {
        e.preventDefault();
        // Set the dropEffect to move
        e.originalEvent.dataTransfer.dropEffect = "move"
    }).on("drop", function(e) {
        if(e.originalEvent.dataTransfer.getData("application/tiea2120.joukkue")) {
            e.preventDefault();
            raahattava.parentNode.removeChild( raahattava );
            ui_joukkuevalinta.prepend(raahattava);
            // Joukkueita ei saa enää raahata tasolla 1

            $(raahattava).attr("draggable", "false");
            kartta.reitti($(raahattava).data("joukkue"));

        } else {
            throw "Pudotettu elementti ei ollut joukkue";
        }
        
    });
}

function bootstrap_kartta() {
    let kokoa = function() {
        ui_kartta.css("width", Math.floor(window.innerWidth) - 16 + "px");
        ui_kartta.css("height", Math.floor(window.innerHeight*0.66) + "px");
    };
    kokoa();
    $(window).resize(kokoa);

    kartta = new Kartta(ui_kartta.attr("id"), {
        crs: L.TileLayer.MML.get3067Proj()
       }).setView([62.2333, 25.7333], 11);
    L.tileLayer.mml_wmts({ layer: "maastokartta" }).addTo(kartta);

    let rastit = kaikkiRastit();

    // Tehtävä vaatii käyttämään omaa bounding boxia.
    let min_lat, min_lon, max_lat, max_lon;
    min_lat = max_lat = parseFloat(Object.values(rastit)[0]['lat']);
    min_lon = max_lon = parseFloat(Object.values(rastit)[0]['lon']);

    console.log(Object.values(rastit)[0]['lat']);
    for(let rasti_idx in rastit) {
        let rasti = rastit[rasti_idx];
        min_lat = Math.min(min_lat, parseFloat(rasti['lat']));
        max_lat = Math.max(max_lat, parseFloat(rasti['lat']));

        min_lon = Math.min(min_lon, parseFloat(rasti['lon']));
        max_lon = Math.max(max_lon, parseFloat(rasti['lon']));

        kartta.rasti(rasti);
    }

    kartta.fitBounds([
        [min_lat, min_lon],
        [max_lat, max_lon]
    ]);
}

var Kartta = L.Map.extend({
    rasti: function(rasti) {
        L.circle(
            [rasti['lat'], rasti['lon']], {
                color: 'red',
                radius: 150
            }
        ).addTo(this);
    },

    reitti: function(joukkue) {
        let rastit = kaikkiRastit();
        let koords = [];
        for(let rasti of joukkue["rastit"].sort((a,b) => a["aika"] - b["aika"])) {
            let rid = String(rasti["rasti"]);
            if(!(rid in rastit)) continue;
            koords.push([rastit[rid]["lat"], rastit[rid]["lon"]]);
        }
        L.polyline(koords, {color: joukkue.väri}).addTo(this);
    }
});

document.addEventListener("DOMContentLoaded", function(event) {

    ui_joukkuelista = $("#joukkueet");
    ui_joukkuevalinta = $("#valitut");
    ui_kartta = $("#map");

    // Bootsrap
    bootsrap_värit();
    bootsrap_joukkuelista();

    bootstrap_kartta();
});