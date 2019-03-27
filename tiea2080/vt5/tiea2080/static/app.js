
if(typeof(kyselyitä_odottamassa) == "undefined") var kyselyitä_odottamassa = 0;

function näytä_näkymä(näkymä) {
    $(".content").hide();
    $osio = $("#osio-"+näkymä);

    uudellenkirjoita_linkit($osio);
    kaava_virheilmoitukset($osio);

    $osio.show();
}

/**
 * Uudelleenkirjoittaa cgi linkit routerin muotoon.
 */
function uudellenkirjoita_linkit(osio) {
    $(osio).find("a[href]").each(function() {
        try {
            this.href = uudelleenkirjoita_url(this.href);
        } catch (error) {
            console.error(error);
        }
    });
    // Käsitellään osion kaavat
    $(osio).find("form[action]").each(function() {
        this.action = uudelleenkirjoita_url(this.action);
    });
}

/**
 * Tunkee hashin sopivaan urlin väliin.
 */
function uudelleenkirjoita_url(kohde) {
    var a = document.createElement("a");
    a.href = kohde;

    // Ulkomaailmaan osoittavat linkit skipataan.
    if(window.location.hostname != a.hostname || !a.pathname.startsWith(window.location.pathname)) {
        return kohde;
    }

    // Jos on jo(?) hash, skipataan myös
    if(a.hash != "") return kohde;

    var url = a.pathname.substring($SCRIPT_ROOT.length+1);
    a.pathname = $SCRIPT_ROOT+"/";
    a.hash = "#!"+url;

    return a.href;
}

var kaava_virheilmoitukset_prosessoidut = [];

/**
 * Sitoo tehtävänmukaiset html elementit kaavan virhekäsittelyyn.
 */
function kaava_virheilmoitukset(osio) {
    $(osio).find(":input").each(function() {
        // Ohita jo prosessoidut kaavat.
        if(this in kaava_virheilmoitukset_prosessoidut) return;
        kaava_virheilmoitukset_prosessoidut.push(this);

        $(this).on("blur", function(e) {
            // Sisällön muuttuessa, hävitä vanhat virheilmoitukset.
            $(e.target).parent().find(".error").remove();
            this.setCustomValidity('');
            this.form.checkValidity();
        }).on("invalid", function(e) {
            $(e.target).parent().find(".error").remove();

            // Kaavassa on virhe, luo boksi sille.
            var err = $("<span/>", {class:"error", text:e.target.validationMessage});
            $(e.target).after(err);
            e.preventDefault();
        });
    });
}

/**
 * Luo jQueryn kyselyn.
 */
function luo_kysely(settings={}) {
    kyselyitä_odottamassa += 1

    odota_kyselyä();

    //settings['dataType'] = "json";

    if(settings['url'][0] != "/")
        settings['url'] = $SCRIPT_ROOT+"/api/1.0/"+settings['url'];

    var error_callback = settings["error"] || function() {};

    settings = $.extend(settings, {
        /**
         * Oma laajennettu virhekäsittelijä. Ohjaa kirjautumissivulle jos palvelin
         * ilmoittaa 403 FORBIDDEN.
         */
        error: function( jqXHR, textStatus, errorThrown ) {
            console.error("Kysely palautti virheen:",  errorThrown, jqXHR);
            if(jqXHR['status'] == 403) {
                routes.runRoute("get", "#!kirjaudu");
            }
            error_callback(jqXHR, textStatus, errorThrown);
        }
    });

    return $.ajax(settings).always(function() {
        kyselyitä_odottamassa -= 1;
    });
    // TODO: Lisää tähän virhetarkistukset.
}

/**
 * Käsittellee html form -elementin, ja palauttaa sopivan datasetin ajax-kutsulle.
 */
function käsittele_kaava(kaava) {
    $kaava = $(kaava);
    var csrf_token = $kaava.find(":input[name=csrf_token]").val();
    var metodi = $kaava.prop("method") || "POST";
    var url = $kaava.prop("action").split("#!")[1];

    settings = {
        type: metodi,
        url: url,
        data: $kaava.serialize(),
        beforeSend: function(xhr, settings) {
            if (!/^(GET|HEAD|OPTIONS|TRACE)$/i.test(settings.type) && !this.crossDomain) {
                xhr.setRequestHeader("X-CSRFToken", csrf_token);
            }
        },

        error: function(req) {
            console.log("Kaavassa on virhe.")

            if("responseJSON" in req) {
                for(var field in req.responseJSON) {
                    try {
                        $kaava.find(":input[name=\""+field+"\"]")[0].setCustomValidity(req.responseJSON[field][0]);

                    } catch (error) {
                        console.error("Virhe asetettaessa kaavan virhettä", field, error);
                    }
                }

                if("message" in req.responseJSON) {
                    viesti = req.responseJSON["message"];
                    try {
                        // Tehtävässä käsketään näyttää virhe submit -napin vieressä.
                        $kaava.find(":submit")[0].setCustomValidity(viesti);
                    } catch (error) {
                        console.error("Virhe asetettaessa kaavan submitille virhettä", error);
                    }
                }
            } else {
                // Ei saatu parempaa virheilmoitusta :(
                try {
                    $kaava.find(":submit")[0].setCustomValidity("Palvelimen virhe: " + req.statusText);
                } catch (error) {}
            }
            $kaava.find(":input").each(function() {this.reportValidity()});
        }
    };

    return settings;
}

function täytä_wtforms(kaava, data) {
    $kaava = $(kaava);

    for(var e of $kaava.find(':input')) {
        var $e = $(e);
        k = $e.prop("name");
        if(k in data) {
            var tyyppi = $e.prop("type");

            // Jotkin vaativat erityistä huomiota
            if(tyyppi == "submit" && data[k] == false) {
                continue;
            } else if(tyyppi == "hidden" && k == "csrf_token") {
                continue;
            } else if(tyyppi == "radio") {
                // Radionapeille monimutkaisempi
                if(data[k] == $e.prop("value"))
                    $e.prop("checked", true);
                else $e.prop("checked", false);
            } else {
                $e.val(data[k]);
            }
        } else {
            console.debug("Kentälle", k, "ei ole dataa");
        }
    }
}

function näkymä_kirjautuminen() {
    näytä_näkymä("kirjautumiskaava_nakyma");
}

function näkymä_muokkaa_joukkuetta(joukkue_id) {

    näytä_näkymä("muokkaa_joukkuetta");

    luo_kysely({
        url: "joukkue/"+joukkue_id
    }).done(function(data) {

        kaava = $("#osio-muokkaa_joukkuetta form");

        // Jokaisen joukkueen kohdalla kaavan kohde pitää kirjoittaa uudelleen.
        kaava.prop("action", $SCRIPT_ROOT+"/#!joukkue/"+joukkue_id+"/muokkaa");
        täytä_wtforms(kaava, data);

        näytä_näkymä("muokkaa_joukkuetta");
    });
}

function näkymä_listaa_joukkueet() {
    luo_kysely({
        url: "joukkue"
    }).done(function(data) {
        var container = $("#joukkueet");
        container.empty();
        näkymä_listaa_joukkueet_lista(data, container);
        näytä_näkymä("listaa_joukkueet")
    });
}

function näkymä_listaa_joukkueet_lista(data, container) {
    data.sort(function(a,b){ return a['sarjan_nimi'].localeCompare(b['sarjan_nimi'])});
    sarjat = data.groupBy("sarjan_nimi")
    for(var sarja in sarjat) {
        var li = $("<li />").append($("<h3/>").append(view_sarja(sarjat[sarja][0])));
        li.appendTo(container);

        var joukkueet = $("<ul/>")
        sarjat[sarja].sort(function(a,b){return a['nimi'].localeCompare(b['nimi'])});

        for(var joukkue of sarjat[sarja]) {
            var joukkue_li = $("<li/>");
            joukkue_li.append(view_joukkue(joukkue));
            joukkueet.append(joukkue_li);

            var jäsenet = $("<ul/>", {class:"jasenet"});
            for(jäsen of joukkue['jasenet']) {
                $("<li />").append(view_jäsen(jäsen)).appendTo(jäsenet);
            }

            jäsenet.appendTo(joukkue_li);
        }

        joukkueet.appendTo(li);
    }
}

var current_user = {
    id: 0
}

function lataa_käyttäjä(settings={}) {
    settings = $.extend(settings, {
        url: "kirjaudu",
        type: "get",
    });

    return luo_kysely(settings).done(function(data) {
        aseta_käyttäjä(data);
    });
}

function aseta_käyttäjä(käyttäjä) {
    current_user = käyttäjä;
    if(current_user.id) {
        $("body").addClass("kirjautunut");

        if(current_user.kilpailun_nimi)
            $("#navi-kilpailu").text(current_user.kilpailun_nimi);
        if(current_user.id && current_user.kilpailun_nimi)
            $("#navi-joukkue").html(view_joukkue(current_user));

    } else {
        $("body").removeClass("kirjautunut");
    }
}

function kirjaudu_ulos(settings={}) {
    settings = $.extend(settings, {
        url: "kirjaudu",
        type: "DELETE" // DELETE! DELETE! DELETE!
    });

    luo_kysely(settings).done(function(data) {
        $("body").removeClass("kirjautunut");
        current_user = {id:0};
    });
}

/**
 * HTML esitys sarja -rakenteelle.
 */
function view_sarja(sarja) {
    var params = {class:"view sarja"};
    if(typeof(sarja) == "object") {
        params['text'] = ("sarjan_nimi" in sarja) ? sarja["sarjan_nimi"] : sarja["nimi"];
        params['data-id'] = ("sarja" in sarja) ? sarja["sarja"] : sarja["id"];
    } else {
        params['text'] = sarja;
    }

    return $("<span/>", params);
}

/**
 * HTML esitys joukkue -rakenteelle.
 */
function view_joukkue(joukkue) {

    var params = {class:"view joukkue"};
    if(typeof(sarja) == "object") {
        params['text'] = ("joukkueen_nimi" in sarja) ? joukkue["joukkueen_nimi"] : joukkue["nimi"];
        params['data-id'] = ("joukkue" in sarja) ? joukkue["joukkue"] : joukkue["id"];
        params['href'] = $SCRIPT_ROOT+"#!joukkue/"+params['data-id']+"/muokkaa";
    } else {
        params['text'] = joukkue;
    }

    return $("<a/>", params);
}

/**
 * HTML esitys jäsen -rakenteelle.
 */
function view_jäsen(jäsen) {
    var params = {class:"view jäsen", text:jäsen};
    return $("<span/>", params);
}

// https://stackoverflow.com/questions/14446511/most-efficient-method-to-groupby-on-a-array-of-objects
Array.prototype.groupBy = function(key) {
    return this.reduce(function(rv, x) {
        (rv[x[key]] = rv[x[key]] || []).push(x);
        return rv;
    }, {});
};

/**
 * Määritellään routet.
 */
var routes = $.sammy(function() {
    this.get("#!joukkue/", näkymä_listaa_joukkueet);
    this.get("#!joukkue/:joukkue_id/muokkaa", function() {
        näkymä_muokkaa_joukkuetta(this.params['joukkue_id']);
    });
    this.post("#!joukkue/:joukkue_id/muokkaa",  function(data) {
        var joukkue_id = this.params['joukkue_id'];
        settings = $.extend(käsittele_kaava(data['target']), {
            url: "joukkue/"+joukkue_id
        });

        luo_kysely(settings).done(function(data) {

            // Jos muokkattiin itseä niin lataa käyttäjä uudelleen.
            if(joukkue_id == current_user['id']) {
                lataa_käyttäjä();
            }
            
            // Ohjataan takaisin
            routes.setLocation("#!joukkue/");
            routes.runRoute("get", "#!joukkue/");
        });
    });

    this.get("#!kirjaudu", näkymä_kirjautuminen);
    this.post("#!kirjaudu", function(data) {
        settings = käsittele_kaava(data['target']);

        luo_kysely(settings).done(function(data) {

            aseta_käyttäjä(data);

            if(routes.getLocation() == $SCRIPT_ROOT+"/#!kirjaudu") {
                routes.setLocation("#!joukkue/");
                routes.runRoute("get", "#!joukkue/");
            } else {
                // Palata aktiiviselle näkymälle.
                routes.runRoute("get", routes.getLocation());
            }
        });
    });

    this.post('#!logout',  function(data) {
        // Kirjaa käyttäjä ulos.
        settings = käsittele_kaava(data['target']);
        settings['success'] = function(data) {
            routes.setLocation("#!kirjaudu");
            routes.runRoute("get", "#!kirjaudu");
        }
        kirjaudu_ulos(settings);
    });

});



$(document).ready(function() {

    //routes.debug = true;
    uudellenkirjoita_linkit($("body"));

    lataa_käyttäjä().always(function() {
        var sivu = "";
        if(current_user['id'])
            sivu = "#!joukkue/";
        else
            sivu = "#!kirjaudu";
        routes.run(sivu);
   });
    
    
});

