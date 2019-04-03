$(document).ready(function() {
    /* Varmistusviesti poistolle */
    $("input[name=poista]").click(function(e) {
        return confirm("Poistetaanko varmasti?");
    });

    (function() {
        /* Kilpailuvalinnan suodatus. */
        $("#kilpailu-filtteri").change(function() {
            var val = $(this).val();
            localStorage.setItem("valittu-kilpailu", val);
            if(val == "") {
                $(".kilpailut > li").show();
            } else {
                $(".kilpailut > li.kilpailu[id='kilpailu-"+val+"']").show();
                $(".kilpailut > li.kilpailu[id!='kilpailu-"+val+"']").hide();
            }
        });
        var kisa = localStorage.getItem("valittu-kilpailu");
        $("#kilpailu-filtteri").val(kisa).change();
    })();

    (function() {
        /* Jos urlissa ``avain`` parametri, etsi siihen liittyvät linkit ja lisää
         luokka ``aktiivinen`` */
        var url_params = new URLSearchParams(window.location.search)
        if(url_params.has("uusi")) {
            var url = "/" + url_params.get("uusi");
            var scrollattu_toggle = false;
            $(".content a[href*=muokkaa]").each(function() {
                if(this.pathname.endsWith(url)) {
                    $(this).addClass("aktiivinen");

                    if(window.pageYOffset == 0 && ! scrollattu_toggle) {
                        // Scrollataan näkyviin.
                        if(window.location.hash == "" && this.id != "")
                            window.location.hash = this.id
                        else
                            this.scrollIntoView(false);

                        scrollattu_toggle = true;
                    }

                }
            });
        }
    })();

    (function() {
        /* Aikaisempien vertaispalautteiden puitteissa lisätty resetointinappi paluunapiksi.
         Resetointi ei kuitenkaan resetoi, vaan ohjaa edelliselle sivulle. Hieman HAX */
        var reset_invoke = () => window.history.back();
        var return_url = (new URLSearchParams(window.location.search)).get("return")
        if(return_url) {
            reset_invoke = () => window.location = return_url;
        } else if(document.referrer && document.referrer != document.location.toString()) {
            var url = new URL(document.referrer)
            var url_params = new URLSearchParams(url);
            // Jos urlissa oli "?uusi=", poista se häiritsemästä.
            url_params.delete("uusi");
            url['search'] = url_params.toString();
            reset_invoke = () => window.location = url.toString();
        }
        $("form .toiminnot input[type=reset]").each(function(e) {
            $(this).click(reset_invoke);
        });
    })();

    /* Luo jokaiselle "nimelle" oma hymiö. Alla lista attribuuteista, joita
    unicode 10 antaa ympätä yhteen ZWJ:n avulla. */

    // Kaikkia ei ole testattu.
    var naamat = ['🙍', '🧙', '🧚', '🧛', '🧜', '🧝', '💆', '💇', '🧘',
                  '💂', '👷', '👳', '🕵️', '👮', '🙎', '🙅', '🙆', '💁', '🙋',
                  '🙇', '🤦', '🤷', '🙍', '👳‍', '⛹️', '🏋️', '🚴', '🚵', '🤸',
                  '🤾', '🤹', '🧘', '🏄', '🚣', '🏊'];
    var sukupuolistamiset = ['♀️', '♂️', ''];
    var rodullistamiset = ['🏻', '🏼', '🏽', '🏾', '🏿'];
    var hiukset = ['🦰', '🦱', '🦲','🦳'];
    var zwj = "&zwj;";

    $(".content span.osallistuja").each(function() {
        var merkit = $(this).text().split("");

        // reduce ottaa tästä tyyppihintin numeroksi
        merkit.push(0);

        // m vamistaa eri merkkien saavan eri arvot eri kohdissa.
        var m = 0.9;
        var koodi = merkit.reduceRight((i, c) => parseInt(c.charCodeAt() * (m += 0.1) + i));

        var tag = naamat[koodi % naamat.length]
                + zwj + rodullistamiset[koodi % rodullistamiset.length]
                + zwj + sukupuolistamiset[koodi % sukupuolistamiset.length]

        $(this).prepend($("<span class=naamake />").html(tag));
    });
});