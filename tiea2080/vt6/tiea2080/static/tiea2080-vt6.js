$(document).ready(function() {

    $("input[name=poista]").click(function(e) {
        return confirm("Poistetaanko varmasti?");
    });

});