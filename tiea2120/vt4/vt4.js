'use strict';

let pupu = document.createElement("img");
pupu.src = "bunny.png";

pupu.addEventListener("load", function(e) {
    let ylä = document.getElementById("ylä");
    let ala = document.getElementById("ala");
    ylä.getContext('2d').drawImage(e.target, 0, 0, 383, 300, 0, 0, 383, 300);
    ala.getContext('2d').drawImage(e.target, 0, 300, 383, 300, 0, 0, 383, 300);
});

let palkit = document.querySelectorAll("#palkit > img");
let i = 0;
for(let palkki of palkit) {
    i++;
    palkki.className += " palkki";
    palkki.style.animationDelay=i / 8 + "s";
}