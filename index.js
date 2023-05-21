const jetpack = require("fs-jetpack");
const moment = require("moment/moment");
const { DiceRoll } = require("@dice-roller/rpg-dice-roller");

var win = nw.Window.get();

$(document).on("keyup", function(event) {

    if (event.code == "Digit4") {
        var roll = new DiceRoll("1d4");
        $("#output").append("<div>" + roll.output + "</div>");
    }
});