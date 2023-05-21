const jetpack = require("fs-jetpack");
const moment = require("moment/moment");
const { DiceRoll } = require("@dice-roller/rpg-dice-roller");
const { v4: uuidv4 } = require('uuid');

var win = nw.Window.get();

var state = "default";
var dicepanel = "";
var dicecount = 1;
var dicedice = 0;

$(document).on("keyup", function(event) {

    console.log(event);

    if ((event.code == "Digit4") && (state == "default")) {
        state = event.code;
        dicepanel = uuidv4();
        dicedice = 4;
        dicecount = 1;

        var html = "<div class='card mb-1'>";
        html += "<div id='dice-" + dicepanel + "' class='card-body'>";
        html += "1d4 Roll[Enter] Cancel[Esc]";
        html += "</div>";
        html += "</div>";

        $("#output").append(html);
        document.getElementById(dicepanel).scrollIntoView();
    }
    else if ((event.code == state) && (event.ctrlKey)) {
        if (dicecount > 1) {
            dicecount -= 1;
        }

        $('#dice-' + dicepanel).html(dicecount + "d4 Roll[Enter] Cancel[Esc]");
    }
    else if (event.code == state) {
        dicecount += 1;
        $('#dice-' + dicepanel).html(dicecount + "d4 Roll[Enter] Cancel[Esc]");
    }
    else if ((event.code == 'Enter') && (state.startsWith('Digit4'))) {
        var roll = new DiceRoll(dicecount + "d" + dicedice);
        $('#dice-' + dicepanel).html(roll);
        state = "default";
    }
});