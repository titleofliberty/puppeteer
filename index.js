const jetpack = require("fs-jetpack");
const moment = require("moment/moment");
const { DiceRoll } = require("@dice-roller/rpg-dice-roller");
const { v4: uuidv4 } = require('uuid');



class puppet {
    constructor(system) {
        this.system = system;
        this.name = "My Character Name";
        this.classification = "Ranger";
        this.level = 2;
        this.race = "Elf";
        this.background = "Outlander";
        this.abilities = {  
            "str": 10,
            "dex": 10,
            "con": 10,
            "int": 10,
            "wis": 10,
            "cha": 10   
        };
    }

    getSystem() {
        return this.system;
    }

    getArmorClass() {
        var ac = 10 + this.getAbilityModifier("dex");

        return ac;
    }        

    getAbilityScore(ability) {
        return this.abilities[ability];
    };

    getAbilityModifier(ability) {
        var mod = 0;
        var score = this.getAbilityScore(ability);

        if (score > 10) {
            mod = Math.floor((score - 10) / 2);
        }
        else if (score < 10) {
            mod = Math.ceil((score - 10) / 2);
        }

        return mod;
    }
}

var win = nw.Window.get();

var state = "default";
var cardpanel = "";
var dicecount = 1;
var dicedice = "";
var dicekey = "";
var hps = 0;

var ppt = new puppet("D&D 5e");

function simpleCard(panel, header, body) {
    var html = "<div id='card-" + panel + "' class='card mb-3'>";
    html += "<div class='card-header'>" + header + "</div>"; 
    html += "<div class='card-body'>" + body + "</div>";
    html += "</div>";
    return html;
}

function diceCard(panel, count, dice) {
    var html = "<div id='card-" + panel + "' class='card mb-3'>";
    html += "<div class='card-header'>Roll Dice</div>"; 
    html += "<div id='dice-" + panel + "' class='card-body'>" + count + "d" + dice + "</div>";
    html += "<div class='card-footer'>Roll[Enter] Cancel[Esc]</div>";
    html += "</div>";
    return html;
}

function hpCard(panel, hp) {
    var html = "<div id='card-" + panel + "' class='card mb-3'>";
    html += "<div class='card-header'>Modify Hit Points</div>";
    html += "<div id='hp-" + panel + "' class='card-body'>" + hp + "</div>";
    html += "<div class='card-footer'>Apply[Enter] Cancel[Esc]</div>";
    html += "</div>";
    return html;
}

function outputKeybindings() {
    jetpack.readAsync("keybindings.html").then(function(data) {
        $("#output").append(data);
    }).catch(function(err) {
        $("#output").append("Error: " + err);
    });
}

function populatePuppet() {
    $("#char-name").html(ppt.name);
    $("#char-class").html(ppt.classification);
    $("#char-level").html("Lvl " + ppt.level);
    $("#char-race").html(ppt.race);
    $("#char-background").html(ppt.background);
    $("#char-ac").html(ppt.getArmorClass());
    console.log('Here');
}

$(document).on("keyup", function(event) {

    const dices = ["1","2","4","5","6","8","0"];

    console.log("Key: " + event.key + " Code: " + event.code + " dicedice: " + dicedice);

    if ((state == "default") && (dices.includes(event.key))) {
        state = "roll";
        cardpanel = uuidv4();
        dicecount = 1;
        dicekey = event.code;

        switch(event.key) {
            case "1": 
                dicedice = "10";
                break;
            case "2": 
                dicedice = "12";
                break;
            case "4": 
                dicedice = "4";
                break;
            case "5": 
                dicedice = "%";
                break;
            case "6": 
                dicedice = "6";
                break;
            case "8": 
                dicedice = "8";
                break;
            case "0": 
                dicedice = "20";
                break;
        }

        var html = diceCard(cardpanel, dicecount, dicedice);

        $("#output").append(html);
        document.getElementById("dice-" + cardpanel).scrollIntoView();
    }
    else if ((state == "roll") && (event.key == "Backspace")) {
        if (dicecount > 1) {
            dicecount -= 1;
        }
        $("#dice-" + cardpanel).html(dicecount + "d" + dicedice);
    }
    else if ((event.key == "Enter") && (state == "roll")) {
        var roll = new DiceRoll(dicecount + "d" + dicedice);
        $("#card-" + cardpanel).replaceWith(simpleCard(cardpanel, "Roll Dice", roll.output));
        state = "default";
    }
    else if ((event.code == "Escape") && (state == "roll")) {
        cardpanel = "";
        $("#card-" + cardpanel).remove();
        state = "default";
    }
    else if ((state == "roll") && (event.code == dicekey)) {
        dicecount += 1;
        $("#dice-" + cardpanel).html(dicecount + "d" + dicedice);
    }
    else if (((event.key == "-") || (event.key == "=")) && (state == "default")) {
        state = "hp";
        cardpanel = uuidv4();
        switch(event.key) {
            case "-":
                hps = -1;
                break;
            case "=":
                hps = 1;
                break;
        }

        var html = hpCard(cardpanel, hps);

        $("#output").append(html);
        document.getElementById("hp-" + cardpanel).scrollIntoView();
    }
    else if ((event.key == "-") && (state == "hp")) {
        hps = hps - 1;
        if (hps == 0) hps = -1;
        $("#hp-" + cardpanel).html(hps);
    }
    else if ((event.key == "=") && (state == "hp")) {
        hps = hps + 1;
        if (hps == 0) hps = 1;
        $("#hp-" + cardpanel).html(hps);
    }
    else if ((event.key == "Enter") && (state == "hp")) {
        console.log('Here');
        var act = "Injured";
        if (hps > 0) act = "Healed";
        $("#card-" + cardpanel).replaceWith(simpleCard(cardpanel, "Hit Points", act + " for " + Math.abs(hps) + " points"));
        state = "default";
    }
    else if ((event.code == "Escape") && (state == "hp")) {
        $("#card-" + cardpanel).remove();
        state = "default";
    }
    else if (event.code == "Delete") {
        $("#output").empty();
    }
    else if (event.code == "F5") {
        outputKeybindings();
    }
});

populatePuppet();