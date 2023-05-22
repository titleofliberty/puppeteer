const jetpack = require("fs-jetpack");
const moment = require("moment/moment");
const { DiceRoll } = require("@dice-roller/rpg-dice-roller");
const { v4: uuidv4 } = require('uuid');

class puppet {
    constructor(system) {
        this.system = system;
        this.name = "My Character Name";
        this.classification = "Ranger";
        this.level = 1;
        this.race = "Elf";
        this.background = "Outlander";
        this.speed = "30 ft.";
        this.maxhp = 10;
        this.hp = 10;
        this.temphp = 0;
        this.strength = {"score": 15, "proficient": false};
        this.dexterity = {"score": 14, "proficient": false};
        this.constitution = {"score": 13, "proficient": false};
        this.intelligence = {"score": 12, "proficient": false};
        this.wisdom = {"score": 10, "proficient": false};
        this.charisma = {"score": 8, "proficient": false};
    }

    getSystem() {
        return this.system;
    }

    getArmorClass() {
        var ac = 10 + this.getAbilityModifier("dex");

        return ac;
    }

    getAbilityModifier(ability) {
        var mod = 0;

        if (ability.score > 10) {
            mod = Math.floor((ability.score - 10) / 2);
        }
        else if (ability.score < 10) {
            mod = Math.ceil((ability.score - 10) / 2);
        }

        if (ability.proficient) {
            mod += this.getProficiencyBonus();
        }

        return mod;
    }

    getProficiencyBonus() {
        var bonus = 2;

        if (this.level > 4) bonus = 3;
        if (this.level > 8) bonus = 4;
        if (this.level > 12) bonus = 5;
        if (this.level > 16) bonus = 6;

        return bonus;
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

function outputAbout() {
    jetpack.readAsync("about.html").then(function(data) {
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
    $("#char-str").html(ppt.getAbilityModifier(ppt.strength));
    $("#char-dex").html(ppt.getAbilityModifier(ppt.dexterity));
    $("#char-con").html(ppt.getAbilityModifier(ppt.constitution));
    $("#char-int").html(ppt.getAbilityModifier(ppt.intelligence));
    $("#char-wis").html(ppt.getAbilityModifier(ppt.wisdom));
    $("#char-cha").html(ppt.getAbilityModifier(ppt.charisma));
    $("#char-ac").html(ppt.getArmorClass());
    $("#char-prof").html(ppt.getProficiencyBonus());
    $("#char-init").html(ppt.getAbilityModifier("dex"));
    $("#char-hp").html(ppt.hp);
    $("#char-max").html(ppt.maxhp);
    $("#char-temp").html(ppt.temphp);
    $("#char-speed").html(ppt.speed);
}

function rollAcrobatics() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.dexterity);
    if (ppt.background == "Criminal") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Entertainer") mod += ppt.getProficiencyBonus();
    
    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Acrobatics Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollAnimalHandling() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.wisdom);
    if (ppt.background == "Outlander") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Sailor") mod += ppt.getProficiencyBonus();
    
    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Animal Handling Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollArcana() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.intelligence);
    if (ppt.background == "Sage") mod += ppt.getProficiencyBonus(); 

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Arcana Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollAthletics() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.strength);
    if (ppt.background == "Athlete") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Soldier") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);
    
    $("#output").append(simpleCard(id, "Athletics Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollDeception() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.charisma);
    if (ppt.background == "Charlatan") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Criminal") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Deception Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollHistory() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.intelligence);
    if (ppt.background == "Sage") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "History Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollInsight() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.wisdom);
    if (ppt.background == "Sage") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Insight Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollIntimidation() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.charisma);
    if (ppt.background == "Criminal") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Gladiator") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Intimidation Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollInvestigation() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.intelligence);
    if (ppt.background == "Sage") mod += ppt.getProficiencyBonus();
    
    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Investigation Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollMedicine() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.wisdom);
    if (ppt.background == "Hermit") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Sage") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Medicine Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollNature() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.intelligence);
    if (ppt.background == "Hermit") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Outlander") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Nature Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollPerception() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.wisdom);
    if (ppt.background == "Criminal") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Outlander") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Sage") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Soldier") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Perception Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollPerformance() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.charisma);
    if (ppt.background == "Entertainer") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Gladiator") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Performance Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollPersuasion() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.charisma);
    if (ppt.background == "Courtier") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Entertainer") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Noble") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Persuasion Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollReligion() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.intelligence);
    if (ppt.background == "Acolyte") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Sage") mod += ppt.getProficiencyBonus();
    
    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Religion Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollSleightOfHand() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.dexterity);
    if (ppt.background == "Criminal") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Entertainer") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Gladiator") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Sailor") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Sleight of Hand Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollStealth() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.dexterity);
    if (ppt.background == "Criminal") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Gladiator") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Sailor") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Urchin") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Stealth Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollSurvival() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.wisdom);
    if (ppt.background == "Outlander") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Sailor") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Soldier") mod += ppt.getProficiencyBonus();
    if (ppt.background == "Urchin") mod += ppt.getProficiencyBonus();
    
    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Survival Check", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollStrengthSave() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.strength);
    if (ppt.classification == "Barbarian") mod += ppt.getProficiencyBonus();
    if (ppt.classification == "Fighter") mod += ppt.getProficiencyBonus();
    if (ppt.classification == "Ranger") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Strength Save", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollDexteritySave() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.dexterity);
    if (ppt.classification == "Bard") mod += ppt.getProficiencyBonus();
    if (ppt.classification == "Monk") mod += ppt.getProficiencyBonus();
    if (ppt.classification == "Rogue") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Dexterity Save", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollConstitutionSave() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.constitution);
    if (ppt.classification == "Barbarian") mod += ppt.getProficiencyBonus();
    if (ppt.classification == "Fighter") mod += ppt.getProficiencyBonus();
    
    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Constitution Save", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollIntelligenceSave() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.intelligence);
    if (ppt.classification == "Wizard") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Intelligence Save", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollWisdomSave() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.wisdom);
    if (ppt.classification == "Cleric") mod += ppt.getProficiencyBonus();
    if (ppt.classification == "Druid") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Wisdom Save", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}
 
function rollCharismaSave() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.charisma);
    if (ppt.classification == "Paladin") mod += ppt.getProficiencyBonus();
    if (ppt.classification == "Sorcerer") mod += ppt.getProficiencyBonus();
    if (ppt.classification == "Warlock") mod += ppt.getProficiencyBonus();

    var roll = new DiceRoll("1d20+" + mod);

    $("#output").append(simpleCard(id, "Charisma Save", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

function rollInitiative() {
    var id = uuidv4();
    var mod = ppt.getAbilityModifier(ppt.dexterity);
    
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
    else if ((event.altKey) && (event.key == "a")) {
        rollAcrobatics();
    }
    else if ((event.altKey) && (event.key == "b")) {
        rollAnimalHandling();
    }
    else if ((event.altKey) && (event.key == "c")) {
        rollArcana();
    }
    else if ((event.altKey) && (event.key == "d")) {
        rollAthletics();
    }
    else if ((event.altKey) && (event.key == "e")) {
        rollDeception();
    }
    else if ((event.altKey) && (event.key == "f")) {
        rollHistory();
    }
    else if ((event.altKey) && (event.key == "g")) {
        rollInsight();
    }
    else if ((event.altKey) && (event.key == "h")) {
        rollIntimidation();
    }
    else if ((event.altKey) && (event.key == "i")) {
        rollInvestigation();
    }
    else if ((event.altKey) && (event.key == "j")) {
        rollMedicine();
    }
    else if ((event.altKey) && (event.key == "k")) {
        rollNature();
    }
    else if ((event.altKey) && (event.key == "l")) {
        rollPerception();
    }
    else if ((event.altKey) && (event.key == "m")) {
        rollPerformance();
    }
    else if ((event.altKey) && (event.key == "n")) {
        rollPersuasion();
    }
    else if ((event.altKey) && (event.key == "o")) {
        rollReligion();
    }
    else if ((event.altKey) && (event.key == "p")) {
        rollSleightOfHand();
    }
    else if ((event.altKey) && (event.key == "q")) {
        rollStealth();
    }
    else if ((event.altKey) && (event.key == "r")) {
        rollSurvival();
    }
    else if ((event.ctrlKey) && (event.key == "s")) {
        rollStrengthSave();
    }
    else if ((event.ctrlKey) && (event.key == "d")) {
        rollDexteritySave();
    }
    else if ((event.ctrlKey) && (event.key == "c")) {
        rollConstitutionSave();
    }
    else if ((event.ctrlKey) && (event.key == "i")) {
        rollIntelligenceSave();
    }
    else if ((event.ctrlKey) && (event.key == "w")) {
        rollWisdomSave();
    }
    else if ((event.ctrlKey) && (event.key == "h")) {
        rollCharismaSave();
    }
    else if ((event.ctrlKey) && (event.key == "n")) {

    }
    else if (event.code == "F1") {
    
    }
    else if (event.code == "F5") {
        outputKeybindings();
    }
    else if (event.code == "F11") {
        outputAbout()
    }
});

populatePuppet();