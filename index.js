const jetpack = require("fs-jetpack");
const moment = require("moment/moment");
const { DiceRoll } = require("@dice-roller/rpg-dice-roller");
const { v4: uuidv4 } = require('uuid');

class puppet {
    constructor(system) {
        this.system = system;
        this.name = "Puppet Name";
        this.classification = "Ranger";
        this.level = 1;
        this.race = "Elf";
        this.background = "Outlander";
        this.speed = "30'";
        this.maxhp = 10;
        this.hp = 10;
        this.temphp = 0;
        this.ac = 12;
        this.strength = {"score": 12, "proficient": false};
        this.dexterity = {"score": 13, "proficient": false};
        this.constitution = {"score": 15, "proficient": false};
        this.intelligence = {"score": 10, "proficient": false};
        this.wisdom = {"score": 16, "proficient": false};
        this.charisma = {"score": 10, "proficient": false};
        this.initiative = 1;
        this.inspiration = 0;
        this.spells = new Map();
        this.inventory = new Map();
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

    modifyHitPoints(mod) {
        if (mod > 0) {
            this.hp = this.hp + Math.abs(mod);
            if (this.hp > this.maxhp) this.hp = this.maxhp;
        }
        else if (mod < 0) {
            for (var i = 0; i < Math.abs(mod); i++) {
                if (this.temphp > 0) {
                    this.temphp -= 1;
                }
                else {
                    this.hp -= 1;
                }
                if (this.hp < 0) this.hp = 0;
            }
        }
        $("#char-hp .card-body").html(this.hp);
        $("#char-temp .card-body").html(this.temphp);
    }
}

var win = nw.Window.get();

var spells = new Map();
var items = new Map();

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

function charCard(panel) {
    var html = "<div id='card-" + panel + "' class='card mb-3'>";
    html += "<div class='card-header'>Puppet Sheet</div>";
    html += "<div class='card-body'>";
    html += "   <div class='row mb-3'>";
    html += "       <div class='col-6'>";
    html += "           <input type='text' class='form-control' id='charName' placeholder='Puppet Name'  data-bs-toggle='tooltip' data-bs-title='Puppet Name'>";
    html += "       </div>";
    html += "       <div class='col-6'>";
    html += "           <select class='form-select' id='system'>";
    html += "               <option selected>D&D 5e</option>";
    html += "           </select>";
    html += "       </div>";
    html += "   </div>";
    html += "   <div class='row mb-3'>";
    html += "       <div class='col-6'>";
    html += "           <input type='text' class='form-control' id='charClass' placeholder='Class'>";
    html += "       </div>";
    html += "       <div class='col-6'>";
    html += "           <input type='number' class='form-control' id='charLevel' min='1' max='20' value='1'>";
    html += "       </div>";
    html += "   </div>";
    html += "   <div class='row mb-3'>";
    html += "       <div class='col-6'>";
    html += "           <input type='text' class='form-control' id='charRace' placeholder='Race'>";
    html += "       </div>";
    html += "       <div class='col-6'>"
    html += "           <input type='text' class='form-control' id='charBack' placeholder='Background'>";
    html += "       </div>";
    html += "   </div>";
    html += "   <div class='d-flex mb-3 gap-3 align-items-center'>";
    html += "           <span class='text-nowrap'>Roll Method:</span>";
    html += "           <select class='form-select' id='system'>";
    html += "               <option selected>Dice Decide</option>";
    html += "               <option>Array</option>";
    html += "           </select>";
    html += "           [R]oll";
    html += "   </div>";
    html += "   <div class='row mb-3'>";
    html += "       <div class='col-4'>";
    html += "           <label for='charStr' class='form-label'>Strength</label>";
    html += "           <input type='number' class='form-control' id='charStr' min='1' max='20' value='1'>";
    html += "       </div>";
    html += "       <div class='col-4'>"
    html += "           <label for='charDex' class='form-label'>Dexterity</label>";
    html += "           <input type='number' class='form-control' id='charDex' min='1' max='20' value='1'>";
    html += "       </div>";
    html += "       <div class='col-4'>"
    html += "           <label for='charCon' class='form-label'>Constitution</label>";
    html += "           <input type='number' class='form-control' id='charCon' min='1' max='20' value='1'>";
    html += "       </div>";
    html += "   </div>";
    html += "   <div class='row mb-3'>";
    html += "       <div class='col-4'>";
    html += "           <label for='charInt' class='form-label'>Intelligence</label>";
    html += "           <input type='number' class='form-control' id='charInt' min='1' max='20' value='1'>";
    html += "       </div>";
    html += "       <div class='col-4'>"
    html += "           <label for='charWis' class='form-label'>Wisdom</label>";
    html += "           <input type='number' class='form-control' id='charWis' min='1' max='20' value='1'>";
    html += "       </div>";
    html += "       <div class='col-4'>"
    html += "           <label for='charCha' class='form-label'>Charma</label>";
    html += "           <input type='number' class='form-control' id='charCha' min='1' max='20' value='1'>";
    html += "       </div>";
    html += "   </div>";
    html += "</div>";
    html += "<div class='card-footer'>";
    html += "   Save[Enter] Cancel[Esc]";
    html += "</div>";
    return html;
}

function hpCard(panel, header, hp) {
    var html = "<div id='card-" + panel + "' class='card mb-3'>";
    html += "<div class='card-header'>Modify " + header + "</div>";
    html += "<div id='hp-" + panel + "' class='card-body'>" + hp + "</div>";
    html += "<div class='card-footer'>Apply[Enter] Cancel[Esc]</div>";
    html += "</div>";
    return html;
}

function menuDefault() {
    $("#menu-top").empty();
}

function menuPuppets() {
    $("#menu-top").empty();

}

function outputKeybindings() {
    $("#output").empty();
    jetpack.readAsync("keybindings.html").then(function(data) {
        $("#output").append(data);
    }).catch(function(err) {
        $("#output").append("Error: " + err);
    });
}

function outputAbout() {
    $("#output").empty();
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
    $("#char-str-mod").html(ppt.getAbilityModifier(ppt.strength));
    $("#char-str-val").html(ppt.strength.score);
    $("#char-dex-mod").html(ppt.getAbilityModifier(ppt.dexterity));
    $("#char-dex-val").html(ppt.dexterity.score);
    $("#char-con-mod").html(ppt.getAbilityModifier(ppt.constitution));
    $("#char-con-val").html(ppt.constitution.score);
    $("#char-int-mod").html(ppt.getAbilityModifier(ppt.intelligence));
    $("#char-int-val").html(ppt.intelligence.score);
    $("#char-wis-mod").html(ppt.getAbilityModifier(ppt.wisdom));
    $("#char-wis-val").html(ppt.wisdom.score);
    $("#char-cha-mod").html(ppt.getAbilityModifier(ppt.charisma));
    $("#char-cha-val").html(ppt.charisma.score);
    $("#char-ac-val").html(ppt.getArmorClass());
    $("#char-prof-val").html(ppt.getProficiencyBonus());
    $("#char-init-val").html(ppt.getAbilityModifier("dex"));
    $("#char-hp-val").html(ppt.hp);
    $("#char-max-val").html(ppt.maxhp);
    $("#char-temp-val").html(ppt.temphp);
    $("#char-speed-val").html(ppt.speed);
    $("#char-insp-val").html(ppt.inspiration);
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
    var roll = new DiceRoll("1d20+" + mod);
    $("#output").append(simpleCard(id, "Initiative Roll", roll.output));
    document.getElementById ("card-" + id).scrollIntoView();
}

$(document).on("keyup", function(event) {

    const dices = ["1","2","4","5","6","8","0"];

    console.log("Key: " + event.key + " Code: " + event.code + " dicedice: " + dicedice);

    if (state == "default") {
        if (event.ctrlKey) {
            if (event.key == "h") {
                state = "hp";
                cardpanel = uuidv4();
                hps = -1;
                $("#output").append(hpCard(cardpanel, "Hit Points", hps));
                document.getElementById("hp-" + cardpanel).scrollIntoView();
            }
            else if (event.key == "t") {
                state = "temp";
                cardpanel = uuidv4();
                hps = ppt.temphp;
                $("#output").append(hpCard(cardpanel, "Temp Hit Points", hps));
                document.getElementById("hp-" + cardpanel).scrollIntoView();
            }
            else if (event.key == "m") {
                state = "max";
                cardpanel = uuidv4();
                hps = ppt.maxhp;
                $("#output").append(hpCard(cardpanel, "Max Hit Points", hps));
                document.getElementById("hp-" + cardpanel).scrollIntoView();
            }
            else if (event.key == "s") {
                rollStrengthSave();
            }
            else if (event.key == "d") {
                rollDexteritySave();
            }
            else if (event.key == "c") {
                rollConstitutionSave();
            }
            else if (event.key == "i") {
                rollIntelligenceSave();
            }
            else if (event.key == "w") {
                rollWisdomSave();
            }
            else if (event.key == "h") {
                rollCharismaSave();
            }
            else if (event.key == "n") {
                rollInitiative();
            }
        }
        else if (event.altKey) {
            if (event.key == "a") {
                rollAcrobatics();
            }
            else if (event.key == "b") {
                rollAnimalHandling();
            }
            else if (event.key == "c") {
                rollArcana();
            }
            else if (event.key == "d") {
                rollAthletics();
            }
            else if (event.key == "e") {
                rollDeception();
            }
            else if (event.key == "f") {
                rollHistory();
            }
            else if (event.key == "g") {
                rollInsight();
            }
            else if (event.key == "h") {
                rollIntimidation();
            }
            else if (event.key == "i") {
                rollInvestigation();
            }
            else if (event.key == "j") {
                rollMedicine();
            }
            else if (event.key == "k") {
                rollNature();
            }
            else if (event.key == "l") {
                rollPerception();
            }
            else if (event.key == "m") {
                rollPerformance();
            }
            else if (event.key == "n") {
                rollPersuasion();
            }
            else if (event.key == "o") {
                rollReligion();
            }
            else if (event.key == "p") {
                rollSleightOfHand();
            }
            else if (event.key == "q") {
                rollStealth();
            }
            else if (event.key == "r") {
                rollSurvival();
            }        
        }
        else {
            if (dices.includes(event.key)) {
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
            else if (event.code == "Delete") {
                $("#output").empty();
            }
            else if (event.code == "F1") {
                window.open("/docs/index.html", "_blank");
            }
            else if (event.code == "F5") {
                outputKeybindings();
            }
            else if (event.code == "F11") {
                outputAbout()
            }
            else if (event.key == "p") {
                state = "puppet";
                var id = uuidv4();
                cardpanel = id;
                $("#output").append(charCard(id));
                document.getElementById ("card-" + id).scrollIntoView();
                initTooltips();
            }
        }
    }
    else if (state == "roll") {
        if (event.key == "Backspace") {
            if (dicecount > 1) {
                dicecount -= 1;
            }
            $("#dice-" + cardpanel).html(dicecount + "d" + dicedice);
        }
        else if (event.key == "Enter") {
            var roll = new DiceRoll(dicecount + "d" + dicedice);
            $("#card-" + cardpanel).replaceWith(simpleCard(cardpanel, "Roll Dice", roll.output));
            state = "default";
        }
        else if (event.key == "Escape") {
            cardpanel = "";
            $("#card-" + cardpanel).remove();
            state = "default";
        }
        else if (event.code == dicekey) {
            dicecount += 1;
            $("#dice-" + cardpanel).html(dicecount + "d" + dicedice);
        }
    }
    else if (state == "hp") {
        if (event.key == "-") {
            hps = hps - 1;
            if (hps == 0) hps = -1;
            $("#hp-" + cardpanel).html(hps);
        }
        else if (event.key == "=") {
            hps = hps + 1;
            if (hps == 0) hps = 1;
            $("#hp-" + cardpanel).html(hps);
        }
        else if (event.key == "Enter") {
            var act = "Injured";
            if (hps > 0) act = "Healed";
            $("#card-" + cardpanel).replaceWith(simpleCard(cardpanel, "Hit Points", act + " for " + Math.abs(hps) + " points"));
            ppt.modifyHitPoints(hps);
            state = "default";
        }
        else if (event.code == "Escape") {
            $("#card-" + cardpanel).remove();
            state = "default";
        }
    }
    else if (state == "temp") {
        if (event.key == "-") {
            hps = hps - 1;
            if (hps < 0) hps = 0;
            $("#hp-" + cardpanel).html(hps);
        }
        else if (event.key == "=") {
            hps = hps + 1;
            $("#hp-" + cardpanel).html(hps);
        }
        else if (event.key == "Enter") {
            ppt.temphp = hps;
            $("#card-" + cardpanel).replaceWith(simpleCard(cardpanel, "Temp Hit Points", "Now " + Math.abs(ppt.temphp) + " points"));
            $("#char-temp .card-body").html(ppt.temphp);
            state = "default";
        }
        else if (event.code == "Escape") {
            $("#card-" + cardpanel).remove();
            state = "default";
        }
    }
    else if (state == "max") {
        if (event.key == "-") {
            hps = hps - 1;
            if (hps < 1) hps = 1;
            $("#hp-" + cardpanel).html(hps);
        }
        else if (event.key == "=") {
            hps = hps + 1;
            $("#hp-" + cardpanel).html(hps);
        }
        else if (event.key == "Enter") {
            ppt.maxhp = hps;
            if (ppt.hp > ppt.maxhp) {
                ppt.hp = ppt.maxhp;
                $("#char-hp .card-body").html(ppt.hp);
            }
            $("#card-" + cardpanel).replaceWith(simpleCard(cardpanel, "Max Hit Points", "Now " + Math.abs(ppt.maxhp) + " points"));
            $("#char-max .card-body").html(ppt.maxhp);
            state = "default";
        }
        else if (event.code == "Escape") {
            $("#card-" + cardpanel).remove();
            state = "default";
        }
    }
    else if (state == "puppet") {
        if (event.code == "Escape") {
            $("#card-" + cardpanel).remove();
            state = "default";
        }
        else if (event.key == "Enter") {
        
        }
    }
    else {

    }
});

function initTooltips() {
    document.addEventListener("DOMContentLoaded", function() {
        var tooltips = document.querySelectorAll('[data-bs-toggle="tooltip"]');
        tooltips.forEach(function(tooltip) {
            new bootstrap.Tooltip(tooltip);
        });
    });    
}

initTooltips();
populatePuppet();