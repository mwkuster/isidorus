//+-----------------------------------------------------------------------------
//+  Isidorus
//+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
//+
//+  Isidorus is freely distributable under the LGPL license.
//+  This ajax module uses the frameworks PrototypeJs and Scriptaculous, both
//+  are distributed under the MIT license.
//+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt and
//+  in trunk/src/ajax/javascripts/external/MIT-LICENSE.txt.
//+-----------------------------------------------------------------------------


// --- Returns an Array of the type [<boolean>, <string>].
// --- If there are exclusive-instance-constraints, the return value is an array
// --- of the form [false, "message"] otherwise [true, ""].
function checkExclusiveInstances(constraints, psis){
    try{
	var exc = constraints.topicConstraints.exclusiveInstances;
	var ret = new Array();
	for(var i = 0; i !== psis.length; ++i){
	    var currentArray = new Array(psis[i]);
	    for(var j = 0; j !== exc.length; ++j){
		for(var k = 0; k !== exc[j].exclusives.length; ++k){
		    for(var l = 0; l !== exc[j].exclusives[k].length; ++l){
			if(exc[j].exclusives[k][l] === psis[i]){
			    for(var m = 0; m != exc[j].owner.length; ++m){
				currentArray.push(exc[j].owner[m]);
			    }
			    break;
			}
		    }
		}
	    }

	    if(currentArray.length > 1)ret.push(currentArray);
	}
	if(ret.length === 0) return null;
	return ret;
    }
    catch(err){
	return null;
    }
}


// --- checks SubjectLocator and SubjectIdentifier contraints and contents
function checkIdentifierConstraints(contents, constraints)
{
    var innerConstents = (!contents ? new Array() : contents);
    if((!constraints || constraints.length === 0) && innerConstraints.length === 0) return false;

    for(var i = 0; i != constraints.length; ++i){
	var regexp = constraints[i].regexp;
	var min = constraints[i].cardMin;
	var max = constraints[i].cardMax;

	var foundContents = 0;
	for(var j = 0; j != innerContents.length; ++j){
	    var rex = new RegExp(regexp);
	    if(rex.match(innerContents[j]) === true) ++foundContents;
	}

	if(foundContents < min || foundContents > max) return false;
    }
    return true;
}


// --- Returns an array of rolePlayerConstraints belonging to the given type in roleType.
// --- roleType is of the form [["psi-1", "psi-2", "..."], ["sub-psi-1", "..."], <...>]
function getRolePlayerConstraintsForRole(roleType, rolePlayerConstraints){
    if(!roleType || roleType.length === 0 || !rolePlayerConstraints || rolePlayerConstraints.length === 0) return new Array();
    
    var foundConstraints = new Array();
    var allRoleTypes = roleType.flatten();
    for(var i = 0; i !== rolePlayerConstraints.length; ++i){
	var allCRoleTypes = rolePlayerConstraints[i].roleType.flatten();
	for(var j = 0; j !== allRoleTypes.length; ++j){
	    if(allCRoleTypes.indexOf(allRoleTypes[j]) !== -1){
		foundConstraints.push(rolePlayerConstraints[i]);
		break;
	    }
	}
    }
    return foundConstraints;
}


// --- Returns an array of otherRoleConstraints belonging to the given roleType and players.
// --- roleType is of the form [["psi-1", "psi-2", "..."], ["sub-psi-1", "..."], <...>]
// --- players is of the form [["t1-psi-1", "t1-psi-2", "..."], ["t2-psi-1", "..."], <...>]
function getOtherRoleConstraintsForRole(roleType, players, otherRoleConstraints){
    if(!roleType || roleType.length === 0 || !players || players.length === 0 || !otherRoleConstraints || otherRoleConstraints.length === 0) return new Array();
	
    var foundConstraints = new Array();
    var allRoleTypes = roleType.flatten();
    var allPlayers = players.flatten();
    for(var i = 0; i !== otherRoleConstraints.length; ++i){
	var roleTypeMatches = false;
	var allCRoleTypes = otherRoleConstraints[i].roleType.flatten();
	for(var j = 0; j !== allPlayers.length; ++j){
	    if(allCRoleTypes.indexOf(allRoleTypes[j]) !== -1){
		var allCPlayers = otherRoleConstraints[i].players.flatten();
		for(var k = 0; k !== allPlayers.length; ++k){
		    if(allCPlayers.indexOf(allPlayers[k]) !== -1){
			foundConstraints.push(otherRoleConstraints[i]);
			break;
		    }
		}
		break;
	    }
	}
    }
    return foundConstraints;
}


// --- Returns the sum of all cardMin values of all rolePlayerConstraints.
function getRolePlayerMinForRole(anyRoleConstraints){
    if(!anyRoleConstraints || anyRoleConstraints === 0) return 0;
    var min = 0;
    for(var i = 0; i !== anyRoleConstraints.length; ++i){
	min += parseInt(anyRoleConstraints[i].cardMin);
    }
    return min;
}


// --- Returns the sum of all cardMax values of all rolePlayerConstraints.
function getRolePlayerMaxForRole(anyRoleConstraints){
    if(!anyRoleConstraints || anyRoleConstraints === 0) return 0;
    var max = 0;
    for(var i = 0; i !== anyRoleConstraints.length; ++i){
	if(anyRoleConstraints[i].cardMax === "MAX_INT") return "*";
	else max += parseInt(anyRoleConstraints[i].cardMax);
    }
    return max;
}


// --- checks the cardinalities of all rolePlayerconstraints depending on a
// --- given associationRoleConstraint
function checkCardinalitiesARC_RPC(associationRoleConstraint, rolePlayerConstraints){
    if(!associationRoleConstraint) throw "From checkCardinalitiesARC(): associationRoleConstraint is not set!";
    if(!rolePlayerConstraints || rolePlayerConstraints.length === 0) throw "From checkCardinalitiesARC(): rolePlayerConstraints is not set!";
    var arMin = parseInt(associationRoleConstraint.cardMin);
    var arMax = associationRoleConstraint.cardMax === "MAX_INT" ? "*" : parseInt(associationRoleConstraint.cardMax);
    var rpcs = getRolePlayerConstraintsForRole(associationRoleConstraint.roleType, rolePlayerConstraints);
    var rpMin = getRolePlayerMinForRole(rpcs);
    var rpMax = getRolePlayerMaxForRole(rpcs);
    var type = associationRoleConstraint.roleType.flatten()[0];

    if(rpMax !== "*" && rpMax < arMin) throw "Not enough players available for roletype \"" + type + "\" (rpMax=" + rpMax + ", arMin=" + arMin + ")";
    if(arMax !== "*" && rpMin > arMax) throw "Too much players for the roletype \"" + type + "\" (rpMin=" + rpMin + ", arMax=" + arMax + ")";
}


// --- Returns all listed players of a constraint of the type
// --- roleplayer-constraint or otherrole-constraint and returns them
// --- as an array.
function extractPlayersOfConstraints(anyConstraints){
    var players = new Array();
    if(!anyConstraints || anyConstraints.length === 0) return players;

    for(var i = 0; i !== anyConstraints.length; ++i){
	for(var j = 0; j !== anyConstraints[i].players.length; ++j){
	    players.push(anyConstraints[i].players[j])
	}
    }

    return players;
}


// --- Returns an array of players where the players from playersToClean will
// --- be deleted from allPlayers.
function cleanPlayers(allPlayers, playersToClean){
    var cleanedPlayers = new Array();
    if(!allPlayers) return cleanedPlayers;
    if(!playersToClean) return allPlayers;

    for(var i = 0; i !== allPlayers.length; ++i){
	var toDel = false;
	for(var j = 0; j !== allPlayers[i].length; ++j){
	    for(var k = 0; k !== playersToClean.length; ++k){
		if(playersToClean[k].indexOf(allPlayers[i][j]) !== -1){
		    toDel = true;
		    break;
		}
	    }
	    if(toDel === true) break;
	}
	if(toDel === false) cleanedPlayers.push(allPlayers[i]);
    }
    return cleanedPlayers;
}


// --- Ads the new created topic as a player to all association constraints
// --- where the player-type is one of the topic's instanceOf-topics.
function addTopicAsPlayer(associationsConstraints, topicInstanceOfs){
    if(!associationsConstraints || associationsConstraints.length === 0 || !topicInstanceOfs || topicInstanceOfs.length === 0) return;
    var instanceOfsPsis = topicInstanceOfs.flatten();
    for(var i = 0; i !== associationsConstraints.length; ++i){
	// --- associationrole-constraints
	if(associationsConstraints[i].rolePlayerConstraints){
	    rpcs = associationsConstraints[i].rolePlayerConstraints;
	    for(var j = 0; j !== rpcs.length; ++j){
		for(var k = 0; k !== rpcs[j].playerType.length; ++k){
		    for(var l = 0; l !== rpcs[j].playerType[k].length; ++l){
			if(instanceOfsPsis.indexOf(rpcs[j].playerType[k][l]) !== -1){
			    rpcs[j].players.push(new Array(CURRENT_TOPIC));
			    break;
			}
		    }
		}
	    }
	}

	// --- otherrole-constraints
	if(associationsConstraints[i].otherRoleConstraints){
	    orcs = associationsConstraints[i].otherRoleConstraints;
	    for(var j = 0; j !== orcs.length; ++j){
		for(var k = 0; k !== orcs[j].playerType.length; ++k){
		    for(var l = 0; l !== orcs[j].playerType[k].length; ++l){
			if(instanceOfsPsis.indexOf(orcs[j].playerType[k][l]) !== -1){
			    orcs[j].players.push(new Array(CURRENT_TOPIC));
			    break;
			}
		    }
		}
		for(var k = 0; k !== orcs[j].otherPlayerType.length; ++k){
		    for(var l = 0; l !== orcs[j].otherPlayerType[k].length; ++l){
			if(instanceOfsPsis.indexOf(orcs[j].otherPlayerType[k][l]) !== -1){
			    orcs[j].otherPlayers.push(new Array(CURRENT_TOPIC));
			    break;
			}
		    }
		}
	    }
	}
    }
}