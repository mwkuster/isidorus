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