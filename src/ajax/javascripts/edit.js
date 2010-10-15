//+-----------------------------------------------------------------------------
//+  Isidorus
//+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
//+
//+  Isidorus is freely distributable under the LLGPL license.
//+  This ajax module uses the frameworks PrototypeJs and Scriptaculous, both
//+  are distributed under the MIT license.
//+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt, and
//+  trunk/docs/LGPL-LICENSE.txt in
//+  trunk/src/ajax/javascripts/external/MIT-LICENSE.txt.
//+-----------------------------------------------------------------------------

function makeEdit(psi)
{
    var content = new Element("div", {"class" : CLASSES.content()});
    var header = new Element("h1").update("Edit a Topic");
    content.insert({"bottom" : header});
    $(CLASSES.subPage()).insert({"bottom" : content});

    try{
	var fragmentFrame = new Element("ul", {"class" : CLASSES.fragmentFrame()});
	content.insert({"bottom" : fragmentFrame});
	var liTopicSelect = new Element("li", {"class" : CLASSES.instanceOfFrame()});
	fragmentFrame.insert({"bottom" : liTopicSelect});

	// --- creates the sub-elements topic, associations and topic map id
	function innerMakeFragment(psis, constraints){
	    function rSuccessHandler(xhr){
		var json = null;
		try{
		    json = xhr.responseText.evalJSON();
		}
		catch(innrErr){}

		makeFragment(liTopicSelect, psis, constraints, json);
	    }
	    requestFragment(psis && psis.length !== 0 ? psis[0] : null, rSuccessHandler, null)
	}
	
	function onSuccessHandler(xhr){
	    var json = null;
	    try{
		json = xhr.responseText.evalJSON();
	    }
	    catch(err){
		alert("Got bad JSON data from " + xhr.request.url + "\n\n" + err);
	    }
	    var edit = null;
	    
	    try{
		if(json === null){
		    var err = new Element("div", {"class" : CLASSES.error()}).update("There exist no valid topic instances!<br/>Please update the TMCL-model or create some new instances.");
		    liTopicSelect.insert({"bottom" : err});   
		}
		else {
		    if(!psi || psi.strip().length === 0) psi = null;
		    edit = new EditC(json.flatten().sort(), innerMakeFragment, psi);
		    liTopicSelect.insert({"bottom" : edit.getFrame()});
		}
	    }
	    catch(err){
		alert("There occurred an error by creating an EditC frame, please reload this page!\n\n" + err);
	    }
	}
	getPsis(onSuccessHandler, null, {"instances" : true});
    }
    catch(err){
	alert("From makeEdit(): " + err);
    }
}
