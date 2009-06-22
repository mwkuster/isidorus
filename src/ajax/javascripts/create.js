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


// --- Creates the "create"-sub-page.
function makeCreate(psi)
{
    var content = new Element("div", {"class" : CLASSES.content()});
    var header = new Element("h1").update("Create a Topic");
    content.insert({"bottom" : header});
    $(CLASSES.subPage()).insert({"bottom" : content});

    try{
	var fragmentFrame = new Element("ul", {"class" : CLASSES.fragmentFrame()});
	content.insert({"bottom" : fragmentFrame});
	var liTopicSelect = new Element("li", {"class" : CLASSES.instanceOfFrame()});
	fragmentFrame.insert({"bottom" : liTopicSelect});

	function innerMakeFragment(psis, constraints){
	    makeFragment(liTopicSelect, psis, constraints, null);
	}

	function onSuccessHandler(xhr){
	    var json = null;
	    try{
		json = xhr.responseText.evalJSON();
	    }
	    catch(innerErr){
		alert("Got bad JSON data from " + xhr.request.url + "\n\n" + innerErr);
	    }
	    var instanceOf = null;
	    try{
		instanceOf = new InstanceOfC(json.flatten().sort(), innerMakeFragment);
		liTopicSelect.insert({"bottom" : instanceOf.getFrame()});
	    }
	    catch(innerErr){
		alert("There occurred an error by creating an InstanceOfC frame, please reload this page!\n\n" + innerErr);
	    }
	} //onSuccessHandler
	
	getPsis(onSuccessHandler, null, true);
    }catch(err){
	alert("From makeCreate(): " + err);
    }
}


// --- Creates the sub-elemts Topic, Associations and Topic Maps ID of a Fragment element.
function makeFragment(context, psis, constraints, contents){
    clearFragment();

    var topicContent = null;
    var associationsContent = null;
    var tmContent = null;
    if(contents){
	topicContent = contents.topic;
	associationsContent = contents.associations;
	tmContent = contents.tmIds;
    }
    
    var instanceOfs = new Array();
    for(var i = 0; psis && i !== psis.length; ++i){
	instanceOfs.push(new Array(psis[i]));
    }
    var topic = new TopicC(topicContent, (constraints ? constraints.topicConstraints : null), instanceOfs);
    var liT = new Element("li", {"class" : CLASSES.topicFrame()}).update(topic.getFrame());
    context.insert({"after" : liT});
    
    var liA = null;
    var associations = null;
    if((constraints && constraints.associationsConstraints && constraints.associationsConstraints.length !== 0) || associationsContent && associationsContent.length !== 0){
	addTopicAsPlayer((constraints ? constraints.associationsConstraints : null), topic.getContent().instanceOfs);
	associations = new AssociationContainerC(associationsContent, (constraints ? constraints.associationsConstraints : null));
	liA = new Element("li", {"class" : CLASSES.associationContainer()}).update(associations.getFrame());
	liT.insert({"after" : liA});
    }
    else {
	liA = liT;
    }
    
    var tmId = new TmIdC(tmContent);
    var liTm = new Element("li", {"class" : CLASSES.tmIdFrame()}).update(tmId.getFrame());
    liA.insert({"after" : liTm});
    var commitButton = new Element("input", {"type" : "button", "value" : "commit fragment"})
    commitButton.observe("click", function(event){
	// --- validates the given data
	var ret = true;
	if(topic.isValid() === false) ret = false;
	if(associations && associations.isValid() === false) ret = false;
	if(tmId.isValid() === false) ret = false;

	if(ret === false){
	    alert("The fragment wasn't committed - Please correct your input data!");
	    return;
	}
	
	// --- if the validation succeeded the fragment will be sent to the server
	var tPsis = topic.getContent().subjectIdentifiers;
	var referencedTopics = topic.getReferencedTopics();
	if(associations){
	    referencedTopics = referencedTopics.concat(associations.getReferencedTopics()).without(CURRENT_TOPIC).uniq();
	}
	function onSuccessHandler(topicStubs){
	    var tsStr = "null";
	    if(topicStubs && topicStubs.length !== 0){
		tsStr = "[";
		for(var i = 0; i !== topicStubs.length; ++i){
		    tsStr += topicStubs[i];
		    if(i !== topicStubs.length - 1) tsStr += ",";
		}
		tsStr += "]";
	    }
	    var jTopic = "\"topic\":" + topic.toJSON();
	    var jTopicStubs = "\"topicStubs\":" + tsStr;
	    var jAssociations = "\"associations\":" + (associations ? associations.toJSON().gsub(CURRENT_TOPIC_ESCAPED, tPsis) : "null");
	    var jTmId = "\"tmIds\":" + tmId.toJSON();
	    var json = "{" + jTopic + "," + jTopicStubs + "," + jAssociations + "," + jTmId + "}";
	    commitFragment(json, function(xhr){ alert("The fragment was committed succesfully!"); }, null);
	}
	
	function onErrorHandler(){
	    // --- currently there is not needed a special handling for errors
	    // --- occurred during this operation
	}
	getTopicStubs(referencedTopics, onSuccessHandler, onErrorHandler);
    });
    var liCB = new Element("li", {"class" : CLASSES.commitButton()});
    liCB.update(commitButton);
    liTm.insert({"after" : liCB});
}


// --- removes old elements from the fragment frame
function clearFragment()
{
    var items = $$("li." + CLASSES.topicFrame());
    for(var i = 0; i != items.length; ++i){
	items[i].remove();
    }
    
    items = $$("li." + CLASSES.associationContainer());
    for(var i = 0; i != items.length; ++i){
	items[i].remove();
    }
    
    items = $$("li." + CLASSES.tmIdFrame());
    for(var i = 0; i !== items.length; ++i){
	items[i].remove();
    }
    
    items = $$("li." + CLASSES.commitButton());
    for(var i = 0; i !== items.length; ++i){
	items[i].remove();
    }
}