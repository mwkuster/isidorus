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


function makeCreate(psi)
{
    var content = new Element("div", {"class" : CLASSES.content()});
    var header = new Element("h1").update("Create a Topic");
    content.insert(header, {"position" : "bottom"});
    $(CLASSES.subPage()).insert(content, {"position" : "bottom"});

    try{
	var fragmentFrame = new Element("ul", {"class" : CLASSES.fragmentFrame()});
	content.insert({"bottom" : fragmentFrame});
	var liTopicSelect = new Element("li", {"class" : CLASSES.instanceOfFrame()});
	fragmentFrame.insert({"bottom" : liTopicSelect});

	
	function makeInstanceOfFrame(context){
	    function makeFragment(psis, constraints){
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

		var instanceOfs = new Array();
		for(var i = 0; psis && i !== psis.length; ++i){
		    instanceOfs.push(new Array(psis[i]));
		}
		var topic = new TopicC(null, (constraints ? constraints.topicConstraints : null), instanceOfs);
		var liT = new Element("li", {"class" : CLASSES.topicFrame()}).update(topic.getFrame());
		context.insert({"after" : liT});

		var liA = null;
		var associations = null;
		if(constraints && constraints.associationsConstraints && constraints.associationsConstraints.length !== 0){
		    addTopicAsPlayer((constraints ? constraints.associationsConstraints : null), topic.getContent().instanceOfs);
		    associations = new AssociationContainerC(null, (constraints ? constraints.associationsConstraints : null));
		    liA = new Element("li", {"class" : CLASSES.associationContainer()}).update(associations.getFrame());
		    liT.insert({"after" : liA});
		}
		else {
		    liA = liT;
		}

		var tmId = new tmIdC(null);
		var liTm = new Element("li", {"class" : CLASSES.tmIdFrame()}).update(tmId.getFrame());
		liA.insert({"after" : liTm});

		var commitButton = new Element("input", {"type" : "button", "value" : "commit fragment", "style" : "float: right; margin-top: -10px;"})
		commitButton.observe("click", function(event){
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
  		    instanceOf = new InstanceOfC(json.flatten().sort(), makeFragment);
		    context.insert({"bottom" : instanceOf.getFrame()});
		}
		catch(innerErr){
		    alert("There occurred an error by creating an InstanceOfC frame, please reload this page!\n\n" + innerErr);
		}
	    }

	    getTypePsis(onSuccessHandler, null);
	}

	makeInstanceOfFrame(liTopicSelect);
    }catch(err){
	alert(err);
    }
}