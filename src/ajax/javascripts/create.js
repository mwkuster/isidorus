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

		addTopicAsPlayer((constraints ? constraints.associationsConstraints : null), topic.getContent().instanceOfs);
		var associations = new AssociationContainerC(null, (constraints ? constraints.associationsConstraints : null), topic);
		var liA = new Element("li", {"class" : CLASSES.associationContainer()}).update(associations.getFrame());
		liT.insert({"after" : liA});

		var commitButton = new Element("input", {"type" : "button", "value" : "commit fragment", "style" : "float: right; margin-top: -10px;"})
		commitButton.observe("click", function(event){
		    try{
		    var tPsis = topic.getContent().subjectIdentifiers;
		    var referencedTopics = topic.getReferencedTopics().concat(associations.getReferencedTopics()).without(CURRENT_TOPIC).uniq();

		    function onSuccessHandler(topicStubs){
			var str = "null";
			if(topicStubs && topicStubs.length !== 0){
			    str = "[";
			    for(var i = 0; i !== topicStubs.length; ++i){
				str += topicStubs[i];
				if(i !== topicStubs.length - 1) str += ",";
			    }
			    str += "]";
			}
			var json = "{\"topic\":" + topic.toJSON() + ",\"topicStubs\":" + str + ",\"associations\":" + associations.toJSON().gsub(CURRENT_TOPIC_ESCAPED, tPsis) + ",\"tmIds\":" + "[\"myTM\"]}";
			alert(json);
		    }

		    function onErrorHandler(){
			// --- currently there is not neede a special handling for errors
			// --- occurred during this operation
		    }

		    getTopicStubs(referencedTopics, onSuccessHandler, onErrorHandler);
		    }catch(err){ alert("test: " + err); }
		});
		var liCB = new Element("li", {"class" : CLASSES.commitButton()});
		liCB.update(commitButton);
		liA.insert({"after" : liCB});
		
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