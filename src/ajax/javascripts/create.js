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