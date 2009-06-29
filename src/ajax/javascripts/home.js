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


function makeHome()
{
    var content = new Element("div", {"class" : CLASSES.content()});
    var header = new Element("h1").update("Topic Map Overview");
    content.insert({"bottom" : header});
    $(CLASSES.subPage()).insert({"bottom" : content});

    function successFun(xhr){
	var json = null;
	try{
	    json = xhr.responseText.evalJSON();
	}
	catch(innrErr){}

	try{
	    if(json === null) {
		alert("The server's response does not contain any topics!");
	    }
	    else {
		var treeView = new TreeViewC(json);
		content.insert({"bottom" : treeView.getFrame()});
	    }
	}
	catch(err) {
	    alert("Could not create a Topic Map overview: " + err);
	}
    }

    requestTreeView(successFun, null);
}


var TreeViewC = Class.create({"initialize" : function(contents){
                                  if(!contents) throw "From NodeC(): content must be set!";
			          try {
				      this.__frame__ = new Element("ul", {"class" : CLASSES.treeView()});
				      for(var i = 0; i !== contents.length; ++i){
					  var tLi = new Element("li").update(new TreeC(contents[i]).getFrame());
					  this.__frame__.insert({"bottom" : tLi});
				      }
			          }
                                  catch(err){
				      throw "From TreeC(): The following exception was thrown:\n" + err;
			          }
                              },
			      "getFrame" : function(){
				  return this.__frame__;
			      }});


var TreeC = Class.create({"initialize" : function(content){
                              if(!content) throw "From NodeC(): content must be set!";
			      try {
				  this.__frame__ = new Element("ul", {"class" : CLASSES.tree()});
				  this.__frame__.update(new NodeC(content).getFrame());
			      }
                              catch(err){
				  throw "From TreeC(): The following exception was thrown:\n" + err;
			      }
                          },
			  "getFrame" : function(){
			      return this.__frame__;
			  }});


var NodeC = Class.create({"initialize" : function(content){
			      if(!content) throw "From NodeC(): content must be set!";
			      try {
				  this.__frame__ = new Element("li", {"class" : CLASSES.node()});
				  this.__topic__ = new Element("ul", {"class" : CLASSES.topicPsis()});
				  this.__frame__.update(this.__topic__);
				  for(var i = 0; content.topic && i !== content.topic.length; ++i){
				      var tLi = new Element("li").update(content.topic[i]);
				      this.__topic__.insert({"bottom" : tLi});
				  }

				  this.__edit__ = new Element("span", {"class" : CLASSES.clickable()}).update("edit");
				  this.__create__ = new Element("span", {"class" : CLASSES.clickable()}).update("create");
				  if(content.isType !== true){
				      this.__create__.writeAttribute({"class" : CLASSES.disabled()});
				  }
				  else {
				      this.__create__.observe("click", function(event){
					  alert("create");
				      });
				      // TODO: define a handler
				  }

				  if(content.isInstance !== true){
				      this.__edit__.writeAttribute({"class" : CLASSES.disabled()});
				  }
				  else {
				      this.__edit__.observe("click", function(event){
					  alert("edit");
				      });
				      // TODO: define a handler
				  }
				  this.__frame__.update(this.__topic__);
				  this.__frame__.insert({"bottom" : this.__edit__});
				  this.__frame__.insert({"bottom" : "|"});
				  this.__frame__.insert({"bottom" : this.__create__});

				  this.__instances__ = null;
				  this.__subtypes__ = null;

				  if(content.instances && content.instances.length !== 0) {
				      this.__instances__ = new Element("ul", {"class" : CLASSES.instances()});
				      for(var i = 0; i !== content.instances.length; ++i){
					  var entry = new NodeC(content.instances[i]);
					  this.__instances__.insert({"bottom" : entry.getFrame()});
				      }
				  }

				  if(content.subtypes && content.subtypes.length !== 0) {
				      this.__subtypes__ = new Element("ul", {"class" : CLASSES.subtypes()});
				      for(var i = 0; i !== content.subtypes.length; ++i){
					  var entry = new NodeC(content.subtypes[i]);
					  this.__subtypes__.insert({"bottom" : entry.getFrame()});
				      }
				  }

				  if(this.__instances__) this.__frame__.insert({"bottom" : this.__instances__});
				  if(this.__subtypes__) this.__frame__.insert({"bottom" : this.__subtypes__});
			      }
 			      catch(err){
				 throw "From NodeC(): The following exception was thrown:\n" + err;
			     }
			  },
			  "getFrame" : function(){
			      return this.__frame__;
			  }});