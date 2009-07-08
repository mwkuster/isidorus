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
    var header = new Element("h1").update("Topic Maps Overview");
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


// --- Represents a list of trees.
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


// --- Represents the root of a tree of nodes and contain all tree's nodes.
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


// --- Represents a tree node with a topic as a list of psis,
// --- an edit and a create button. Furter the node can contain
// --- more nodes as listings of instances and subtypes of the
// --- current node's topic.
var NodeC = Class.create({"initialize" : function(content){
			      if(!content) throw "From NodeC(): content must be set!";
			      try {
				  this.__frame__ = new Element("li", {"class" : CLASSES.node()});
				  this.__isMinimized__ = false;

				  function setClickHandler(myself){
				      myself.__frame__.observe("click", function(event){
					  if(myself.__isMinimized__ === false){
					      if(myself.__instances__) myself.__instances__.hide();
					      if(myself.__subtypes__) myself.__subtypes__.hide();
					      myself.__frame__.setStyle({"color" : "#ff7f00"});
					      myself.__isMinimized__ = true;
					  }
					  else {
					      if(myself.__instances__) myself.__instances__.show();
					      if(myself.__subtypes__) myself.__subtypes__.show();
					      myself.__frame__.setStyle({"color" : "inherit"});
					      myself.__isMinimized__ = false;
					  }
					  Event.stop(event);
				      });
				  }
				  setClickHandler(this);
				  
				  if((content.instances && content.instances.length !== 0) || (content.subtypes && content.subtypes.length !== 0)) {
				      this.getFrame().setStyle({"cursor" : "pointer"});
				  }
				  else {
				      this.getFrame().setStyle({"cursor" : "text"});
				  }

				  this.__topic__ = new Element("ul", {"class" : CLASSES.topicPsis()});
				  this.__frame__.insert({"bottom" : this.__topic__});
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
					  setNaviClasses($(PAGES.create));
					  makePage(PAGES.create, content.topic[0]);
					  Event.stop(event);
				      });
				  }

				  if(content.isInstance !== true){
				      this.__edit__.writeAttribute({"class" : CLASSES.disabled()});
				  }
				  else {
				      this.__edit__.observe("click", function(event){
					  setNaviClasses($(PAGES.edit));
					  makePage(PAGES.edit, content.topic[0]);
					  Event.stop(event);
				      });
				  }
				  this.__frame__.insert({"bottom" : this.__edit__});
				  this.__frame__.insert({"bottom" : "<span>|</span>"});
				  this.__frame__.insert({"bottom" : this.__create__});
				  for(var i = 1; content.topic && i < content.topic.length; ++i) this.__frame__.insert({"bottom" : "<br/><span>&nbsp;</span>"});

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