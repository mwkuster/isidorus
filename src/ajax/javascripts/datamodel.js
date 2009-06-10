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

// --- The base class of all Frames defined in this file.
var FrameC = Class.create({"initialize" : function(content, owner, min, max){
                               if(!owner) throw "From FrameC(): owner must be set but is null";
                               if(max !== -1 && (min > max || max === 0))throw "From FrameC(): min must be > max(" + max + ") and > 0 but is " + min;
                               if(!owner.__frames__) owner.__frames__ = new Array();
                               owner.__frames__.push(this);

                               this.__frame__ = new Element("div");
                               this.__remove__ = new Element("span", {"class" : CLASSES.clickable()}).update("-");
                               this.__add__ = new Element("span", {"class" : CLASSES.clickable()}).update("+");

                               checkRemoveAddButtons(owner, min, max);

                               this.__error__ = new Element("div", {"class" : CLASSES.error()});
                               this.__error__.hide();
                               this.__content__ = new Element("span").update(content);

                               this.__frame__.insert({"bottom" : this.__remove__});
                               this.__frame__.insert({"bottom" : this.__content__});
                               this.__frame__.insert({"bottom" : this.__add__});
                               this.__frame__.insert({"bottom" : this.__error__});

                               setRemoveAddHandler(this, owner, min, max, function(){
				   return new FrameC("", owner, min, max);
			       });
                           },                            
			   "getFrame" : function(){
			       return this.__frame__;
			   },
			   "remove" : function(){
			       return this.getFrame().remove();
			   },
			   "hide" : function(){
			       this.getFrame().hide();
			   },
			   "show" : function(){
			       this.getFrame().show();
			   },
			   "getContent" : function(){
			       return this.__content__.textContent;
			   },
			   "toJSON" : function(){
			       return this.getContent().toJSON();
			   },
			   "showError" : function(message){
			       this.__error__.update(message);
			       this.__error__.show();
			   },
			   "hideError" : function(){
			       this.__error__.hide();
			   },
			   "hideRemoveButton" : function(){
			       this.__remove__.hide();
			   },
			   "showRemoveButton" : function(){
			       this.__remove__.show();
			   },
			   "hideAddButton" : function(){
			       this.__add__.hide();
			   },
			   "showAddButton" : function(){
			       this.__add__.show();
			   },
			   "append" : function(elem){
			       return this.getFrame().insert({"after" : elem});
			   }});


// --- This class represents a textrow with the functionality of FrameC plus the method isValid
// --- which returns a boolean value depending on the instance's value and the given regular expression.
var TextrowC = Class.create(FrameC, {"initialize" : function($super, content, regexp, owner, min, max, cssTitle){
                                         $super(content, owner, min, max);
                                         owner.__frames__.pop();
                                         owner.__frames__.push(this);

                                         this.__regexp__ = new RegExp(regexp);
                                         this.__frame__.writeAttribute({"class" : CLASSES.textrowWithRemoveButton()});
                                         this.__content__.remove();
                                         this.__content__ = new Element("input", {"type" : "text", "value" : content});
                                         if(cssTitle && cssTitle.length){
					     this.__content__.writeAttribute({"title" : cssTitle});
					 }
                                         this.__remove__.insert({"after" : this.__content__});

                                         checkRemoveAddButtons(owner, min, max);
                                         setRemoveAddHandler(this, owner, min, max, function(){
					     return new TextrowC("", regexp, owner, min, max, cssTitle);
					 });
                                      },
				     "getContent" : function(){
					 return this.__content__.value;
				     },
				     "isValid" : function(){
					 return this.__regexp__.match(this.getContent());
				     },
				     "showRemoveButton" : function($super){
					 this.__remove__.show();
					 this.getFrame().writeAttribute({"class" : CLASSES.textrowWithRemoveButton()});
				     },
				     "hideRemoveButton" : function(){
					 this.__remove__.hide();
					 this.getFrame().writeAttribute({"class" : CLASSES.textrowWithoutRemoveButton()});
				     }});


// --- This class represents a selectrow with the functionality of FrameC.
var SelectrowC = Class.create(FrameC, {"initialize" : function($super, contents, owner, min, max){
                                           if(!contents || !contents.length)throw "From SelectrowC(): contents must be an array!";
                                           $super(contents, owner, min, max);
                                           owner.__frames__.pop();
                                           owner.__frames__.push(this);

                                           this.__frame__.writeAttribute({"class" : CLASSES.selectrowWithRemoveButton()});
                                           this.__content__.remove();
                                           this.__content__ = new Element("select");
                                           for(var i = 0; i != contents.length; ++i){
					       // --- the attribute value must be set for IE
                                       	       this.__content__.insert({"bottom" : new Element("option", {"value" : contents[i]}).update(contents[i])});
                                           }
                                           this.__remove__.insert({"after" : this.__content__});

                                           checkRemoveAddButtons(owner, min, max);
                                           setRemoveAddHandler(this, owner, min, max, function(){
                                       	       return new SelectrowC(contents, owner, min, max);
                                           });
                                       },
				       "getContent" : function(){
					   return this.__content__.value;
				       },
				       "showRemoveButton" : function(){
					   this.__remove__.show();
					   this.getFrame().writeAttribute({"class" : CLASSES.selectrowWithRemoveButton()});
				       },
				       "hideRemoveButton" : function(){
					   this.__remove__.hide();
					   this.getFrame().writeAttribute({"class" : CLASSES.selectrowWithoutRemoveButton()});
				       }});


// --- The base Class for alomost all frames which contains other frames like names, occurrences, ...
var ContainerC = Class.create({"initialize" : function(){
                                   this.__frame__ = new Element("div");
                                   this.__error__ = new Element("div", {"class" : CLASSES.error()});
                                   this.__error__.hide();
                                   this.__frame__.insert({"bottom" : this.__error__});
                               },
			       "hide" : function(){
				   this.__frame__.hide();
			       },
			       "show" : function(){
				   this.__frame__.show();
			       },
			       "getFrame" : function(){
				   return this.__frame__;
			       },
			       "getContent" : function(unique, removeNull){
				   return "";
			       },
			       "toJSON" : function(unique, removeNull){
				   return this.getContent(unique, removeNull).toJSON();
			       },
			       "showError" : function(message){
				   this.__error__.update(message);
				   this.__error__.show();
			       },
			       "hideError" : function(){
				   this.__error__.hide();
			       },
			       "append" : function(newElem){
				   this.getFrame().insert({"after" : newElem});
			       },
			       "remove" : function(){
				   this.getFrame().remove();
			       },
			       "showRemoveButton" : function(){
				   try{ this.__remove__.show(); } catch(err) {}
			       },
			       "hideRemoveButton" : function(){
				   try{ this.__remove__.hide(); } catch(err) {}
			       },
			       "showAddButton" : function(){
				   try{ this.__add__.show(); } catch(err) {}
			       },
			       "hideAddButton" : function(){
				   try{ this.__add__.hide(); } catch(err) {}
			       }});


// --- Represents a container for all instanceOf-Psis of a fragment's topic
var InstanceOfC = Class.create(ContainerC, {"initialize" : function($super, contents, successFun){
                                                $super();
                                                this.__frame__.writeAttribute({"class" : CLASSES.instanceOfFrame()});
                                                this.__container__ = new Object();
                                                try{
						    var row = new SelectrowC(contents, this.__container__, 1, -1);
                                            	    this.__error__.insert({"before" : row.getFrame()});
                                                }
                                                catch(err){
                                            	    throw "From InstanceOfC(): The following exception was thrown:\n" + err;
                                            	    this.__container__ = null;
                                                }
                                                this.__commit__ = new Element("input", {"type" : "button", "value" : "get constraints"});

                                                function setHandler(myself){
						    function onSuccessHandler(xhr){
							var json = null;
							try{
							    json = xhr.responseText.evalJSON();
							}
							catch(err){
							    alert("Got bad JSON data from " + xhr.request.url + "!\n\n" + err);
							}

							var ret = checkExclusiveInstances(json, myself.getContent(true));
							if(ret){
							    var str = "Some topics own exclusive-instance-constraints, please deselect the corresponding topics!<br/>";
							    for(var i = 0; i != ret.length; ++i){
								for(var j = 0; j != ret[i].length; ++j){
								    if(j === 0){
									str += "<br/>" + ret[i][j];
								    }
								    else {
									str += "&nbsp;&nbsp;&nbsp;&nbsp;" + ret[i][j];
								    }
								    str += "<br/>";
								}
							    }
							    var items = $$("li." + CLASSES.topicFrame());
							    for(var i = 0; i != items.length; ++i){
								items[i].remove();
							    }
							    myself.showError(str);
							}
							else {
							    //successFun(contents, json);							    
							    successFun(myself.getContent(true, true), json);
							}
						    }

                                            	    myself.__commit__.observe("click", function(event){
							myself.hideError();
                                                        requestConstraints(myself.toJSON(true), onSuccessHandler, null);
                                                    });
                                                }
                                                setHandler(this);

                                                this.__error__.insert({"before" : this.__commit__});
                                            },
					    "getContent" : function(unique, removeNull){
						var values = new Array();
						for(var i = 0; i != this.__container__.__frames__.length; ++i){
						    if(unique === true && values.indexOf(this.__container__.__frames__[i].getContent()) !== -1) continue;
						    if(removeNull === true && this.__container__.__frames__[i].getContent().strip().length === 0) continue;
						    values.push(this.__container__.__frames__[i].getContent().strip());
						}
						return values;
					    }});



// --- Representation of a itemIdentity frame.
var ItemIdentityC = Class.create(ContainerC, {"initialize" : function($super, contents){
                                                  $super();
                                                  this.__frame__.writeAttribute({"class" : CLASSES.itemIdentityFrame()});
                                                  this.__container__ = new Object();
 
                                                  try{
                                              	      for(var i = 0; i != contents.length; ++i){
                                              		  new TextrowC(contents[i], ".*", this.__container__, 1, -1, null);
                                              		  this.__error__.insert({"before" : this.__container__.__frames__[i].getFrame()});
                                              	      }
                                                  }
                                                  catch(err){
                                              	      this.__container__ = new Object();
						      new TextrowC("", ".*", this.__container__, 1, -1, null);
                                              	      this.__error__.insert({"before" : this.__container__.__frames__[i].getFrame()});
                                                  }
                                              },
					      "getContent" : function(unique, removeNull){
						  var values = new Array();
						  for(var i = 0; i != this.__container__.__frames__.length; ++i){
						      if(unique === true && values.indexOf(this.__container__.__frames__[i].getContent()) !== -1) continue;
						      if(removeNull === true && this.__container__.__frames__[i].getContent().strip().length === 0) continue;
						      values.push(this.__container__.__frames__[i].getContent().strip());
						  }
						  return values;
					      },
					      "toJSON" : function(unique, removeNull){
						  var content = this.getContent(unique, removeNull);
						  return content.length === 0 ? "null" : content.toJSON();
					      }});


// --- Representation of a subjectLocator and subjectIdentifier frames.
var IdentifierC = Class.create(ContainerC, {"initialize" : function($super, contents, constraints, cssClass){
						$super();
						this.__frame__.writeAttribute({"class" : cssClass});
                                                this.__containers__ = new Array();

						try{
						    if((!contents || contents.length === 0) && constraints && constraints.length > 0){
							for(var i = 0; i != constraints.length; ++i){
							    this.__containers__.push(new Object());
							    var min = parseInt(constraints[i].cardMin);
							    var max = constraints[i].cardMax !== "MAX_INT" ? parseInt(constraints[i].cardMax) : "*";
							    if(max !== 0){
								var cssTitle = "min: " + min + "   max: " + max + "   regular expression: " + constraints[i].regexp;
								for(var j = 0; j != (min === 0 ? 1 : min); ++j){
								    var row = new TextrowC("", constraints[i].regexp, this.__containers__[i],
											   min === 0 ? 1 : min, max === "*" ? -1 : max, cssTitle);
								    this.__error__.insert({"before" : row.getFrame()});
								}
							    }
							}
						    }
						    else {
							// TODO: check already existing contents and order them to the corresponding fields
						    }
						}
						catch(err){
						    alert("From IdentifierC(): " + err);
						}
					    },
					    "getContent" : function(unique, removeNull){
						var values = new Array();
						try{
						    for(var i = 0; i != this.__containers__.length; ++i){
							for(var j = 0; j != this.__containers__[i].__frames__.length; ++j){
							    if(unique === true && values.indexOf(this.__containers__[i].__frames__[j].getContent()) !== -1) continue;
							    if(removeNull === true && this.__containers__[i].__frames__[j].getContent().strip().length === 0) continue;
							    values.push(this.__containers__[i].__frames__[j].getContent().strip());
							}
						    }
						}
						catch(err){
						    return values;
						}
						return values;
					    },
					    "toJSON" : function(unique, removeNull){
						var content = this.getContent(unique, removeNull);
						return content.length === 0 ? "null" : content.toJSON();
					    },
					    "isValid" : function(){
						// TODO: check the validity of this frame with the passed constraints and return a boolean value
						return true;
					    }});


// --- Represantation of a scope frame, doesn't contain SelectrowCs, because the values must be unique!
// --- So this class uses another implementation.
var ScopeC = Class.create(ContainerC, {"initialize" : function($super, contents, min, max){
                                           $super();
                                           this.__frame__.writeAttribute({"class" : CLASSES.scopeFrame()});
                                           this.__error__ = this.__error__.remove();

                                           this.__container__ = null;
                                           this.__contents__ = contents;
                                           this.resetRows(this.__contents__, min, max);
                                        },
				       "resetRows" : function(contents, min, max){
					   try{
					       for(var i = 0; i != this.__container__.__frames__.length; ++i){
						   this.__container__.__frames__[i].remove();
					       }
					       this.__container__ = new Object();
					   }
					   catch(err){
					       this.__container__ = new Object();
					   };

					   this.__contents__ = contents;
					   if(!contents || contents.length < min) throw "From ScopeC.resetRows(): contents.length (" +
					       (contents ? contents.length : "null") + ") must be > min (" + min + ")!";
					   if(max !== -1 && min > max)throw "From FrameC(): min must be > max(" + max + ") and > 0 but is " + min;

					   // --- creates an empty div element
					   if(max === 0){
					       this.getFrame().update("");
					       var div = new Element("div", {"class" : CLASSES.selectrowWithoutRemoveButton()});
					       div.insert({"top" : select});
					       this.getFrame().insert({"bottom" : div});
					       return;
					   }

					   // --- creates an array with all available psis
					   var options = new Array();
					   for(var i = 0; i != contents.length; ++i){
					       var topicPsis = new Array();
					       for(var j = 0; j != contents[i].length; ++j){
						   for(var k = 0; k != contents[i][j].length; ++k){
						       topicPsis.push(contents[i][j][k]);
						   }
					       }
					       options.push(topicPsis);
					   }

					   function checkValues(myself){
					       var rows = myself.getFrame().select("div");
					       var selectedItems = new Array();
					       // --- collects all old selected values and removes the elements
					       for(var i = 0; i != rows.length; ++i){
						   var selects = rows[i].select("select");
						   if(selects[0].value.strip().length !== 0) selectedItems.push(selects[0].value);
						   selects[0].update("");
					       }
					       
					       // --- recreates the original values
					       var values = options.clone();
					       for(var i = 0; i != rows.length && i != selectedItems.length; ++i){
						   var select = rows[i].select("select")[0];
						   var selectedIdx = new Array();
						   for(var j = 0; j != values.length; ++j){
						       if(values[j].indexOf(selectedItems[i]) !== -1){
							   for(var k = 0; k != values[j].length; ++k){
							       select.insert({"bottom" : new Element("option", {"value" : values[j][k]}).update(values[j][k])});
							       if(values[j][k] === selectedItems[i])select.writeAttribute({"selected" : "selected"});
							       //values = values.without(values[j]);
							       selectedIdx.push(j);
							   }
							   break;
						       }
						   }
						   var cleanedValues = new Array();
						   for(var k = 0; k != values.length; ++k){
						       if(selectedIdx.indexOf(k) === -1){
							   cleanedValues.push(values[k]);
							   }
						   }
						   values = cleanedValues;
					       }

					       // --- if there is an empty value "" (if cardMin == 0), this value should be the last
					       // --- in the array (only when there is another value selected)
					       for(var h = 0; h != rows.length; ++h){
						   var select = rows[h].select("select")[0].value;
						   if(select !== ""){
						       for(var i = 0; i != values.length; ++i){
							   for(var j = 0; j != values[i].length; ++j){
							       if(values[i][j].length === 0){
								   values[i] = values[values.length - 1];
								   values[values.length - 1] = new Array("");
							       }
							   }
						       }
						       break;
						   }
					       }

					       // --- fills all empty select elements
					       for(var i = 0; i != rows.length; ++i){
						   var select = rows[i].select("select")[0];
						   if(select.childElements().length === 0 && values.length !== 0){
						       for(var j = 0; j != values[0].length; ++j){
							   select.insert({"bottom" : new Element("option", {"value" : values[0][j]}).update(values[0][j])});
						       }
						       values.shift();
						   }
					       }

					       // --- adds the values which wasn't distributed
					       for(var i = 0; i != rows.length; ++i){
						   var select = rows[i].select("select")[0];
						   for(var j = 0; j != values.length; ++j){
						       for(var k = 0; k != values[j].length; ++k){
							   select.insert({"bottom" : new Element("option" , {"value" :values[j][k]}).update(values[j][k])});
						       }
						   }
					       }
					   }// checkValues

					   
					   function addHandlers(myself){
					       var rows = myself.getFrame().select("div");
					       checkValues(myself);
					       
					       function addHandler(event){
						   var div = new Element("div", {"class" : CLASSES.selectrowWithRemoveButton()});
						   myself.getFrame().insert({"bottom" : div});
						   var select = new Element("select");
						   div.insert({"top" : select});
						   addHandlers(myself);
					       }

					       function removeHandler(event){
						   event.element().up().remove();
						   addHandlers(myself);
					       }

					       for(var i = 0; i != rows.length; ++i){
						   var spans = rows[i].select("span." + CLASSES.clickable());
						   var removeS = null;
						   var addS = null;
						   if(spans.length === 0){
						       removeS = new Element("span", {"class" : CLASSES.clickable()}).update("-");
						       removeS.observe("click", removeHandler);
						       addS = new Element("span", {"class" : CLASSES.clickable()}).update("+");
						       addS.observe("click", addHandler);
						       rows[i].insert({"top" : removeS});
						       rows[i].insert({"bottom" : addS});
						   }
						   else {
						       removeS = spans[0];
						       addS = spans[1];
						   }

						   if(max === -1 || max > rows.length){
						       addS.show()
						   }
						   else {
						       addS.hide();
						   }

						   if(min !== -1 || min < rows.length){
						       removeS.show()
						       rows[i].writeAttribute({"class" : CLASSES.selectrowWithRemoveButton()});
						   }
						   else {
						       removeS.hide();
						       rows[i].writeAttribute({"class" : CLASSES.selectrowWithoutRemoveButton()});
						   }
						   if(i == 0 && rows.length === 1 && max > 1){
						       rows[i].writeAttribute({"class" : CLASSES.selectrowWithoutRemoveButton()});
						       removeS.hide();
						   }
					       }
					   } // addHandlers

					   for(var i = 0; i != (min === -1 ? 1 : min); ++i){
					       var div = new Element("div", {"class" : CLASSES.selectrowWithoutRemoveButton()});
					       var select = new Element("select");
					       for(var j = 0; j != options.length; ++j){
						   if(j === i || j > min){
						       for(var k = 0; k != options[j].length; ++k){
							   select.insert({"bottom" : new Element("option", {"value" : options[j][k]}).update(options[j][k])});
						       }
						   }
					       }

					       
					       div.insert({"top" : select});
					       this.getFrame().insert({"bottom" : div});
					       addHandlers(this);
					   }
				       },
				       "isUsed" : function(){
					   return this.getContent(true, true).length !== 0;
				       },
				       "getContent" : function(unique, removeNull){
					   var values = new Array();
					   try{
					       var rows = this.getFrame().select("div");
					       for(var i = 0; i != rows.length; ++i){
						   var select = rows[i].select("select")[0].value;
						   if(unique === true && values.indexOf(select) !== -1) continue;
						   if(removeNull === true && select.length === 0) continue;
						   values.push(select);
					       }
					   }
					   catch(err){
					       return new Array();
					   }
					   return values;
				       }});



// --- Contains all scope frames of an element (there can be more than one scope constraint)
var ScopeContainerC = Class.create(ContainerC, {"initialize" : function($super, contents, constraints){
                                                    $super();
                                                    this.__frame__.writeAttribute({"class" : CLASSES.scopeContainer()});
                                                    this.__container__ = new Array();
                                                    this.resetValues(contents, constraints);
                                                },
						"resetValues" : function(contents, constraints){
						    try{
							for(var i = 0; i != this.__container__.length; ++i){
							    this.__container__[i].remove();
							}
							this.__container__ = new Array();
						    }
						    catch(err){
							this.__container__ = new Array();
						    }

						    // --- sets contents corresponding to the passed constraints
						    if(constraints && constraints.length){
							for(var i = 0; i != constraints.length; ++i){
							    var scopeTypes = constraints[i].scopeTypes;
							    var min = parseInt(constraints[i].cardMin);
							    var max = constraints[i].cardMax !== "MAX_INT" ? parseInt(constraints[i].cardMax) : "*";
							    
							    // TODO: check and adds contents to the types
							    
							    // --- if min === 0 && there is no content, adds an empty option
							    if(min === 0){ // TODO: check contents of this type
								scopeTypes.unshift(new Array(new Array(""))); // [[""]]
							    }
							    this.__container__.push(new ScopeC(scopeTypes, min === 0 ? 1 : min, max === "*" ? -1 : max));
							    this.__error__.insert({"before" : this.__container__[this.__container__.length - 1].getFrame()});
							}
						    }
						    else {
							this.getFrame().insert({"top" : new Element("div", {"class" : CLASSES.selectrowWithoutRemoveButton()})});
						    }  
						},
						"isUsed" : function(){
						    for(var i = 0; i != this.__container__.length; ++i){
							if(this.__container__[i].isUsed() === true) return true;
						    }
						    return false;
						},
						"isValid" : function(){
						    for(var i = 0; i != this.__container__.length; ++i){
							if(this.__container__[i].isUsed() === true) return true;
						    }
						    return false;
						},
						"getContent" : function(){
						    var values = new Array();
						    try{
						    for(var i = 0; i != this.__container__.length; ++i){
							var cValues = this.__container__[i].getContent(true, true);
							for(var j = 0; j != cValues.length; ++j){
							    if(values.indexOf(cValues[j]) !== -1) continue;
							    values.push(cValues[j]);
							}
						    }
						    }catch(err){
							return new Array();
						    }
						    return values;
						},
						"toJSON" : function(){
						    if(this.getContent().length === 0) return "null";
						    return this.getContent().toJSON();
						}});


// --- Representation of a variant element
var VariantC = Class.create(ContainerC, {"initialize" : function($super, contents, owner){
                                             $super();
                                             if(!owner.__frames__) owner.__frames__ = new Array();
                                             owner.__frames__.push(this);
                                             this.__frame__.writeAttribute({"class" : CLASSES.variantFrame()});
                                             this.__table__ = new Element("table", {"class" : CLASSES.variantFrame()});
                                             this.__frame__.insert({"top" : this.__table__});
    
                                             try{
						 // --- control row + itemIdentity
						 makeControlRow(this, 4, contents ? contents.itemIdentities : null);
						 checkRemoveAddButtons(owner, 1, -1);
						 setRemoveAddHandler(this, owner, 1, -1, function(){
						     return new VariantC(null, owner);
						 });
						 
						 // --- scopes
						 this.__scopes__ = null;
						 //TODO: implement -> also in the server
						 this.__table__.insert({"bottom" : newRow(CLASSES.scopeContainer(), "Scope", new Element("div"))});
						 
						 // --- resource value and datatype
						 makeResource(this, contents, null, null, null);
						 
						 // --- minimize
						 this.minimize();
					     }
                                             catch(err){
						 alert("From VariantC(): " + err);
                                             }
                                         },
					 "getContent" : function(){
					     var resourceRef = null;
					     var resourceData = null;
					     if(this.__datatype__.__frames__[0].getContent() === ANY_URI){
						 resourceRef = this.__value__.textContent.strip();
					     }
					     else {
						 var datatype = STRING;
						 if(this.__datatype__.__frames__[0].getContent().strip() !== "")
						     datatype = this.__datatype__.__frames__[0].getContent().strip();
						 resoureceData = {"datatype" : datatype, "value" : this.__value__.textContent.strip()};
					     }

					     // TODO: scopes
					     if(this.__itemIdentity__.getContent(true, true).length === 0 &&
						resourceRef === null && resourceData === null) return null;
					     return {"itemIdentities" : this.__itemIdentity__.getContent(true, true),
						     "scopes" : null,
						     "resourceRef" : resourceRef,
						     "resourceData" : resourceData};
					     
					 },
					 "toJSON" : function(){
					     var resourceRef = null;
					     var resourceData = null;
					     if(this.__datatype__.__frames__[0].getContent() === ANY_URI){
						 resourceRef = this.__value__.value.strip().toJSON();
					     }
					     else {
						 var datatype = STRING.toJSON();
						 if(this.__datatype__.__frames__[0].getContent().strip() !== "")
						     datatype = this.__datatype__.__frames__[0].getContent().strip().toJSON();
						 resourceData = "{\"datatype\":" + datatype + ",\"value\":" + this.__value__.value.strip().toJSON() + "}";
					     }

					     // TODO: scopes
					     return "{\"itemIdentities\":" + this.__itemIdentity__.toJSON(true, true) + 
						 ",\"scopes\":null,\"resourceRef\":" +  resourceRef + ",\"resourceData\":" + resourceData + "}";
					     
					 },
					 "isValid" : function(){
					     return this.__value__.value.strip() !== "";
					 },
					 "isUsed" : function(){
					     return (this.__itemIdentity__.getContent(true, true).length !== 0 || 
						     this.__value__.value.strip() !== "" || this.__datatype__.__frames__[0].getContent().strip() !== "");
					 },
					 "showRemoveButton" : function(){
					     this.__remove__.show();
					 },
					 "hideRemoveButton" : function(){
					     this.__remove__.hide();
					 },
					 "showAddButton" : function(){
					     this.__add__.show();
					 },
					 "hideAddButton" : function(){
					     this.__add__.hide();
					 },
					 "minimize" : function(){
					     var trs = this.__table__.select("tr");
					     for(var i = 0; i != trs.length; ++i){
						 if(i === 0) trs[i].show();
						 else trs[i].hide();
					     }
					 }});


// --- contains all variants of a name element
var VariantContainerC = Class.create(ContainerC, {"initialize" : function($super, contents){
                                                      $super();
                                                      this.__frame__.writeAttribute({"class" : CLASSES.variantContainer()});
                                                      this.__container__ = new Object();

                                                      if(contents && contents.length != 0){
							  for(var i = 0; i != contents.length; ++i){
							      var variant = new VariantC(contents[i], this.__container__);
							      this.__frame__.insert({"bottom" : variant.getFrame()});
							  }
						      }
                                                      else {
							  var variant = new VariantC(null, this.__container__);
							  this.__frame__.insert({"bottom" : variant.getFrame()});
						      }
                                                  },
						  "getContent" : function(){
						      var values = new Array();
						      for(var i = 0; i != this.__container__.__frames__.length; ++i){
							  if(this.__container__.__frames__[i].isUsed() === true){
							      values.push(this.__container__.__frames__[i].getContent());
							  }
						      }
						      return values;
						  },
						  "isValid" : function(){
						      for(var i = 0; i != this.__container__.__frames__.length; ++i){
							  if(this.__container__.__frames__[i].isUsed() === true && 
							     this.__container__.__frames__[i].isValid() === false) return false;
						      }
						      return true;
						  },
						  "toJSON" : function(){
						      var str = "[";
						      for(var i = 0; i != this.__container__.__frames__.length; ++i){
							  if(this.__container__.__frames__[i].isUsed() === true){
							      str += this.__container__.__frames__[i].toJSON();
							  }
							  if(i < this.__container__.__frames__.length - 1){
							      str += ","
							  }
						      }
						      str += "]";
						      return str === "[]" ? null : str;
						  }});


// --- representation of a name element
var NameC = Class.create(ContainerC, {"initialize" : function($super, contents, nametypescopes, simpleConstraint, owner, min, max, cssTitle){
                                          $super();
                                          if(!owner) throw "From NameC(): owner must be set but is null";
                                          if(max !== -1 && (min > max || max === 0))throw "From FrameC(): min must be > max(" + max + ") and > 0 but is " + min;
                                          if(!owner.__frames__) owner.__frames__ = new Array();
                                          owner.__frames__.push(this);
    
                                          this.__frame__.writeAttribute({"class" : CLASSES.nameFrame()});
                                          this.__table__ = new Element("table", {"class" : CLASSES.nameFrame()});
                                          this.__frame__.insert({"top" : this.__table__});
    
                                          try{
					      // --- control row + ItemIdentity
					      makeControlRow(this, 5, contents ? contents.itemIdentities : null);
					      checkRemoveAddButtons(owner, min, max);
					      setRemoveAddHandler(this, owner, min, max, function(){
						  return new NameC(null, nametypescopes, simpleConstraint, owner, min, max, cssTitle);
					      });
					      
					      // --- type
					      var types = new Array();
					      for(var i = 0; nametypescopes && i !== nametypescopes.length; ++i){
						  for(var j = 0; j != nametypescopes[i].nameType.length; ++j){
						      types.push(nametypescopes[i].nameType[j]);
						      if(contents && contents.type && contents.type[0] === nametypescopes[i].nameType[j]){
							  var selected = nametypescopes[i].nameType[j];
							  types[types.length - 1] = types[0];
							  types[0] = selected;
						      }
						  }
					      }
					      this.__type__ = new Object();
					      var tr = newRow(CLASSES.typeFrame(), "Type", new SelectrowC(types, this.__type__, 1, 1).getFrame());
					      this.__table__.insert({"bottom" : tr});
					      
					      // --- scopes
					      this.__scope__ = new ScopeContainerC(contents && contents.scopes ? scopes : null, nametypescopes && nametypescopes[0].scopeConstraints ? nametypescopes[0].scopeConstraints : null);
					      this.__table__.insert({"bottom" : newRow(CLASSES.scopeContainer(), "Scope", this.__scope__.getFrame())});
					      onTypeChangeScope(this, contents ? contents.scopes : null, nametypescopes, "name");

					      // --- value
					      this.__value__ = new Object();
					      var _min = parseInt(simpleConstraint.cardMin);
					      var _max = simpleConstraint.cardMax !== "MAX_INT" ? parseInt(simpleConstraint.cardMax) : "*";
					      var cssTitleV = "min: " + _min + "   max: " + _max + "   regular expression: " + (simpleConstraint ? simpleConstraint.regexp : ".*");
					      new TextrowC((contents ? contents.value : ""), (simpleConstraint ? simpleConstraint.regexp : ".*"), this.__value__, 1, 1, cssTitleV);
					      this.__table__.insert({"bottom" : newRow(CLASSES.valueFrame(), "Value", this.__value__.__frames__[0].getFrame())});
					      
					      // --- variants
					      this.__variants__ = new VariantContainerC(contents? contents.variants : null);
					      this.__table__.insert({"bottom" : newRow(CLASSES.variantContainer(), "Variants", this.__variants__.getFrame())});
					      
					      // --- adds a second show handler, so the variants will be hidden, when the entire
					      // --- name element will be shown
					      function addSecondShowHandler(myself){
						  myself.__table__.select("tr")[0].observe("click", function(event){
						      try{
							  for(var i = 0; i != myself.__variants__.__container__.__frames__.length; ++i){
							      myself.__variants__.__container__.__frames__[i].minimize();
							  }
						      }catch(tmp){ alert(tmp);}
						  });
					      }
					      
					      addSecondShowHandler(this);
                                          }
                                          catch(err){
                                      	      alert("From NameC(): " + err);
                                          }
                                      },
				      "getContent" : function(){
					  if(this.isUsed() === false) return null;
					  return {"itemIdentities" : this.__itemIdentity__.getContent(true, true),
						  "scopes" : this.__scope__.getContent(),
						  "value" : this.__value__.__frames__[0].getContent(),
						  "variants" : this.__variants__.getContent()};
				      },
				      "toJSON" : function(){
					  if(this.isUsed() === false) return "null";
					  return "{\"itemIdentities\":" + this.__itemIdentity__.toJSON(true, true) +
					      ",\"type\":[" + this.__type__.__frames__[0].toJSON() +
					      "],\"scopes\":" + this.__scope__.toJSON() + 
					      ",\"value\":" + this.__value__.__frames__[0].toJSON() +
					      ",\"variants\":" + this.__variants__.toJSON() + "}";
				      },
				      "isUsed" : function(){
					  return this.__itemIdentity__.getContent(true, true).length !== 0 ||
					      this.__value__.__frames__[0].getContent().strip().length !== 0 ||
					      this.__variants__.getContent().length !== 0;
				      },
				      "isValid" : function(){
					  // TODO: check the content and the constraints + variants.isValid()
					  return true;
				      },
				      "minimize" : function(){
					  var trs = this.__table__.select("tr");
					  for(var i = 0; i != trs.length; ++i){
					      if(i === 0) trs[i].show();
					      else trs[i].hide();
					  }
				      }});



// --- contains all names of a topic
var NameContainerC = Class.create(ContainerC, {"initialize" : function($super, contents, constraints){
                                                   $super();
                                                   this.__frame__.writeAttribute({"class" : CLASSES.nameContainer()});
                                                   this.__containers__ = new Array();

                                                   try{
						       if((!contents || contents.length === 0) && constraints && constraints.length > 0){
							   for(var i = 0; i != constraints.length; ++i){
							       this.__containers__.push(new Array());
							       for(var j = 0; j != constraints[i].constraints.length; ++j){
								   this.__containers__[i].push(new Object());
								   var min = parseInt(constraints[i].constraints[j].cardMin);
								   var max = constraints[i].constraints[j].cardMax !== "MAX_INT" ? parseInt(constraints[i].constraints[j].cardMax) : "*";
								   var regexp = constraints[i].constraints[j].regexp;
								   if(max !== 0){
								       var title = "min: " + min + "   max: " + max + "   regular expression: " + regexp;
								       var name = new NameC("", constraints[i].nametypescopes, constraints[i].constraints[j],
											    this.__containers__[i][j], min === 0 ? 1 : min, max === "*" ? -1 : max, title);
								       this.__error__.insert({"before" : name.getFrame()});
								       if(min === 0)name.minimize();
								   }
							       }
							   }
                                               	       }
                                               	       else {
                                               		   // TODO: check already existing contents and order them to the corresponding fields
                                               	       }
                                                   }
                                                   catch(err){
                                               	       alert("From NameContainerC(): " + err);
                                                   }
                                               },
					       "getContent" : function(){
						   var values = new Array();
						   try{
						       for(var i = 0; i != this.__containers__.length; ++i){
							   for(var j = 0; j != this.__containers__[i].length; ++j){
							       for(var k = 0; k != this.__containers__[i][j].__frames__.length; ++k){
								   if(this.__containers__[i][j].__frames__[k].isUsed() === true){
								       values.push(this.__containers__[i][j].__frames__[k].getContent());
								   }
							       }
							   }
						       }
						       return values;
						   }
						   catch(err){
						       return values;
						   }
						},
					       "toJSON" : function(){
						   try{
						       var str = "[";
						       for(var i = 0; i != this.__containers__.length; ++i){
							   for(var j = 0; j != this.__containers__[i].length; ++j){
							       for(var k = 0; k != this.__containers__[i][j].__frames__.length; ++k){
								   if(this.__containers__[i][j].__frames__[k].isUsed() === true){
								       str += this.__containers__[i][j].__frames__[k].toJSON() + ",";
								   }
							       }
							   }
						       }
						       if(str.endsWith(",")) str = str.slice(0, str.length - 1);
						       str += "]";
						       return str === "[]" ? null : str;
						   }
						   catch(err){
						       return "null";
						   }
					       },
					       "isValid" : function(){
						   // TODO: check the validity of this frame with the passed constraints and return a boolean value + isValid() of all names
						   return true;
					       }});


// --- represenation of an occurrence element
var OccurrenceC = Class.create(ContainerC, {"initialize" : function($super, contents, occurrenceTypes, constraint, uniqueConstraints, owner, min, max, cssTitle){
                                                $super();
                                                if(!owner.__frames__) owner.__frames__ = new Array();
                                                owner.__frames__.push(this);
                                                this.__frame__.writeAttribute({"class" : CLASSES.occurrenceFrame()});
                                                this.__table__ = new Element("table", {"class" : CLASSES.occurrenceFrame()});
                                                this.__frame__.insert({"top" : this.__table__});

                                                try{
						    // --- control row + itemIdentity
						    makeControlRow(this, 5, contents ? contents.itemIdentities : null);
						    checkRemoveAddButtons(owner, 1, max);
						    setRemoveAddHandler(this, owner, 1, max, function(){
							return new OccurrenceC(null, occurrenceTypes, constraint, uniqueConstraints, owner, min, max, cssTitle);
						    });

						    // --- type
						    var types = new Array();
						    for(var i = 0; occurrenceTypes && i !== occurrenceTypes.length; ++i){
							for(var j = 0; j != occurrenceTypes[i].occurrenceType.length; ++j){
							    types.push(occurrenceTypes[i].occurrenceType[j]);
							    if(contents && contents.type && contents.type[0] === ooccurrenceTypes[i].occurrenceType[j]){
								var selected = occurrenceTypes[i].occurrenceType[j];
								types[types.length - 1] = types[0];
								types[0] = selected;
							    }
							}
						    }
						    this.__type__ = new Object();
						    var tr = newRow(CLASSES.typeFrame(), "Type", new SelectrowC(types, this.__type__, 1, 1).getFrame());
						    this.__table__.insert({"bottom" : tr});

						    // --- scopes
						    this.__scope__ = new ScopeContainerC(contents && contents.scopes ? contents.scopes : null, occurrenceTypes && occurrenceTypes[0].scopeConstraints ? occurrenceTypes[0].scopeConstraints : null);
						    this.__table__.insert({"bottom" : newRow(CLASSES.scopeContainer(), "Scope", this.__scope__.getFrame())});
						    onTypeChangeScope(this, contents.scopes, occurrenceTypes, "occurrence");

						    // --- resource value and datatype
						    var _min = parseInt(constraint.cardMin);
						    var _max = constraint.cardMax !== "MAX_INT" ? parseInt(constraint.cardMax) : "*";
						    var cssTitle = "min: " + _min + "   max: " + _max + "   regular expression: " + constraint.regexp;
						    makeResource(this, contents, constraint, (occurrenceTypes ? occurrenceTypes[0].datatypeConstraint : null), cssTitle);
						}
                                                catch(err){
						    alert("From OccurrenceC(): " + err);
                                                }	 
                                            },
					    "getContent" : function(){
						if(this.isUsed() === true){
						    var resourceRef = null;
						    var resourceData = null;
						    if(this.__datatype__.__frames__[0].getContent() === ANY_URI){
							resourceRef = this.__value__.value;
						    }
						    else {
							resourceData = {"datatype" : this.__datatype__.__frames__[0].getContent(),
									"value" : this.__value__.value};
						    }
						    return {"itemIdentities" : this.__itemIdentity__.getContent(true, true),
							    "type" : new Array(this.__type__.__frames__[0].getContent()),
							    "scopes" : this.__scope__.getContent(),
							    "resourceRef" : resourceRef,
							    "resourceData" : resourceData};
						}
						else {
						    return null;
						}
					    },
					    "toJSON" : function(){
						if(this.isUsed() === true){
						    var resourceRef = "null";
						    var resourceData = "null";
						    if(this.__datatype__.__frames__[0].getContent() === ANY_URI){
							resourceRef = this.__value__.value.toJSON();
						    }
						    else {
							resourceData = "{\"datatype\":" + this.__datatype__.__frames__[0].toJSON() +
							    ",\"value\":" + this.__value__.value.toJSON() + "}";
						    }
						    return "{\"itemIdentities\":" + this.__itemIdentity__.toJSON(true, true) +
							",\"type\":[" + this.__type__.__frames__[0].toJSON() +
							"],\"scopes\":" + this.__scope__.toJSON() +
							",\"resourceRef\":" + resourceRef +
							",\"resourceData\":" + resourceData + "}";
						}
						else {
						    return "null";
						}
					    },
					    "isUsed" : function(){
						return this.__itemIdentity__.getContent(true, true).length !== 0 ||
						    this.__value__.value.strip().length !== 0;
					    },
					    "isValid" : function(){
						// TODO: check the content and the constraints
						return true;
					    },
					    "minimize" : function(){
						var trs = this.__table__.select("tr");
						for(var i = 0; i != trs.length; ++i){
						    if(i === 0) trs[i].show();
						    else trs[i].hide();
						}
					    }});
			       

// --- contains all occurrences of an topic element
var OccurrenceContainerC = Class.create(ContainerC, {"initialize" : function($super, contents, constraints){
						         $super();
						         this.__containers__ = new Array();
                                                         this.__frame__.writeAttribute({"class" : CLASSES.occurrenceContainer()});

						         try{
							     if((!contents || contents.length === 0) && constraints && constraints.length > 0){
								 for(var i = 0; i != constraints.length; ++i){
								     this.__containers__.push(new Array());
								     for(var j = 0; j != constraints[i].constraints.length; ++j){
									 this.__containers__[i].push(new Object());
									 var min = parseInt(constraints[i].constraints[j].cardMin);
									 var max = constraints[i].constraints[j].cardMax !== "MAX_INT" ? parseInt(constraints[i].constraints[j].cardMax) : "*";
									 var regexp = constraints[i].constraints[j].regexp;
									 if(max !== 0){
									     var title = "min: " + min + "   max: " + max + "   regular expression: " + regexp;
									     var occurrence = new OccurrenceC("", constraints[i].occurrenceTypes, constraints[i].constraints[j],
													      constraints[i].uniqueConstraints, this.__containers__[i][j],
													      min === 0 ? 1 : min, max === "*" ? -1 : max, title);
									     this.__error__.insert({"before" : occurrence.getFrame()});
									     if(min === 0)occurrence.minimize();
									 }
								     }
								 }
                                               		     }
                                               		     else {
                                               			 // TODO: check already existing contents and order them to the corresponding fields
                                               		     }
							 }
						         catch(err){
							     alert("From OccurrenceContainerC(): " + err);
						         }
						     },
						     "isValid" : function(){
							 // TODO: implement this method
							 return true;
						     },
						     "getContent" : function(){
							 var values = new Array();
							 try{
							     for(var i = 0; i != this.__containers__.length; ++i){
								 for(var j = 0; j != this.__containers__[i].length; ++j){
								     for(var k = 0; k != this.__containers__[i][j].__frames__.length; ++k){
									 if(this.__containers__[i][j].__frames__[k].isUsed() === true){
									     values.push(this.__containers__[i][j].__frames__[k].getContent());
									 }
								     }
								 }
							     }
							     return values;
							 }
							 catch(err){
							     return values;
							 }
						     },
						     "toJSON" : function(){							 
							 try{
							     var str = "[";
							     for(var i = 0; i != this.__containers__.length; ++i){
								 for(var j = 0; j != this.__containers__[i].length; ++j){
								     for(var k = 0; k != this.__containers__[i][j].__frames__.length; ++k){
									 if(this.__containers__[i][j].__frames__[k].isUsed() === true){
									     str += this.__containers__[i][j].__frames__[k].toJSON() + ",";
									 }
								     }
								 }
							     }
							     if(str.endsWith(",")) str = str.slice(0, str.length - 1);
							     str += "]";
							     return str === "[]" ? null : str;
							 }
							 catch(err){
							     return "null";
							 }
						     }});
						    

// --- representation of a topic element.
var TopicC = Class.create(ContainerC, {"initialize" : function($super, content, constraints, instanceOfs){
                                           $super();
                                           this.__minimized__ = false;
                                           this.__instanceOfs__ = (!instanceOfs || instanceOfs.length === 0 ? null : instanceOfs);
                                           try{
					       this.__frame__ .writeAttribute({"class" : CLASSES.topicFrame()});
					       this.__table__ = new Element("table", {"class" : CLASSES.topicFrame()});
					       this.__frame__.insert({"top" : this.__table__});
					       this.__caption__ = new Element("caption", {"class" : CLASSES.clickable()}).update("Topic");
					       this.__table__.insert({"top" : this.__caption__});

					       function setMinimizeHandler(myself){
						   myself.__caption__.observe("click", function(event){
						       myself.minimize();
						   });
					       }
					       setMinimizeHandler(this);
					       
					       // --- topic id
					       this.__topicid__ = new Object();
					       new TextrowC((content ? content.topicid : null), ".*", this.__topicid__, 1, 1, null);
					       this.__table__.insert({"bottom" : newRow(CLASSES.topicIdFrame(), "Topic ID", this.__topicid__.__frames__[0].getFrame())});
					       
					       // --- itemIdentity
					       this.__itemIdentity__ = new ItemIdentityC(content ? content.itemIdentities : null);
					       this.__table__.insert({"bottom" : newRow(CLASSES.itemIdentityFrame(), "ItemIdentity", this.__itemIdentity__.getFrame())});

					       // --- subjectLocator
					       var _contents = (content ? content.subjectLocators : null);
					       var _constraints = (constraints ? constraints.subjectLocatorConstraints : null);
					       this.__subjectLocator__ = new IdentifierC(_contents, _constraints, CLASSES.subjectLocatorFrame());
					       this.__table__.insert({"bottom" : newRow(CLASSES.subjectLocatorFrame(), "SubjectLocator", this.__subjectLocator__.getFrame())});

					       // --- subjectIdentifier
					       _contents = (content ? content.subjectIdentifiers : null);
					       _constraints = (constraints ? constraints.subjectIdentifierConstraints : null);
					       this.__subjectIdentifier__ = new IdentifierC(_contents, _constraints, CLASSES.subjectIdentifierFrame());
					       this.__table__.insert({"bottom" : newRow(CLASSES.subjectIdentifierFrame(), "SubjectIdentifier", this.__subjectIdentifier__.getFrame())});

					       // --- names
					       _contents = (content ? content.names : null);
					       _constraints = (constraints ? constraints.topicNameConstraints : null);
					       this.__name__ = new NameContainerC(_contents, _constraints);
					       this.__table__.insert({"bottom" : newRow(CLASSES.nameContainer(), "Names", this.__name__.getFrame())});
					       
					       // --- occurrences
					       _contents = (content ? content.occurrences : null);
					       _constraints = (constraints ? constraints.topicOccurrenceConstraints : null);
					       this.__occurrence__ = new OccurrenceContainerC(_contents, _constraints);
					       this.__table__.insert({"bottom" : newRow(CLASSES.occurrenceContainer(), "Occurrences", this.__occurrence__.getFrame())});
					   }catch(err){
					       alert("From TopciC(): " + err);
					   }
                                       },
				       "isValid" : function(){
					   // TODO: implement
					   return true;
				       },
				       "getContent" : function(){
					   try{
					   return {"id" : this.__topicid__.__frames__[0].getContent().strip(),
						   "itemIdentities" : this.__itemIdentity__.getContent(true, true),
						   "subjectLocators" : this.__subjectLocator__.getContent(true, true),
						   "subjectIdentifiers" : this.__subjectIdentifier__.getContent(true, true),
						   "instanceOfs" : this.__instanceOfs__,
						   "names" : this.__name__.getContent(),
						   "occurrences" : this.__occurrence__.getContent()};
					   }
					   catch(err){
					       return null;
					   }
				       },
				       "toJSON" : function(){
					   try{
					       return "{\"id\":" + this.__topicid__.__frames__[0].getContent().strip().toJSON() +
						   ",\"itemIdentities\":" + this.__itemIdentity__.toJSON(true, true) + 
						   ",\"subjectLocators\":" + this.__subjectLocator__.toJSON(true, true) +
						   ",\"subjectIdentifiers\":" + this.__subjectIdentifier__.toJSON(true, true) +
						   ",\"instanceOfs\":" + (!this.__instanceOfs__ ? "null" : this.__instanceOfs__.toJSON()) +
						   ",\"names\":" + this.__name__.toJSON() +
						   ",\"occurrences\":" + this.__occurrence__.toJSON() + "}";
					   }
					   catch(err){
					       return "null";
					   }
				       },
				       "minimize" : function(){
					   var rows = new Array();
					   rows.push(this.getFrame().select("tr." + CLASSES.topicIdFrame())[0],
						     this.getFrame().select("tr." + CLASSES.itemIdentityFrame())[0],
						     this.getFrame().select("tr." + CLASSES.subjectLocatorFrame())[0],
						     this.getFrame().select("tr." + CLASSES.subjectIdentifierFrame())[0],
						     this.getFrame().select("tr." + CLASSES.nameContainer())[0],
						     this.getFrame().select("tr." + CLASSES.occurrenceContainer())[0]);
					   for(var i = 0; i != rows.length; ++i){
					       if(this.__minimized__ === false) rows[i].hide();
					       else rows[i].show();
					   }
					   this.__minimized__ = !this.__minimized__;
				       },
				       "hasPsi" : function(){
					   return this.__subjectIdentifier__.getContent(true, true).length !== 0;
				       }});


// --- representation of a role element.
var RoleC = Class.create(ContainerC, {"initialize" : function($super, itemIdentities, roleTypes, rolePlayers, owner){
				          $super();
				          if(!owner.__frames__) owner.__frames__ = new Array();
                                          if(!roleTypes || roleTypes.length === 0) throw "From RoleC(): roleTypes must be set!";
                                          if(!rolePlayers || rolePlayers.length === 0) throw "From RoleC(): rolePlayers must be set";
				          owner.__frames__.push(this);
				          this.__frame__.writeAttribute({"class" : CLASSES.roleFrame()});
				          this.__table__ = new Element("table", {"class" : CLASSES.roleFrame()});
				          this.__frame__.insert({"top" : this.__table__});
                                          this.__roleTypes__ = roleTypes;
                                          this.__rolePlayers__ = rolePlayers;
    
				          try{
					      // --- control row + itemIdentity
					      makeControlRow(this, 3, itemIdentities); // make control row have to be changed to a separate control row for roles
					      checkRemoveAddButtons(owner, 1, -1);
					      setRemoveAddHandler(this, owner, 1, -1, function(){ /*do nothing*/ });
					      // --- gets the add and remove button
					      var cTd = this.__table__.select("tr." + CLASSES.itemIdentityFrame())[0].select("td." + CLASSES.controlColumn())[0].select("span." + CLASSES.clickable());
					      this.__removeButton__ = cTd[1];
					      this.__addButton__ = cTd[2];

					      // --- type
					      var types = this.__roleTypes__.flatten();
					      this.__type__ = new Object();
					      var tr = newRow(CLASSES.typeFrame(), "Type", new SelectrowC(types, this.__type__, 1, 1).getFrame());
					      this.__table__.insert({"bottom" : tr});

					      // --- player
					      var players = this.__rolePlayers__.flatten();
					      this.__player__ = new Object();
					      tr = newRow(CLASSES.playerFrame(), "Player", new SelectrowC(players, this.__player__, 1, 1).getFrame());
					      this.__table__.insert({"bottom" : tr});
					  }
				          catch(err){
					      alert("From RoleC(): " + err);
					  }
				      },
				      "setAddHandler" : function(handler){
					  if(!handler) return;
					  this.__addButton__.stopObserving();
					  var addButton = this.__addButton__;
					  function addHandler(myself){
					      addButton.observe("click", function(event){ handler(myself); });
					  }
					  addHandler(this);
				      },
				      "setRemoveHandler" : function(handler){
					  if(!handler) return;
					  this.__removeButton__.stopObserving();
					  var removeButton = this.__removeButton__;
					  function addHandler(myself){
					      removeButton.observe("click", function(event){ handler(myself); });
					  }
					  addHandler(this);
				      },
				      "addPlayer" : function(player){
					  if(!player || player.length === 0) return;
					  var selected = this.getPlayer();
					  var select = this.__player__.__frames__[0].getFrame().select("select")[0];
					  select.update("");
					  if(this.__rolePlayers__){
					      var j = 0;
					      for(var i = 0; i !== player.length; ++i){
						  j = 0;
						  for( ; j !== this.__rolePlayers__.length; ++j){
						      if(this.__rolePlayers__[j].indexOf(player[i]) !== -1) break;
						  }
						  if(j !== this.__rolePlayers__.length){
						      this.__rolePlayers__[j] = player;
						      break;
						  }
					      }
					      if(j === this.__rolePlayers__.length)this.__rolePlayers__.push(player);
					  }
					  else {
					      this.__rolePlayers__ = new Array(player);
					  }
					  for(var i = 0; i !== this.__rolePlayers__.length; ++i){
					      for(var j = 0; j !== this.__rolePlayers__[i].length; ++j){
						  var opt = new Element("option", {"value" : this.__rolePlayers__[i][j]}).update(this.__rolePlayers__[i][j]);
						  if(this.__rolePlayers__[i][j] !== selected){
						      select.insert({"bottom" : opt});
						  }
						  else {
						      opt.writeAttribute({"selected" : "selected"});
						      select.insert({"top" : opt});
						  }
					      }
					  }
				      },
				      "removePlayer" : function(player){
					  if(!player || player.length === 0 || !this.__rolePlayers__ || this.__rolePlayers__.length === 0) return;
					  var selected = this.getPlayer();
					  var select = this.__player__.__frames__[0].getFrame().select("select")[0];
					  select.update("");
					  var j = 0;
                                          for(var i = 0; i !== player.length; ++i){
					      j = 0;
					      for( ; j !== this.__rolePlayers__.length; ++j){
						  if(this.__rolePlayers__[j].indexOf(player[i]) !== -1) break;
					      }
					      if(j !== this.__rolePlayers__.length) break;
					  }
					  this.__rolePlayers__ = this.__rolePlayers__.slice(0, j).concat(this.__rolePlayers__.slice(j + 1, this.__rolePlayers__.length));
					  for(var i = 0; i !== this.__rolePlayers__.length; ++i){
					      for(var j = 0; j !== this.__rolePlayers__[i].length; ++j){
						  var opt = new Element("option", {"value" : this.__rolePlayers__[i][j]}).update(this.__rolePlayers__[i][j]);
						  if(this.__rolePlayers__[i][j] !== selected){
						      select.insert({"bottom" : opt});
						  }
						  else {
						      opt.writeAttribute({"selected" : "selected"});
						      select.insert({"top" : opt});
						  }
					      }
					  }
				      },
				      "getType" : function(){
					  return this.__type__.__frames__[0].getContent();
				      },
				      "getPlayer" : function(){
					  return this.__player__.__frames__[0].getContent();
				      },
				      "getContent" : function(){
					  if(this.isUsed()){
					      return {"itemIdentities" : this.__itemIdentity__.getContent(true, true),
						      "type" : new Array(this.getType()),
						      "topicRef" : new Array(this.getPlayer())};
					  }

					  return null;
				      },
				      "toJSON" : function(){
					  if(this.isUsed()){
					      return "{\"itemIdentities\":" +  this.__itemIdentity__.toJSON(true, true) +
						     ",\"type\":[" + this.getType().toJSON() + "]" +
						     ",\"topicRef\":[" + this.getPlayer().toJSON() + "]}";
					  }

					  return "null";
				      },
				      "isValid" : function(){
					  return this.getType().length !== 0 && this.getPlayer().length !== 0;
				      },
				      "isUsed" : function(){
					  return this.getType().length !== 0 || this.getPlayer().length !== 0 || this.__itemIdentity__.getContent(true, true).length !== 0;
				      }});


// --- contains all roles of an association
var RoleContainerC = Class.create(ContainerC, {"initialize" : function($super, contents, associationRoleConstraints, rolePlayerConstraints, otherRoleConstraints){
                                                  $super();
                                                   this.__frame__.writeAttribute({"class" : CLASSES.roleContainer()});
                                                   this.__arContainer__ = new Object();
                                                   this.__orContainer__ = new Object();
                                                   try{
						       if((!contents || contents.length === 0) && associationRoleConstraints){
							   this.resetValues(associationRoleConstraints, rolePlayerConstraints, otherRoleConstraints);
                                               	       }
                                               	       else {
                                               		   // TODO: check already existing contents and order them to the corresponding fields
                                               	       }
                                                   }
                                                   catch(err){
                                               	       alert("From RoleContainerC(): " + err);
                                                   }
                                               },
					       "resetValues" : function(associationRoleConstraints, rolePlayerConstraints, otherRoleConstraints){
                                                   this.__associationRoleConstraints__ = associationRoleConstraints;
                                                   this.__rolePlayerConstraints__ = rolePlayerConstraints;
						   this.__otherRoleConstraints__ = otherRoleConstraints;

						   try{
						       for(var i = 0; this.__arContainer__.__frames__ && i !== this.__arContainer__.__frames__.length; ++i){
							   this.__arContainer__.__frames__[i].remove();
						       }
						       this.__arContainer__ = new Object();
						   }
						   catch(err){
						       this.__arContainer__ = new Object();
						   }
						   try{
						       for(var i = 0; this.__orContainer__.__frames__ && i !== this.__orContainer__.__frames__.length; ++i){
							   this.__orContainer__.__frames__[i].remove();
						       }
						       this.__orContainer__ = new Object();
						   }
						   catch(err){
						       this.__orContainer__ = new Object();
						   }

						   // --- creates all roles from existing associationroleconstraints and roleplayerConstraints
						   // TODO: insert existing contents to the corresponding constraints
						   for(var i = 0; this.__associationRoleConstraints__ && i !== this.__associationRoleConstraints__.length; ++i){
						       var arc = this.__associationRoleConstraints__[i];
						       var foundRpcs = getRolePlayerConstraintsForRole(arc.roleType, this.__rolePlayerConstraints__);
						       this.__makeRolesFromARC__(arc, foundRpcs);
						   }
						   // --- creates roles from otherrole-constraints
						   for(var i = 0; i !== this.__arContainer__.__frames__.length; ++i){
						       this.__makeRolesFromORC__(this.__arContainer__.__frames__[i].getType(), this.__arContainer__.__frames__[i].getPlayer());
						   }
					       },
					       "__makeRolesFromARC__" : function(associationRoleConstraint, rolePlayerConstraints){
						   if(!associationRoleConstraint || !rolePlayerConstraints || rolePlayerConstraints.length === 0) return;
						   checkCardinalitiesARC_RPC(associationRoleConstraint, rolePlayerConstraints);

						   // --- creates all roles with all needed players
						   var rolesCreated = 0;
						   var allAvailablePlayers = extractPlayersOfConstraints(rolePlayerConstraints);
						   var roleType = associationRoleConstraint.roleType;
						   var roleMin = associationRoleConstraint.cardMin === 0 ? 1 : parseInt(associationRoleConstraint.cardMin);
						   for(var i = 0; i !== rolePlayerConstraints.length; ++i){
						       var playerMin = rolePlayerConstraints[i].cardMin === 0 ? 1 : parseInt(rolePlayerConstraints[i].cardMin);
						       var currentAvailablePlayers = rolePlayerConstraints[i].players;
						       var cleanedPlayers = cleanPlayers(allAvailablePlayers, currentAvailablePlayers);
						       for(var j = playerMin; j < currentAvailablePlayers.length; ++j){
							   cleanedPlayers.push(currentAvailablePlayers[j]);
						       }
						       if(currentAvailablePlayers.length < playerMin) throw "From __makeRolesFromARC__(): not enough players(=" + currentAvailablePlayers.length + ") to reach card-min(=" + playerMin + ") of roletype\"" + roleType.flatten()[0] + "\"!";
						       for(var j = 0; j !== playerMin; ++j){
							   var selectedPlayer = currentAvailablePlayers[j];
							   var _players = cleanedPlayers;
							   _players.unshift(selectedPlayer);
							   var role = new RoleC(null, roleType, _players, this.__arContainer__);
							   this.__checkARCButtons__(role, associationRoleConstraint, rolePlayerConstraints[i]);
							   this.__setRoleChangePlayerHandler__(role, this.__arContainer__.__frames__, rolePlayerConstraints, null);
							   this.__setARCAddHandler__(role, associationRoleConstraint, rolePlayerConstraints[i]);
							   this.__setARCRemoveHandler__(role, associationRoleConstraint, rolePlayerConstraints[i]);
							   this.__error__.insert({"before" : role.getFrame()});
							   ++rolesCreated;
						       }
						   }

						   // --- creates all further needed roles with players that owns a card-max > existing players
						   while(rolesCreated < roleMin){
						       var currentlyCreated = 0;
						       for(var i= 0; i !== rolePlayerConstraints.length; ++i){
							   // existing roles --> all roles that owns a player which is selected of those listed in the roleplayer-constraint
							   var existingRoles = this.getExistingRoles(roleType, rolePlayerConstraints[i].players, this.__arContainer__.__frames__);
							   var availablePlayers = rolePlayerConstraints[i].players;
							   if(existingRoles.length < rolePlayerConstraints[i].cardMax && availablePlayers.length > existingRoles.length){
							       var currentAvailablePlayers = rolePlayerConstraints[i].players;
							       var cleanedPlayers = cleanPlayers(allAvailablePlayers, currentAvailablePlayers);

							       // --- adds players that are not selected yet
							       for(var j = 0; j !== currentAvailablePlayers.length; ++j){
								   if(this.getExistingRoles(roleType, currentAvailablePlayers[j], this.__arContainer__.__frames__).length === 0){
								       cleanedPlayers.push(currentAvailablePlayers[j]);
								   }
							       }
							       
							       // --- removes the player which will be seleted by the new created role of all other select-elements
							       for(var j = 0; j !== this.__arContainer__.__frames__.length; ++j){
								   this.__arContainer__.__frames__[j].removePlayer(cleanedPlayers[0]);
							       }
							       
							       var role = new RoleC(null, roleType, cleanedPlayers, this.__arContainer__);
							       this.__checkARCButtons__(role, associationRoleConstraint, rolePlayerConstraints[i]);
							       this.__setRoleChangePlayerHandler__(role, this.__arContainer__.__frames__, rolePlayerConstraints, null);
							       this.__setARCAddHandler__(role, associationRoleConstraint, rolePlayerConstraints[i]);
							       this.__setARCRemoveHandler__(role, associationRoleConstraint, rolePlayerConstraints[i]);
							       this.__error__.insert({"before" : role.getFrame()});
							       ++rolesCreated;
							       ++currentlyCreated;
							   }
						       }
						       if(currentlyCreated === 0) throw "Not enough players to create all needed roles of the type \"" + roleType.flatten()[0] + "\"!";
						   }
					       },
					       "__makeRolesFromORC__" : function(roleType, player){
						   var orpcs = getOtherRoleConstraintsForRole(new Array(roleType), new Array(player), this.__otherRoleConstraints__);
						   for(var i = 0; i !== orpcs.length; ++i){
						       var cPlayers = orpcs[i].players;
						       var cRoleType = orpcs[i].roleType;
						       var cOtherPlayers = orpcs[i].otherPlayers;
						       var cOtherRoleType = orpcs[i].otherRoleType;
						       var cMin = orpcs[i].cardMin === 0 ? 1 : parseInt(orpcs[i].cardMin);
						       if(!cOtherPlayers || cOtherPlayers.length < cMin) throw "from __makeRolesFromORC__(): not enough players(=" + cOtherPlayers.length + ") for roletype + \"" + cOtherRoleType.flatten()[0] + "\"!";
						       var existingRoles = this.getExistingRoles(cOtherRoleType, cOtherPlayers, this.__orContainer__.__frames__);
						       for(var j = 0; j < cMin - existingRoles.length; ++j){
							   // --- removes all players that are already selected from the
							   // --- current players list
							   var cleanedPlayers = new Array();
							   for(var k = 0; k !== cOtherPlayers.length; ++k){
							       if(this.getExistingRoles(cOtherRoleType, cOtherPlayers[k], this.__orContainer__.__frames__).length === 0){
							   	   cleanedPlayers.push(cOtherPlayers[k]);
							       }
							   }

							   // --- removes the player that will be selected in this role
							   // --- from all existing roles
							   for(var j = 0; this.__orContainer__.__frames__ && j !== this.__orContainer__.__frames__.length; ++j){
							       this.__orContainer__.__frames__[j].removePlayer(cleanedPlayers[0]);
							   }
							   
							   var role = new RoleC(null, cOtherRoleType, cleanedPlayers, this.__orContainer__);
							   this.__checkORCButtons__(role, orpcs[i]);
							   this.__setRoleChangePlayerHandler__(role, this.__orContainer__.__frames__, null, orpcs);
							   this.__setORCAddHandler__(role, orpcs[i], orpcs);
							   this.__setORCRemoveHandler__(role, orpcs[i], orpcs);
							   this.__error__.insert({"before" : role.getFrame()});
						       }
						   }
					       },
					       "__checkORCButtons__" : function(role, constraint){
						   if(!role || !constraint) return;
						   var cOtherPlayers = constraint.otherPlayers;
						   var cOtherRoleType = constraint.otherRoleType;
						   var cardMax = constraint.cardMax === "MAX_INT" ? "*" : parseInt(constraint.cardMax);
						   var cardMin = parseInt(constraint.cardMin);
						   var existingRoles = this.getExistingRoles(cOtherRoleType, cOtherPlayers, this.__orContainer__.__frames__);
						   var cleanedPlayers = new Array();
						   for(var i = 0; i !== cOtherPlayers.length; ++i){
						       if(this.getExistingRoles(cOtherRoleType, cOtherPlayers[i], this.__orContainer__.__frames__).length === 0){
							   cleanedPlayers.push(cOtherPlayers[i]);
						       }
						   }

						   // --- add button
						   if(cardMax > existingRoles.length && cleanedPlayers.length !== 0){
						       for(var i = 0; i !== existingRoles.length; ++i) existingRoles[i].showAddButton();
						   }
						   else {
						       for(var i = 0; i !== existingRoles.length; ++i) existingRoles[i].hideAddButton();
						   }

						   // --- remove button
						   if(cardMin > existingRoles.length){
						       for(var i = 0; i !== existingRoles.length; ++i) existingRoles[i].showRemoveButton();
						   }
						   else {
						       for(var i = 0; i !== existingRoles.length; ++i) existingRoles[i].hideRemoveButton();
						   }
					       },
					       "__checkARCButtons__" : function(role, arConstraint, rpConstraint){
						   if(!role || !arConstraint || !rpConstraint) return;

						   var roleMax = arConstraint.cardMax === "MAX_INT" ? "*" : parseInt(arConstraint.cardMax);
						   var playerMax = rpConstraint.cardMax === "MAX_INT" ? "*" : parseInt(rpConstraint.cardMax);
						   var playerMin = rpConstraint.cardMin === 0 ? 1 : parseInt(rpConstraint.cardMin);
						   var players = rpConstraint.players;
						   var existingRoles = this.getExistingRoles(arConstraint.roleType, rpConstraint.players, this.__arContainer__.__frames__);
						   var allExistingRoles = new Array();
						   var tPsis = arConstraint.roleType.flatten();
						   for(var i = 0; i !== this.__arContainer__.__frames__.length; ++i){
						       if(tPsis.indexOf(this.__arContainer__.__frames__[i].getType()) !== -1)
							   allExistingRoles.push(this.__arContainer__.__frames__[i]);
						   }
						   var cleanedPlayers = new Array();
						   for(var i = 0; i !== players.length; ++i){
						       if(this.getExistingRoles(arConstraint.roleType, players[i], this.__arContainer__.__frames__).length === 0){
							   cleanedPlayers.push(players[i]);
						       }
						   }
						   
						   // --- add button
						   if(cleanedPlayers.length !== 0 && playerMax > existingRoles.length && roleMax > allExistingRoles.length){
						       for(var i = 0; i !== existingRoles.length; ++i) existingRoles[i].showAddButton();   
						   }
						   else {
						       for(var i = 0; i !== existingRoles.length; ++i) existingRoles[i].hideAddButton(); 
						   }

						   // --- remove button
						   // --- If there is just one roleplayer-constraint to the passed roletype, there will be
						   // --- checked this card-min otherwise the minimum cardinality of this roletype and the
						   // --- players depending on the found roleplayer-constraints hast to exist only once.
						   // --- The defined cardinality will be cheched before committing, so the user will
						   // --- be warned before committing a bad fragment.
						   var foundRpcs = getRolePlayerConstraintsForRole(arConstraint.roleType, this.__rolePlayerConstraints__);
						   if(foundRpcs.length === 1 && playerMin < existingRoles.length){
						       for(var i = 0; i !== existingRoles.length; ++i) existingRoles[i].showRemoveButton();
						   }
						   else if(foundRpcs.length > 1 && existingRoles.length > 1){
						       for(var i = 0; i !== existingRoles.length; ++i) existingRoles[i].showRemoveButton();
						   }
						   else {
						       for(var i = 0; i !== existingRoles.length; ++i) existingRoles[i].hideRemoveButton();
						   }
					       },
					       "__setORCAddHandler__" : function(role, currentConstraint, constraints){
						   if(!role || !currentConstraint || !constraints || constraints.length === 0) return;

						   var roleContainer = this;
						   function addHandler(myself){
						       var cOtherPlayers = currentConstraint.otherPlayers;
						       var cOtherRoleType = currentConstraint.otherRoleType;
						       var cardMax = currentConstraint.cardMax === "MAX_INT" ? "*" : parseInt(currentConstraint.cardMax);
						       var cardMin = currentConstraint.cardMin === 0 ? 1 : parseInt(currentConstraint.cardMin);
						       var existingRoles = roleContainer.getExistingRoles(cOtherRoleType, cOtherPlayers, roleContainer.__orContainer__.__frames__);
						       var cleanedPlayers = new Array();
						       for(var i = 0; i !== cOtherPlayers.length; ++i){
							   if(roleContainer.getExistingRoles(cOtherRoleType, cOtherPlayers[i], roleContainer.__orContainer__.__frames__).length === 0){
							       cleanedPlayers.push(cOtherPlayers[i]);
							   }
						       }
						       
						       // --- creates new role
						       if(cleanedPlayers.length !== 0){
							   var role = new RoleC(null, cOtherRoleType, cleanedPlayers, roleContainer.__orContainer__);
							   roleContainer.__checkORCButtons__(role, currentConstraint);
							   roleContainer.__setRoleChangePlayerHandler__(role, roleContainer.__orContainer__.__frames__, null, constraints);
							   roleContainer.__setORCAddHandler__(role, currentConstraint, constraints);
							   roleContainer.__setORCRemoveHandler__(role, currentConstraint, constraints);
							   roleContainer.__error__.insert({"before" : role.getFrame()});
							   // --- removes the selected player from all other roles
							   for(var i = 0; i !== existingRoles.length; ++i){
							       existingRoles[i].removePlayer(new Array(role.getPlayer()));
							   }
							   var allRoles = existingRoles;
							   allRoles.push(role);
						       	   roleContainer.__innerCheckORCButtons__(allRoles, cardMin, cardMax);
						       }
						   }
						   
						   role.setAddHandler(addHandler);
					       },
					       "__setORCRemoveHandler__" : function(role, currentConstraint, constraints){
						   if(!role || !currentConstraint || !constraints) return;
						   
						   var roleContainer = this;
						   function removeHandler(myself){
						       var cOtherPlayers = currentConstraint.otherPlayers;
						       var cOtherRoleType = currentConstraint.otherRoleType;
						       var cardMax = currentConstraint.cardMax === "MAX_INT" ? "*" : parseInt(currentConstraint.cardMax);
						       var cardMin = currentConstraint.cardMin === 0 ? 1 : parseInt(currentConstraint.cardMin);
						       var playerToAdd = null;
						       for(var i = 0; i !== cOtherPlayers.length; ++i){
							   if(cOtherPlayers[i].indexOf(role.getPlayer()) !== -1){
							       playerToAdd = cOtherPlayers[i];
							   }
						       }
						       roleContainer.__orContainer__.__frames__ = roleContainer.__orContainer__.__frames__.without(role);
						       role.remove();
						       var existingRoles = roleContainer.getExistingRoles(cOtherRoleType, cOtherPlayers, roleContainer.__orContainer__.__frames__);
						       for(var i = 0; i !== existingRoles.length; ++i){
							   existingRoles[i].addPlayer(playerToAdd);
						       }
						       roleContainer.__innerCheckORCButtons__(existingRoles, cardMin, cardMax);
						   }

						   role.setRemoveHandler(removeHandler);
					       },
					       "__innerCheckORCButtons__" : function(existingRoles, cardMin, cardMax){
						   if(!existingRoles) return;
						   
						   // --- checks all control buttons after an add or remove operation
						   if(cardMax !== "*" && existingRoles.length >= cardMax){
						       for(var i = 0; i !== existingRoles.length; ++i){
							   existingRoles[i].hideAddButton();
						       }
						   }
						   else {
						       for(var i = 0; i !== existingRoles.length; ++i){
							   existingRoles[i].showAddButton();
						       }
						   }

						   if(cardMin < existingRoles.length){
						       for(var i = 0; i !== existingRoles.length; ++i){
							   existingRoles[i].showRemoveButton();
						       }
						   }
						   else {
						       for(var i = 0; i !== existingRoles.length; ++i){
							   existingRoles[i].hideRemoveButton();
						       }
						   }
					       },
					       "__setARCAddHandler__" : function(role, arConstraint, rpConstraint){
						   if(!role || !arConstraint || !rpConstraint) return;

						   function addHandler(myself){
						       //alert("myself: " + myself + "!!!");
						   }

						   role.setAddHandler(addHandler);
					       },
					       "__setARCRemoveHandler__" : function(role, arConstraint, rpConstraint){
						   if(!role || !arConstraint || !rpConstraint) return;

						   function removeHandler(myself){
						       //alert("myself: " + myself + "!!!");
						   }

						   role.setRemoveHandler(removeHandler);
					       },
					       "getExistingRoles" : function(roleType, players, roles){
						   var rolesFound = new Array();
						   if(!roles || roles.length === 0) return rolesFound;
						   
						   var allTypes = roleType && roleType.length !== 0 ? roleType.flatten() : new Array();
						   var allPlayers = players && players.length !== 0 ? players.flatten() : new Array();
						   for(var i = 0; i !== roles.length; ++i){
						       if(allTypes.indexOf(roles[i].getType()) !== -1 && allPlayers.indexOf(roles[i].getPlayer()) !== -1) rolesFound.push(roles[i]);
						   }

						   return rolesFound;
					       },
					       "__setRoleChangePlayerHandler__" : function(role, roleContainer, arConstraints, orConstraints){
						   var constraints = null;
						   if(arConstraints && arConstraints.length !== 0) constraints = arConstraints;
						   else if(orConstraints && orConstraints.length !== 0) constraints = orConstraints;
						   else if(arConstraints && orConstraints && arConstraints.length !== 0 && orConstraints.length !== 0) throw "From __setRoleChangePlayerHandler__(): one of the parameters arConstraints or orConstraints must be set to null";
						   role.__lastPlayer__ = new Array(role.getPlayer());
						   var select = role.__table__.select("tr." + CLASSES.playerFrame())[0].select("td." + CLASSES.content())[0].select("select")[0];
						   function setEvent(myself){
						       select.observe("change", function(event){
							   role.__lastPlayer__.push(role.getPlayer());
							   if(role.__lastPlayer__.length > 2) role.__lastPlayer__.shift();
							   if(!roleContainer || roleContainer.length < 2 || ! constraints || constraints.length === 0) return;
							   role.getLastPlayer = function(){
							       return role.__lastPlayer__[role.__lastPlayer__.length - 2];
							   }
							   // --- selects the players which have to be removed or added to
							   // --- the found roles
							   var playerToAdd = new Array(role.getLastPlayer());
							   var playerToRemove = new Array(role.getPlayer());
							   
							   // --- collects all roles depending on the same constraint as the passed role
							   var existingRoles = new Array();
							   for(var i = 0; constraints && i !== constraints.length; ++i){
							       var roleType = orConstraints ? constraints[i].otherRoleType : constraints[i].roleType;
							       var players = orConstraints ? constraints[i].otherPlayers : constraints[i].players;
							       
							       // --- adds new psi of the roles have to be added
							       for(var j = 0; j !== players.length; ++j){
								   if(players[j].indexOf(playerToRemove[0]) !== -1){
								       for(var l = 0; l !== players[j].length; ++l){
									   if(players[j][l] !== playerToRemove[0]) playerToRemove.push(players[j][l]);
								       }
								   }
								   if(players[j].indexOf(playerToAdd[0]) !== -1){
								       for(var l = 0; l !== players[j].length; ++l){
									   if(players[j][l] !== playerToAdd[0]) playerToAdd.push(players[j][l]);
								       }
								   }
							       }
							       
							       var foundRoles = myself.getExistingRoles(roleType, players, roleContainer);
							       for(var j = 0; j !== foundRoles.length; ++j){
								   existingRoles.push(foundRoles[j]);
							       }
							   }
							   existingRoles = existingRoles.without(role);
							   
							   // --- removes and adds the new selected psi-value
							   // --- and the old deselected psi if the player is another one
							   if(playerToRemove.indexOf(role.getLastPlayer()) === -1){
							       for(var i = 0; i !== existingRoles.length; ++i){
								   existingRoles[i].addPlayer(playerToAdd);
								   existingRoles[i].removePlayer(playerToRemove);
							       }
							   }
						       });
						   }
						   setEvent(this);
					       },
					       "getContent" : function(){
						   if((!this.__orContainer__.__frames__ && this.__orContainer__.frames__.length === 0) || (!this.__arContainer__.__frames__ && this.__arContainer__.__frames__.length === 0)) return null;
						   var roles = new Array();
						   for(var i = 0; this.__arContainer__.__frames__ && i !== this.__arContainer__.__frames__.length; ++i){
						       roles.push(this.__arContainer__.__frames__[i].getContent());
						   }
						   for(var i = 0; this.__orContainer__.__frames__ && i !== this.__orContainer__.__frames__.length; ++i){
						       roles.push(this.__orContainer__.__frames__[i].getContent());
						   }
						   return roles;
					       },
					       "toJSON" : function(){
						   if((!this.__orContainer__.__frames__ && this.__orContainer__.frames__.length === 0) || (!this.__arContainer__.__frames__ && this.__arContainer__.__frames__.length === 0)) return "null";
						   var roles = "[";
						   for(var i = 0; this.__arContainer__.__frames__ && i !== this.__arContainer__.__frames__.length; ++i){
						       roles += this.__arContainer__.__frames__[i].toJSON() + ",";
						   }
						   for(var i = 0; this.__orContainer__.__frames__ && i !== this.__orContainer__.__frames__.length; ++i){
						       roles += this.__orContainer__.__frames__[i].toJSON() + ",";
						   }
						   return roles.substring(0, roles.length - 1) + "]";
					       },
					       "isValid" : function(){
						   // TODO: implement
						   return true;
					       }});


// --- representation of an association element
var AssociationC = Class.create(ContainerC, {"initialize" : function($super, contents, constraints, owner){
					         $super();
					         if(!owner) throw "From NameC(): owner must be set but is null";
                                                 if(!owner.__frames__) owner.__frames__ = new Array();
                                                 owner.__frames__.push(this);
    
                                                 this.__frame__.writeAttribute({"class" : CLASSES.associationFrame()});
                                                 this.__table__ = new Element("table", {"class" : CLASSES.associationFrame()});
                                                 this.__frame__.insert({"top" : this.__table__});
                                                 this.__constraints__ = constraints;
                                                 this.__contents__ = contents;

					         try{
						     // --- control row + ItemIdentity
						     makeControlRow(this, 4, contents ? contents.itemIdentities : null);
						     checkRemoveAddButtons(owner, 1, -1);
						     setRemoveAddHandler(this, owner, 1, -1, function(){
							 return new AssociationC(null, constraints, owner);
						     });

						     // --- type
						     var types = new Array();
						     for(var i = 0; constraints && i !== constraints.length; ++i){
							 for(var j = 0; j != constraints[i].associationType.length; ++j){
							     types.push(constraints[i].associationType[j]);
							     if(contents && contents.type && contents.type[0] === constraints[i].associationType[j]){
								 var selected = constraints[i].associationType[j];
								 types[types.length - 1] = types[0];
								 types[0] = selected;
							     }
							 }
						     }
						     this.__type__ = new Object();
						     var tr = newRow(CLASSES.typeFrame(), "Type", new SelectrowC(types, this.__type__, 1, 1).getFrame());
						     this.__table__.insert({"bottom" : tr});
						     
						     // --- scopes
						     this.__scope__ = new ScopeContainerC(this.__contents__ && this.__contents__.scopes ? this.__contents__.scopes : null, this.__constraints__ && this.__constraints__[0].scopeConstraints ? this.__constraints__[0].scopeConstraints : null);
						     this.__table__.insert({"bottom" : newRow(CLASSES.scopeContainer(), "Scope", this.__scope__.getFrame())});

						     // --- roles
						     var _roleConstraints = _playerConstraints = _otherRoleConstraints = null;
						     if(this.__constraints__){
							 _roleConstraints = this.__constraints__[0].associationRoleConstraints;
							 _playerConstraints = this.__constraints__[0].rolePlayerConstraints;
							 _otherRoleConstraints = this.__constraints__[0].otherRoleConstraints;
						     }

						     this.__roles__ = new RoleContainerC(this.__contents__ ? this.__contents__.roles : null, _roleConstraints, _playerConstraints, _otherRoleConstraints);
						     this.__table__.insert({"bottom" : newRow(CLASSES.roleContainer(), "Roles", this.__roles__.getFrame())});
						     
						     // --- registers the onChangeHandler of the Type-selectrow
						     onTypeChangeScope(this, null, null, "association");
						 }
					         catch(err){
						     alert("From AssociationC(): " + err);
						 }
					     },
					     "resetValues" : function(){
						 // --- scope, depends only to the associationtype, roles can be ignored				 
						 // --- finds the scopes depending to the selected type
						 var foundIdx = -1;
						 for(var i = 0; this.__constraints__ && i != this.__constraints__.length; ++i)
						 {
						     if(foundIdx !== -1) break;
						     for(var j = 0; j != this.__constraints__[i].associationType.length; ++j){
							 if(this.__type__.__frames__[0].getContent() === this.__constraints__[i].associationType[j]){
							     foundIdx = i;
							     break;
							 }
						     }
						 }
						 this.__scope__.resetValues(null, (foundIdx === -1 ? null : this.__constraints__[foundIdx].scopeConstraints));

						 var _roleConstraints = _playerConstraints = _otherRoleConstraints = null;
						 if(foundIdx !== -1){
						     _roleConstraints = this.__constraints__[foundIdx].associationRoleConstraints;
						     _playerConstraints = this.__constraints__[foundIdx].rolePlayerConstraints;
						     _otherRoleConstraints = this.__constraints__[foundIdx].otherRoleConstraints;
						 }
						 this.__roles__.resetValues(_roleConstraints, _playerConstraints, _otherRoleConstraints);
					     },
					     "getContent" : function(){
						 if(!this.isUsed()) return null;
						 return {"itemIdentities" : this.__itemIdentity__.getContent(true, true),
							 "type" : new Array(this.__type__.__frames__[0].getContent()),
							 "scopes" : this.__scope__.getContent(),
							 "roles" : this.__roles__.getContent()};
					     },
					     "toJSON" : function(){
						 if(!this.isUsed()) return "null";
						 return "{\"itemIdentities\":" + this.__itemIdentity__.toJSON(true, true) +
						     ",\"type\":[" + this.__type__.__frames__[0].toJSON() + "]" +
						     ",\"scopes\":" + this.__scope__.toJSON() +
						     ",\"roles\":" + this.__roles__.toJSON() + "}";
					     },
					     "isValid" : function(){
						 // TODO: implement
						 return true;
					     },
					     "isUsed" : function(){
						 // TODO: implement (activate button)
						 return true;
					     }});


// --- contains all fragment's associations depending on the main topic
var AssociationContainerC = Class.create(ContainerC, {"initialize" : function($super, contents, constraints, mainTopic){
						          $super();
						          this.__minimized__ = false;
                                                          this.__mainTopic__ = mainTopic;
						          try{
							      this.__frame__ .writeAttribute({"class" : CLASSES.associationContainer()});
							      this.__table__ = new Element("table", {"class" : CLASSES.associationContainer()});
							      this.__frame__.insert({"top" : this.__table__});
							      this.__caption__ = new Element("caption", {"class" : CLASSES.clickable()}).update("Associations");
							      this.__table__.insert({"top" : this.__caption__})

							      this.__container__ = new Object();

							      for(var i = 0; contents && i != contents.length; ++i){
								  var association = new AssociationC(contents[i], constraints, this.__container__);
								  var tr = new Element("tr", {"class" : CLASSES.associationFrame()});
								  var td = new Element("td", {"class" : CLASSES.content()});
								  td.update(association.getFrame());
								  tr.update(td);
								  this.__table__.insert({"bottom" : tr});
							      }

							      if(!this.__container__.__frames__ && constraints && constraints.length !== 0){
								  var association = new AssociationC(null, constraints, this.__container__);
								  var tr = new Element("tr", {"class" : CLASSES.associationFrame()});
								  var td = new Element("td", {"class" : CLASSES.content()});
								  td.update(association.getFrame());
								  tr.update(td);
								  this.__table__.insert({"bottom" : tr});
							      }
							      function setMinimizeHandler(myself){
								  myself.__caption__.observe("click", function(event){
								      myself.minimize();
								  });
							      }
							      setMinimizeHandler(this);

							      var button = new Element("input", {"type" : "button", "value" : "toJSON()"});
							      function test(myself){
								  button.observe("click", function(event){
								      try{
								      alert("content:\n\n" + myself.getContent());
								      alert("JSON:\n\n" + myself.toJSON().gsub("\"topicRef\":\\[\"\\*\\*current-topic\\*\\*\"\\]", myself.__mainTopic__.getContent().subjectIdentifiers.toJSON()));
								      }catch(err){ alert("test: " + err); }
								  });
							      }
							      test(this);
							      this.getFrame().insert({"bottom" : button});
							  }
						          catch(err){
							      alert("From AssociationContainerC(): " + err);
							  }
						      },
						      "getContent" : function(){
							  var associations = new Array();
							  for(var i = 0; this.__container__.__frames__ && i !== this.__container__.__frames__.length; ++i){
							      if(this.__container__.__frames__[i].isUsed() === true) associations.push(this.__container__.__frames__[i].getContent());
							  }
							  if(associations.length === 0) return null;
							  return associations;
						      },
						      "toJSON" : function(){
							  var associations = "[";
							  for(var i = 0; this.__container__.__frames__ && i !== this.__container__.__frames__.length; ++i){
							      if(this.__container__.__frames__[i].isUsed() === true) associations += this.__container__.__frames__[i].toJSON() +",";
							  }

							  if(associations === "[") return "null";
							  return associations.substring(0, associations.length - 1) + "]";
						      },
						      "isValid" : function(){
							  // TODO: implement
							  return true;
						      },
						      "minimize" : function(){
							  var rows = this.__table__.select("tr." + CLASSES.associationFrame());
							  for(var i = 0; i != rows.length; ++i){
							      if(this.__minimized__ === false) rows[i].hide();
							      else rows[i].show();
							  }
							  this.__minimized__ = !this.__minimized__;
						      }});






















































// --- helper function to create a dom-fragment of the form
// --- <tr class="rowClass"><td class="description">description</td>
//----  <td class="content">content</td></tr>
function newRow(rowClass, description, content){
    var tr = new Element("tr", {"class" : rowClass});
    tr.insert({"top" : new Element("td", {"class" : CLASSES.description()}).update(description)});
    tr.insert({"bottom" : new Element("td", {"class" : CLASSES.content()}).update(content)});
    return tr;
}


// --- Helper function for the constructors of all classes
// --- of the type FrameC.
// --- There will be set the remome and add handler.
function setRemoveAddHandler(myself, owner, min, max, call){
    myself.__remove__.stopObserving();
    myself.__add__.stopObserving();
    myself.__remove__.observe("click", function(event){
	myself.remove();
	owner.__frames__ = owner.__frames__.without(myself);
	if(min >= owner.__frames__.length){
	    for(var i = 0; i != owner.__frames__.length; ++i){
		owner.__frames__[i].hideRemoveButton();
	    }
	}
	if(max > owner.__frames__.length){
	    for(var i = 0; i != owner.__frames__.length; ++i){
		owner.__frames__[i].showAddButton();
	    }
	}
    });
    
    myself.__add__.observe("click", function(event){
	var newElem = call();
	myself.append(newElem.getFrame());
	if(remove === true && min !== -1 && owner.__frames__.length > min){
	    for(var i = 0; i != owner.__frames__.length; ++i){
		owner.__frames__[i].showRemoveButton();
	    }
	}
	if(max > -1 && max <= owner.__frames__.length){
	    for(var i = 0; i != owner.__frames__.length; ++i){
		owner.__frames__[i].hideAddButton();
	    }
	}
    });
}


// --- Helper function for the constructors of all classes
// --- of the type FrameC.
// --- There will be checked the visibility of the remove and
// --- add buttons.
function checkRemoveAddButtons(owner, min, max){
    if(min >= owner.__frames__.length){
	for(var i = 0; i != owner.__frames__.length; ++i){
	    owner.__frames__[i].hideRemoveButton();
	}
    }

    if(min > -1 && min < owner.__frames__.length){
	for(var i = 0; i != owner.__frames__.length; ++i){
	    owner.__frames__[i].showRemoveButton();
	}
    }
    
    if(max > -1 && max <= owner.__frames__.length){
        for(var i = 0; i != owner.__frames__.length; ++i){
	    owner.__frames__[i].hideAddButton();
	}
    }
}


// --- creates a control row for NameC, OccurrenceC and VariantC with a nested ItemIdentity frame.
function makeControlRow(myself, rowspan, contents)
{
    var tr = new Element("tr", {"class" : CLASSES.itemIdentityFrame()});
    var tdCtrl = new Element("td", {"class" : CLASSES.controlColumn(), "rowspan" : rowspan});
    tr.insert({"top" : tdCtrl})
    var tdDesc = new Element("td", {"class" : CLASSES.description()}).update("ItemIdentity");
    tr.insert({"bottom" : tdDesc});
    var min = new Element("span", {"class" : CLASSES.clickable()}).update("&#171;");
    myself.__min__ = min;
    myself.__remove__ = new Element("span", {"class" : CLASSES.clickable()}).update("-");
    myself.__add__ = new Element("span", {"class" : CLASSES.clickable()}).update("+");
    tdCtrl.insert({"top" : min});
    tdCtrl.insert({"bottom" : "<br/>"});
    tdCtrl.insert({"bottom" : myself.__remove__});
    tdCtrl.insert({"bottom" : "<br/>"});
    tdCtrl.insert({"bottom" : myself.__add__});
    var tdCont = new Element("td", {"class" : CLASSES.content()});
    tr.insert({"bottom" : tdCont});
    myself.__itemIdentity__ = new ItemIdentityC(contents ? contents.itemIdentities : null);
    tdCont.insert({"top" : myself.__itemIdentity__.getFrame()});
    myself.__table__.insert({"bottom" : tr});

    var trCtrl = new Element("tr", {"class" : CLASSES.showHiddenRows()});
    trCtrl.insert({"top" : new Element("td", {"class" : CLASSES.clickable()}).update("&#187")});
    myself.__table__.insert({"top" : trCtrl});
    trCtrl.hide();
    trCtrl.observe("click", function(){
	var trs = myself.__table__.select("tr");
	for(var i = 0; i != trs.length; ++i) trs[i].show();
	trCtrl.hide();
    });

    // --- min click-handler
    min.observe("click", function(event){
	var trs = myself.__table__.select("tr");
	for(var i = 0; i != trs.length; ++i){
	    if(i === 0) trs[i].show();
	    else trs[i].hide();
	}
    });
}


// --- This function adds a onchange handler to the type-selct-element
// --- of the instance passed through the variable myself.
// --- On changing there will be reset the scope frame to the corresponding
// --- type and when what is set to "occurrence" there will be set a corresponding
// --- datatype-value.
function onTypeChangeScope(myself, contents, constraints, what){
    try{
	var select = myself.__table__.select("tr." + CLASSES.typeFrame())[0].select("td." + CLASSES.content())[0].select("select")[0];
	select.observe("change", function(event){
	    var type = event.element().value;
	    
	    var foundIdx = -1;
	    if(what === "name"){
		for(var i = 0; constraints && i !== constraints.length; ++i){
		    if(foundIdx !== -1) break;
		    for(var j = 0; j !== constraints[i].nameType.length; ++j){
			if(foundIdx !== -1) break;
			if(constraints[i].nameType[j] === type){
			    foundIdx = i;
			    break;
			}
		    }
		}
		myself.__scope__.resetValues(contents, (foundIdx === -1 ? null : constraints[foundIdx].scopeConstraints));
	    }
	    else if(what === "occurrence"){
		for(var i = 0; constraints && i !== constraints.length; ++i){
		    if(foundIdx !== -1) break;
		    for(var j = 0; j !== constraints[i].occurrenceType.length; ++j){
			if(foundIdx !== -1) break;
			if(constraints[i].occurrenceType[j] === type){
			    foundIdx = i;
			    break;
			}
		    }
		}
		if(foundIdx !== -1 && constraints[foundIdx].datatypeConstraint){
		    var dc = constraints[foundIdx].datatypeConstraint;
		    myself.__datatype__.__frames__[0].getFrame().select("input")[0].writeAttribute({"readonly" : "readonly", "value" : dc});
		}
		else {
		    myself.__datatype__.__frames__[0].getFrame().select("input")[0].writeAttribute({"value" : ""});
		    myself.__datatype__.__frames__[0].getFrame().select("input")[0].removeAttribute("readonly");
		}
		myself.__scope__.resetValues(contents, (foundIdx === -1 ? null : constraints[foundIdx].scopeConstraints));
	    }
	    else if(what === "variant"){
		// do nothing all values will be stored
	    }
	    else if(what === "association"){
		myself.resetValues();
	    }
	});
    }
    catch(err){}
}


// --- sets the resource value and datatype of names and occurrences
function makeResource(myself, content, constraints, datatypeConstraint, cssTitle){
    var value = "";
    var datatype = "";
    if(content && content.resourceRef && content.resourceRef.length !== 0){
	value = content.resourceRef;
	datatype = ANY_URI;
    }
    else if(content && content.resourceData){
	value = content.resourceData.value;
	datatype = contents.resourceData.datatype;
    }
    
    try{
	this.__value__.remove();
	this.__value__ = null;
    }catch(err){}
    try{
	this.__datatype__.__frames__[0].remove();
	this.__datytype__ = new Object();
    }catch(err){}

    myself.__value__ = new Element("textarea", {"rows" : 3}).update(value);
    myself.__table__.insert({"bottom" : newRow(CLASSES.valueFrame(), "Resource Value", myself.__value__)});
    if(cssTitle && cssTitle.length !== 0) myself.__value__.writeAttribute({"title" : cssTitle});

    // --- datatype
    myself.__datatype__ = new Object();
    if(datatypeConstraint && datatypeConstraint.length !== 0){
	new TextrowC(datatypeConstraint, datatypeConstraint, myself.__datatype__, 1, 1, null);
	myself.__datatype__.__frames__[0].getFrame().select("input")[0].writeAttribute({"readonly" : "readonly"});
    }
    else {
	new TextrowC(datatype, ".*", myself.__datatype__, 1, 1, null);
    }
    myself.__table__.insert({"bottom" : newRow(CLASSES.datatypeFrame(), "Datatype", myself.__datatype__.__frames__[0].getFrame())});
}