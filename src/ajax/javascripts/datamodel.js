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

                               checkRemoveAddButtons(owner, min, max, null);

                               this.__error__ = new Element("div", {"class" : CLASSES.error()});
                               this.__error__.hide();
                               this.__content__ = new Element("span").update(content);

                               this.__frame__.insert({"bottom" : this.__remove__});
                               this.__frame__.insert({"bottom" : this.__content__});
                               this.__frame__.insert({"bottom" : this.__add__});
                               this.__frame__.insert({"bottom" : this.__error__});
                               this.__disabled__ = false;

                               setRemoveAddHandler(this, true, owner, min, max, function(){
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
			   },
			   "isUsed" : function(){
			       return !this.__disabled__;
			   }});


// --- This class represents a textrow with the functionality of FrameC plus the method isValid
// --- which returns a boolean value depending on the instance's value and the given regular expression.
var TextrowC = Class.create(FrameC, {"initialize" : function($super, content, regexp, owner, min, max, cssTitle, dblClickHandler){
                                         $super(content, owner, min, max);
                                         owner.__frames__.pop();
                                         owner.__frames__.push(this);
                                         this.__owner__ = owner;
                                         this.__min__ = min;
                                         this.__max__ = max;

                                         this.__regexp__ = new RegExp(regexp);
                                         this.__regExpString__ = regexp;
                                         this.__frame__.writeAttribute({"class" : CLASSES.textrowWithRemoveButton()});
                                         this.__content__.remove();
                                         this.__content__ = new Element("input", {"type" : "text", "value" : content, "size" : 48});
                                         this.__dblClickHandler__ = dblClickHandler;
                                         if(cssTitle && cssTitle.length){
					     this.__content__.writeAttribute({"title" : cssTitle});
					 }
                                         this.__remove__.insert({"after" : this.__content__});

                                         checkRemoveAddButtons(owner, min, max, null);
                                         var myself = this;
                                         setRemoveAddHandler(this, true, owner, min, max, function(){
					     return new TextrowC("", regexp, owner, min, max, cssTitle, dblClickHandler);
					 });
    
                                         this.getFrame().observe("dblclick", function(event){
					     dblClickHandler(owner, event);
					 });
                                      },
				     "dblClick" : function(){
					 if(this.__dblClickHandler__) this.__dblClickHandler__(this.__owner__);
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
				     },
				     "disable" : function(){
					 this.hideError();
					 this.__content__.writeAttribute({"readonly" : "readonly"});
					 this.hideRemoveButton();
					 this.hideAddButton();
					 this.__disabled__ = true;
				     },
				     "enable" : function(){
					 this.__content__.removeAttribute("readonly");
					 checkRemoveAddButtons(this.__owner__, this.__min__, this.__max__, null);
					 this.__disabled__ = false;
				     },
				     "getRegexp" : function(){
					 return this.__regExpString__;
				     }});


// --- This class represents a selectrow with the functionality of FrameC.
var SelectrowC = Class.create(FrameC, {"initialize" : function($super, contents, owner, min, max){
                                           if(!contents || !contents.length)throw "From SelectrowC(): contents must be a non-empty array!";
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

                                           checkRemoveAddButtons(owner, min, max, null);
                                           setRemoveAddHandler(this, true, owner, min, max, function(){
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
				       },
				       "disable" : function(){
					   this.hideError();
					   this.__content__.writeAttribute({"disabled" : "disables"});
					   this.__disabled__ = true;
				       },
				       "enable" : function(){
					   this.__content__.removeAttribute("disabled");
					   this.__disabled__ = false;
				       }});
			      

// --- The base Class for alomost all frames which contains other frames like names, occurrences, ...
var ContainerC = Class.create({"initialize" : function(){
                                   this.__frame__ = new Element("div");
                                   this.__error__ = new Element("div", {"class" : CLASSES.error()});
                                   this.__error__.hide();
                                   this.__frame__.insert({"bottom" : this.__error__});
                                   this.__disabled__ = false;
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
			       },
			       "isUsed" : function(){
				   return !this.__disabled__;
			       }});


// --- Representation of a
var EditC = Class.create(ContainerC, {"initialize" : function($super, contents, successFun){
                                          $super();
                                          this.__frame__.writeAttribute({"class" : CLASSES.editFrame()});
                                          this.__container__ = new Object();
                                          try{
					      var row = new SelectrowC(contents, this.__container__, 1, 1);
					      this.__error__.insert({"before" : row.getFrame()});
					  }
                                          catch(err){
					      throw "From EditC(): The following exception was thrown:\n" + err;
					      this.__container__ = null;
					  }
                                          this.__commit__ = new Element("input", {"type" : "button", "value" : "generate fragment"});

                                          function setHandler(myself){
					      function onSuccessHandler(xhr){
						  var json = null;
						  try{
						      json = xhr.responseText.evalJSON();
						  }
						  catch(err){
						      alert("Got bad JSON data from " + xhr.request.url + "!\n\n" + err);
						  }
						  successFun(new Array(myself.getContent()), json);
					      }
					      
                                              myself.__commit__.observe("click", function(event){
						  myself.hideError();
						  clearFragment();
                                                  requestConstraints("[" + myself.toJSON() + "]", onSuccessHandler, null)
                                              });
                                          }
                                          setHandler(this);

                                          this.__error__.insert({"before" : this.__commit__});
                                      },
				      "getContent" : function(){
					  return this.__container__.__frames__[0].getContent();
				      },
				      "toJSON" : function(){
					  return this.getContent().toJSON();
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
                                                this.__commit__ = new Element("input", {"type" : "button", "value" : "generate fragment"});

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
							    clearFragment();
							    myself.showError(str);
							}
							else {						    
							    successFun(myself.getContent(true, true), json);
							}
						    }

                                            	    myself.__commit__.observe("click", function(event){
							myself.hideError();
							clearFragment();
                                                        requestConstraints(myself.toJSON(true), onSuccessHandler, null, true);
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
var ItemIdentityC = Class.create(ContainerC, {"initialize" : function($super, contents, parent){
                                                  $super();
                                                  this.__frame__.writeAttribute({"class" : CLASSES.itemIdentityFrame()});
                                                  this.__container__ = new Object();
 
                                                  try{
                                              	      for(var i = 0; i != contents.length; ++i){
                                              		  new TextrowC(decodeURI(contents[i]), ".*", this.__container__, 1, -1, null);
                                              		  this.__error__.insert({"before" : this.__container__.__frames__[i].getFrame()});
                                              	      }
                                                  }
                                                  catch(err){
                                              	      this.__container__ = new Object();
						      new TextrowC("", ".*", this.__container__, 1, -1, null);
                                              	      this.__error__.insert({"before" : this.__container__.__frames__[i].getFrame()});
                                                  }
                                                  finally {
						      function setDeactivateHandler(myself){
							  myself.__frame__.observe("dblclick", function(event){
							      if(myself.__container__.__frames__.length === 1){
								  if(myself.isUsed() === true){
								      myself.disable();
								      Event.stop(event);
								  }
								  else {
								      myself.enable();
								      if(parent.isUsed() === true) Event.stop(event);
								  }
							      }
							  });
						      }
						      setDeactivateHandler(this);

						      if(!contents || contents.length === 0) this.disable();
						  }
                                              },
					      "getContent" : function(unique, removeNull){
						  var values = new Array();
						  for(var i = 0; i != this.__container__.__frames__.length; ++i){
						      if(unique === true && values.indexOf(this.__container__.__frames__[i].getContent()) !== -1) continue;
						      if(removeNull === true && this.__container__.__frames__[i].getContent().strip().length === 0) continue;
						      values.push(this.__container__.__frames__[i].getContent().strip());
						  }
						  for(var i = 0; i !== values.length; ++i)values[i] = encodeURI(values[i]);
						  return values;
					      },
					      "toJSON" : function(unique, removeNull){
						  var content = this.getContent(unique, removeNull);
						  return content.length === 0 ? "null" : content.toJSON();
					      },
					      "disable" : function(){
						  this.hideError();
						  if(this.__container__.__frames__){
						      for(var i = 0; i !== this.__container__.__frames__.length; ++i){
							  this.__container__.__frames__[i].disable();
						      }
						  }
						  this.__disabled__ = true;
					      },
					      "enable" : function(){
						  if(this.__container__.__frames__){
						      for(var i = 0; i !== this.__container__.__frames__.length; ++i){
							  this.__container__.__frames__[i].enable();
						      }
						  }
						  this.__disabled__ = false;
					      }});


// --- Representation of a subjectLocator and subjectIdentifier frame.
var IdentifierC = Class.create(ContainerC, {"initialize" : function($super, contents, constraints, cssClass){
						$super();
						this.__frame__.writeAttribute({"class" : cssClass});
                                                this.__containers__ = new Array();
                                                this.__constraints__ = constraints;

						try{
						    if(constraints && constraints.length > 0){
							var cContents = new Array();
							if(contents) cContents = contents.clone();

							var ret = makeConstraintsAndContents(cContents, constraints, null);
							var constraintsAndContents = ret.constraintsAndContents;
							cContents = ret.contents;

							// --- creates all rows
							for(var i = 0; i != constraints.length; ++i){
							    this.__containers__.push(new Object());
							    var min = parseInt(constraints[i].cardMin);
							    var max = constraints[i].cardMax !== MAX_INT ? parseInt(constraints[i].cardMax) : MMAX_INT;
							    var regexp = constraints[i].regexp;
							    var _contents = null;
							    for(var j = 0; j !== constraintsAndContents.length; ++j){
								if(constraintsAndContents[j].constraint === constraints[i]){
								    _contents = constraintsAndContents[j].contents;
								    break;
								}
							    }
							    var _c_ = "";
							    for(var x = 0; x !== _contents.length; ++x) _c_ += "[" + x + "/" +  _contents.length + "]: " + _contents[x] + "\n";
							    if(max !== 0 || _contents && _contents.length){
								// -- creates the roles
								var cssTitle = "min: " + min + "   max: " + max + "   regular expression: " + constraints[i].regexp;
								var endIdx = (min === 0 ? 1 : min);
								endIdx = _contents && _contents.length > endIdx ? _contents.length : endIdx;
								for(var j = 0; j != endIdx; ++j){
								    var dblClickHandler = null;
								    if(min === 0) dblClickHandler = dblClickHandlerF;
								    var _content = "";
								    if(_contents && _contents.length > j) _content = _contents[j];
								    var row = new TextrowC(_content, constraints[i].regexp, this.__containers__[i],
											   min === 0 ? 1 : min, max === MMAX_INT ? -1 : max, cssTitle, dblClickHandler);
								    if(!_content) row.dblClick();
								    this.__error__.insert({"before" : row.getFrame()});
								}
							    }
							}
							// --- not used contents
							if(cContents.length !== 0){
							    this.__containers__.push(new Object());
							    for(var i = 0; i !== cContents.length; ++i){
								var owner = this.__containers__[this.__containers__.length - 1];
								var cssTitle = "No constraint found for this identifier!";
								var row = new TextrowC(cContents[i], "^.+$", owner, 0, 1, cssTitle, null);
								this.__error__.insert({"before" : row.getFrame()});
							    }
							}

						    }
						    else if(contents && contents.length !== 0){
							this.__containers__.push(new Object());
							var cssTitle = "No constraint found for this identifier";
							for(var i = 0; i !== contents.length; ++i){
							    var row = new TextrowC(contents[i], null, this.__containers__[0], 0, 1, cssTitle, null);
							    this.__error__.insert({"before" : row.getFrame()});
							}
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
						    for(var i = 0; i !== values.length; ++i) values[i] = encodeURI(values[i]);
						    return values;
						}
						for(var i = 0; i !== values.length; ++i) values[i] = encodeURI(values[i]);
						return values;
					    },
					    "toJSON" : function(unique, removeNull){
						var content = this.getContent(unique, removeNull);
						return content.length === 0 ? "null" : content.toJSON();
					    },
					    "isValid" : function(){
						var allIdentifiers = new Array();
						var errorStr = "";
						var ret = true;
						
						// --- checks if there are any constraints
						if((!this.__constraints__ || this.__constraints__.length === 0) && this.__containers__.length !== 0){
						    for(var i = 0; i !== this.__containers__.length; ++i){
							for(var j = 0; this.__containers__[i].__frames__ && j !== this.__containers__[i].__frames__.length; ++j){
							    this.__containers__[i].__frames__[j].showError("No constraints found for this identifier!");
							}
						    }
						    return false;
						}
						else if(!this.__constraints__ || this.__constraints__.length === 0) return true;
						
						// --- collects all non-empty identifiers
						for(var i = 0; i !== this.__containers__.length; ++i){
						    for(var j = 0; this.__containers__[i].__frames__ && j !== this.__containers__[i].__frames__.length; ++j){
							var row = this.__containers__[i].__frames__[j];
							row.hideError();
							if(row.isUsed() === true && row.getContent().strip().length !== 0) allIdentifiers.push(row);
						    }
						}
						
						var checkedIdentifiers = new Array();
						for(var i = 0; i !== this.__constraints__.length; ++i){
						    var regexp = new RegExp(this.__constraints__[i].regexp);
						    var cardMin = parseInt(this.__constraints__[i].cardMin);
						    var cardMax = this.__constraints__[i].cardMax === MAX_INT ? MMAX_INT : parseInt(this.__constraints__[i].cardMax);
						    var currentIdentifiers = new Array();
						    for(var j = 0; j !== allIdentifiers.length; ++j){
							if(regexp.match(allIdentifiers[j].getContent()) === true) currentIdentifiers.push(allIdentifiers[j]);
						    }
						    checkedIdentifiers = checkedIdentifiers.concat(currentIdentifiers);
						    
						    // --- checks card-min and card-max for the current constraint
						    if(cardMin > currentIdentifiers.length){
							if(errorStr.length !== 0) errorStr += "<br/><br/>";
							errorStr += "card-min of the constraint regexp: \"" + this.__constraints__[i].regexp + "\" card-min: " + cardMin + " card-max: " + cardMax + " is not satisfied (" + currentIdentifiers.length + ")!";
							ret = false;
						    }
						    if(cardMax !== MMAX_INT && cardMax < currentIdentifiers.length){
							if(errorStr.length !== 0) errorStr += "<br/><br/>";
							errorStr += "card-max of the constraint regexp: \"" + this.__constraints__[i].regexp + "\" card-min: " + cardMin + " card-max: " + cardMax + " is not satisfied (" + currentIdentifiers.length + ")!";
							ret = false;
						    }
						}
						
						// --- checks if there are some identifiers which don't satisfies any constraint
						checkedIdentifiers = checkedIdentifiers.uniq();
						if(checkedIdentifiers.length < allIdentifiers.length){							
						    ret = false;
						    for(var i = 0; i !== allIdentifiers.length; ++i){
							if(checkedIdentifiers.indexOf(allIdentifiers[i]) === -1) allIdentifiers[i].showError("This Identifier does not satisfie any constraint!");
						    }
						}
						
						if(ret === true) this.hideError();
						else this.showError(errorStr);
						return ret;
					    }});


// --- Represantation of a scope frame, doesn't contain SelectrowCs, because the values must be unique!
// --- So this class uses another implementation.
var ScopeC = Class.create(ContainerC, {"initialize" : function($super, contents, selectedContents, min, max){
                                           $super();
                                           this.__frame__.writeAttribute({"class" : CLASSES.scopeFrame()});
                                           this.__error__ = this.__error__.remove();

                                           this.__container__ = null;
                                           this.__contents__ = contents;
                                           this.resetRows(this.__contents__, min, max, selectedContents);
                                        },
				       "resetRows" : function(contents, min, max, selectedContents){
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
					   this.__min__ = min;
					   this.__max__ = max;

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
							       var opt = new Element("option", {"value" : values[j][k]}).update(values[j][k]);
							       select.insert({"bottom" : opt});
							       if(values[j][k] === selectedItems[i]) opt.writeAttribute({"selected" : "selected"});
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

					       function changeHandler(event){
						   try{
						   var eventOwner = event.element();
						   var newValue = eventOwner.value;
						   var oldValue = null;
						   var allValues = new Array();
						   var allOpts = myself.getFrame().select("option");
						   var allOwnOpts = eventOwner.select("option");
						   for(var i = 0; i !== allOwnOpts.length; ++i) allOpts = allOpts.without(allOwnOpts[i]);

						   // --- collects all selected values
						   for(var i = 0; i !== allOpts.length; ++i) allValues.push(allOpts[i].value);
						   allValues = allValues.uniq();
						   var foundContent = new Array();
						   for(var i = 0; i !== allValues.length; ++i){
						       for(var j = 0; contents && j !== contents.length; ++j){
							   for(var k = 0; k !== contents[j].length; ++k){
							       if(contents[j][k].indexOf(allValues[i]) !== -1) foundContent.push(contents[j]);
							       if(contents[j][k].indexOf(newValue) !== -1) foundContent.push(contents[j]);
							   }
						       }
						   }
						   foundContent = foundContent.uniq();
						   // --- searches for the content to be removed from all other select elements
						   // --- and for the values to be inserted to all other elements
						   var contentToAdd = null;
						   var contentToRemove = null;
						   if(contents && contents.length !== 0){
						       for(var i = 0; i !== contents.length; ++i){
							   if(foundContent.indexOf(contents[i]) === -1) contentToAdd = contents[i];
							   if(!contentToRemove){
							       for(var j = 0; j !== contents[i].length; ++j){
								   if(contentToRemove) break;
								   for(var k = 0; k !== contents[i][j].length; ++k){
								       if(contents[i][j][k].indexOf(newValue) !== -1){
									   contentToRemove = contents[i];
									   break;
								       }
								   }
							       }
							   }
						       }
						   }

						   // --- iterates through all select elements and adds/removes the found values
						   var selects = myself.getFrame().select("select");
						   selects = selects.without(eventOwner);
						   if(contentToAdd) contentToAdd = contentToAdd.flatten();
						   if(contentToRemove) contentToRemove = contentToRemove.flatten();
						   for(var i = 0; i !== selects.length; ++i){
						       var opts = selects[i].select("option");
						       var val = selects[i].value;
						       for(var j = 0; j !== opts.length; ++j){
							   if(contentToRemove.indexOf(opts[j].value) !== -1) opts[j].remove();
						       }
						       
						       if(contentToAdd){
							   var selectOpts = new Array();
							   for(var j = 0; j !== opts.length; ++j) selectOpts.push(opts[j].value);
							   var iter = 0;
							   for( ; iter !== contentToAdd.length; ++iter){
							       if(selectOpts.indexOf(contentToAdd[iter]) !== -1) break;
							   }
							   if(iter === contentToAdd.length){
							       for(var j = 0; j !== contentToAdd.length; ++j){
								   selects[i].insert({"bottom" : new Element("option", {"value" : contentToAdd[j]}).update(contentToAdd[j])});
							       }
							   }
						       }
						   }
						   }catch(err){ alert("ch: " + err);}
					       } // changeHandler

					       for(var i = 0; i != rows.length; ++i){
						   var selectE = rows[i].select("select");
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
						   if(selectE.length !== 0){
						       selectE[0].stopObserving("change");
						       selectE[0].observe("change", changeHandler);
						   }
					       }
					   } // addHandlers


					   var endIdx = (min === -1 ? 1 : min);
					   if(selectedContents && selectedContents.length > endIdx) endIdx = selectedContents.length;
					   if(endIdx > options.length) throw "From ScopeC(): not enough scope-topics(" + options.length + ") to satisfie card-min(" + min + ")!";
					   for(var i = 0; i != endIdx; ++i){
					       var currentScope = null;
					       if(selectedContents && selectedContents.length > i) currentScope = selectedContents[i];
					       var currentOptions = options.clone();
					       
					       var optionsToRemove = new Array();
					       for(var j = 0; selectedContents && j !== selectedContents.length; ++j){
						   for(var k = 0; k !== selectedContents[j].length; ++k){
						       for(var l = 0; l !== currentOptions.length; ++l){
							   if(currentOptions[l].indexOf(selectedContents[j][k]) !== -1) optionsToRemove.push(currentOptions[l]);
						       }
						   }
					       }
					       
					       optionsToRemove = optionsToRemove.uniq();
					       for(var j = 0; j !== optionsToRemove.length; ++j) currentOptions = currentOptions.without(optionsToRemove[j]);
					       if(currentScope) currentOptions.unshift(currentScope);
					       var div = new Element("div", {"class" : CLASSES.selectrowWithoutRemoveButton()});
					       var select = new Element("select");
					       for(var j = 0; j != currentOptions.length; ++j){
						   for(var k = 0; k != currentOptions[j].length; ++k){
						       select.insert({"bottom" : new Element("option", {"value" : currentOptions[j][k]}).update(currentOptions[j][k])});
						   }
					       }
				       
					       div.insert({"top" : select});
					       this.getFrame().insert({"bottom" : div});
					       addHandlers(this);
					   }
				       },
				       "isUsed" : function(){
					   return this.getContent(true, true).length !== 0 && !this.__disabled__;
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

					   for(var i = 0; i !== values.length; ++i)
					       values[i] = new Array(values[i]);
					   return values;
				       },
				       "disable" : function(){
					   this.hideError();
					   var rows = this.getFrame().select("div");
					   for(var i = 0; i != rows.length; ++i){
					       rows[i].select("select")[0].disable();
					       var buttons = rows[i].select("span." + CLASSES.clickable());
					       buttons[0].hide();
					       buttons[1].hide();
					   }
					   this.__disabled__ = true;
				       },
				       "enable" : function(){
					   var rows = this.getFrame().select("div");
					   for(var i = 0; i != rows.length; ++i){
					       rows[i].select("select")[0].enable();
					       var buttons = rows[i].select("span." + CLASSES.clickable());
					       if(this.__min__ < rows.length && rows.length !== 1) buttons[0].show();
					       if(this.__max__ === -1 || this.__max__ > rows.length) buttons[1].show();
					   }
					   this.__disabled__ = false;
				       }});



// --- Contains all scope frames of an element (there can be more than one scope constraint)
var ScopeContainerC = Class.create(ContainerC, {"initialize" : function($super, contents, constraints){
                                                    $super();
                                                    this.__frame__.writeAttribute({"class" : CLASSES.scopeContainer()});
                                                    this.__container__ = new Array();
                                                    this.resetValues(contents, constraints);
                                                    this.__constraints__ = constraints;

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

						    this.__constraints__ = constraints;

						    // --- sets contents corresponding to the passed constraints
						    if(constraints && constraints.length){
							var cContents = contents ? contents.clone() : null;
							var foundContents = new Array();
							for(var i = 0; i != constraints.length; ++i){
							    var scopeTypes = constraints[i].scopeTypes;
							    var min = parseInt(constraints[i].cardMin);
							    var max = constraints[i].cardMax !== MAX_INT ? parseInt(constraints[i].cardMax) : MMAX_INT;
							    
							    // --- checks already existing scopes with the given scope-constraints
							    var currentFoundContents = new Array();
							    if(cContents && cContents.length !== 0){
								var allCurrentTypes = scopeTypes ? scopeTypes.flatten() : new Array();
								for(var j = 0; j !== cContents.length; ++j){
								    for(var k = 0; k !== allCurrentTypes.length; ++k){
									if(cContents[j].indexOf(allCurrentTypes[k]) !== -1){
									    foundContents.push(cContents[j]);
									    currentFoundContents.push(cContents[j]);
									    break;
									}
								    }
								}
								foundContents = foundContents.uniq();
							    }
							    							    							    
							    // --- if min === 0 adds an empty option
							    if(min === 0){
								scopeTypes.unshift(new Array(new Array(""))); // [[""]]
							    }
							    
							    var scp  = new ScopeC(scopeTypes, currentFoundContents, min === 0 ? 1 : min, max === MMAX_INT ? -1 : max);
							    this.__container__.push(scp);
							    this.__error__.insert({"before" : scp.getFrame()});
							}
							
							// --- removes contents that are already used
							if(cContents && cContents.length !== 0){
							    for(var i = 0; i !== foundContents.length; ++i) cContents = cContents.without(foundContents[i]);
							    
							    // --- inserts all contents that doesn't correspond with any constraint
							    for(var i = 0; i !== cContents.length; ++i) cContents[i] = new Array(cContents[i]);
							    var cmax = cContents.length;
							    for(var i = 0; i !== cContents.length; ++i){
								var scp = new ScopeC(new Array(cContents[i]), null, 1, 1);
								this.__container__.push(scp);
								this.__error__.insert({"before" : scp.getFrame()});
							    }
							}
						    }
						    else if(contents && contents.length){
							for(var i = 0; i !== contents.length; ++i){
							    var scp = new ScopeC(new Array(new Array(contents[i])), null, 1, 1);
							    this.__container__.push(scp);
							    this.__error__.insert({"before" : scp.getFrame()});
							}
						    }
						    else {
							this.getFrame().insert({"top" : new Element("div", {"class" : CLASSES.selectrowWithoutRemoveButton()})});
						    }  
						},
						"isUsed" : function(){
						    if(this.__disabled__ === true) return false;
						    for(var i = 0; i != this.__container__.length; ++i){
							if(this.__container__[i].isUsed() === true) return true;
						    }
						    return false;
						},
						"isValid" : function(){
						    var errorStr = "";
						    var ret = true;
						    var allContent = this.getContent();
						    if(!allContent) allContent = new Array();
						    var allFoundContent = new Array();
						    if(allContent) allContent = allContent.flatten();
						    if((!this.__constraints__ || this.__constraints__length === 0) && allContent.length !== 0){
							this.showError("No constraints found for the existing scopes!");
							return false;
						    }
						    for(var i = 0; this.__constraints__ && i !== this.__constraints__.length; ++i){
							var min = parseInt(this.__constraints__[i].cardMin);
							var max = this.__constraints__[i].cardMax === MAX_INT ? MMAX_INT : parseInt(this.__constraints__[i].cardMax);
							var scopes = this.__constraints__[i].scopeTypes;
							if(scopes) scopes = scopes.flatten();
							else scopes = new Array();
							
							// --- checks all available types for the current constraint
							var currentFoundContent = new Array();
							for(var j = 0; j !== allContent.length; ++j){
							    if(scopes.indexOf(allContent[j]) !== -1){
								currentFoundContent.push(allContent[j]);
								allFoundContent.push(allContent[j]);
							    }
							}
							currentFoundContent = currentFoundContent.uniq();
							allFoundContent = allFoundContent.uniq();
							
							// --- find topics for the found psis
							var foundScopes = 0;
							var _scopes = this.__constraints__[i].scopeTypes;
							for(var j = 0; _scopes && j !== _scopes.length; ++j){
							    for(var k = 0; k !== _scopes[j].length; ++k){
								for(var l = 0; l !== currentFoundContent.length; ++l){
								    if(_scopes[j][k].indexOf(currentFoundContent[l]) !== -1){
									++foundScopes;
									break;
								    }
								}
							    }
							}
							// --- checks card-min/card-max
							var scStr = "";
							for(var j = 0; j !== scopes.length; ++j){
							    if(scopes[j].length !== 0) scStr += "<br/>&nbsp;&nbsp;*" + scopes[j];
							}
							
							if(min > foundScopes){
							    if(errorStr.length !== 0) errorStr += "<br/><br/>";
							    errorStr += "card-min(" + min + ") of the scope-constraint with the available scopes" + scStr + "<br/>is not satisfied(" + foundScopes + ")!"
							    ret = false;
							}
							if(max !== MMAX_INT && max < foundScopes){
							    if(errorStr.length !== 0) errorStr += "<br/><br/>";
							    errorStr += "card-max(" + max + ") of the scope-constraint with the available scopes" + scStr + "<br/>is not satisfied(" + foundScopes + ")!"
							    ret = false;
							}
						    }
						    
						    // --- removes all checked contents
						    for(var i = 0; i !== allFoundContent.length; ++i) allContent = allContent.without(allFoundContent[i]);
						    if(allContent && allContent.length !== 0){
							allContent = allContent.flatten();
							scStr = "";
							for(var j = 0; j !== allContent.length; ++j){
							    if(allContent[j].length !== 0) scStr += "<br/>&nbsp;&nbsp;*" + allContent[j];
							}
							if(errorStr.length !== 0) errorStr += "<br/><br/>";
							errorStr += "No constraint found for the scopes \"" + scStr + "\"!";
							ret = false;
						    }
						    
						    if(ret === true) this.hideError();
						    else if(errorStr.length !== 0)this.showError(errorStr);
						    return ret;
						},
						"getContent" : function(){
						    var values = new Array();
						    try{
						    for(var i = 0; i != this.__container__.length; ++i){
							var cValues = this.__container__[i].getContent(true, true);
							for(var j = 0; j != cValues.length; ++j){
							    if(values.indexOf(cValues[j]) !== -1) continue;
							    values.push(cValues[j][0]);
							}
						    }
						    }catch(err){
							return new Array();
						    }
						    if(values.length === 0) return null;
						    values = values.uniq();
						    for(var i = 0; i !== values.length; ++i) values[i] = new Array(values[i]);
						    return values;
						},
						"toJSON" : function(){
						    if(!this.getContent() || this.getContent().length === 0) return "null";
						    return this.getContent().toJSON();
						},
						"disable" : function(){
						    this.hideError();
						    for(var i = 0; i !== this.__container__.length; ++i) this.__container__[i].disable();
						    this.__disabled__ = true;
						},
						"enable" : function(){
						    for(var i = 0; i !== this.__container__.length; ++i) this.__container__[i].enable();
						    this.__disabled__ = false;
						}});


// --- Representation of a variant element
var VariantC = Class.create(ContainerC, {"initialize" : function($super, contents, owner, dblClickHandler, parent){
                                             $super();
                                             if(!owner.__frames__) owner.__frames__ = new Array();
                                             owner.__frames__.push(this);
                                             this.__frame__.writeAttribute({"class" : CLASSES.variantFrame()});
                                             this.__table__ = new Element("table", {"class" : CLASSES.variantFrame()});
                                             this.__frame__.insert({"top" : this.__table__});
                                             this.__owner__ = owner;
					     this.__dblClickHandler__ = dblClickHandler;
                                             this.__isMinimized__ = false;
    
                                             try{
						 var itemIdentityContent = null;
						 var scopesContent = null;
						 if(contents){
						     itemIdentityContent = contents.itemIdentities;
						     scopesContent = contents.scopes;
						 }

						 // --- control row + itemIdentity
						 makeControlRow(this, 4, itemIdentityContent);
						 checkRemoveAddButtons(owner, 1, -1, null);
						 setRemoveAddHandler(this, true, owner, 1, -1, function(){
						     return new VariantC(null, owner, dblClickHandler, parent);
						 });
						 						 
						 // --- scopes
						 this.__scopes__ = null;
						 //TODO: implement -> also in the server
						 this.__table__.insert({"bottom" : newRow(CLASSES.scopeContainer(), "Scope", new Element("div"))});
						 
						 // --- resource value and datatype
						 makeResource(this, contents, null, null, null, {"rows" : 3, "cols" : 55});
						 
						 this.getFrame().observe("dblclick", function(event){
						     dblClickHandler(owner, event);
						     if(parent.isUsed() === true)Event.stop(event);
						 });
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
					 "isEmpty" : function(){
					     return this.__value__.value.length === 0;
					 },
					 "isValid" : function(){
					     if(this.__value__.value.strip() === ""){
						 this.showError("Resource Value must be set!");
						 return false;
					     }
					     else {
						 this.hideError();
						 return true;
					     }
					 },
					 "isUsed" : function(){
					     return !this.__disabled__;
					 },
					 "disable" : function(){
					     this.hideError();
					     this.__itemIdentity__.disable();
					     // TODO: scope
					     this.__value__.writeAttribute({"readonly" : "readonly"});
					     this.__datatype__.__frames__[0].disable();
					     this.hideRemoveButton();
					     this.hideAddButton();
					     this.getFrame().writeAttribute({"class" : CLASSES.disabled()});
					     this.__disabled__ = true;
					 },
					 "enable" : function(){
					     this.getFrame().select("tr." + CLASSES.showHiddenRows())[0].hide();
					     this.__itemIdentity__.enable();
					     // TODO: scope
					     this.__value__.removeAttribute("readonly")
					     this.__datatype__.__frames__[0].enable();
					     if(this.__owner__.__frames__.length > 1) this.showRemoveButton();
					     this.showAddButton();
					     this.getFrame().removeAttribute("style");
					     this.getFrame().writeAttribute({"class" : CLASSES.variantFrame()});
					     this.__disabled__ = false;
					 },
					 "minimize" : function(){
					     if(this.__isMinimized__ === false) {
						 this.getFrame().select("tr." + CLASSES.showHiddenRows())[0].show();
						 this.getFrame().select("tr." + CLASSES.itemIdentityFrame())[0].hide();
						 this.getFrame().select("tr." + CLASSES.scopeContainer())[0].hide();
						 this.getFrame().select("tr." + CLASSES.valueFrame())[0].hide();
						 this.getFrame().select("tr." + CLASSES.datatypeFrame())[0].hide();
						 this.__isMinimized__ = true;
					     }
					     else {
						 this.getFrame().select("tr." + CLASSES.showHiddenRows())[0].hide();
						 this.getFrame().select("tr." + CLASSES.itemIdentityFrame())[0].show();
						 this.getFrame().select("tr." + CLASSES.scopeContainer())[0].show();
						 this.getFrame().select("tr." + CLASSES.valueFrame())[0].show();
						 this.getFrame().select("tr." + CLASSES.datatypeFrame())[0].show();
						 this.__isMinimized__ = false;
					     }
					 }});


// --- contains all variants of a name element
var VariantContainerC = Class.create(ContainerC, {"initialize" : function($super, contents, parent){
                                                      $super();
                                                      this.__frame__.writeAttribute({"class" : CLASSES.variantContainer()});
                                                      this.__container__ = new Object();

                                                      if(contents && contents.length != 0){
							  for(var i = 0; i != contents.length; ++i){
							      var variant = new VariantC(contents[i], this.__container__, dblClickHandlerF, parent);
							      this.__frame__.insert({"bottom" : variant.getFrame()});
							      variant.minimize();
							  }
						      }
                                                      else {
							  var variant = new VariantC(null, this.__container__, dblClickHandlerF, parent);
							  this.__frame__.insert({"bottom" : variant.getFrame()});
							  variant.minimize();
							  variant.disable();
						      }
                                                  },
						  "getContent" : function(){
						      var values = new Array();
						      for(var i = 0; i != this.__container__.__frames__.length; ++i){
							  if(this.__container__.__frames__[i].isUsed() === true && this.__container__.__frames__[i].isEmpty() === false){
							      values.push(this.__container__.__frames__[i].getContent());
							  }
						      }
						      return values;
						  },
						  "isValid" : function(){
						      var ret = true;
						      for(var i = 0; i != this.__container__.__frames__.length; ++i){
							  if(this.__container__.__frames__[i].isUsed() === true && 
							     this.__container__.__frames__[i].isValid() === false) ret = false;;
						      }
						      return ret;
						  },
						  "toJSON" : function(){
						      var str = "[";
						      for(var i = 0; i != this.__container__.__frames__.length; ++i){
							  if(this.__container__.__frames__[i].isUsed() === true  && this.__container__.__frames__[i].isEmpty() === false){
							      str += this.__container__.__frames__[i].toJSON() + ",";
							  }
						      }
						      str = str.substring(0, str.length - 1) + "]"
						      return str === "]" ? null : str;
						  },
						  "isUsed" : function(){
						      return !this.__disabled__;
						  },
						  "disable" : function(){
						      this.hideError();
						      if(this.__container__.__frames__){
							  for(var i = 0; i !== this.__container__.__frames__.length; ++i)
							      this.__container__.__frames__[i].disable();
						      }
						      this.__disabled__ = true;
						  },
						  "enable" : function(){
						      if(this.__container__.__frames__){
							  for(var i = 0; i !== this.__container__.__frames__.length; ++i)
							      this.__container__.__frames__[i].enable();
						      }
						      this.__disabled__ = false;
						  }});


// --- representation of a name element
var NameC = Class.create(ContainerC, {"initialize" : function($super, contents, nametypescopes, simpleConstraint, owner, min, max, dblClickHandler){
                                          $super();
                                          if(!owner) throw "From NameC(): owner must be set but is null";
                                          if(max !== -1 && (min > max || max === 0))throw "From FrameC(): min must be > max(" + max + ") and > 0 but is " + min;
                                          if(!owner.__frames__) owner.__frames__ = new Array();
                                          owner.__frames__.push(this);
    
                                          this.__frame__.writeAttribute({"class" : CLASSES.nameFrame()});
                                          this.__table__ = new Element("table", {"class" : CLASSES.nameFrame()});
                                          this.__frame__.insert({"top" : this.__table__});
                                          this.__max__ = max;
                                          this.__owner__ = owner;
                                          this.__dblClickHandler__ = dblClickHandler;
                                          this.__constraint__ = simpleConstraint;
                                          this.__isMinimized__ = false;
    
                                          try{
					      var itemIdentityContent = null;
					      var typeContent = null;
					      var scopesContent = null;
					      var valueContent = "";
					      var variantsContent = null;
					      if(contents){
						  itemIdentityContent = contents.itemIdentities;
						  typeContent = contents.type;
						  scopesContent = contents.scopes;
						  valueContent = contents.value;
						  variantsContent = contents.variants;
					      }
					      
					      // --- control row + ItemIdentity
					      makeControlRow(this, 5, itemIdentityContent);
					      checkRemoveAddButtons(owner, min, max, this);
					      setRemoveAddHandler(this, this.__constraint__, owner, min, max, function(){
						  return new NameC(null, nametypescopes, simpleConstraint, owner, min, max, dblClickHandler);
					      });

					      // --- type
					      var types = makeTypes(this, typeContent, nametypescopes);

					      // --- scopes
					      this.__scope__ = new ScopeContainerC(scopesContent, nametypescopes && nametypescopes[0].scopeConstraints ? nametypescopes[0].scopeConstraints : null);
					      this.__table__.insert({"bottom" : newRow(CLASSES.scopeContainer(), "Scope", this.__scope__.getFrame())});
					      onTypeChangeScope(this, contents ? contents.scopes : null, nametypescopes, "name");

					      // --- value
					      var noConstraint = false;
					      if(!simpleConstraint){
						  simpleConstraint = {"regexp" : ".*", "cardMin" : 0, "cardMax" : MAX_INT};
						  noConstraint = true;
					      }
					      this.__value__ = new Object();
					      var _min = parseInt(simpleConstraint.cardMin);
					      var _max = simpleConstraint.cardMax !== MAX_INT ? parseInt(simpleConstraint.cardMax) : MMAX_INT;
					      var cssTitle = "No constraint found for this name";
					      if(noConstraint === false){
						  cssTitle = "min: " + _min + "   max: " + _max + "   regular expression: " + (simpleConstraint ? simpleConstraint.regexp : ".*");
					      }
					      this.__cssTitle__ = cssTitle;
					      new TextrowC(valueContent, (simpleConstraint ? simpleConstraint.regexp : ".*"), this.__value__, 1, 1, cssTitle);
					      this.__table__.insert({"bottom" : newRow(CLASSES.valueFrame(), "Value", this.__value__.__frames__[0].getFrame())});
					      
					      // --- variants
					      this.__variants__ = new VariantContainerC(variantsContent, this);
					      this.__table__.insert({"bottom" : newRow(CLASSES.variantContainer(), "Variants", this.__variants__.getFrame())});
					      
					      // --- adds a second show handler, so the variants will be hidden, when the entire
					      // --- name element will be shown
					      function addSecondShowHandler(myself){
						  myself.__table__.select("tr")[0].observe("click", function(event){
						      for(var i = 0; i != myself.__variants__.__container__.__frames__.length; ++i){
							  myself.__variants__.__container__.__frames__[i].minimize();
						      }
						  });
					      }
					      
					      addSecondShowHandler(this);

					      this.getFrame().observe("dblclick", function(event){
						  dblClickHandler(owner, event);
					      });
                                          }
                                          catch(err){
                                      	      alert("From NameC(): " + err);
                                          }
                                      },
				      "isEmpty" : function(){
					  return  this.__value__.__frames__[0].getContent().length === 0;
				      },
				      "getContent" : function(){
					  if(this.isUsed() === false) return null;
					  var type = this.__type__.__frames__[0].getContent();
					  return {"itemIdentities" : this.__itemIdentity__.getContent(true, true),
						  "type" : type ? new Array(type) : null,
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
				      "isValid" : function(){
					  var valueValid = this.__value__.__frames__[0].isValid();
					  if(valueValid === false) this.showError("The name-value \"" + this.__value__.__frames__[0].getContent() + "\" doesn't matches the constraint \"" + this.__value__.__frames__[0].getRegexp() + "\"!");
					  else this.hideError();
					  var variantsValid = this.__variants__.isValid();
					  var scopeValid = this.scopeIsValid();
					  return valueValid && variantsValid && scopeValid;
				      },
				      "scopeIsValid" : function(){
					  return this.__scope__.isValid();
				      },
				      "minimize" : function(){
					  if(this.__isMinimized__ === false){
					      this.getFrame().select("tr." + CLASSES.showHiddenRows())[0].show();
					      this.getFrame().select("tr." + CLASSES.itemIdentityFrame())[0].hide();
					      this.getFrame().select("tr." + CLASSES.typeFrame())[0].hide();
					      this.getFrame().select("tr." + CLASSES.scopeContainer())[0].hide();
					      this.getFrame().select("tr." + CLASSES.valueFrame())[0].hide();
					      this.getFrame().select("tr." + CLASSES.variantContainer())[0].hide();
					      this.__isMinimized__ = true;
					  }
					  else {
					      this.getFrame().select("tr." + CLASSES.showHiddenRows())[0].hide();
					      this.getFrame().select("tr." + CLASSES.itemIdentityFrame())[0].show();
					      this.getFrame().select("tr." + CLASSES.typeFrame())[0].show();
					      this.getFrame().select("tr." + CLASSES.scopeContainer())[0].show();
					      this.getFrame().select("tr." + CLASSES.valueFrame())[0].show();
					      this.getFrame().select("tr." + CLASSES.variantContainer())[0].show();
					      this.__isMinimized__ = false;
					  }
				      },
				      "disable" : function(){
					  this.hideError();
					  this.__itemIdentity__.disable();
					  this.__type__.__frames__[0].disable();
					  this.__scope__.disable();
					  this.__value__.__frames__[0].disable();
					  this.__variants__.disable();
/*
					  disableItemIdentity(this);
					  disableType(this);
					  disableScope(this);
					  disableValue(this);
					  disableVariants(this);
					  this.getFrame().setStyle(DISABLED_BACKGROUND_COLOR);*/
					  this.getFrame().writeAttribute({"class" : CLASSES.disabled()});
					  this.getFrame().writeAttribute({"title" : this.__cssTitle__});
					  this.hideAddButton();
					  this.__disabled__ = true;
				      },
				      "enable" : function(){
					  this.__itemIdentity__.enable();
					  this.__type__.__frames__[0].enable();
					  this.__scope__.enable();
					  this.__value__.__frames__[0].enable();
					  this.__variants__.enable();
/*
					  enableItemIdentity(this);
					  enableType(this);
					  enableScope(this);
					  enableValue(this);
					  enableVariants(this);*/
					  this.getFrame().writeAttribute({"class" : CLASSES.nameFrame()});
					  this.getFrame().removeAttribute("title");
					  checkRemoveAddButtons(this.__owner__, 1, this.__max__, this);
					  this.__disabled__ = false;
				      }});



// --- contains all names of a topic
var NameContainerC = Class.create(ContainerC, {"initialize" : function($super, contents, constraints){
                                                   $super();
                                                   this.__frame__.writeAttribute({"class" : CLASSES.nameContainer()});
                                                   this.__containers__ = new Array();
                                                   this.__constraints__ = constraints;

                                                   try{
						       if(constraints && constraints.length > 0){
							   var cContents = new Array();
							   if(contents) cContents = contents.clone();
							   for(var i = 0; i != constraints.length; ++i){
							       var simpleConstraints = constraints[i].constraints;

							       var allTypes = new Array();
							       for(var k = 0; k !== constraints[i].nametypescopes.length; ++k){
								   allTypes = allTypes.concat(constraints[i].nametypescopes[k].nameType);
							       }
							       allTypes = allTypes.flatten().uniq();

							       var ret = makeConstraintsAndContents(cContents, simpleConstraints, allTypes);
							       var constraintsAndContents = ret.constraintsAndContents;
							       cContents = ret.contents;

							       // --- creation of the frames with the found contents
							       this.__containers__.push(new Array());
							       for(var j = 0; j != constraints[i].constraints.length; ++j){
								   this.__containers__[i].push(new Object());
								   var min = parseInt(constraints[i].constraints[j].cardMin);
								   var max = constraints[i].constraints[j].cardMax !== MAX_INT ? parseInt(constraints[i].constraints[j].cardMax) : MMAX_INT;
								   var _contents = null;
								   for(var k = 0; k !== constraintsAndContents.length; ++k){
								       if(constraintsAndContents[k].constraint === constraints[i].constraints[j]){
									   _contents = constraintsAndContents[k].contents;
									   break;
								       }
								   }
								   var endIdx = (min === 0 ? 1 : min);
								   endIdx = _contents && _contents.length > endIdx ? _contents.length : endIdx;
								   var regexp = constraints[i].constraints[j].regexp;
								   if(max !== 0 || _contents && _contents.length){
								       var dblClickHandler = null;
								       if(min === 0) dblClickHandler = dblClickHandlerF;
								       for(var k = 0; k !== endIdx; ++k){
									   var _content = null;
									   if(_contents && _contents.length > k) _content = _contents[k];
									   var name = new NameC(_content, constraints[i].nametypescopes, constraints[i].constraints[j], this.__containers__[i][j], min === 0 ? 1 : min, max === MMAX_INT ? -1 : max, dblClickHandler);
									   if(min === 0)name.disable();
									   this.__error__.insert({"before" : name.getFrame()});
									   if(min === 0)name.minimize();
								       }
								   }
							       }
							   }
							   // --- inserts not used contents
							   if(cContents.length !== 0){
							       this.__containers__.push(new Array(new Object()));
							       var owner = this.__containers__[0][0];
							       var cssTitle = "No constraint found for this name";
							       for(var i = 0; i !== cContents.length; ++i){
								   var name = new NameC(cContents[i], null, null, owner, 0, 1, null);
								   this.__error__.insert({"before" : name.getFrame()});
							       }
							   }
                                               	       }
						       else if(contents && contents.length !== 0){
							   this.__containers__.push(new Array(new Object()));
							   var owner = this.__containers__[0][0];
							   var cssTitle = "No constraint found for this name";
							   for(var i = 0; i !== contents.length; ++i){
							       var name = new NameC(contents[i], null, null, owner, 0, 1, null);
							       this.__error__.insert({"before" : name.getFrame()});
							   }
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
								   if(this.__containers__[i][j].__frames__[k].isUsed() === true && this.__containers__[i][j].__frames__[k].isEmpty() === false){
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
								   if(this.__containers__[i][j].__frames__[k].isUsed() === true  && this.__containers__[i][j].__frames__[k].isEmpty() === false){
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
						   var ret = true;
						   var errorStr = "";
						   
						   // --- checks if there are any constraints
						   if((!this.__constraints__ || this.__constraints__.length === 0) && this.__containers__.length !== 0){
						       var nameTypes = new Array();
						       for(var i = 0; i !== this.__containers__.length; ++i){
							   for(var j = 0; j !== this.__containers__[i].length; ++j){
							       for(var k = 0; k !== this.__containers__[i][j].__frames__.length; ++k){
								   this.__containers__[i][j].__frames__[k].hideError();
								   if(this.__containers__[i][j].__frames__[k].isUsed() === true){
								       this.__containers__[i][j].__frames__[k].showError("No constraints found for this name!");
								   }
							       }
							   }
						       }
						       return false;
						   }
						   else if(!this.__constraints__ || this.__constraints__.length === 0) return true;
						   
						   // --- summarizes all names
						   var allNames = new Array();
						   for(var i = 0; i !== this.__containers__.length; ++i){
						       for(var j = 0; j !== this.__containers__[i].length; ++j){
							   for(var k = 0; k !== this.__containers__[i][j].__frames__.length; ++k){
							       this.__containers__[i][j].__frames__[k].hideError();
							       if(this.__containers__[i][j].__frames__[k].scopeIsValid() === false) ret = false;
							       if(this.__containers__[i][j].__frames__[k].isUsed() === true && this.__containers__[i][j].__frames__[k].isEmpty() === false){
								   allNames.push(this.__containers__[i][j].__frames__[k]);
							       }
							   }
						       }
						   }
						   
						   // --- checks every constraint and the existing names corresponding to the constraint
						   for(var i = 0; i !== this.__constraints__.length; ++i){
						       var currentConstraintTypes = new Array();
						       for(var j = 0; j !== this.__constraints__[i].nametypescopes.length; ++j){
							   currentConstraintTypes = currentConstraintTypes.concat(this.__constraints__[i].nametypescopes[j].nameType);
						       }
						       currentConstraintTypes = currentConstraintTypes.uniq();
						       
						       // --- collects all names to the current constraint
						       var currentNames = new Array();
						       for(var j = 0; j !== allNames.length; ++j){
							   var type = allNames[j].getContent().type;
							   if(type && currentConstraintTypes.indexOf(type[0]) !== -1) currentNames.push(allNames[j]);
							   
						       }
						       // --- removes all current found names from "allNames"
						       for(var j = 0; j !== currentNames.length; ++j) allNames = allNames.without(currentNames[j]);
						       // --- removes empty names (for constraints that have a subset of regexp)
						    
						       // --- checks the regExp, card-min and card-max for the found types
						       var satisfiedNames = new Array();
						       for(var j = 0; j !== this.__constraints__[i].constraints.length; ++j){
							   var regexp = new RegExp(this.__constraints__[i].constraints[j].regexp);
							   var cardMin = parseInt(this.__constraints__[i].constraints[j].cardMin);
							   var cardMax = this.__constraints__[i].constraints[j].cardMax === MAX_INT ? MMAX_INT : parseInt(this.__constraints__[i].constraints[j].cardMax);
							   var matchedNames = 0;
							   for(var k = 0; k !== currentNames.length; ++k){
							       if(regexp.match(currentNames[k].getContent().value) === true){
								   ++matchedNames;
								   satisfiedNames.push(currentNames[k]);
							       }
							   }
							   if(matchedNames < cardMin){
							       ret = false;
							       if(errorStr.length !== 0) errorStr += "<br/><br/>";
							       errorStr += "card-min of the constraint regexp: \"" + this.__constraints__[i].constraints[j].regexp + "\" card-min: " + cardMin + " card-max: " + cardMax + " for the nametype \"" + currentConstraintTypes + " is not satisfied (" + matchedNames + ")!";
							   }
							   if(cardMax !== MMAX_INT && matchedNames > cardMax){
							       ret = false;
							       if(errorStr.length !== 0) errorStr += "<br/><br/>";
							       errorStr += "card-max of the constraint regexp: \"" + this.__constraints__[i].constraints[j].regexp + "\" card-min: " + cardMin + " card-max: " + cardMax + " for the nametype \"" + currentConstraintTypes + " is not satisfied (" + matchedNames + ")!";
							   }
						       }
						       
						       // --- checks if there are names which wasn't checked --> bad value
						       satisfiedNames = satisfiedNames.uniq();
						       for(var j = 0; j !== satisfiedNames.length; ++j)currentNames = currentNames.without(satisfiedNames[j]);
						       if(currentNames.length !== 0){
							   ret = false;
							   for(var j = 0; j !== currentNames.length; ++j)
							       currentNames[j].showError("This name does not satisfie any constraint!");
						       }    
						   }
						       
						   // --- all names are valid -> hide the error-div-element
						   if(ret === true) this.hideError();
						   else this.showError(errorStr);
						   return ret;
					       }});


// --- represenation of an occurrence element
var OccurrenceC = Class.create(ContainerC, {"initialize" : function($super, contents, occurrenceTypes, constraint, uniqueConstraints, owner, min, max, cssTitle, dblClickHandler){
                                                $super();
                                                if(!owner.__frames__) owner.__frames__ = new Array();
                                                owner.__frames__.push(this);
                                                this.__frame__.writeAttribute({"class" : CLASSES.occurrenceFrame()});
                                                this.__table__ = new Element("table", {"class" : CLASSES.occurrenceFrame()});
                                                this.__frame__.insert({"top" : this.__table__});
                                                this.__max__ = max;
                                                this.__constraint__ = constraint;
                                                this.__owner__ = owner;
                                                this.__dblClickHandler__ = dblClickHandler;
                                                this.__isMinimized__ = false;

                                                try{
						    var itemIdentityContent = null;
						    var typeContent = null;
						    var scopesContent = null;
						    if(contents){
							itemIdentityContent = contents.itemIdentities;
							typeContent = contents.type;
							scopesContent = contents.scopes;
						    }
						    
						    // --- control row + itemIdentity
						    makeControlRow(this, 5, itemIdentityContent);
						    checkRemoveAddButtons(owner, 1, max, this);
						    setRemoveAddHandler(this, this.__constraint__, owner, 1, max, function(){
							return new OccurrenceC(null, occurrenceTypes, constraint, uniqueConstraints, owner, min, max, cssTitle, dblClickHandler);
						    });

						    // --- type
						    var types = makeTypes(this, typeContent, occurrenceTypes);
						    
						    // --- scopes
						    var scopes = null;
						    if(contents){
							if(typeContent){
							    for(var i = 0; occurrenceTypes && i !== occurrenceTypes.length; ++i){
								if(scopes) break;
								for(var j = 0; j !== occurrenceTypes[i].occurrenceType.length; ++j){
								    if(typeContent.indexOf(occurrenceTypes[i].occurrenceType[j]) !== -1){
									scopes = occurrenceTypes[i].scopeConstraints;
									break;
								    }
								}
							    }
							}
						    }
						    else if(occurrenceTypes && occurrenceTypes[0].scopeConstraints){
							scopes = occurrenceTypes[0].scopeConstraints;
						    }
						    this.__scope__ = new ScopeContainerC(scopesContent, scopes);
						    this.__table__.insert({"bottom" : newRow(CLASSES.scopeContainer(), "Scope", this.__scope__.getFrame())});
						    onTypeChangeScope(this, contents && contents.scopes ? contents.scopes : null, occurrenceTypes, "occurrence");

						    // --- resource value and datatype
						    var noConstraint = false;
						    if(!constraint){
							constraint = {"regexp" : ".*", "cardMin" : 0, "cardMax" : MAX_INT};
							noConstraint = true;
						    }
						    var _min = parseInt(constraint.cardMin);
						    var _max = constraint.cardMax !== MAX_INT ? parseInt(constraint.cardMax) : MMAX_INT;
						    var cssTitle = "No constraint found for this occurrence";
						    if(noConstraint === false) cssTitle = "min: " + _min + "   max: " + _max + "   regular expression: " + constraint.regexp;
						    this.__cssTitle__ = cssTitle;
						    makeResource(this, contents, constraint, (occurrenceTypes ? occurrenceTypes[0].datatypeConstraint : null), cssTitle, {"rows" : 5, "cols" : 70});

						    this.getFrame().observe("dblclick", function(event){
							dblClickHandler(owner, event);
						    });
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
					    "isEmpty" : function(){
						return this.__value__.value.length === 0;
					    },
					    "isValid" : function(){
						var regexp = new RegExp(this.__constraint__.regexp);
						// TODO: validate the data via the given datatype
						// TODO: validate the uniqeuoccurrence-constraint
						var scopeValid = this.scopeIsValid();
						return regexp.match(this.__value__.value) && scopeValid;
					    },
					    "scopeIsValid" : function(){
						return this.__scope__.isValid();
					    },
					    "minimize" : function(){
						if(this.__isMinimized__ === false){
						    this.getFrame().select("tr." + CLASSES.showHiddenRows())[0].show();
						    this.getFrame().select("tr." + CLASSES.itemIdentityFrame())[0].hide();
						    this.getFrame().select("tr." + CLASSES.typeFrame())[0].hide();
						    this.getFrame().select("tr." + CLASSES.scopeContainer())[0].hide();
						    this.getFrame().select("tr." + CLASSES.valueFrame())[0].hide();
						    this.getFrame().select("tr." + CLASSES.datatypeFrame())[0].hide();
						    this.__isMinimized__ = true;
						}
						else {
						    this.getFrame().select("tr." + CLASSES.showHiddenRows())[0].hide();
						    this.getFrame().select("tr." + CLASSES.itemIdentityFrame())[0].show();
						    this.getFrame().select("tr." + CLASSES.typeFrame())[0].show();
						    this.getFrame().select("tr." + CLASSES.scopeContainer())[0].show();
						    this.getFrame().select("tr." + CLASSES.valueFrame())[0].show();
						    this.getFrame().select("tr." + CLASSES.datatypeFrame())[0].show();
						    this.__isMinimized__ = false;
						}
					    },
					    "disable" : function(){
						this.hideError();
						this.__itemIdentity__.disable();
						this.__type__.__frames__[0].disable();
						this.__scope__.disable();
						this.__value__.writeAttribute({"readonly" : "readonly"});
						this.__datatype__.__frames__[0].disable();
						this.getFrame().writeAttribute({"class" : CLASSES.disabled()});
						this.getFrame().writeAttribute({"title" : this.__cssTitle__});
						this.hideAddButton();
						this.__disabled__ = true;
					    },
					    "enable" : function(){
						this.__itemIdentity__.enable();
						this.__type__.__frames__[0].enable();
						this.__scope__.enable();
						this.__value__.removeAttribute("readonly");
						if(this.__datatypeIsSet__ === false) this.__datatype__.__frames__[0].enable();
						this.getFrame().writeAttribute({"class" : CLASSES.occurrenceFrame()});
						this.getFrame().removeAttribute("style");
						this.getFrame().removeAttribute("title");
						checkRemoveAddButtons(this.__owner__, 1, this.__max__, this);
						this.__disabled__ = false;
					    }});
			       

// --- contains all occurrences of an topic element
var OccurrenceContainerC = Class.create(ContainerC, {"initialize" : function($super, contents, constraints){
						         $super();
						         this.__containers__ = new Array();
                                                         this.__frame__.writeAttribute({"class" : CLASSES.occurrenceContainer()});
                                                         this.__constraints__ = constraints;

						         try{
							     if(constraints && constraints.length > 0){
								 var cContents = new Array();
								 if(contents) cContents = contents.clone();
								 
								 for(var i = 0; i != constraints.length; ++i){
								     var simpleConstraints = constraints[i].constraints;
								     
								     var allTypes = new Array();
								     for(var k = 0; k !== constraints[i].occurrenceTypes.length; ++k){
									 allTypes = allTypes.concat(constraints[i].occurrenceTypes[k].occurrenceType);
								     }
								     allTypes = allTypes.flatten().uniq();
								     
								     var ret = makeConstraintsAndContents(cContents, simpleConstraints, allTypes);
								     var constraintsAndContents = ret.constraintsAndContents;
								     cContents = ret.contents;

								     var _c_ = "";
								     for(var j = 0; j !== constraintsAndContents.length; ++j){
									 for(var k = 0; k !== constraintsAndContents[j].contents.length; ++k){
									     var val = constraintsAndContents[j].contents[k].resourceRef;
									     if(!val){
										 if(constraintsAndContents[j].contents[k].resourceData)
										     val = constraintsAndContents[j].contents[k].resourceData.value;
									     }
									     _c_ +=  val + "\n";
									 }
								     }

								     this.__containers__.push(new Array());
								     for(var j = 0; j != constraints[i].constraints.length; ++j){
									 this.__containers__[i].push(new Object());
									 var min = parseInt(constraints[i].constraints[j].cardMin);
									 var max = constraints[i].constraints[j].cardMax !== MAX_INT ? parseInt(constraints[i].constraints[j].cardMax) : MMAX_INT;
									 var _contents = null;
									 for(var k = 0; k !== constraintsAndContents.length; ++k){
									     if(constraintsAndContents[k].constraint === constraints[i].constraints[j]){
										 _contents = constraintsAndContents[k].contents;
										 break;
									     }
									 }
									 var endIdx = (min === 0 ? 1 : min);
									 endIdx = _contents && _contents.length > endIdx ? _contents.length : endIdx;
									 var regexp = constraints[i].constraints[j].regexp;
									 if(max !== 0 || (_contents && contents.length)){
									     var dblClickHandler = null;
									     if(min === 0) dblClickHandler = dblClickHandlerF;
									     var title = "min: " + min + "   max: " + max + "   regular expression: " + regexp;
									     for(var k = 0; k !== endIdx; ++k){
										 var _content = null;
										 if(_contents && _contents.length > k) _content = _contents[k];
										 var occurrence = new OccurrenceC(_content, constraints[i].occurrenceTypes, constraints[i].constraints[j], constraints[i].uniqueConstraints, this.__containers__[i][j], min === 0 ? 1 : min, max === MMAX_INT ? -1 : max, title, dblClickHandler);
										 if(min === 0 && !_content){
										     occurrence.disable();
										     occurrence.minimize();
										 }
										 this.__error__.insert({"before" : occurrence.getFrame()});
									     }
									 }
								     }
								 }
								 // --- inserts not used contents
								 if(cContents.length !== 0){
								     this.__containers__.push(new Array(new Object()));
								     var owner = this.__containers__[0][0];
								     var cssTitle = "No constraint found for this occurrence";
								     for(var i = 0; i !== cContents.length; ++i){
									 var occurrence = new OccurrenceC(cContents[i], null, null, null, owner, 0, 1, cssTitle, null);
									 this.__error__.insert({"before" : occurrence.getFrame()});
								     }
								 }
                                               		     }
							     else if(contents && contents.length !== 0){
								 this.__containers__.push(new Array(new Object()));
								 var owner = this.__containers__[0][0];
								 var cssTitle = "No constraint found for this occurrence";
								 for(var i = 0; i !== contents.length; ++i){
								     var occurrence = new OccurrenceC(contents[i], null, null, null, owner, 0, 1, null);
								     this.__error__.insert({"before" : occurrence.getFrame()});
								 }
							     }
							 }
						         catch(err){
							     alert("From OccurrenceContainerC(): " + err);
						         }
						     },
						     "isValid" : function(){
							 var ret = true;
							 var errorStr = "";
							 
							 // --- checks if there are any constraints
							 if((!this.__constraints__ || this.__constraints__.length === 0) && this.__containers__.length !== 0){
							     for(var i = 0; i !== this.__containers__.length; ++i){
								 for(var j = 0; j !== this.__containers__[i].length; ++j){
								     for(var k = 0; k !== this.__containers__[i][j].__frames__.length; ++k){
									 this.__containers__[i][j].__frames__[k].hideError();
									 if(this.__containers__[i][j].__frames__[k].isUsed() === true){
									     var type = this.__containers__[i][j].__frames__[k].showError("No constraints found for this occurrence!");
									 }
								     }
								 }
							     }
							     return false;
							 }
							 else if(!this.__constraints__ || this.__constraints__.length === 0) return true;

							 // --- summarizes all occurrences
							 var allOccurrences = new Array();
							 for(var i = 0; i !== this.__containers__.length; ++i){
							     for(var j = 0; j !== this.__containers__[i].length; ++j){
								 for(var k = 0; k !== this.__containers__[i][j].__frames__.length; ++k){
								     if(this.__containers__[i][j].__frames__[k].isUsed() === true && this.__containers__[i][j].__frames__[k].isEmpty() === false){
									 allOccurrences.push(this.__containers__[i][j].__frames__[k]);
								     }
								     this.__containers__[i][j].__frames__[k].hideError();
								     if(this.__containers__[i][j].__frames__[k].scopeIsValid() === false) ret = false;
								 }
							     }
							 }
							 
							 // --- checks every constraint and the existing occurrences corresponding to the current constraint
							 for(var i = 0; i !== this.__constraints__.length; ++i){
							     var currentConstraintTypes = new Array();
							     for(var j = 0; j !== this.__constraints__[i].occurrenceTypes.length; ++j){
								 currentConstraintTypes = currentConstraintTypes.concat(this.__constraints__[i].occurrenceTypes[j].occurrenceType);
							     }
							     currentConstraintTypes = currentConstraintTypes.uniq();
							     
							     // --- collects all occurrences to the current constraint
							     var currentOccurrences = new Array();
							     for(var j = 0; j !== allOccurrences.length; ++j){
								 var type = allOccurrences[j].getContent().type;
								 if(type && currentConstraintTypes.indexOf(type[0]) !== -1) currentOccurrences.push(allOccurrences[j]);
							     }
							     // --- removes all current found occurrences from "allOccurrences"
							     for(var j = 0; j !== currentOccurrences.length; ++j) allOccurrences = allOccurrences.without(currentOccurrences[j]);
							     // --- checks the regExp, card-min and card-max for the found types
							     var satisfiedOccurrences = new Array();
							     for(var j = 0; j !== this.__constraints__[i].constraints.length; ++j){
								 var regexp = new RegExp(this.__constraints__[i].constraints[j].regexp);
								 var cardMin = parseInt(this.__constraints__[i].constraints[j].cardMin);
								 var cardMax = this.__constraints__[i].constraints[j].cardMax === MAX_INT ? MMAX_INT : parseInt(this.__constraints__[i].constraints[j].cardMax);
								 var matchedOccurrences = 0;
								 for(var k = 0; k !== currentOccurrences.length; ++k){
								     var value = currentOccurrences[k].getContent().resourceRef;
								     if(!value) value = currentOccurrences[k].getContent().resourceData.value;
								     if(regexp.match(value) === true){
									 ++matchedOccurrences;
									 satisfiedOccurrences.push(currentOccurrences[k]);
								     }
								 }
								 // TODO: check the unique-occurrence
								 // TODO: check the occurrence's datatype and its content
								 if(matchedOccurrences < cardMin){
								     ret = false;
								     if(errorStr.length !== 0) errorStr += "<br/><br/>";
								     errorStr += "card-min of the constraint regexp: \"" + this.__constraints__[i].constraints[j].regexp + "\" card-min: " + cardMin + " card-max: " + cardMax + " for the occurrencetype \"" + currentConstraintTypes + " is not satisfied (" + matchedOccurrences + ")!";
								 }
								 if(cardMax !== MMAX_INT && matchedOccurrences > cardMax){
								     ret = false;
								     if(errorStr.length !== 0) errorStr += "<br/><br/>";
								     errorStr += "card-max of the constraint regexp: \"" + this.__constraints__[i].constraints[j].regexp + "\" card-min: " + cardMin + " card-max: " + cardMax + " for the occurrencetype \"" + currentConstraintTypes + " is not satisfied (" + matchedOccurrences + ")!";
								 }
							     }
							     
							     // --- checks if there are any occurrences which wasn't checked --> bad value
							     satisfiedOccurrences = satisfiedOccurrences.uniq();
							     for(var j = 0; j !== satisfiedOccurrences.length; ++j)
								 currentOccurrences = currentOccurrences.without(satisfiedOccurrences[j]);
							     if(currentOccurrences.length !== 0){
								 ret = false;
								 for(var j = 0; j !== currentOccurrences.length; ++j)
								     currentOccurrences[j].showError("This occurrence does not satisfie any constraint!");
							     }
							 }
							 
							 if(ret === true) this.hideError();
							 else this.showError(errorStr);
							 return ret;
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
					       var topicidContent = null;
					       var itemIdentityContent = null;
					       var subjectLocatorContent = null;
					       var subjectIdentifierContent = null;
					       var namesContent = null;
					       var occurrencesContent = null;
					       if(content){
						   topicidContent = content.id
						   itemIdentityContent = content.itemIdentities
						   subjectLocatorContent = content.subjectLocators;
						   subjectIdentifierContent = content.subjectIdentifiers;
						   namesContent = content.names;
						   occurrencesContent = content.occurrences;
					       }
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
					       new TextrowC(topicidContent, ".*", this.__topicid__, 1, 1, null);
					       this.__table__.insert({"bottom" : newRow(CLASSES.topicIdFrame(), "Topic ID", this.__topicid__.__frames__[0].getFrame())});
					       
					       // --- itemIdentity
					       this.__itemIdentity__ = new ItemIdentityC(itemIdentityContent, this);
					       this.__table__.insert({"bottom" : newRow(CLASSES.itemIdentityFrame(), "ItemIdentity", this.__itemIdentity__.getFrame())});

					       // --- subjectLocator
					       var _constraints = (constraints ? constraints.subjectLocatorConstraints : null);
					       this.__subjectLocator__ = new IdentifierC(subjectLocatorContent, _constraints, CLASSES.subjectLocatorFrame());
					       this.__table__.insert({"bottom" : newRow(CLASSES.subjectLocatorFrame(), "SubjectLocator", this.__subjectLocator__.getFrame())});

					       // --- subjectIdentifier
					       _constraints = (constraints ? constraints.subjectIdentifierConstraints : null);
					       this.__subjectIdentifier__ = new IdentifierC(subjectIdentifierContent, _constraints, CLASSES.subjectIdentifierFrame());
					       this.__table__.insert({"bottom" : newRow(CLASSES.subjectIdentifierFrame(), "SubjectIdentifier", this.__subjectIdentifier__.getFrame())});

					       // --- names
					       _constraints = (constraints ? constraints.topicNameConstraints : null);
					       this.__name__ = new NameContainerC(namesContent, _constraints);
					       this.__table__.insert({"bottom" : newRow(CLASSES.nameContainer(), "Names", this.__name__.getFrame())});
					       
					       // --- occurrences
					       _constraints = (constraints ? constraints.topicOccurrenceConstraints : null);
					       this.__occurrence__ = new OccurrenceContainerC(occurrencesContent, _constraints);
					       this.__table__.insert({"bottom" : newRow(CLASSES.occurrenceContainer(), "Occurrences", this.__occurrence__.getFrame())});
					   }catch(err){
					       alert("From TopciC(): " + err);
					   }
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
				       },
				       "isValid" : function(){
					   var ret = true;
					   if(this.__topicid__.__frames__[0].getContent().strip().length === 0){
					       ret = false;
					       this.__topicid__.__frames__[0].showError("The topic must contain a topic ID!");
					   }
					   else {
					       this.__topicid__.__frames__[0].hideError();
					   }
					   if(this.__subjectIdentifier__.getContent().length === 0){
					       ret = false;
					       this.showError("The topic must contain at least one SubjectIdentifier!<br/>If it is not possible to insert one - please create a subjectidentifier-constraint for this topic (-type)!");
					   }
					   else if(ret === true){
					       this.hideError();
					   }

					   if(this.__subjectLocator__.isValid() === false) ret = false;
					   if(this.__subjectIdentifier__.isValid() === false) ret = false;
					   if(this.__name__.isValid() === false) ret = false;
					   if(this.__occurrence__.isValid() === false) ret = false;

					   return ret;
				       },
				       "getReferencedTopics" : function(){
					   var referencedTopics = new Array();
					   var names = this.getContent().names;
					   if(names){
					       for(var i = 0; i !== names.length; ++i){
						   // TODO: variant (-scope topicStubs)
						   var type = names[i].type;
						   if(type){
						       if(referencedTopics.indexOf(type[0]) === -1) referencedTopics.push(type[0]);
						   }
						   var scopes = names[i].scopes;
						   if(scopes){
						       for(var j = 0; j !== scopes.length; ++j){
							   if(referencedTopics.indexOf(scopes[j][0]) === -1) referencedTopics.push(scopes[j][0]);
						       }
						   }
					       }
					   }

					   var occurrences = this.getContent().occurrences;
					   if(occurrences){
					       for(var i = 0; i !== occurrences.length; ++i){
						   var type = occurrences[i].type;
						   if(type){
						       if(referencedTopics.indexOf(type[0]) === -1) referencedTopics.push(type[0]);
						   }
						   var scopes = occurrences[i].scopes;
						   if(scopes){
						       for(var j = 0; j !== scopes.length; ++j){
							   if(referencedTopics.indexOf(scopes[j][0]) === -1) referencedTopics.push(scopes[j][0]);
						       }
						   }
					       }
					   }

					   if(this.__instanceOfs__){
					       for(var i = 0; i !== this.__instanceOfs__.length; ++i){
						   if(referencedTopics.indexOf(this.__instanceOfs__[i][0]) === -1) referencedTopics.push(this.__instanceOfs__[i][0]);
					       }
					   }
					   return referencedTopics;
				       }});


// --- representation of a role element.
var RoleC = Class.create(ContainerC, {"initialize" : function($super, itemIdentities, roleTypes, rolePlayers, owner, typeMin, parent){
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
                                          this.__owner__ = owner;
                                          this.__typeMin__ = typeMin;
                                          this.__parentElem__ = parent;
                                          this.__constraint__ = true; // is needed for checkAddRemoveButtons
                                          this.__isMinimized__ = false;
    
				          try{
					      // --- control row + itemIdentity
					      makeControlRow(this, 3, itemIdentities); // make control row have to be changed to a separate control row for roles
					      checkRemoveAddButtons(owner, 1, -1, this);
					      setRemoveAddHandler(this, this.__constraint__, owner, 1, -1, function(){ /*do nothing*/ });
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

					      function setDblClickHandler(myself){
						  myself.getFrame().observe("dblclick", function(event){
						      if(myself.__typeMin__ === 0){
							  var roles = new Array();
							  for(var i = 0; i !== owner.__frames__.length; ++i){
							      if(roleTypes.flatten().indexOf(owner.__frames__[i].getType()) !== -1)
								  roles.push(owner.__frames__[i]);
							  }
							  
							  if(roles.length === 1 && roles[0].isUsed() === true){
							      roles[0].disable();
							  }
							  else if(roles.length === 1 && roles[0].isUsed() === false){
							      roles[0].enable();
							  }
							  if(parent.isUsed() === true)Event.stop(event);
						      }
						  });
					      }
					      setDblClickHandler(this);
					      
					  }
				          catch(err){
					      alert("From RoleC(): " + err);
					  }
				      },
				      "addItemIdentities" : function(additionalItemIdentities){
					  if(!additionalItemIdentities || additionalItemIdentities.length === 0) return;

					  var con = this.getContent();
					  if(!con) con = new Array();
					  else con = con.itemIdentities;

					  con = con.concat(additionalItemIdentities);
					  var td = this.__itemIdentity__.getFrame().parentNode;
					  this.__itemIdentity__.remove();
					  
					  this.__itemIdentity__ = new ItemIdentityC(con.uniq(), this);
					  td.update(this.__itemIdentity__.getFrame());
				      },
				      "selectPlayer" : function(playerPsi){
					  if(this.getPlayer() === playerPsi) return;
					  var opts = this.__player__.__frames__[0].getFrame().select("select")[0].select("option");
					  for(var i = 0; i !== opts.length; ++i){
					      if(opts[i].value !== playerPsi) opts[i].removeAttribute("selected");
					      else {
						  opts[i].writeAttribute({"selected" : "selected"});
						  this.__player__.__frames__[0].getFrame().select("select")[0].insert({"top" : opts[i]});
					      }
					  }
				      },
				      "selectType" : function(typePsi){
					  if(this.getType() === typePsi) return;
					  var opts = this.__type__.__frames__[0].getFrame().select("select")[0].select("option");
					  for(var i = 0; i !== opts.length; ++i){
					      if(opts[i].value !== typePsi) opts[i].removeAttribute("selected");
					      else {
						  opts[i].writeAttribute({"selected" : "selected"});
						  this.__type__.__frames__[0].getFrame().select("select")[0].insert({"top" : opts[i]});
					      }
					  }
				      },
				      "getAllPlayers" : function(){
					  if(!this.__rolePlayers__ || this.__rolePlayers__.length === 0) return new Array();
					  return this.__rolePlayers__.clone();
				      },
				      "getAllTypes" : function(){
					  if(!this.__roleTypes__ || this.__roleTypes__.length === 0) return new Array();
					  return this.__roleTypes__;
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
				      "isUsed" : function(){
					  return !this.__disabled__;
				      },
				      "disable" : function(){
					  this.hideError();
					  this.__itemIdentity__.disable()
					  this.__type__.__frames__[0].disable();
					  this.__player__.__frames__[0].disable();
					  this.getFrame().writeAttribute({"class" : CLASSES.disabled()});
					  this.__disabled__ = true;
				      },
				      "enable" : function(){
					  this.__itemIdentity__.enable()
					  this.__type__.__frames__[0].enable();
					  this.__player__.__frames__[0].enable();
					  this.getFrame().writeAttribute({"class" : CLASSES.roleFrame()});
					  this.__disabled__ = false;
				      },
				      "minimize" : function(){
					  if(this.__isMinimized__ === false) {
					      this.getFrame().select("tr." + CLASSES.showHiddenRows())[0].show();
					      this.getFrame().select("tr." + CLASSES.itemIdentityFrame())[0].hide();
					      this.getFrame().select("tr." + CLASSES.typeFrame())[0].hide();
					      this.getFrame().select("tr." + CLASSES.playerFrame())[0].hide();
					      this.__isMinimized__ = true;
					  }
					  else {
					      this.getFrame().select("tr." + CLASSES.showHiddenRows())[0].hide();
					      this.getFrame().select("tr." + CLASSES.itemIdentityFrame())[0].show();
					      this.getFrame().select("tr." + CLASSES.typeFrame())[0].show();
					      this.getFrame().select("tr." + CLASSES.playerFrame())[0].show();
					      this.__isMinimized__ = false;
					  }
				      }});


// --- contains all roles of an association
var RoleContainerC = Class.create(ContainerC, {"initialize" : function($super, contents, associationRoleConstraints, rolePlayerConstraints, otherRoleConstraints, parent){
                                                  $super();
                                                   this.__frame__.writeAttribute({"class" : CLASSES.roleContainer()});
                                                   this.__arContainer__ = new Object(); this.__arContainer__.__frames__ = new Array();
                                                   this.__orContainer__ = new Object(); this.__orContainer__.__frames__ = new Array();
                                                   this.__associationRoleConstraints__ = associationRoleConstraints;
                                                   this.__otherRoleConstraints__ = otherRoleConstraints;
                                                   this.__rolePlayerConstraints__ = rolePlayerConstraints;
                                                   this.__parentElem__ = parent;

                                                   try{
						       this.resetValues(associationRoleConstraints, rolePlayerConstraints, otherRoleConstraints, contents);
						       this.__createFromContent__(contents);
						   }
                                                   catch(err){
                                               	       alert("From RoleContainerC(): " + err);
                                                   }
                                               },
					       "__orderContentsToRoles__" : function(contents, roleContainer, usedContents, alreadyUsedRoles){
						   if(!roleContainer || roleContainer.length === 0){
						       return {"usedContents" : usedContents, "contents" : contents, "alreadyUsedRoles" : alreadyUsedRoles};
						   }

						   for(var i = 0; i !== contents.length; ++i){
						       var rType = contents[i].type;
						       var player = contents[i].topicRef;
						       var itemIdentities = contents[i].itemIdentities;
						       
						       // --- searches existing roles in the role-container
						       for(var j = 0; j !== roleContainer.length; ++j){
							   var role = roleContainer[j];
							   if(alreadyUsedRoles.indexOf(role) !== -1) continue;
							   
							   var typesOfRole = role.getAllTypes().flatten();
							   var playersOfRole = role.getAllPlayers().flatten();
							   var iter = 0;
							   for( ; iter !== rType.length; ++iter){
							       if(typesOfRole.indexOf(rType[iter]) !== -1) break;
							   }
							   if(iter === rType.length) continue;
							   for(iter = 0; iter !== player.length; ++iter){
							       if(playersOfRole.indexOf(player[iter]) !== -1) break;
							   }
							   if(iter === player.length) continue;
							   
							   alreadyUsedRoles.push(role);
							   usedContents.push(contents[i]);
							   
							   // --- inserts the deselected player of all other roles of this type
							   var oldPlayer = new Array(role.getPlayer());
							   var _tmp = role.getAllPlayers();
							   for(var i = 0; i !== _tmp.length; ++i){
							       if(_tmp[i].indexOf(oldPlayer[0]) !== -1){
								   oldPlayer = _tmp[i];
								   break;
							       }
							   }
							   
							   for(var k = 0; k !== roleContainer.length; ++k){
							       if(roleContainer[k] === role) continue;
							       roleContainer[k].addPlayer(oldPlayer);
							   }
							   
							   // --- removes the current player from all other roles with this type
							   for(var k = 0; k !== roleContainer.length; ++k){
							       if(roleContainer[k] === role) continue;
							       roleContainer[k].removePlayer(player);
							   }
							   
							   // --- selects the currentPlayer/type
							   role.selectPlayer(player[0]);

							   // --- selects the current roletype
							   role.selectType(rType[0]);

							   // --- creates itemIdentities for the current role
							   role.addItemIdentities(itemIdentities);
							   break;
						       }
						   }
						   // --- removes all used contents from contents
						   for(var i = 0; i !== usedContents.length; ++i) contents = contents.without(usedContents[i]);
						   
						   return {"usedContents" : usedContents, "contents" : contents, "alreadyUsedRoles" : alreadyUsedRoles};
					       },
					       "__createAdditionalRolesFromContents__" : function(contents,usedContents, alreadyUsedRoles, isARC){
						   var roleContainer = this.__orContainer__.__frames__; 
						   if(isARC === true) roleContainer = this.__arContainer__.__frames__;
						       
						   if(roleContainer && roleContainer.length !== 0){
						       var currentUsedContents = new Array();
						       for(var i = 0; i !== contents.length; ++i){
							   var rType = contents[i].type;
							   var player = contents[i].topicRef;
							   var itemIdentities = contents[i].itemIdentities;
							   
							   // --- gets all existing roles corresponding to the current content
							   var existingRoles = new Array();
							   for(var j = 0; j !== roleContainer.length; ++j){
							       var iTypes = roleContainer[j].getAllTypes().flatten();
							       var iPlayers = roleContainer[j].getAllPlayers().flatten();
							       var iter = 0;
							       for( ; iter !== rType.length; ++iter) if(iTypes.indexOf(rType[iter]) !== -1) break;
							       if(iter === rType.length) continue;
							       for(iter = 0; iter !== player.length; ++iter) if(iPlayers.indexOf(player[iter]) !== -1) break;
							       if(iter === player.length) continue;
							       
							       existingRoles.push(roleContainer[j]);
							   }
							   
							   // --- collects the selected players
							   if(existingRoles && existingRoles.length > 0){
							       var selectedPlayers = new Array();
							       for(var j = 0; j !== existingRoles.length; ++j){
								   var _tmp = existingRoles[j].getAllPlayers();
								   for(var k = 0; k !== _tmp.length; ++k){
								       if(_tmp[k].indexOf(existingRoles[j].getPlayer()) !== -1){
									   selectedPlayers.push(_tmp[k]);
									   break;
								       }
								   }
							       }
							       selectedPlayers = selectedPlayers.flatten();
							       var allPlayers = existingRoles[0].getAllPlayers();
							       var playersToRemove = new Array();
							       for(var j = 0; j !== allPlayers.length; ++j){
								   for(var k = 0; k !== selectedPlayers.length; ++k){
								       if(allPlayers[j].indexOf(selectedPlayers[k]) !== -1){
									   playersToRemove.push(allPlayers[j]);
									   break;
								       }
								   }
							       }
							       for(var j = 0; j !== playersToRemove.length; ++j) allPlayers = allPlayers.without(playersToRemove[j]);
							       var newTypes = existingRoles[0].getAllTypes();
							       var min = 0;
							       var arc = null;
							       var orc = null;
							       if(isARC === true){
								   for(var j = 0; this.__associationRoleConstraints__ && j !== this.__associationRoleConstraints__.length; ++j){
								       if(arc) break;
								       var arcTypes = this.__associationRoleConstraints__[j].roleType;
								       if(arcTypes) arcTypes = arcTypes.flatten();
								       var nTs = newTypes.flatten();
								       for(var k = 0; k !== nTs.length; ++k){
									   if(arcTypes.indexOf(nTs[k]) !== -1){
									       arc = this.__associationRoleConstraints__[j];
									       min = parseInt(arc.cardMin);
									       break;
									   }
								       }
								   }
							       }
							       else {
								   for(var j = 0; this.__otherRoleConstraints__ && j !== this.__otherRoleConstraints__.length; ++j){
								       if(orc) break;
								       var oPlayers = this.__otherRoleConstraints__[j].otherPlayers;
								       if(oPlayers) oPlayers = oPlayers.flatten();
								       var oTypes = this.__otherRoleConstraints__[j].otherRoleType;
								       if(oTypes) oTypes = oTypes.flatten();

								       for(var k = 0; k !== rType.length; ++k){
									   if(orc) break;
									   if(oTypes.indexOf(rType[k]) !== -1){
									       for(var l = 0; l !== player.length; ++l){
										   if(oPlayers.indexOf(player[l]) !== -1){
										       orc = this.__otherRoleConstraints__[j];
										       min = parseInt(orc.cardMin);
										       break;
										   }
									       }
									   }
								       }
								   }
							       }
							       var role = null;
							       if(isARC === true) role = new RoleC(null, newTypes, allPlayers, this.__arContainer__, min, this.__parentElem__);
							       else role = new RoleC(null, newTypes, allPlayers, this.__orContainer__, min, this.__parentElem__);
							       for(var j = 0; j !== roleContainer.length; ++j){
								   if(roleContainer[j] !== role) roleContainer[j].removePlayer(player);
							       }
							       role.selectPlayer(player[0]);
							       role.selectType(rType[0]);
							       
							       if(isARC === true){
								   var rpcs = getRolePlayerConstraintsForRole(newTypes, this.__rolePlayerConstraints__);
								   var allAvailablePlayers = extractPlayersOfConstraints(rpcs);
								   var allRolesToCheck = existingRoles;
								   allRolesToCheck.push(role);
								   this.__checkARCButtons__(allRolesToCheck, allAvailablePlayers, arc);
								   this.__setARCAddHandler__(role, allAvailablePlayers, arc);
								   this.__setARCRemoveHandler__(role, arc);
								   this.__setRoleChangePlayerHandler__(role, this.__arContainer__.__frames__, rpcs, null);
							       }
							       else {
								   var orpcs = new Array();
								   var ac = this.__arContainer__.__frames__;
								   for(var j = 0; ac && j !== ac.length; ++j){
								       var fType = new Array(ac[j].getType());
								       var fPlayer = new Array(ac[j].getPlayer());
								       orpcs = orpcs.concat(getOtherRoleConstraintsForRole(fType, fPlayer, this.__otherRoleConstraints__));
								   }
								   var _orpcs = new Array();
								   for(var j = 0; j !== orpcs.length; ++j){
								       var players = orpcs[j].otherPlayers;
								       if(players) players = players.flatten();
								       var types = orpcs[j].otherRoleType;
								       if(types) types = types.flatten();
								       if(!types || !players) continue;
								       for(var k = 0; k !== rType.length; ++k){
									   if(types.indexOf(rType[k]) !== -1){
									       for(var l = 0; l !== player.length; ++l){
										   if(players.indexOf(player[l]) !== -1) _orpcs.push(orpcs[j]);
									       }
									   }
								       }
								   }

								   orpcs = _orpcs.uniq();
								   this.__checkORCButtons__(role, orc);
								   this.__setRoleChangePlayerHandler__(role, this.__orContainer__.__frames__, null, orpcs);
								   this.__setORCAddHandler__(role, orc, orpcs);
								   this.__setORCRemoveHandler__(role, orc, orpcs);
							       }
							       // --- adds itemIdentities
							       role.addItemIdentities(itemIdentities);
							       
							       var lastRole = roleContainer[roleContainer.length -2];
							       lastRole.getFrame().insert({"after" : role.getFrame()});
							       currentUsedContents.push(contents[i]);
							   }
						       }

						       // --- removes all used contents from contents
						       if(!usedContents) usedContents = new Array();
						       usedContents = usedContents.concat(currentUsedContents).uniq();
						       for(var i = 0; i !== usedContents.length; ++i) contents = contents.without(usedContents[i]);
						   }
						   return {"usedContents" : usedContents, "contents" : contents, "alreadyUsedRoles" : alreadyUsedRoles};
					       },
					       "__createNewRolesFromContents__" : function(contents){
						   if(!contents || contents.length === 0) return;

						   for(var i = 0; i !== contents.length; ++i){
						       var rType = contents[i].type;
						       if(!rType) rType = new Array("");
						       rType = new Array(rType);
						       var rPlayer = contents[i].topicRef;
						       if(!rPlayer) rPlayer = new Array("");
						       rPlayer = new Array(rPlayer);
						       var itemIdentities = contents[i].itemIdentities;

						       // itemIdentities, roleTypes, rolePlayers, owner, typeMin, parent){
						       var role = new RoleC(itemIdentities, rType, rPlayer, this.__arContainer__, 0, this.__parentElem__);
						       if(this.__arContainer__.__frames__ && this.__arContainer__.__frames__.length > 1){
							   var insertPoint = this.__arContainer__.__frames__[this.__arContainer__.__frames__.length - 2];
							   insertPoint.getFrame().insert({"after" : role.getFrame()});
						       }
						       else {
							   this.__error__.insert({"before" : role.getFrame()})
						       }
						       role.hideAddButton();
						   }
					       },
					       "__createFromContent__" : function(contents){
						   if(!contents || contents.lenght === 0) return;
						   
						   var cContents = contents;
						   var usedContents = new Array();
						   var alreadyUsedRoles = new Array();
						       
						   // --- searches for associaitonrole-constraints and roleplayer-constraints
						   var ret = this.__orderContentsToRoles__(cContents, this.__arContainer__.__frames__, usedContents, alreadyUsedRoles);
						   cContents = ret.contents;
						   usedContents = ret.usedContents;
						   alreadyUsedRoles = ret.alreadyUsedRoles;

						   // --- searches for otherrole-constraints
						   ret = this.__orderContentsToRoles__(cContents, this.__orContainer__.__frames__, usedContents, alreadyUsedRoles);
						   cContents = ret.contents;
						   usedContents = ret.usedContents;
						   alreadyUsedRoles = ret.alreadyUsedRoles;
						   
						   // --- creates additional roles (associationrole-constraints)
						   ret = this.__createAdditionalRolesFromContents__(cContents, usedContents, alreadyUsedRoles, true);
						   cContents = ret.contents;
						   usedContents = ret.usedContents;
						   alreadyUsedRoles = ret.alreadyUsedRoles;

						   // --- creates additional roles (associationrole-constraints)
						   ret = this.__createAdditionalRolesFromContents__(cContents, usedContents, alreadyUsedRoles, false);
						   cContents = ret.contents;
						   usedContents = ret.usedContents;
						   alreadyUsedRoles = ret.alreadyUsedRoles;

						   this.__createNewRolesFromContents__(cContents);
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
						   for(var i = 0; this.__associationRoleConstraints__ && i !== this.__associationRoleConstraints__.length; ++i){
						       var arc = this.__associationRoleConstraints__[i];
						       var foundRpcs = getRolePlayerConstraintsForRole(arc.roleType, this.__rolePlayerConstraints__);
						       this.__makeRolesFromARC__(arc, foundRpcs);
						   }
						   // --- creates roles from otherrole-constraints
						   for(var i = 0; this.__arContainer__.__frames__ && i !== this.__arContainer__.__frames__.length; ++i){
						       this.__makeRolesFromORC__(this.__arContainer__.__frames__[i].getType(), this.__arContainer__.__frames__[i].getPlayer());
						   }
					       },
					       "__makeRolesFromARC__" : function(associationRoleConstraint, rolePlayerConstraints){
						   if(!associationRoleConstraint || !rolePlayerConstraints || rolePlayerConstraints.length === 0) return;
						   checkCardinalitiesARC_RPC(associationRoleConstraint, rolePlayerConstraints);
						   
						   // --- creates all roles with all needed players
						   var currentRoles = new Array();
						   var rolesCreated = 0;
						   var allAvailablePlayers = extractPlayersOfConstraints(rolePlayerConstraints);
						   var roleType = associationRoleConstraint.roleType;
						   var roleMin = associationRoleConstraint.cardMin === 0 ? 1 : parseInt(associationRoleConstraint.cardMin);
						   var roleMinOrg = parseInt(associationRoleConstraint.cardMin);
						   for(var i = 0; i !== rolePlayerConstraints.length; ++i){
						       var playerMin = rolePlayerConstraints[i].cardMin === 0 ? 1 : parseInt(rolePlayerConstraints[i].cardMin);
						       if(rolePlayerConstraints[i].players.length < playerMin) throw "From __makeRolesFromARC__(): not enough players(=" + rolePlayerConstraints[i].players.length + ") to reach card-min(=" + playerMin + ") of roletype\"" + roleType.flatten()[0] + "\"!";
						       for(var k = 0; k !== playerMin; ++k){
							   // --- creates a new role
							   var selectedPlayers = new Array();
							   for(var j = 0; j !== currentRoles.length; ++j) selectedPlayers.push(currentRoles[j].getPlayer());
							   var currentPlayers = cleanPlayers(rolePlayerConstraints[i].players, selectedPlayers)
							   var cleanedPlayers = cleanPlayers(allAvailablePlayers, selectedPlayers);
							   cleanedPlayers = cleanPlayers(cleanedPlayers, currentPlayers);
							   cleanedPlayers = currentPlayers.concat(cleanedPlayers);
							   var role = new RoleC(null, roleType, cleanedPlayers, this.__arContainer__, roleMinOrg, this.__parentElem__);
							   this.__setRoleChangePlayerHandler__(role, this.__arContainer__.__frames__, rolePlayerConstraints, null);
							   this.__error__.insert({"before" : role.getFrame()});
							   // --- removes the new role's selected item from all other existing roles
							   for(var j = 0; j !== currentRoles.length; ++j){
							       currentRoles[j].removePlayer(new Array(role.getPlayer()));
							   }
							   ++rolesCreated;
							   currentRoles.push(role);
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
							       
							       var role = new RoleC(null, roleType, cleanedPlayers, this.__arContainer__, roleMinOrg, this.__parentElem__);
							       currentRoles.push(role);
							       this.__setRoleChangePlayerHandler__(role, this.__arContainer__.__frames__, rolePlayerConstraints, null);
							       this.__error__.insert({"before" : role.getFrame()});
							       ++rolesCreated;
							       ++currentlyCreated;
							   }
						       }
						       if(currentlyCreated === 0) throw "Not enough players to create all needed roles of the type \"" + roleType.flatten()[0] + "\"!";
						   };
						   this.__checkARCButtons__(currentRoles, allAvailablePlayers, associationRoleConstraint);
						   for(var i = 0; i !== currentRoles.length; ++i){
						       this.__setARCAddHandler__(currentRoles[i], allAvailablePlayers, associationRoleConstraint);
						       this.__setARCRemoveHandler__(currentRoles[i], associationRoleConstraint);
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
						       var cMinOrg = parseInt(orpcs[i].cardMin);
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
							   
							   var role = new RoleC(null, cOtherRoleType, cleanedPlayers, this.__orContainer__, cMinOrg, this.__parentElem__);
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
						   var cardMax = constraint.cardMax === MAX_INT ? MMAX_INT : parseInt(constraint.cardMax);
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
						   if(cardMin >= existingRoles.length){
						       for(var i = 0; i !== existingRoles.length; ++i) existingRoles[i].hideRemoveButton();
						   }
						   else {
						       for(var i = 0; i !== existingRoles.length; ++i) existingRoles[i].showRemoveButton();
						   }
					       },
					       "__checkARCButtons__" : function(rolesToCheck, players, associationRoleConstraint){
						   if(!rolesToCheck || !associationRoleConstraint) return;
						   var cardMin = associationRoleConstraint.cardMin === 0 ? 1 : parseInt(associationRoleConstraint.cardMin);
						   var cardMax = associationRoleConstraint.cardMax === MAX_INT ? MMAX_INT : parseInt(associationRoleConstraint.cardMax);
						   var lenPlayers = players ? players.length : 0;
						   if(cardMin < rolesToCheck.length) {
						       for(var i = 0; i !== rolesToCheck.length; ++i) rolesToCheck[i].showRemoveButton();
						   }
						   else {
						       for(var i = 0; i !== rolesToCheck.length; ++i) rolesToCheck[i].hideRemoveButton();
						   }

						   if(cardMax === MMAX_INT || cardMax > rolesToCheck.length && rolesToCheck.length < lenPlayers){
						       for(var i = 0; i !== rolesToCheck.length; ++i) rolesToCheck[i].showAddButton();
						   }
						   else {
						       for(var i = 0; i !== rolesToCheck.length; ++i) rolesToCheck[i].hideAddButton();
						   }
					       },
					       "__setORCAddHandler__" : function(role, currentConstraint, constraints){
						   if(!role || !currentConstraint || !constraints || constraints.length === 0) return;

						   var roleContainer = this;
						   function addHandler(myself){
						       var cOtherPlayers = currentConstraint.otherPlayers;
						       var cOtherRoleType = currentConstraint.otherRoleType;
						       var cardMax = currentConstraint.cardMax === MAX_INT ? MMAX_INT : parseInt(currentConstraint.cardMax);
						       var cardMin = currentConstraint.cardMin === 0 ? 1 : parseInt(currentConstraint.cardMin);
						       var cardMinOrg = parseInt(currentConstraint.cardMin);;
						       var existingRoles = roleContainer.getExistingRoles(cOtherRoleType, cOtherPlayers, roleContainer.__orContainer__.__frames__);
						       var cleanedPlayers = new Array();
						       for(var i = 0; i !== cOtherPlayers.length; ++i){
							   if(roleContainer.getExistingRoles(cOtherRoleType, cOtherPlayers[i], roleContainer.__orContainer__.__frames__).length === 0){
							       cleanedPlayers.push(cOtherPlayers[i]);
							   }
						       }
						       
						       // --- creates new role
						       if(cleanedPlayers.length !== 0){
							   var role = new RoleC(null, cOtherRoleType, cleanedPlayers, roleContainer.__orContainer__, cardMinOrg, this.__parentElem__);
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
						       var cardMax = currentConstraint.cardMax === MAX_INT ? MMAX_INT : parseInt(currentConstraint.cardMax);
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
						   if(cardMax !== MMAX_INT && existingRoles.length >= cardMax){
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
					       "__setARCAddHandler__" : function(role, players, associationRoleConstraint){
						   if(!role || !associationRoleConstraint) return;
						   var lenPlayers = players ? players.length : 0;

						   var roleContainer = this;
						   function addHandler(myself){
						       var roleType = associationRoleConstraint.roleType.flatten();
						       var rolesToCheck = new Array();
						       for(var i = 0; i !== roleContainer.__arContainer__.__frames__.length; ++i){
							   if(roleType.indexOf(roleContainer.__arContainer__.__frames__[i].getType()) !== -1)
							       rolesToCheck.push(roleContainer.__arContainer__.__frames__[i]);
						       }
						       
						       // --- creates a new role
						       var cardMax = associationRoleConstraint.cardMax === MAX_INT ? MMAX_INT : parseInt(associationRoleConstraint.cardMax);
						       var cardMin = parseInt(associationRoleConstraint.cardMin);
               					       if(cardMax === MMAX_INT || cardMax > rolesToCheck.length){
							   var usedPlayers = new Array();
							   for(var i = 0; i !== rolesToCheck.length; ++i) usedPlayers.push(rolesToCheck[i].getPlayer());
							   var cleanedPlayers = cleanPlayers(players ? players : new Array(), usedPlayers);
							   var role = new RoleC(null, roleType, cleanedPlayers, roleContainer.__arContainer__, cardMin, this.__parentElem__);
							   var foundRpcs = getRolePlayerConstraintsForRole(roleType, roleContainer.__rolePlayerConstraints__);
							   roleContainer.__setRoleChangePlayerHandler__(role, roleContainer.__arContainer__.__frames__, foundRpcs, null);
							   roleContainer.__setARCAddHandler__(role, players, associationRoleConstraint);
							   roleContainer.__setARCRemoveHandler__(role, associationRoleConstraint);

							   // --- removes the new role's selected item from all other existing roles
							   for(var j = 0; j !== rolesToCheck.length; ++j){
							       rolesToCheck[j].removePlayer(new Array(role.getPlayer()));
							   }							   
							   roleContainer.__arContainer__.__frames__[roleContainer.__arContainer__.__frames__.length - 2].getFrame().insert({"after" : role.getFrame()});
							   rolesToCheck.push(role);
							   roleContainer.__checkORCRoles__(role);
						       }

						       roleContainer.__checkARCButtons__(rolesToCheck, players, associationRoleConstraint);
						   }

						   role.setAddHandler(addHandler);
					       },
					       "__setARCRemoveHandler__" : function(role, associationRoleConstraint){
						   if(!role || !associationRoleConstraint) return;

						   var roleContainer = this;
						   function removeHandler(myself){
						       var cardMin = associationRoleConstraint.cardMin === 0 ? 1 : parseInt(associationRoleConstraint.cardMin);
						       var roleType = associationRoleConstraint.roleType.flatten();
						       var rolesToCheck = new Array();
						       for(var i = 0; i !== roleContainer.__arContainer__.__frames__.length; ++i){
							   if(roleType.indexOf(roleContainer.__arContainer__.__frames__[i].getType()) !== -1)
							       rolesToCheck.push(roleContainer.__arContainer__.__frames__[i]);
						       }

						       // --- removes the role
						       if(cardMin < rolesToCheck.length){
							   // --- gets the player which is selected by the role has to be removed
							   var player = null;
							   for(var i = 0; roleContainer.__rolePlayerConstraints__ && i !== roleContainer.__rolePlayerConstraints__.length; ++i){
							       if(player !== null) break;
							       for(var j = 0; roleContainer.__rolePlayerConstraints__[i].players && j !== roleContainer.__rolePlayerConstraints__[i].players.length; ++j){
								   if(roleContainer.__rolePlayerConstraints__[i].players[j].indexOf(role.getPlayer()) !== -1){
								       player = roleContainer.__rolePlayerConstraints__[i].players[j];
								       break;
								   }
							       }
							   }
							   rolesToCheck = rolesToCheck.without(role);
							   role.remove();
							   roleContainer.__arContainer__.__frames__ = roleContainer.__arContainer__.__frames__.without(role);

							   // --- adds the player which was selected by the removed role to all other
							   // --- existing roles of the same type
							   for(var i = 0; i !== rolesToCheck.length; ++i){
							       rolesToCheck[i].addPlayer(player);
							   }
						       }
						       var foundRpcs = getRolePlayerConstraintsForRole(associationRoleConstraint.roleType, roleContainer.__rolePlayerConstraints__);
						       var players = extractPlayersOfConstraints(foundRpcs);
						       roleContainer.__checkARCButtons__(rolesToCheck, players, associationRoleConstraint);
						       roleContainer.__checkORCRoles__(null);
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

							   // --- chekcs roles created from otherrole-contraints
							   myself.__checkORCRoles__(role);
						       });
						   }
						   setEvent(this);
					       },
					       "__checkORCRoles__" : function(changedRole){
						   // --- removes all roles created from otherrole-constraints, which
						   // --- currently must not exist
						   var toRemove = new Array();
						   for(var i = 0; i !== this.__orContainer__.__frames__.length; ++i){
						       var oRole = this.__orContainer__.__frames__[i];
						       var orcs = new Array(); // found orcs for the existing orc-roles
						       var checkedOrcs = new Array();
						       for(var j = 0; this.__otherRoleConstraints__ && j !== this.__otherRoleConstraints__.length; ++j){
							   var orc = this.__otherRoleConstraints__[j];
							   if(orc.otherRoleType.flatten().indexOf(oRole.getType()) !== -1 && orc.otherPlayers.flatten().indexOf(oRole.getPlayer()) !== -1) orcs.push(orc);
						       }
						       
						       for(var j = 0; j !== orcs.length; ++j){
							   for(var k = 0; this.__arContainer__.__frames__ && k !== this.__arContainer__.__frames__.length; ++k){
							       var aRole = this.__arContainer__.__frames__[k];
							       if(orcs[j].roleType.flatten().indexOf(aRole.getType()) !== -1 && orcs[j].players.flatten().indexOf(aRole.getPlayer()) !== -1){
								   checkedOrcs.push(orcs[j]);
								   break;
							       }
							   }
						       }
						       
						       // --- no otherrole-constraints exist for this roles, so they have to be removed
						       if(checkedOrcs.length === 0) toRemove.push(oRole);
						   }
						   for(var i = 0; i !== toRemove.length; ++i){
						       this.__orContainer__.__frames__ = this.__orContainer__.__frames__.without(toRemove[i]);
						       toRemove[i].remove();
						   }
						   
						   // --- creates new roles from other role-constraints, which has to exist or are optional
						   if(changedRole) this.__makeRolesFromORC__(changedRole.getType(), changedRole.getPlayer());
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
					       "disable" : function(){
						   this.hideError();
						   if(this.__orContainer__.__frames__){
						       for(var i = 0; i !== this.__orContainer__.__frames__.length; ++i) this.__orContainer__.__frames__[i].disable();
						   }
						   if(this.__arContainer__.__frames__){
						       for(var i = 0; i !== this.__arContainer__.__frames__.length; ++i) this.__arContainer__.__frames__[i].disable();
						   }
						   this.__disabled__ = true;
					       },
					       "enable" : function(){
						   if(this.__orContainer__.__frames__){
						       for(var i = 0; i !== this.__orContainer__.__frames__.length; ++i) this.__orContainer__.__frames__[i].enable();
						   }
						   if(this.__arContainer__.__frames__){
						       for(var i = 0; i !== this.__arContainer__.__frames__.length; ++i) this.__arContainer__.__frames__[i].enable();
						   }
						   this.__disable__ = false;
					       },
					       "isValid" : function(){
						   var ret = true;
						   var errorStr = "";
						   
						   var arcs = this.__associationRoleConstraints__;
                                                   var orcs = this.__otherRoleConstraints__;
                                                   var rpcs = this.__rolePlayerConstraints__;
						   
						   // --- checks if there exist any constraints
						   if(!arcs || arcs.length === 0){
						       this.showError("No association-constraints found for this association!");
						       return false;
						   }
						   
						   if(!rpcs || rpcs.length === 0){
						       this.showError("No roleplayer-constraints found for this association!");
						       return false;
						   }
						   
						   // --- collects all used roles depending on associationrole-constraints
						   var allAroles = new Array();
						   var allAroles2 = new Array();
						   for(var i = 0; this.__arContainer__.__frames__ && i !== this.__arContainer__.__frames__.length; ++i){
						       this.__arContainer__.__frames__[i].hideError();
						       if(this.__arContainer__.__frames__[i].isUsed() === true){
							   allAroles.push(this.__arContainer__.__frames__[i]);
							   allAroles2.push(this.__arContainer__.__frames__[i]);
						       }
						   }
						   
						   // --- collects all used roles depending on otherrole-constraints
						   var allOroles = new Array();
						   for(var i = 0; i !== this.__orContainer__.__frames__.length; ++i){
						       this.__orContainer__.__frames__[i].hideError();
						       if(this.__orContainer__.__frames__[i].isUsed() === true)
							   allOroles.push(this.__orContainer__.__frames__[i]);
						   }
						   
						   // --- checks all associationrole-constraints
						   var checkedRoles = new Array();
						   for(var i = 0; i !== arcs.length; ++i){
						       var currentRoles = new Array();
						       var rType = arcs[i].roleType.flatten();
						       var cardMin = parseInt(arcs[i].cardMin);
						       var cardMax = arcs[i].cardMax === MAX_INT ? MMAX_INT : parseInt(arcs[i].cardMax);
						       
						       // --- collects all roles for the current constraint
						       for(var j = 0; j !== allAroles.length; ++j){
							   if(rType.indexOf(allAroles[j].getType()) !== -1) currentRoles.push(allAroles[j]);
						       }
						       allAroles = allAroles.uniq();
						       
						       if(cardMin > currentRoles.length){
							   ret = false;
							   if(errorStr.length !== 0) errorStr += "<br/><br/>";
							   errorStr += "card-min of the associationrole-constraint card-min: " + cardMin + " card-max: " + cardMax + " for the roletype \"" + rType + " is not satisfied (" + currentRoles.length + ")!";
						       }
						       if(cardMax !== MMAX_INT && cardMax < currentRoles.length){
							   ret = false;
							   if(errorStr.length !== 0) errorStr += "<br/><br/>";
							   errorStr += "card-max of the associationrole-constraint card-min: " + cardMin + " card-max: " + cardMax + " for the roletype \"" + rType + " is not satisfied (" + currentRoles.length + ")!";
						       }
						       
						       // --- checks roleplayer-constraints for the found roles
						       var currentRpcs = getRolePlayerConstraintsForRole(rType, rpcs);
						       if(currentRpcs.length === 0){
							   ret = false;
							   for(var j = 0; j !== currentRoles.length; ++j) currentRoles[j].showError("This role does not satisfie any roleplayer-constraint!");
						       }
						       for(var j = 0; j !== currentRpcs.length; ++j){
							   var players = currentRpcs[i].players;
							   var pType = currentRpcs[i].playerType.flatten();
							   cardMin = parseInt(currentRpcs[i].cardMin);
							   cardMax = currentRpcs[i].cardMax === MAX_INT ? MMAX_INT : parseInt(currentRpcs[i].cardMax);
							   var foundRoles = this.getExistingRoles(rType, players, currentRoles);
							   if(cardMin > foundRoles.length){
							       ret = false;
							       if(errorStr.length !== 0) errorStr += "<br/><br/>";
							       errorStr += "card-min of the roleplayer-constraint card-min: " + cardMin + " card-max: " + cardMax + " for the roletype \"" + rType + " and the playertype \"" + pType + "\" is not satisfied (" + foundRoles.length + ")!";
							   }
							   if(cardMax !== MMAX_INT && cardMax < foundRoles.length){
							       ret = false;
							       if(errorStr.length !== 0) errorStr += "<br/><br/>";
							       errorStr += "card-max of the roleplayer-constraint card-min: " + cardMin + " card-max: " + cardMax + " for the roletype \"" + rType + " and the playertype \"" + pType + "\" is not satisfied (" + foundRoles.length + ")!";
							   }
							   // --- marks all found roles from "allAroles"
							   for(var k = 0; k !== foundRoles.length; ++k) checkedRoles.push(foundRoles[k]);
						       }
						   }
						   
						   // --- checks roles that does not belong to any constraint
						   for(var i = 0; i !== checkedRoles.length; ++i) allAroles = allAroles.without(checkedRoles[i]);
						   
						   if(allAroles.length !== 0){
						       for(var i = 0; i !== allAroles.length; ++i) allAroles[i].showError("This role does not satisfie any associationrole- or roleplayer-constraints!");
						   }
						   
						   // --- checks otherrole-constraints
						   // --- collects all neede otherrole-constraints
						   var usedOrcs = new Array();
						   allAroles = allAroles2;
						   for(var i = 0; i !== allAroles.length; ++i){
						       usedOrcs = usedOrcs.concat(getOtherRoleConstraintsForRole(new Array(allAroles[i].getType()), new Array(allAroles[i].getPlayer()), orcs));
						   }
						   
						   checkedRole = new Array();
						   for(var i = 0; i !== usedOrcs.length; ++i){
						       var players = usedOrcs[i].otherPlayers;
						       var pType = usedOrcs[i].otherPlayerType;
						       var rType = usedOrcs[i].otherRoleType;
						       var cardMin = parseInt(usedOrcs[i].cardMin);
						       var cardMax = usedOrcs[i].cardMax === MAX_INT ? MMAX_INT : parseInt(usedOrcs[i].cardMax);
						       var foundRoles = this.getExistingRoles(rType, players, allOroles);
						       checkedRoles = checkedRoles.concat(foundRoles);
						       if(cardMin > foundRoles.length){
							   ret = false;
							   if(errorStr.length !== 0) errorStr += "<br/><br/>";
							   errorStr += "card-min of the otherrole-constraint card-min: " + cardMin + " card-max: " + cardMax + " for the roletype \"" + rType + " and the playertype \"" + pType + "\" is not satisfied (" + foundRoles.length + ")!";
						       }
						       if(cardMax !== MMAX_INT && cardMax < foundRoles.length){
							   ret = false;
							   if(errorStr.length !== 0) errorStr += "<br/><br/>";
							   errorStr += "card-max of the otherrole-constraint card-min: " + cardMin + " card-max: " + cardMax + " for the roletype \"" + rType + " and the playertype \"" + pType + "\" is not satisfied (" + foundRoles.length + ")!";
						       }
						   }
						   
						   if(ret === false) this.showError(errorStr);
						   else this.hideError();
						   return ret;
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
                                                 this.__constraints__ = constraints;
                                                 this.__owner__ = owner;
                                                 this.__dblClickHandler__ = dblClickHandlerF;
                                                 this.__isMinimized__ = false;
    
					         try{
						     var itemIdentityContent = null;
						     var typeContent = null;
						     var scopesContent = null;
						     var rolesContent = null;
						     if(contents){
							 itemIdentityContent = contents.itemIdentities;
							 typeContent = contents.type;
							 scopesContent = contents.scopes;
							 rolesContent = contents.roles;
						     }
						     
						     // --- control row + ItemIdentity
						     makeControlRow(this, 4, itemIdentityContent);
						     checkRemoveAddButtons(owner, 1, -1, this);
						     setRemoveAddHandler(this, this.__constraints__, owner, 1, -1, function(){
							 return new AssociationC(null, constraints, owner);
						     });

						     // --- type
						     var types = makeTypes(this, typeContent, constraints);
						     if(types.flatten().length === 0 || types.flatten()[0].strip().length === 0 || !constraints || constraints.length === 0) this.hideAddButton();
						     
						     // --- scopes
						     var currentConstraint = this.getCurrentConstraint();
						     this.__scope__ = new ScopeContainerC(scopesContent, currentConstraint && currentConstraint.scopeConstraints ? currentConstraint.scopeConstraints : null);
						     this.__table__.insert({"bottom" : newRow(CLASSES.scopeContainer(), "Scope", this.__scope__.getFrame())});

						     // --- roles
						     var _roleConstraints = null;
						     var _playerConstraints = null;
						     var _otherRoleConstraints = null;
						     var cc = this.getCurrentConstraint();
						     if(cc){
							 _roleConstraints =  cc.associationRoleConstraints;
							 _playerConstraints = cc.rolePlayerConstraints;
							 _otherRoleConstraints = cc.otherRoleConstraints;
						     }

						     this.__roles__ = new RoleContainerC(rolesContent, _roleConstraints, _playerConstraints, _otherRoleConstraints, this);
						     this.__table__.insert({"bottom" : newRow(CLASSES.roleContainer(), "Roles", this.__roles__.getFrame())});
						     
						     // --- registers the onChangeHandler of the Type-selectrow
						     onTypeChangeScope(this, null, null, "association");

						     function setDblClickHandler(myself){
							 myself.getFrame().observe("dblclick", function(event){
							     myself.__dblClickHandler__(owner, event);
							 });
						     }
						     setDblClickHandler(this);
						 }
					         catch(err){
						     alert("From AssociationC(): " + err);
						 }
					     },
					     "resetValues" : function(){
						 var cc = this.getCurrentConstraint();
						 this.__scope__.resetValues(null, (cc ? cc.scopeConstraints : null));

						 var _roleConstraints = null;
						 var _playerConstraints = null;
						 var _otherRoleConstraints = null;
						 if(cc){
						     _roleConstraints = cc.associationRoleConstraints;
						     _playerConstraints = cc.rolePlayerConstraints;
						     _otherRoleConstraints = cc.otherRoleConstraints;
						 }
						 this.__roles__.resetValues(_roleConstraints, _playerConstraints, _otherRoleConstraints);
					     },
					     "getContent" : function(){
						 if(!this.isUsed()) return null;
						 var type = this.__type__.__frames__[0].getContent();
						 return {"itemIdentities" : this.__itemIdentity__.getContent(true, true),
							 "type" : type ? new Array(type) : null,
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
					     "getCurrentConstraint" : function(){
						 if(!this.__constraints__ || this.__constraints__.length === 0) return null;
						 var currentConstraint = null;
						 for(var i = 0; i !== this.__constraints__.length; ++i){
						     var aType = this.__constraints__[i].associationType;
						     aType = aType.flatten();
						     if(aType.indexOf(this.__type__.__frames__[0].getContent()) !== -1){
							 currentConstraint = this.__constraints__[i];
							 break;
						     }
						 }

						 return currentConstraint;
					     },
					     "isValid" : function(){
						 if(!this.getCurrentConstraint()){
						     this.showError("No constraints found for this association!");
						     return false;
						 }
						 else {
						     this.hideError();
						 }

						 return this.__roles__.isValid() && this.__scope__.isValid();
					     },
					     "disable" : function(){
						 this.hideError();
						 this.__itemIdentity__.disable();
						 this.__roles__.disable();
						 this.__type__.__frames__[0].disable();
						 this.__scope__.disable();
						 this.hideRemoveButton();
						 this.hideAddButton();
						 this.getFrame().writeAttribute({"class" : CLASSES.disabled()});
						 this.__disabled__ = true;
					     },
					     "enable" : function(){
						 this.__itemIdentity__.enable();
						 this.__roles__.enable();
						 this.__type__.__frames__[0].enable();
						 this.__scope__.enable();
						 if(this.__owner__.__frames__.length > 1 || !this.__constraints__ || this.__constraints__.length !== 0) this.showRemoveButton();
						 if(this.__constraints__ && this.__constraints__.length !== 0) this.showAddButton();
						 this.getFrame().writeAttribute({"class" : CLASSES.associationFrame()});
						 this.__disabled__ = false;
					     },
					     "minimize" : function(){
						 if(this.__isMinimized__ === false) {
						     this.getFrame().select("tr." + CLASSES.showHiddenRows())[0].show();
						     this.getFrame().select("tr." + CLASSES.itemIdentityFrame())[0].hide();
						     this.getFrame().select("tr." + CLASSES.typeFrame())[0].hide();
						     this.getFrame().select("tr." + CLASSES.scopeContainer())[0].hide();
						     this.getFrame().select("tr." + CLASSES.roleContainer())[0].hide();
						     this.__isMinimized__ = true;
						 }
						 else {
						     this.getFrame().select("tr." + CLASSES.showHiddenRows())[0].hide();
						     this.getFrame().select("tr." + CLASSES.itemIdentityFrame())[0].show();
						     this.getFrame().select("tr." + CLASSES.typeFrame())[0].show();
						     this.getFrame().select("tr." + CLASSES.scopeContainer())[0].show();
						     this.getFrame().select("tr." + CLASSES.roleContainer())[0].show();
						     this.__isMinimized__ = false;
						 }
					     }});


// --- contains all fragment's associations depending on the main topic
var AssociationContainerC = Class.create(ContainerC, {"initialize" : function($super, contents, constraints){
						          $super();
						          this.__minimized__ = false;
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
							      if(!constraints || constraints.length === 0){
								  for(var i = 0; i !== this.__container__.__frames__.length; ++i){
								      this.__container__.__frames__[i].hideAddButton();
								  }
							      }

							      if(!this.__container__.__frames__ && constraints && constraints.length !== 0){
								  var association = new AssociationC(null, constraints, this.__container__);
								  var tr = new Element("tr", {"class" : CLASSES.associationFrame()});
								  var td = new Element("td", {"class" : CLASSES.content()});
								  td.update(association.getFrame());
								  tr.update(td);
								  this.__table__.insert({"bottom" : tr});
								  association.disable();
							      }
							      function setMinimizeHandler(myself){
								  myself.__caption__.observe("click", function(event){
								      myself.minimize();
								  });
							      }
							      setMinimizeHandler(this);
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
							  var ret = true;
							  for(var i = 0; i !== this.__container__.__frames__.length; ++i){
							      if(this.__container__.__frames__[i].isUsed() === true && this.__container__.__frames__[i].isValid() === false)
								  ret = false;
							  }

							  return ret;
						      },
						      "minimize" : function(){
							  var rows = this.__table__.select("tr." + CLASSES.associationFrame());
							  for(var i = 0; i != rows.length; ++i){
							      if(this.__minimized__ === false) rows[i].hide();
							      else rows[i].show();
							  }
							  this.__minimized__ = !this.__minimized__;
						      },
						      "getReferencedTopics" : function(){
							  var referencedTopics = new Array();
							  var associations = this.getContent();
							  if(associations){
							      for(var i = 0; i !== associations.length; ++i){
								  var assType = associations[i].type;
								  if(referencedTopics.indexOf(assType[0]) === -1) referencedTopics.push(assType[0]);
								  var scopes = associations[i].scopes;
								  if(scopes){
								      for(var j = 0; j !== scopes.length; ++j){
									  if(referencedTopics.indexOf(scopes[j][0]) === -1) referencedTopics.push(scopes[j][0]);
								      }
								  }
								  var roles = associations[i].roles;
								  if(roles){
								      for(var j = 0; j !== roles.length; ++j){
									  var roleType = roles[j].type;
									  if(roleType && referencedTopics.indexOf(roleType[0]) === -1) referencedTopics.push(roleType[0]);
									  var player = roles[j].topicRef;
									  if(player && referencedTopics.indexOf(player[0]) === -1) referencedTopics.push(player[0]);
								      }
								  }
							      }
							  }
							  return referencedTopics;
						      }});


// --- Representation of a topic map if frame.
var TmIdC = Class.create(ContainerC, {"initialize" : function($super, contents){
                                          $super();
                                          try{
                                              this.__frame__.writeAttribute({"class" : CLASSES.itemIdentityFrame()});
                                              this.__container__ = new Object();
                                              this.__frame__.writeAttribute({"class" : CLASSES.tmIdFrame()});
                                              this.__table__ = new Element("table", {"class" : CLASSES.tmIdFrame()});
                                              this.__frame__.insert({"top" : this.__table__});
                                              this.__caption__ = new Element("caption", {"class" : CLASSES.clickable()}).update("Topic Map ID");
                                              this.__table__.update(this.__caption__);
                                              var value = contents && contents.length !== 0 ? decodeURI(contents[0]) : "";
                                              this.__contentrow__ = new Element("input", {"type" : "text", "value" : value, "size" : 48});
                                              this.__tr__ = new Element("tr", {"class" : CLASSES.tmIdFrame()});
                                              var td =new Element("td", {"class" : CLASSES.content()});
                                              this.__tr__.update(td);
                                              td.update(this.__contentrow__);
                                              this.__table__.insert({"bottom" : this.__tr__});
					      
                                              this.__minimized__ = false;
                                              function setMinimizeHandler(myself){
						  myself.__caption__.observe("click", function(event){
						      myself.minimize();
						  });
					      }
                                              setMinimizeHandler(this);
					  }
                                          catch(err){
					      alert("From tmIdC(): " + err);
					  }
				      },
				      "getContent" : function(){
					  if(this.__contentrow__.value.strip().length === 0) return null;
					  return new Array(encodeURI(this.__contentrow__.value.strip()));
				      },
				      "toJSON" : function(){
					  return (this.getContent() === null ? "null" : this.getContent().toJSON());
				      },
				      "isValid" : function(){
					  if(this.getContent() !== null){
					      this.hideError();
					      return true;
					  }
					  else {
					      this.showError("Please enter a Topic Map ID!");
					      return false;
					  }
				      },
				      "minimize": function(){
					  if(this.__minimized__ === false) this.__tr__.hide();
					  else this.__tr__.show();

					  this.__minimized__ = !this.__minimized__;
				      }});


// --- A handler for the dblclick-event. So a frame can be disabled or enabled.
function dblClickHandlerF(owner, event)
{
    if(owner.__frames__.length === 1){
	if(owner.__frames__[0].isUsed() === true){
	    owner.__frames__[0].disable();
	}
	else {
	    if(!owner.__frames__[0].__parentElem__ || owner.__frames__[0].__parentElem__.isUsed() === true) owner.__frames__[0].enable();
	}
    }
}


// --- helper function to create a dom-fragment of the form
// --- <tr class="rowClass"><td class="description">description</td>
//----  <td class="content">content</td></tr>
function newRow(rowClass, description, content)
{
    var tr = new Element("tr", {"class" : rowClass});
    tr.insert({"top" : new Element("td", {"class" : CLASSES.description()}).update(description)});
    tr.insert({"bottom" : new Element("td", {"class" : CLASSES.content()}).update(content)});
    return tr;
}


// --- Helper function for the constructors of all classes
// --- of the type FrameC.
// --- There will be set the remome and add handler.
function setRemoveAddHandler(myself, constraint, owner, min, max, call)
{
    myself.__remove__.stopObserving();
    myself.__add__.stopObserving();
    myself.__remove__.observe("click", function(event){
	var disabled = false;
	try{ disabled = myself.__disabled__; } catch(err){ };
	if(disabled === false){
	    myself.remove();
	    owner.__frames__ = owner.__frames__.without(myself);
	    if(min >= owner.__frames__.length && constraint){
		for(var i = 0; i != owner.__frames__.length; ++i){
		    owner.__frames__[i].hideRemoveButton();
		}
	    }
	    if((max === -1 || max > owner.__frames__.length) && constraint){
		for(var i = 0; i != owner.__frames__.length; ++i){
		    owner.__frames__[i].showAddButton();
		}
	    }
	}
    });
    
    myself.__add__.observe("click", function(event){
	var disabled = false;
	try{ disabled = myself.__disabled__; } catch(err){ };
	if(disabled === false){
	    var newElem = call();
	    myself.append(newElem.getFrame());
	    if((remove === true && min !== -1 && owner.__frames__.length > min) || !constraint){
		for(var i = 0; i != owner.__frames__.length; ++i){
		    owner.__frames__[i].showRemoveButton();
		}
	    }
	    if((max > -1 && max <= owner.__frames__.length) || !constraint){
		for(var i = 0; i != owner.__frames__.length; ++i){
		    owner.__frames__[i].hideAddButton();
		}
	    }
	}
    });
}


// --- Helper function for the constructors of all classes
// --- of the type FrameC and some of the type ContainerC.
// --- There will be checked the visibility of the remove and
// --- add buttons.
function checkRemoveAddButtons(owner, min, max, myself)
{
    var constraint = true;
    if(myself && !myself.__constraint__ && (!myself.__constraints__ || myself.__constraints__.length === 0)) constraint = false;

    if(min >= owner.__frames__.length && constraint === true){
	for(var i = 0; i != owner.__frames__.length; ++i){
	    owner.__frames__[i].hideRemoveButton();
	}
    }

    if((min > -1 && min < owner.__frames__.length) || constraint === false){
	for(var i = 0; i != owner.__frames__.length; ++i){
	    owner.__frames__[i].showRemoveButton();
	}
    }
    
    if((max > -1 && max <= owner.__frames__.length) || constraint === false){
        for(var i = 0; i != owner.__frames__.length; ++i){
	    owner.__frames__[i].hideAddButton();
	}
    }

    if((max === -1 || max > owner.__frames__.length) && constraint === true){
	for(var i = 0; i != owner.__frames__.length; ++i){
	    owner.__frames__[i].showAddButton();
	}
    }
}


// --- creates a control row for NameC, OccurrenceC and VariantC with a nested ItemIdentity frame.
function makeControlRow(myself, rowspan, itemIdentities)
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
    myself.__itemIdentity__ = new ItemIdentityC(itemIdentities, myself);
    tdCont.insert({"top" : myself.__itemIdentity__.getFrame()});
    myself.__table__.insert({"bottom" : tr});

    var trCtrl = new Element("tr", {"class" : CLASSES.showHiddenRows()});
    trCtrl.insert({"top" : new Element("td", {"class" : CLASSES.clickable()}).update("&#187;")});
    myself.__table__.insert({"top" : trCtrl});
    trCtrl.hide();
    trCtrl.observe("click", function(){
	/*var trs = myself.__table__.select("tr");
	for(var i = 0; i != trs.length; ++i) trs[i].show();
	trCtrl.hide();*/
	try{myself.minimize();}catch(err){ alert("err: " + err); }
    });

    // --- min click-handler
    min.observe("click", function(event){
	/*
	var trs = myself.__table__.select("tr");
	for(var i = 0; i != trs.length; ++i){
	    if(i === 0) trs[i].show();
	    else trs[i].hide();
	}
        */
	try{myself.minimize();}catch(err){ alert("err: " + err); }
    });
}


// --- This function adds a onchange handler to the type-selct-element
// --- of the instance passed through the variable myself.
// --- On changing there will be reset the scope frame to the corresponding
// --- type and when what is set to "occurrence" there will be set a corresponding
// --- datatype-value.
function onTypeChangeScope(myself, contents, constraints, what)
{
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
function makeResource(myself, content, constraints, datatypeConstraint, cssTitle, size)
{
    if(!size) size = {"rows" : 3, "cols" : 60};
    var value = "";
    var datatype = "";
    if(content && content.resourceRef && content.resourceRef.length !== 0){
	value = content.resourceRef;
	datatype = ANY_URI;
    }
    else if(content && content.resourceData){
	value = content.resourceData.value;
	datatype = content.resourceData.datatype;
    }

    try{
	this.__value__.remove();
	this.__value__ = null;
    }catch(err){}
    try{
	this.__datatype__.__frames__[0].remove();
	this.__datatype__ = new Object();
    }catch(err){}
    myself.__value__ = new Element("textarea", size).setValue(value);
    myself.__table__.insert({"bottom" : newRow(CLASSES.valueFrame(), "Resource Value", myself.__value__)});
    if(cssTitle && cssTitle.length !== 0) myself.__value__.writeAttribute({"title" : cssTitle});

    // --- datatype
    myself.__datatype__ = new Object();
    if((datatypeConstraint && datatypeConstraint.length !== 0) || datatype === ANY_URI){
	new TextrowC(datatypeConstraint, datatypeConstraint, myself.__datatype__, 1, 1, null);
	myself.__datatype__.__frames__[0].getFrame().select("input")[0].writeAttribute({"readonly" : "readonly"});
	myself.__datatypeIsSet__ = true;
    }
    else {
	new TextrowC(datatype, ".*", myself.__datatype__, 1, 1, null);
	myself.__datatypeIsSet__ = false;
    }
    myself.__table__.insert({"bottom" : newRow(CLASSES.datatypeFrame(), "Datatype", myself.__datatype__.__frames__[0].getFrame())});
}


// --- Orders the passed contents to a corresponding constraint.
// --- There will be searched for every content the constraint
// --- with the longest string.length of the regular expression.
// --- If there is a constraint invalidated by card-min
// --- there will be tried to move some contents to other constraint
// --- with a matching regular expression and a card-min/card-max that is
// --- not bad.
// --- If for types ist set to an array of a length > 0, the constraint
// --- and content type must be for a name or occurrence.
// --- The return value is an object of the form
// --- {"constraintsAndContents" : constraintsAndContents, "contents" : contents}
// --- constraintsAndContents contains all constraints with an array of contents
// --- belonging to the constraint, contents is an array of all contents
// --- which were passed to this function without the matched contents for found
// --- constraints.
function makeConstraintsAndContents(contents, simpleConstraints, forTypes)
{
    var isForTypes = forTypes && forTypes.length !== 0;

    var constraintsAndContents = new Array();
    for(var j = 0; j !== contents.length; ++j){
	// --- searches only for contents that have the given type of the current constraint
	if(isForTypes){
	    var cContentIsInConstraint = false;
	    for(var k = 0; contents[j].type && k !== contents[j].type.length; ++k){
		if(forTypes.indexOf(contents[j].type[k]) !== -1){
		    cContentIsInConstraint = true;
		    break;
		}
	    }
	    // --- cContent's type is not in the current constraint
	    if(cContentIsInConstraint === false) continue;
	}
	
	// --- searches a constraint for every existing content
	var tmpConstraint = null;
	for(var k = 0; k !== simpleConstraints.length; ++k){
	    var rex = new RegExp(simpleConstraints[k].regexp);
	    var contentValue = (isForTypes === true ? contents[j].value : contents[j]);
	    if(!contentValue){ // must be an occurrence
		if(contents[j].resourceRef) contentValue = contents[j].resourceRef;
		else if(contents[j].resourceData) contentValue = contents[j].resourceData.value;
	    }
	    if(rex.match(contentValue) === true && (tmpConstraint === null || (tmpConstraint && (simpleConstraints[k].regexp.length > tmpConstraint.regexp.length)))){
		tmpConstraint = simpleConstraints[k];
	    }
	}
	if(tmpConstraint){
	    var found = false;
	    for(var k = 0; k !== constraintsAndContents.length; ++k){
		if(constraintsAndContents[k].constraint === tmpConstraint){
		    constraintsAndContents[k].contents.push(contents[j]);
		    found = true;
		    break;
		}
	    }
	    if(found === false){
		constraintsAndContents.push({"constraint" : tmpConstraint, "contents" : new Array(contents[j])})
	    }
	}
    }
    // --- removes all moved contents from contents
    for(var j = 0; j !== constraintsAndContents.length; ++j){
	for(var k = 0; k !== constraintsAndContents[j].contents.length; ++k){
	    contents = contents.without(constraintsAndContents[j].contents[k]);
	}
    }

    // --- adds all constraints to constraintsAndcontents that are not used now
    // --- this is neccessary to find constraint with card-min > 0, but which has
    // --- still no contents because the regular expression is too short,
    for(var j = 0; j !== simpleConstraints.length; ++j){
	var k = 0;
	for( ; k !== constraintsAndContents.length; ++k){
	    if(constraintsAndContents[k].constraint === simpleConstraints[j]) break;
	}
	if(k === constraintsAndContents.length){
	    constraintsAndContents.push({"constraint" : simpleConstraints[j], "contents" : new Array()});
	}
    }

    // --- checks the card-min of all used constraints
    for(var j = 0; j !== constraintsAndContents.length; ++j){
	var min = parseInt(constraintsAndContents[j].constraint.cardMin);
	var len = constraintsAndContents[j].contents.length;
	var rex = new RegExp(constraintsAndContents[j].constraint.regexp);
	if(len < min){
	    for(var k = 0; k !== constraintsAndContents.length; ++k){
		if(constraintsAndContents[j] === constraintsAndContents[k]) continue;
		var _min = parseInt(constraintsAndContents[k].constraint.cardMin);
		var _len = constraintsAndContents[k].contents.length;
		var contentsToMove = new Array();
		for(var l = 0; l !== constraintsAndContents[k].contents.length; ++l){
		    if(_min >= _len - contentsToMove.length || min <= len + contentsToMove.length) break;
		    var contentValue = (isForTypes === true ? constraintsAndContents[k].contents[l].value : constraintsAndContents[k].contents[l]);
		    if(!contentValue){ // must be an occurrence
			if(constraintsAndContents[k].contents[l].resourceRef) contentValue = constraintsAndContents[k].contents[l].resourceRef;
			else if(constraintsAndContents[k].contents[l].resourceData) contentValue = constraintsAndContents[k].contents[l].resourceData.value;
		    }
		    if(rex.match(contentValue) === true){
			contentsToMove.push(constraintsAndContents[k].contents[l]);
		    }
		}
		constraintsAndContents[j].contents = constraintsAndContents[j].contents.concat(contentsToMove);
		// --- removes the moved contents from the source object
		for(var l = 0; l !== contentsToMove.length; ++l){
		    constraintsAndContents[k].contents = constraintsAndContents[k].contents.without(contentsToMove[l]);
		}
		if(constraintsAndContents[j].contents.length >= min) break;
	    }
	}
    }
    
    // --- to check card-max is not necessary, because if there is any constraint not satisfied the
    // --- validation will fail anyway
    
    return {"constraintsAndContents" : constraintsAndContents, "contents" : contents};
}


// --- creates a type frames for a name- or an occurrence- frame.
function makeTypes(myself, typeContent, xtypescopes)
{
    var types = new Array();
    var matched = false;
    for(var i = 0; xtypescopes && i !== xtypescopes.length; ++i){
	var xtype = xtypescopes[i].nameType;
	if(!xtype) xtype = xtypescopes[i].occurrenceType;
	if(!xtype) xtype = xtypescopes[i].associationType;
	for(var j = 0; xtype && j != xtype.length; ++j){
	    types.push(xtype[j]);
	    if(typeContent && typeContent[0] === xtype[j]){
		var selected = xtype[j];
		matched = true;
		if(types.length !== 0) types[types.length - 1] = types[0];
		types[0] = selected;
	    }
	}
    }
    if(matched === false && typeContent && typeContent.length !== 0) types.unshift(typeContent);

    if(types.length === 0 && typeContent && typeContent.length !== 0) types = typeContent;
    if(!types || types.length === 0) types = new Array("");
    myself.__type__ = new Object();
    var tr = newRow(CLASSES.typeFrame(), "Type", new SelectrowC(types, myself.__type__, 1, 1).getFrame());
    myself.__table__.insert({"bottom" : tr});
    return types;
}

// --- some helper to enable/disable some table rows

function disableItemIdentity(myself)
{
    myself.__itemIdentity__.disable();
    myself.__table__.select("tr." + CLASSES.itemIdentityFrame())[0].setStyle(DISABLED_BACKGROUND_COLOR);
}
function enableItemIdentity(myself)
{
    myself.__itemIdentity__.enable();
    myself.__table__.select("tr." + CLASSES.itemIdentityFrame())[0].removeAttribute("style");
}
function disableType(myself)
{
    myself.__type__.__frames__[0].disable();
    myself.__table__.select("tr." + CLASSES.typeFrame())[0].setStyle(DISABLED_BACKGROUND_COLOR);
}
function enableType(myself)
{
    myself.__type__.__frames__[0].enable();
    myself.__table__.select("tr." + CLASSES.typeFrame())[0].removeAttribute("style");
}
function disableScope(myself){
    myself.__scope__.disable();
    myself.__table__.select("tr." + CLASSES.scopeContainer())[0].setStyle(DISABLED_BACKGROUND_COLOR);
}
function enableScope(myself)
{
    myself.__scope__.enable();
    myself.__table__.select("tr." + CLASSES.scopeContainer())[0].removeAttribute("style");
}
function disableValue(myself)
{
    try{ myself.__value__.__frames__[0].disable(); } catch(err){}
    try{ myself.__value__.writeAttribute({"readonly" : "readonly"}); } catch(err){}
    myself.__table__.select("tr." + CLASSES.valueFrame())[0].setStyle(DISABLED_BACKGROUND_COLOR);
}
function enableValue(myself)
{
    try{ myself.__value__.__frames__[0].enable(); } catch(err){}
    try{ myself.__value__.removeAttribute("readonly"); } catch(err){}
    myself.__table__.select("tr." + CLASSES.valueFrame())[0].removeAttribute("style");
}
function disableDatatype(myself)
{
    myself.__datatype__.__frames__[0].disable();
    myself.__table__.select("tr." + CLASSES.datatypeFrame())[0].setStyle(DISABLED_BACKGROUND_COLOR);
}
function enableDatatype(myself)
{
    if(myself.__datatypeIsSet__ === false) myself.__datatype__.__frames__[0].enable();
    myself.__table__.select("tr." + CLASSES.datatypeFrame())[0].removeAttribute("style");
}
function disableVariants(myself)
{
    myself.__variants__.disable();
    myself.__table__.select("tr." + CLASSES.variantContainer())[0].setStyle(DISABLED_BACKGROUND_COLOR);
}
function enableVariants(myself)
{
    myself.__variants__.enable();
    myself.__table__.select("tr." + CLASSES.variantContainer())[0].removeAttribute("style");
}
function disableRole(myself)
{
    myself.__roles__.disable();
    myself.__table__.select("tr." + CLASSES.roleContainer())[0].setStyle(DISABLED_BACKGROUND_COLOR);
}
function enableRole(myself)
{
    myself.__roles__.enable();
    myself.__table__.select("tr." + CLASSES.roleContainer())[0].removeAttribute("style");
}
function disablePlayer(myself)
{
    myself.__player__.__frames__[0].disable();
    myself.__table__.select("tr." + CLASSES.playerFrame())[0].setStyle(DISABLED_BACKGROUND_COLOR);
}
function enablePlayer(myself)
{
    myself.__player__.__frames__[0].enable();
    myself.__table__.select("tr." + CLASSES.playerFrame())[0].removeAttribute("style");
}