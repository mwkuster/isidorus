//+-----------------------------------------------------------------------------
//+  Isidorus
//+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
//+
//+  Isidorus is freely distributable under the LGPL license.
//+  This ajax module uses the frameworks PrototypeJs and Scriptaculous, both
//+  are distributed under the MIT license.
//+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt and
//+  in trunk/src/ajax/javascripts/external/MIT-LICENSE.txt
//+-----------------------------------------------------------------------------


// --- creates a div-DOM-Element which contains all necessary information
// --- of a topic.
// --- the information itself will be generated from the passed json data
// --- representing a fragment.
// --- changed values can also be commmited
function makeFragmentNode(content)
{
    if(!content)content = new Object(); // to avoid null-reference errors
    
    // --- flattens a two dimensional array and returns a one dimensional array
    // --- with all elements of the passed array
    function flatten(x2Array)
    {
	try{
	    var fArray = new Array();
	    x2Array.each(function(x1Array, idx)
			 {
			     if(x1Array !== null){
				 x1Array.each(function(elem, innerIdx)
					      {
						  fArray.push(elem);
					      });
			     }
			 });
	    return fArray;
	}
	catch(err){
	    return new Object();
	}
    }


    // --- creates a row -> a div element with a span element which allows to remove this row
    // --- and a text field.
    // --- content will be set as the value of the text field 
    function makeRow(content)
    {
	var row = new Element("div", {"class" : CLASSES.row()});
	var rowDel = new Element("span", {"class" : CLASSES.removeBtnRow()}).update("x");
	row.insert(rowDel, {"position" : "bottom"});
	rowDel.observe("click", function(event)
		       {
			   var parent = event.element().parentNode;
			   var rows = parent.parentNode.getElementsByClassName(CLASSES.row());
			   if(rows.length > 1){
			       parent.remove();
			       if(rows.length === 1){
				   rows[0].childElements()[0].hide();
			       }
			   }
					       });
	var textfield = new Element("input", {"type" : "text", "class" : CLASSES.textRow(), "value" : content});
	row.insert(textfield, {"position" : "bottom"});
	return row;
    }// makeRow


    // --- creates a header div with a title, a remove button, add button and show/hide button.
    // --- the title should be a string which will inserted into a span element within the header.
    // --- if withHideBtn is set to a string with a length greater 0, the header will contain a hide button
    // --- and all elements of the header's owner node with a class name equal to the string withHideBtn will
    // --- be hidden or shown.
    // --- if with addButton is set to true, the header will contain an addButton, which allows to
    // --- adds a new row with a text field and a removeButton for the row.
    // --- if withRemoveButton is set to true, the header will contain a removeButton which allows
    // --- to remove the entire parent node.
    function makeHeader(title, withHideBtn, withAddBtn, withRemoveBtn, addHandler)
    {
	if(typeof(addHandler) !== "function"){
	    addHandler = function(event)
	    {
		var parentNode = event.element().parentNode;
		var ownerNode = parentNode.parentNode;
		var row = makeRow("");
		ownerNode.insert(row, {"position" : "bottom"});
		var rows = ownerNode.getElementsByClassName(CLASSES.row());
		if(rows.length === 1){
		    rows[0].childElements()[0].hide();
		}
		else if(rows.length > 1){
		    rows[0].childElements()[0].show();
		}
		
		// --- the owner frame is hidden, so the new created node has to be hidden, too
		if(withAddBtn === true && parentNode.getElementsByClassName(CLASSES.hideBtnHeader())[0].textContent === "show"){
		    row.hide();
		}
	    };
	}

	var headerNode = new Element("div" , {"class" : CLASSES.header()});
	var titleNode = new Element("span", {"class" : CLASSES.headerTitle()}).update(title);
	headerNode.insert(titleNode, {"position" : "bottom"});

	if(typeof(withHideBtn) === "string" && withHideBtn.length !== 0){
	    var hideNode = new Element("span", {"class" : CLASSES.hideBtnHeader()}).update("hide");
	    headerNode.insert(hideNode, {"position" : "bottom"});
	    hideNode.observe("click", function(event){
		var me = event.element();
		var ownerNode = me.parentNode.parentNode;

		if(me.textContent === "hide"){
		    var rowsToHide = ownerNode.getElementsByClassName(withHideBtn);
		    for(var i = 0; i !== rowsToHide.length; ++i)
			rowsToHide[i].hide();
		    me.textContent = "show";
		}
		else {
		    var rowsToShow = ownerNode.getElementsByClassName(withHideBtn);
		    for(var i = 0; i !== rowsToShow.length; ++i)
			rowsToShow[i].show();
		    me.textContent = "hide";
		}
	    });
	}

	if(withAddBtn === true){
	    var addNode = new Element("span", {"class" : CLASSES.addBtnHeader()}).update("add");
	    headerNode.insert(addNode, {"position" : "bottom"});
	    addNode.observe("click", addHandler);
	}

	if(withRemoveBtn === true){
	    var removeNode = new Element("span", {"class" : CLASSES.removeBtnHeader()}).update("del");
	    headerNode.insert(removeNode, {"position" : "bottom"});
	    removeNode.observe("click", function(event)
			       {
				   var ownerNode = event.element().parentNode.parentNode;
				   ownerNode.remove();
			       });
	}

	return headerNode;
    }// makeHeader


    // --- returns a div-node representing a list frame of a variable amount of rows.
    // --- title is a string describing the heade title.
    // --- classValue is the class-attribute's value of the outer frame element (div).
    // --- contents is an array which will be set as the content of a row, everey element of the
    // --- array will be boudn to one row.
    // --- withHideBtn is a boolean value describing if the header row will have a hide button.
    // --- withAddBtn is a boolean value describing if the header row will have an add button
    // --- withRemoveBtn is a boolean value describing if the header row will have a remove button
    function makeListFrame(title, classValue, contents, withHideBtn, withAddBtn, withRemoveBtn)
    {
	var listElem = new Element("div", {"class" : CLASSES.listFrame() + " " + classValue});
	var header = makeHeader(title, "row", withAddBtn, withRemoveBtn, null);
	listElem.insert(header, {"position" : "bottom"});

	try{
	    contents.each(function(content, idx)
			  {
			      listElem.insert(makeRow(content), {"position" : "bottom"});
			  });

	    if(contents.length === 1)
		listElem.getElementsByClassName(CLASSES.row())[0].childElements()[0].hide();
	}
	catch(err){
	    listElem.insert(makeRow(""), {"position" : "bottom"});
	    listElem.getElementsByClassName(CLASSES.row())[0].childElements()[0].hide();
	}

	return listElem;
    }// makeListFrame



    // --- returns a div element representing a resourceRef or resourceData frame.
    // --- title describes the title field of the header - if title is set to false
    // --- there will be generated the title "resourceRef", "resourceData" or resource
    // --- depending on the passed values resourceRef and resourceData.
    // --- if withHideBtn is set to true, the header will contain a hide button.
    function makeResourceFrame(title, classValue, resourceRef, resourceData, withHideBtn)
    {
	var content = "";
	var contentType = "";

	if(resourceRef !== null && typeof(resourceRef) !== "undefined"){
	    content = resourceRef;
	    contentType = "http://www.w3.org/2001/XMLSchema#anyURI";
	    if(title === false || title === null)title = "resourceRef";
	}
	else if(resourceData !== null && typeof(resourceData) !== "undefined"){
	    content = resourceData.value;
	    contentType = (resourceData.datatype === null ? "http://www.w3.org/2001/XMLSchema#string" : resourceData.datatype);
	    if(title === false || title === null)title = "resourceData";
	}
	else {
	    if(title === false || title === null)title = "resource";
	}

	var resourceFrame = new Element("div", {"class" : CLASSES.resourceFrame() + " " + classValue});
	var header = makeHeader(title, (withHideBtn === true ? CLASSES.row() : false), false, false, null);
	resourceFrame.insert(header, {"position" : "bottom"});
	var dataRow = new Element("div", {"class" : CLASSES.row()});
	dataRow.insert(new Element("textarea", {"class" : CLASSES.textareaRow()}).update(content));
	resourceFrame.insert(dataRow, {"position" : "bottom"});
	var datatypeRow = new Element("div", {"class" : CLASSES.row()});
	datatypeRow.insert(new Element("input", {"type" : "text", "class" : CLASSES.textRow(), "value" : contentType}));
	resourceFrame.insert(datatypeRow, {"position" : "bottom"});
	return resourceFrame;
    }// makeResourceFrame


    // --- returns a node representing a variant frame with
    // --- an itemIdentity frame, a scope frame and a resource frame.
    // --- if title is set to a string this will be set as the header titlte of this varian frame.
    // --- content is the json representation of a variant.
    // --- withHideBtn is a boolean value, if it is set to true, there will be a hide button in the
    // --- frame header which is able to hide/whow all subframes.
    // --- if withRemoveBtn is set to true, there will be a remove button available in the header row.
    function makeVariantFrame(title, content, withHideBtn, withRemoveBtn)
    {
	if(!content)content = new Object(); // to avoid null reference errors

	var variantFrame = new Element("div", {"class" : CLASSES.variantFrame()});
	var header = makeHeader(title, (withHideBtn === true ? CLASSES.inVariantFrame() : false), false, withRemoveBtn, null);
	variantFrame.insert(header, {"position" : "bottom"});
	var itemIdentityFrame = makeListFrame("itemIdentity", CLASSES.itemIdentityFrame() + " " + CLASSES.inVariantFrame(), content.itemIdentities, true, true, false);
	variantFrame.insert(itemIdentityFrame, {"position" : "bottom"});
	var scopeFrame = makeListFrame("scope", CLASSES.scopeFrame() + " " + CLASSES.inVariantFrame(), content.scopes, true, true, false);
	variantFrame.insert(scopeFrame, {"position" : "bottom"});
	var resourceFrame = makeResourceFrame(null, CLASSES.inVariantFrame(), content.resourceRef, content.resourceData, true);
	variantFrame.insert(resourceFrame, {"position" : "bottom"});
	return variantFrame;
    }// makeVariantFrame


    // --- returns a node representing a list of variant frames.
    // --- title is the header title of the entire list frame.
    // --- subtitle sets every variant's header title.
    // --- content is the json representaiton of a list of variant elements.
    // --- withHideBtn is a boolean value, if set to true, the header will contain a hide
    // --- button which is able to hide/show all sub frames
    // --- withAddBtn describes if there is an add button available in the header.
    function makeVariantsFrame(title, subTitle, classValue, content, withHideBtn, withAddBtn)
    {
	// --- defines the clickhandler of the header's add button,
	// --- so there will be created new varaint frames instead of
	// --- new rows
	var headerAddHandler = function(event)
	{
	    var variantFrame = makeVariantFrame(subTitle, null, true, true);
	    variantsFrame.insert(variantFrame, {"position" : "bottom"});

	    // --- the owner frame is hidden, so the new created node has to be hidden, too
	    var parentNode = event.element().parentNode;
	    if(withAddBtn === true && parentNode.getElementsByClassName(CLASSES.hideBtnHeader())[0].textContent === "show"){
		variantFrame.hide();
	    }
	};

	var variantsFrame = new Element("div", {"class" : CLASSES.variantsFrame() + " " + classValue});
	var header = makeHeader(title, CLASSES.variantFrame(), withAddBtn, false, headerAddHandler);
	variantsFrame.insert(header, {"position" : "bottom"});

	// --- tries to itereate through the content array and create a variant
	// --- frame for every element
	try{
	    content.each(function(elem, idx)
			 {
			     var variantFrame = makeVariantFrame(subTitle, elem, true, true);
			     variantsFrame.insert(variantFrame, {"position" : "bottom"});
			 });
	}
	catch(err){}

	return variantsFrame;
    }// makeVariantsFrame


    
    // --- returns a node representing a name frame.
    // --- title represents the frame's header title.
    // --- content is the json representation of a name element.
    // --- withHideBtn should be a string with the length > 0, so there will be a hide button
    // --- available which hides/shows all frame elements with the class contained in withHideBtn.
    // --- withRemoveBtn describes if the frame header has a remove button to remove the entire frame.
    function makeNameFrame(title, content, withHideBtn, withRemoveBtn)
    {
	if(!content)content = new Object(); // to avoid null reference errors

	var nameFrame = new Element("div", {"class" : CLASSES.nameFrame()});
	var header = makeHeader(title, (withHideBtn === true ? CLASSES.inNameFrame() : false), false, withRemoveBtn, null);
	nameFrame.insert(header, {"position" : "bottom"});
	var itemIdentityFrame = makeListFrame("itemIdentity", CLASSES.itemIdentityFrame() + " " + CLASSES.inNameFrame(), content.itemIdentities, true, true, false);
	nameFrame.insert(itemIdentityFrame, {"position" : "bottom"});
	var typeFrame = makeListFrame("type", CLASSES.typeFrame() + " " + CLASSES.inNameFrame(), content.type, true, false, false);
	nameFrame.insert(typeFrame, {"position" : "bottom"});
	var scopeFrame = makeListFrame("scope", CLASSES.scopeFrame() + " " + CLASSES.inNameFrame(), content.scopes, true, true, false);
	nameFrame.insert(scopeFrame, {"position" : "bottom"});
	var nameValue = (content.value ? new Array(content.value) : new Array(""));
	var valueFrame = makeListFrame("value", CLASSES.valueFrame() + " " + CLASSES.inNameFrame(), nameValue, true, false, false);
	nameFrame.insert(valueFrame, {"position" : "bottom"});
	var variantsFrame = makeVariantsFrame("variants", "variant", CLASSES.inNameFrame(), content.variants, CLASSES.frame(), true);
	nameFrame.insert(variantsFrame);

	return nameFrame;
    }// makeNameFrame


    // --- returns a node representing a list of name frames.
    // --- title is the title of the entire list frame.
    // --- subtitle will be used as title of every inner name frame.
    // --- content is the json representation of a list of topic names.
    // --- if withHideBtn is set to true, the main header will contain a hide button.
    // --- if withAddBtn is set to true, the main header will contain an add button,
    // --- so it is possible to add new name frames to the list.
    function makeNamesFrame(title, subtitle, classValue, content, withHideBtn, withAddBtn)
    {
	// --- defines the clickhandler of the header's add button,
	// --- so there will be created new name frames instead of
	// --- new rows
	var headerAddHandler = function(event)
	{
	    var nameFrame = makeNameFrame(subtitle, null, true, true);
	    namesFrame.insert(nameFrame, {"position" : "bottom"});

	    // --- the owner frame is hidden, so the new created node has to be hidden, too
	    var parentNode = event.element().parentNode;
	    if(withAddBtn === true && parentNode.getElementsByClassName(CLASSES.hideBtnHeader())[0].textContent === "show"){
		nameFrame.hide();
	    }
	};

	var namesFrame = new Element("div", {"class" : CLASSES.namesFrame() + " " + classValue});
	var header = makeHeader(title, CLASSES.nameFrame(), withAddBtn, false, headerAddHandler);
	namesFrame.insert(header, {"position" : "bottom"});

	// --- tries to itereate through the content array and create a name
	// --- frame for every element
	try{
	    content.each(function(elem, idx)
			 {
			     var nameFrame = makeNameFrame(subtitle, elem, true, true);
			     namesFrame.insert(nameFrame, {"position" : "bottom"});
			 });
	}
	catch(err){}
	
	return namesFrame;
    }// makeNamesFrame


    // --- returns a node representing a name frame.
    // --- title is the frame's header title.
    // --- content is the json representation of a name.
    // --- if withHideBtn is set tot true, the header will conatina a hide button.
    // --- if withRemoveBtn is set to true the header will contain a remove button.
    function makeOccurrenceFrame(title, content, withHideBtn, withRemoveBtn)
    {
	if(!content)content = new Object(); // to avoid null reference errors

	var occurrenceFrame = new Element("div", {"class" : CLASSES.occurrenceFrame()});
	var header = makeHeader(title, (withHideBtn === true ? CLASSES.inOccurrenceFrame() : false), false, withRemoveBtn, null);
	occurrenceFrame.insert(header, {"position" : "bottom"});

	var itemIdentityFrame = makeListFrame("itemIdentity", CLASSES.itemIdentityFrame() + " " + CLASSES.inOccurrenceFrame(), content.itemIdentities, true, true, false);
	occurrenceFrame.insert(itemIdentityFrame, {"position" : "bottom"});
	var typeFrame = makeListFrame("type", CLASSES.typeFrame() + " " + CLASSES.inOccurrenceFrame(), content.type, true, false, false);
	occurrenceFrame.insert(typeFrame, {"position" : "bottom"});
	var scopeFrame = makeListFrame("scope", CLASSES.scopeFrame() + " " + CLASSES.inOccurrenceFrame(), content.scopes, true, true, false);
	occurrenceFrame.insert(scopeFrame, {"position" : "bottom"});
	var resourceFrame = makeResourceFrame(null, CLASSES.inOccurrenceFrame(), content.resourceRef, content.resourceData, true);
	occurrenceFrame.insert(resourceFrame, {"position" : "bottom"});

	return occurrenceFrame;
    }// makeOccurrenceFrame


    // --- returns a node representing list of occurrences.
    // --- title is the title of the entire list frame.
    // --- subtitle is the title of all inner occurrence frames.
    // --- if withHideBtn is set tot true, the header will conatina a hide button.
    // --- if withRemoveBtn is set to true the header will contain a remove button.
    function makeOccurrencesFrame(title, subtitle, classValue, content, withHideBtn, withAddBtn)
    {
	// --- defines the clickhandler of the header's add button,
	// --- so there will be created new occurrence frames instead of
	// --- new rows
	var headerAddHandler = function(event)
	{
	    var occurrenceFrame = makeOccurrenceFrame(subtitle, null, true, true);
	    occurrencesFrame.insert(occurrenceFrame, {"position" : "bottom"});

	    // --- the owner frame is hidden, so the new created node has to be hidden, too
	    var parentNode = event.element().parentNode;
	    if(withAddBtn === true && parentNode.getElementsByClassName(CLASSES.hideBtnHeader())[0].textContent === "show"){
		occurrenceFrame.hide();
	    }
	};

	var occurrencesFrame = new Element("div", {"class" : CLASSES.occurrencesFrame() + " " + classValue});
	var header = makeHeader(title, CLASSES.occurrenceFrame(), withAddBtn, false, headerAddHandler);
	occurrencesFrame.insert(header, {"position" : "bottom"});

	// --- tries to itereate through the content array and create a name
	// --- frame for every element
	try{
	    content.each(function(elem, idx)
			 {
			     var occurrenceFrame = makeOccurrenceFrame(subtitle, elem, true, true);
			     occurrencesFrame.insert(occurrenceFrame, {"position" : "bottom"});
			 });
	}
	catch(err){}
	return occurrencesFrame;
    }// makeOccurrencesFrame


    // --- returns a node representing a topic frame.
    // --- title is the title of the main topic frame.
    // --- content is the json representation of a topic.
    // --- if withHideBtn  is set to true, there will be a hide button in the frame's header.
    // --- if withRemoveBtn is set to true, there will be a remove button in the frame's header.
    function makeTopicFrame(title, content, withHideBtn)
    {
	if(!content)content = new Object(); // to avoid null reference errors

	// --- creates the topic's main frame
	var topicFrame = new Element("div", {"class" : CLASSES.topicFrame()});
	var header = makeHeader(title, (withHideBtn === true ? CLASSES.inTopicFrame() : false), false, false, null);
	header.writeAttribute({"class" : CLASSES.topicHeader()});
	topicFrame.insert(header, {"position" : "bottom"});

	// --- creates the topic's id frame
	var topicId = (typeof(content.id) !== "string" ? "" : [content.id]);
	var topicIdFrame = makeListFrame("topic id", CLASSES.topicIdFrame() + " " + CLASSES.inTopicFrame(), topicId, true, false, false);
	topicFrame.insert(topicIdFrame);

	// --- creates the topic's itemIdentity frame
	var itemIdentityFrame = makeListFrame("itemIdentity", CLASSES.itemIdentityFrame() + " " + CLASSES.inTopicFrame(), content.itemIdentities, true, true, false);
	topicFrame.insert(itemIdentityFrame, {"position" : "bottom"});

	// --- creates the topic's subjectLocator frame
	var subjectLocatorFrame = makeListFrame("subjectLocator", CLASSES.subjectLocatorFrame() + " " + CLASSES.inTopicFrame(), content.subjectLocators, true, true, false);
	topicFrame.insert(subjectLocatorFrame, {"position" : "bottom"});

	// --- creates the topic's subjectIdentifier frame
	var subjectIdentifierFrame = makeListFrame("subjectIdentifier", CLASSES.subjectIdentifierFrame() + " " + CLASSES.inTopicFrame(), content.subjectIdentifiers, true, true, false);
	topicFrame.insert(subjectIdentifierFrame, {"position" : "bottom"});

	// --- creates the topic's instanceOf frame
	var instanceOfFrame = makeListFrame("instanceOf", CLASSES.instanceOfFrame() + " " + CLASSES.inTopicFrame(), flatten(content.instanceOfs), true, true, false);
	topicFrame.insert(instanceOfFrame, {"position" : "bottom"});

	// --- creates the topic's name frame
	var nameFrame = makeNamesFrame("names", "name", CLASSES.inTopicFrame(), content.names, CLASSES.namesFrame(), true);
	topicFrame.insert(nameFrame, {"position" : "bottom"});

	// --- creates the topic's name frame
	var occurrenceFrame = makeOccurrencesFrame("occurrences", "occurrence", CLASSES.inTopicFrame(), content.occurrences, CLASSES.occurrencesFrame(), true);
	topicFrame.insert(occurrenceFrame, {"position" : "bottom"});

	return topicFrame;
    }// makeTopicFrame

    

    // --- returns a node representing a topicStub.
    // --- title is the topicStub frame's title.
    // --- content is the json representation of a topicstub.
    // --- if withHideBtn is set to true, the frame's header will contain a hide button.
    function makeTopicStubFrame(title, content, withHideBtn, withRemoveBtn)
    {
	if(!content)content = new Object(); // to avoid null reference errors

	// --- creates the frame
	var topicStubFrame = new Element("div", {"class" : CLASSES.topicStubFrame()});
	var header = makeHeader(title, (withHideBtn === true ? CLASSES.inTopicStubFrame() : false), false, withRemoveBtn, null);
	topicStubFrame.insert(header, {"position" : "bottom"});

	// --- creates the topicStub's id frame
	var topicId = (typeof(content.id) !== "string" ? "" : [content.id]);
	var topicIdFrame = makeListFrame("topic id", CLASSES.topicIdFrame() + " " + CLASSES.inTopicStubFrame(), topicId, true, false, false);
	topicStubFrame.insert(topicIdFrame);

	// --- creates the topic's itemIdentity frame
	var itemIdentityFrame = makeListFrame("itemIdentity", CLASSES.itemIdentityFrame() + " " + CLASSES.inTopicStubFrame(), content.itemIdentities, true, true, false);
	topicStubFrame.insert(itemIdentityFrame, {"position" : "bottom"});

	// --- creates the topic's subjectLocator frame
	var subjectLocatorFrame = makeListFrame("subjectLocator", CLASSES.subjectLocatorFrame() + " " + CLASSES.inTopicStubFrame(), content.subjectLocators, true, true, false);
	topicStubFrame.insert(subjectLocatorFrame, {"position" : "bottom"});

	// --- creates the topic's subjectIdentifier frame
	var subjectIdentifierFrame = makeListFrame("subjectIdentifier", CLASSES.subjectIdentifierFrame() + " " + CLASSES.inTopicStubFrame(), content.subjectIdentifiers, true, true, false);
	topicStubFrame.insert(subjectIdentifierFrame, {"position" : "bottom"});

	return topicStubFrame;
    }// makeTopicStubFrame


    // --- returns a node representing a list of topicStub frames.
    // --- title is the title of the main frame's header.
    // --- subtitle is the title of all topicStub frames within the main frame.
    // --- content is the json representation of a list of topicStubs.
    // --- withHideBtn is a boolean value. is it set to true the main frame will contain a hide button.
    // --- withAddBtn is a boolean value. is it set to true the main frame will contain an add button.
    function makeTopicStubsFrame(title, subtitle, content, withHideBtn, withAddBtn)
    {
	// --- defines the clickhandler of the header's add button,
	// --- so there will be created new topicStub frames instead of
	// --- new rows
	var headerAddHandler = function(event)
	{
	    var topicStubFrame = makeTopicStubFrame(subtitle, null, true, true);
	    topicStubsFrame.insert(topicStubFrame, {"position" : "bottom"});

	    // --- the owner frame is hidden, so the new created node has to be hidden, too
	    var parentNode = event.element().parentNode;
	    if(withAddBtn === true && parentNode.getElementsByClassName(CLASSES.hideBtnHeader())[0].textContent === "show"){
		topicStubFrame.hide();
	    }
	};

	var topicStubsFrame = new Element("div", {"class" : CLASSES.topicStubsFrame()});
	var header = makeHeader(title, (withHideBtn === true ? CLASSES.topicStubFrame() : false), withAddBtn, false, headerAddHandler);
	header.writeAttribute({"class" : CLASSES.topicStubsHeader()});
	topicStubsFrame.insert(header, {"position" : "bottom"});

	// --- tries to itereate through the content array and create a name
	// --- frame for every element
	try{
	    content.each(function(elem, idx)
			 {
			     var topicStubFrame = makeTopicStubFrame(subtitle, elem, true, true);
			     topicStubsFrame.insert(topicStubFrame, {"position" : "bottom"});
			 });
	}
	catch(err){}
	return topicStubsFrame;
    }// makeTopicStubsFrame


    // --- returns a node representing an association role frame.
    // --- title is the main frame's title.
    // --- content is the json representation of a role.
    // --- withHideBtn is a boolean value. is it set to true, the frame's header will contain a hide button.
    // --- withRemove is a boolean value. is it set to true, the frame's header will contain a remove button.
    function makeRoleFrame(title, content, withHideBtn, withRemoveBtn)
    {
	if(!content)content = new Object(); // to avoid null reference errors

	// --- creates the frame
	var roleFrame = new Element("div", {"class" : CLASSES.roleFrame()});
	var header = makeHeader(title, (withHideBtn === true ? CLASSES.inRoleFrame() : false), false, withRemoveBtn, null);
	roleFrame.insert(header, {"position" : "bottom"});

	// --- creates the topic's itemIdentity frame
	var itemIdentityFrame = makeListFrame("itemIdentity", CLASSES.itemIdentityFrame() + " " + CLASSES.inRoleFrame(), content.itemIdentities, true, true, false);
	roleFrame.insert(itemIdentityFrame, {"position" : "bottom"});

	var typeFrame = makeListFrame("type", CLASSES.typeFrame() + " " + CLASSES.inRoleFrame(), content.type, true, false, false);
	roleFrame.insert(typeFrame, {"position" : "bottom"});

	var playerFrame = makeListFrame("player", CLASSES.playerFrame() + " " + CLASSES.inRoleFrame(), content.topicRef, true, true, false);
	roleFrame.insert(playerFrame, {"position" : "bottom"});

	return roleFrame;
    }// withRemoveBtn


    // --- returns a node representing a list of role frames.
    // --- title is the title of the main frame's header.
    // --- subtitle is the title of all role frames within the main frame.
    // --- content is the json representation of a list of association roles.
    // --- withHideBtn is a boolean value. is it set to true the main frame will contain a hide button.
    // --- withAddBtn is a boolean value. is it set tot true the main frame will contain an add button.
    function makeRolesFrame(title, subtitle, classValue, content, withHideBtn, withAddBtn)
    {
	// --- defines the clickhandler of the header's add button,
	// --- so there will be created new role frames instead of
	// --- new rows
	var headerAddHandler = function(event)
	{
	    var roleFrame = makeRoleFrame(subtitle, null, true, true);
	    rolesFrame.insert(roleFrame, {"position" : "bottom"});

	    // --- the owner frame is hidden, so the new created node has to be hidden, too
	    var parentNode = event.element().parentNode;
	    if(withAddBtn === true && parentNode.getElementsByClassName(CLASSES.hideBtnHeader())[0].textContent === "show"){
		roleFrame.hide();
	    }
	};

	var rolesFrame = new Element("div", {"class" : CLASSES.rolesFrame() + " " + classValue});
	var header = makeHeader(title, (withHideBtn === true ? CLASSES.roleFrame() : false), withAddBtn, false, headerAddHandler);
	rolesFrame.insert(header, {"position" : "bottom"});

	// --- tries to itereate through the content array and create a name
	// --- frame for every element
	try{
	    content.each(function(elem, idx)
			 {
			     var roleFrame = makeRoleFrame(subtitle, elem, true, true);
			     rolesFrame.insert(roleFrame, {"position" : "bottom"});
			 });
	}
	catch(err){}

	return rolesFrame;
    }// makeRolesFrame


    // --- returns a node representing an assocaition frame.
    // --- title is the association frame's header title.
    // --- content is the json representation of an association.
    // --- withHideBtn is a boolean value. is it set to true the main frame will contain a hide button.
    // --- withAddBtn is a boolean value. is it set to true the main frame will contain an add button.
    function makeAssociationFrame(title, content, withHideBtn, withRemoveBtn)
    {
	if(!content)content = new Object(); // to avoid null reference errors

	// --- creates the frame
	var associationFrame = new Element("div", {"class" : CLASSES.associationFrame()});
	var header = makeHeader(title, (withHideBtn === true ? CLASSES.inAssociationFrame() : false), false, withRemoveBtn, null);
	associationFrame.insert(header, {"position" : "bottom"});

	// --- creates the topic's itemIdentity frame
	var itemIdentityFrame = makeListFrame("itemIdentity", CLASSES.itemIdentityFrame() + " " + CLASSES.inAssociationFrame(), content.itemIdentities, true, true, false);
	associationFrame.insert(itemIdentityFrame, {"position" : "bottom"});

	var typeFrame = makeListFrame("type", CLASSES.typeFrame() + " " + CLASSES.inAssociationFrame(), content.type, true, false, false);
	associationFrame.insert(typeFrame, {"position" : "bottom"});

	var scopeFrame = makeListFrame("scope", CLASSES.scopeFrame() + " " + CLASSES.inAssociationFrame(), content.scopes, true, true, false);
	associationFrame.insert(scopeFrame, {"position" : "bottom"});

	var roleFrame = makeRolesFrame("roles", "role", CLASSES.inAssociationFrame(), content.roles, true, true);
	associationFrame.insert(roleFrame, {"position" : "bottom"});

	return associationFrame;
    }// makeAssociaitonFrame


    // --- returns a node representing a list of association frames.
    // --- title is the title of the main frame's header.
    // --- subtitle is the title of all association frames within the main frame.
    // --- content is the json representation of a list of associations.
    // --- withHideBtn is a boolean value. is it set to true the main frame will contain a hide button.
    // --- withAddBtn is a boolean value. is it set to true the main frame will contain an add button.
    function makeAssociationsFrame(title, subtitle, content, withHideBtn, withAddBtn)
    {
	// --- defines the clickhandler of the header's add button,
	// --- so there will be created new role frames instead of
	// --- new rows
	var headerAddHandler = function(event)
	{
	    var associationFrame = makeAssociationFrame(subtitle, null, true, true);
	    associationsFrame.insert(associationFrame, {"position" : "bottom"});

	    // --- the owner frame is hidden, so the new created node has to be hidden, too
	    var parentNode = event.element().parentNode;
	    if(withAddBtn === true && parentNode.getElementsByClassName(CLASSES.hideBtnHeader())[0].textContent === "show"){
		associationFrame.hide();
	    }
	};

	var associationsFrame = new Element("div", {"class" : CLASSES.associationsFrame()});
	var header = makeHeader(title, (withHideBtn === true ? CLASSES.associationFrame() : false), withAddBtn, false, headerAddHandler);
	header.writeAttribute({"class" : CLASSES.associationsHeader()});
	associationsFrame.insert(header, {"position" : "bottom"});

	// --- tries to itereate through the content array and create a name
	// --- frame for every element
	try{
	    content.each(function(elem, idx)
			 {
			     var associationFrame = makeAssociationFrame(subtitle, elem, true, true);
			     associationsFrame.insert(associationFrame, {"position" : "bottom"});
			 });
	}
	catch(err){}

	return associationsFrame;
    }// makeAssociationsFrame


    function makeTmIdsFrame(title, content, withHideBtn, withAddBtn)
    {
	var tmIdsFrame =  makeListFrame("tm ids", CLASSES.tmIdsFrame(), content, true, true, false);
	tmIdsFrame.childElements()[0].writeAttribute({"class" : CLASSES.tmIdsHeader()});
	return tmIdsFrame;
    }// makeTmIdsFrame



    // --- some test data to debug this function
    var testData = {"topic" : {"id" : "topic-id",
			       "itemIdentities" : ["itemIdentity-1", "itemIdentity-2"],
			       "subjectLocators" : ["subjectLocator-1", "subjectLocator-2"],
			       "subjectIdentifiers" : ["psi-1", "psi-2"],
			       "instanceOfs" : [["psi-1-1", "psi-1-2"], ["psi-2-1"]],
			       "names" : [{"itemIdentities" : ["itemIdentity-n1-1", "itemIdentity-n1-2"],
					   "type" : ["type-n1-1", "type-n1-2"],
					   "scopes" : ["scope-n1-1", "scope-n1-2"],
					   "value" : "value-n1",
					   "variants" : [{"itemIdentities" : ["itemIdentity-n1-v1-1", "itemIdentity-n1-v1-2"],
							  "scopes" : ["scope-n1-v1-1", "scope-n1-v1-2"],
							  "resourceRef": "resourceRef",
							  "resourceData" : null},
							 {"itemIdentities" : ["itemIdentity-n1-v2-1", "itemIdentity-n1-v2-2"],
							  "scopes" : ["scope-n1-v2-1", "scope-n1-v2-2"],
							  "resourceRef": null,
							  "resourceData" : {"datatype" : "datatype-n1-v2",
									    "value" : "value-n1-v2"}}]},
					  {"itemIdentities" : ["itemIdentity-n1-1", "itemIdentity-n1-2"],
					   "type" : ["type-n1-1", "type-n1-2"],
					   "scopes" : ["scope-n1-1", "scope-n1-2"],
					   "value" : "value-n1",
					   "variants" : null}],
			       "occurrences" : [{"itemIdentities" : ["itemIdentity-o1-1", "itemIdentity-o1-2"],
						 "type" : ["type-o1-1"],
						 "scopes" : [["scope-o1-s1-1"], ["scope-o1-s2-1", "scope-o1-s2-2"]],
						 "resourceRef" : "resourceRef-o1"},
						{"itemIdentities" : null,
						 "type" : ["type-o2-1", "type-o2-2"],
						 "scopes" : [["scope-o2-s2-1", "scope-o2-s2-2"]],
						 "resourceData" : {"datatype" : "datatype-o2",
								   "value" : "value-o2"}}]},
		    "topicStubs" : [{"id" : "stub-id-1",
				     "itemIdentities" : null,
				     "subjectLocators" : null,
				     "subjectIdentifiers" : ["stub-1-psi-1"]},
				    {"id" : "stub-id-2",
				     "itemIdentities" : ["stub-2-itemIdentity-1", "stub-2-itemIdentity-2"],
				     "subjectLocators" : null,
				     "subjectIdentifiers" : ["stub-2-psi-1", "stub-2-psi-2"]},
				    {"id" : "stub-id-3",
				     "itemIdentities" : ["stub-3-itemIdentity-1", "stub-3-itemIdentity-2"],
				     "subjectLocators" : ["stub-1-subjectLocator-1"],
				     "subjectIdentifiers" : ["stub-3-psi-1"]}],
		    "associations" : [{"itemIdentities" : ["itemIdentity-a1-1", "itemIdentity-a1-2"],
				       "type" : ["type-a1-1"],
				       "scopes" : [["scope-1-a1-1"], ["scope-2-a1-1", "scope-2-a1-2"]],
				       "roles": [{"itemIdentities" : ["itemIdentity-a1-r1-1", "itemIdentity-a1-r1-2"],
						  "type" : ["type-a1-r1-1"],
						  "topicRef" : ["player-a1-r1"]},
						 {"itemIdentities" : null,
						  "type" : ["type-a1-r2-1", "type-a1-r2-2"],
						  "topicRef" : ["player-a1-r1-1", "player-a1-r1-2"]}]},
				      {"itemIdentities" : null,
				       "type" : ["type-a2-1", "type-a1-2"],
				       "scopes" : [["scope-1-a2-1"]],
				       "roles": [{"itemIdentities" : ["itemIdentity-a2-r1-1"],
						  "type" : ["type-a2-r1-1"],
						  "topicRef" : ["player-a2-r1"]},
						 {"itemIdentities" : null,
						  "type" : ["type-a2-r2-1"],
						  "topicRef" : ["player-a2-r1-1"]}]}],
		    "tmIds" : ["tm-id-1", "tm-id-2"]};
    
    // --- creates the fragment frame
    var fragment = new Element("div", {"class" : CLASSES.fragmentFrame() + " " + PAGES.edit});
    var topicFrame = makeTopicFrame("topic", content.topic, true);
    fragment.insert(topicFrame, {"position" : "bottom"});
    var topicStubsFrame = makeTopicStubsFrame("topicStubs", "topicStub", content.topicStubs, true, true)
    fragment.insert(topicStubsFrame, {"position" : "bottom"});
    var associationsFrame = makeAssociationsFrame("associations", "association", content.associations, true, true)
    fragment.insert(associationsFrame, {"position" : "bottom"});
    var tmIdsFrame = makeTmIdsFrame("tm ids", content.tmIds, true, true);
    fragment.insert(tmIdsFrame, {"position" : "bottom"});
    var commitButton = new Element("input", {"type" : "button", "value" : "commit", "class" : CLASSES.button()});
    fragment.insert(commitButton, {"position" : "bottom"});


    // --- the onClickHandler for the commit button.
    // --- this handler will generate json string of all contents of this fragment node
    // --- valid to the json schema in docs/xtm_json.txt.
    function onclickHandler(event)
    {
	var validToBeSent = true;

	var fragmentString = "";
	var topicString = "";
	var topicStubsString = "";
	var associationsString = "";
	var tmIdsString = "";


	// --- a helper function to display error messages
	function errMsg(what)
	{
	    alert("\"" + what + "\" is missing, the json fragment could not be committed - please enter a valid \"" + what + "\"!");
	    validToBeSent = false;
	}


	// --- a helper function to create a list of strings.
	// --- nodes is a nodeList.
	// --- the return value is a json list.
	function makeList(nodes)
	{
	    if(!nodes || nodes.length === 0) return "null";

	    // --- separates only values which are a valid string, i.e. the string trimmed
	    // --- of leading and trailing spaces must be greater than 0
	    var textValues = new Array();
	    for(var i = 0; i !== nodes.length; ++i){
		var clearedValue = nodes[i].value.strip();
		if(clearedValue.length === 0);
		else textValues.push(clearedValue);
	    }
	    
		
	    // --- creates a json list of all values in the array textValues
	    var jsonList = "null";
	    for(var i = 0; i !== textValues.length; ++i){
		// --- adds the first item
		if(i === 0)jsonList = "[";
		
		// --- adds the text value
		jsonList += textValues[i].toJSON();
		
		// --- adds the separator or the end character
		if(i !== textValues.length - 1)jsonList += ",";
		else jsonList += "]";
	    }
	    
	    return jsonList;
	}

	
	// --- like the function makeList, but the parameter strings is an array of strings
	// --- and there will be no string escaped to a json string (.toJSON())
	function makeListFromStrings(textValues)
	{
	    if(!textValues || textValues.length === 0)return "null";

	    var jsonList = "null";
	    for(var i = 0; i !== textValues.length; ++i){
		// --- adds the first item
		if(i === 0)jsonList = "[";
		
		// --- adds the text value
		jsonList += textValues[i];
		
		// --- adds the separator or the end character
		if(i !== textValues.length - 1)jsonList += ",";
		else jsonList += "]";
	    }
	    
	    return jsonList;	    
	}


	// --- a helper function to get all resourceRef and REsourceData values
	function resourceToJson(reElem)
	{
	    if(!reElem)return {"resourceRef" : "null", "resourceData" : "null"};

	    try{
		var daElem = reElem.getElementsByClassName(CLASSES.textRow())[0];
		var _datatype = daElem.value.strip().toJSON();
		if(_datatype.length === 2){
		    _datatype = "\"http://www.w3.org/2001/XMLSchema#string\"";
		}

		var vaElem = reElem.getElementsByClassName(CLASSES.textareaRow())[0];
		var _value = vaElem.value.strip().toJSON();
		if(_value.length === 2){
		    errMsg("Resource value");
		    return;
		}

		var resourceX = {"resourceRef" : "null", "resourceData" : "null"};
		if(_datatype === "\"http://www.w3.org/2001/XMLSchema#anyURI\""){
		    resourceX.resourceRef = _value;
		}
		else {
		    resourceX.resourceData = {"datatype" : _datatype, "value" : _value};
		}

		return resourceX;
	    }
	    catch(err){
		validToBeSent = false;
	    }
	}


	// --- a helper function to transform all variant values to
	// --- a json string.
	function variantToJson(varElem)
	{
	    if(!varElem)return "null";

	    try{
		var iiElems = varElem.getElementsByClassName(CLASSES.itemIdentityFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _itemIdentities = makeList(iiElems);
		_itemIdentities = "\"itemIdentities\":" + _itemIdentities;
		
		var scElems = varElem.getElementsByClassName(CLASSES.scopeFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _scopes = makeList(scElems);
		_scopes = "\"scopes\":" + _scopes;

		var reElem = varElem.getElementsByClassName(CLASSES.resourceFrame())[0];
		var _resource = resourceToJson(reElem);
		var _resourceRef = "\"resourceRef\":" + _resource.resourceRef;
		var _resourceData = "\"resourceData\":";
		if(_resource.resourceData === "null"){
		    _resourceData += "null";
		}
		else {
		    _resourceData += "{\"datatype\":" + _resource.resourceData.datatype + ",\"value\":" + _resource.resourceData.value + "}";
		}

		return "{" + _itemIdentities + "," + _scopes + "," + _resourceRef + "," + _resourceData + "}";
	    }
	    catch(err){
		validToBeSent = false;
	    }
	}


	// --- a helper function to extrahate all values from a name node.
	// --- the reurn value is a name in json representation of the
	// --- way described in docs/xtm_json.txt.
	function nameToJson(naElem)
	{
	    if(!naElem)return "null";

	    try{
		var iiElems = naElem.getElementsByClassName(CLASSES.itemIdentityFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _itemIdentities = makeList(iiElems);
		_itemIdentities = "\"itemIdentities\":" + _itemIdentities;

		var tyElem = naElem.getElementsByClassName(CLASSES.typeFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _type = makeList(tyElem);
		_type = "\"type\":" + _type;

		var scElems = naElem.getElementsByClassName(CLASSES.scopeFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _scopes = makeList(scElems);
		_scopes = "\"scopes\":" + _scopes;

		var vaElems = naElem.getElementsByClassName(CLASSES.valueFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _value = vaElems[0].value.strip().toJSON();
		if(_value.length === 2){
		    errMsg("Name value"); // === 2 -> "".toJSON() => "\"\""
		    return;
		}
		_value = "\"value\":" + _value;

		var varElems = naElem.getElementsByClassName(CLASSES.variantsFrame())[0].getElementsByClassName(CLASSES.variantFrame());
 		var _variants = "\"variants\":";
		if(varElems.length === 0){
		    _variants += "null";
		}
		else {
		    var _varStrings = new Array();
		    for(var i = 0; i !== varElems.length; ++i){
			_varStrings.push(variantToJson(varElems[i]));
		    }
		    _variants += makeListFromStrings(_varStrings);
		}

		return "{" + _itemIdentities + "," + _type + "," + _scopes + "," + _value + "," + _variants + "}";
	    }
	    catch(err){
		validToBeSent = false;
	    }
	}


	// --- a helper function to extrahate all values from an occurrence node.
	// --- the reurn value is an occurrence in json representation of the
	// --- way described in docs/xtm_json.txt.
	function occurrenceToJson(ocElem)
	{
	    if(!ocElem)return "null";

	    try{
		var iiElems = ocElem.getElementsByClassName(CLASSES.itemIdentityFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _itemIdentities = makeList(iiElems);
		_itemIdentities = "\"itemIdentities\":" + _itemIdentities;
		
		var tyElem = ocElem.getElementsByClassName(CLASSES.typeFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _type = makeList(tyElem);
		_type = "\"type\":" + _type;
		
		var scElems = ocElem.getElementsByClassName(CLASSES.scopeFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _scopes = makeList(scElems);
		_scopes = "\"scopes\":" + _scopes;

		var reElem = ocElem.getElementsByClassName(CLASSES.resourceFrame())[0];
		var _resource = resourceToJson(reElem);
		var _resourceRef = "\"resourceRef\":" + _resource.resourceRef;
		var _resourceData = "\"resourceData\":";
		if(_resource.resourceData === "null"){
		    _resourceData += "null";
		}
		else {
		    _resourceData += "{\"datatype\":" + _resource.resourceData.datatype + ",\"value\":" + _resource.resourceData.value + "}";
		}

		return "{" + _itemIdentities + "," + _type + "," + _scopes + "," + _resourceRef + "," + _resourceData + "}";
	    }
	    catch(err){
		validToBeSent = false;
	    }
	}


	// --- a helper function to extrahate all topic values and return them as
	// --- a json string.
	function topicToJson()
	{
	    try{
		var _topicId = "";
		_topicId = topicFrame.getElementsByClassName(CLASSES.topicIdFrame())[0].getElementsByClassName(CLASSES.textRow())[0].value;
		if(_topicId.length === 0){ errMsg("topic id"); return; }
		_topicId = "\"id\":" + _topicId.toJSON();
		
		var iiElems = topicFrame.getElementsByClassName(CLASSES.itemIdentityFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _itemIdentities = "\"itemIdentities\":" + makeList(iiElems);
		
		var slElems =  topicFrame.getElementsByClassName(CLASSES.subjectLocatorFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _subjectLocators = "\"subjectLocators\":" + makeList(slElems);
		
		var siElems =  topicFrame.getElementsByClassName(CLASSES.subjectIdentifierFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _subjectIdentifiers = "\"subjectIdentifiers\":" + makeList(siElems);
		if(_itemIdentities === "\"itemIdentities\":null" && _subjectLocators === "\"subjectLocators\":null" && _subjectIdentifiers === "\"subjectIdentifiers\":null"){
		    alert("An identifier is missing one of the elements \"itemIdentity\", \"subjectLocator\" or \"subjectIdentifier\" must be set!");
		    validToBeSent = false;
		}

		var ioElems =  topicFrame.getElementsByClassName(CLASSES.instanceOfFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _instanceOfs = "\"instanceOfs\":" + makeList(ioElems);
		
		var naElems = topicFrame.getElementsByClassName(CLASSES.namesFrame())[0].getElementsByClassName(CLASSES.nameFrame());
		var _names = "\"names\":";
		if(naElems.length === 0){
		    _names += "null";
		}
		else {
		    var _naStrings = new Array();
		    for(var i = 0; i !== naElems.length; ++i){
			_naStrings.push(nameToJson(naElems[i]));
		    }
		    _names += makeListFromStrings(_naStrings);
		}
		
		var ocElems = topicFrame.getElementsByClassName(CLASSES.occurrencesFrame())[0].getElementsByClassName(CLASSES.occurrenceFrame());
		var _occurrences = "\"occurrences\":";
		if(ocElems.length === 0){
		    _occurrences += "null";
		}
		else {
		    var _ocStrings = new Array();
		    for(var i = 0; i !== ocElems.length; ++i){
			_ocStrings.push(occurrenceToJson(ocElems[i]));
		    }
		    _occurrences += makeListFromStrings(_ocStrings);
		}
		
		return "{" + _topicId + "," + _itemIdentities + "," + _subjectLocators + "," +_instanceOfs + "," + _subjectIdentifiers + "," + _names + "," + _occurrences + "}";
	    }
	    catch(err){
		validToBeSent = false;
	    }
	}


	// --- a helper function to extrahate all topicstubs values and return them as
	// --- a json string.
	function topicStubToJson(tsElem)
	{
	    if(!tsElem) return "null";
	    try{
		var _topicId = "";
		_topicId = tsElem.getElementsByClassName(CLASSES.topicIdFrame())[0].getElementsByClassName(CLASSES.textRow())[0].value;
		if(_topicId.length === 0){ errMsg("topic id"); return; }
		_topicId = "\"id\":" + _topicId.toJSON();
		
		var iiElems = tsElem.getElementsByClassName(CLASSES.itemIdentityFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _itemIdentities = "\"itemIdentities\":" + makeList(iiElems);
		
		var slElems =  tsElem.getElementsByClassName(CLASSES.subjectLocatorFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _subjectLocators = "\"subjectLocators\":" + makeList(slElems);
		
		var siElems =  tsElem.getElementsByClassName(CLASSES.subjectIdentifierFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _subjectIdentifiers = "\"subjectIdentifiers\":" + makeList(siElems);
		if(_itemIdentities === "\"itemIdentities\":null" && _subjectLocators === "\"subjectLocators\":null" && _subjectIdentifiers === "\"subjectIdentifiers\":null"){
		    alert("An identifier is missing one of the elements \"itemIdentity\", \"subjectLocator\" or \"subjectIdentifier\" must be set!");
		    validToBeSent = false;
		}

		return "{" + _topicId + "," + _itemIdentities + "," + _subjectLocators + "," + _subjectIdentifiers + "}";
	    }
	    catch(err){
		validToBeSent = false;
	    }
	}


	// --- a helper function to extrahate all association role values and return them as
	// --- a json string.
	function roleToJson(rlElem)
	{
	    if(!rlElem)return "null";

	    try{
		var iiElems = rlElem.getElementsByClassName(CLASSES.itemIdentityFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _itemIdentities = makeList(iiElems);
		_itemIdentities = "\"itemIdentities\":" + _itemIdentities;
		
		var tyElem = rlElem.getElementsByClassName(CLASSES.typeFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _type = makeList(tyElem);
		_type = "\"type\":" + _type;

		var plElem = rlElem.getElementsByClassName(CLASSES.playerFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _player = "\"topicRef\":" + makeList(plElem);
		if(_player === "\"topicRef\":null"){
		    errMsg("Player");
		    return;
		}
		
		return "{" + _itemIdentities + "," + _type + "," + _player + "}";
	    }
	    catch(err){
		validToBeSent = false;
	    }
	}


	// --- a helper function to extrahate all association values and return them as
	// --- a json string.
	function associationToJson(asElem)
	{
	    if(!asElem)return "null";

	    try{
		var iiElems = asElem.getElementsByClassName(CLASSES.itemIdentityFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _itemIdentities = makeList(iiElems);
		_itemIdentities = "\"itemIdentities\":" + _itemIdentities;
		
		var tyElem = asElem.getElementsByClassName(CLASSES.typeFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _type = makeList(tyElem);
		_type = "\"type\":" + _type;
		
		var scElems = asElem.getElementsByClassName(CLASSES.scopeFrame())[0].getElementsByClassName(CLASSES.textRow());
		var _scopes = makeList(scElems);
		_scopes = "\"scopes\":" + _scopes;

		var rlElems = associationsFrame.getElementsByClassName(CLASSES.rolesFrame())[0].getElementsByClassName(CLASSES.roleFrame());
		var _roles = "\"roles\":";
		if(rlElems.length === 0){
		    _roles += "null";
		}
		else {
		    var _rlStrings = new Array();
		    for(var i = 0; i !== rlElems.length; ++i){
			_rlStrings.push(roleToJson(rlElems[i]));
		    }
		    _roles += makeListFromStrings(_rlStrings);
		}
		
		return "{" + _itemIdentities + "," + _type + "," + _scopes + "," + _roles + "}";
	    }
	    catch(err){
		validToBeSent = false;
	    }
	}


	// --- gets all topic values -------------------------------------------
	var _topic = "\"topic\":" + topicToJson();

	var _topicStubs = "\"topicStubs\":";
	try{
	    var tsElems = topicStubsFrame.getElementsByClassName(CLASSES.topicStubFrame());
	    if(tsElems.length === 0){
		_topicStubs += "null";
	    }
	    else {
		var _tsStrings = new Array();
		for(var i = 0; i !== tsElems.length; ++i){
		    _tsStrings.push(topicStubToJson(tsElems[i]));
		}
		_topicStubs += makeListFromStrings(_tsStrings);
	    }
	}
	catch(err){
	    validToBeSent = false;
	}
	
	var _associations = "\"associations\":";
	try{
	    var asElems = associationsFrame.getElementsByClassName(CLASSES.associationFrame());
	    if(asElems.length === 0){
		_associations += "null";
	    }
	    else {
		var _asStrings = new Array();
		for(var i = 0; i !== asElems.length; ++i){
		    _asStrings.push(associationToJson(asElems[i]));
		}
		_associations += makeListFromStrings(_asStrings);
	    }
	}
	catch(err){
	    validToBeSent = false;
	}

	var _tmIds = "\"tmIds\":";
	try{
	    var tmElem = tmIdsFrame.getElementsByClassName(CLASSES.textRow());
		_tmIds += makeList(tmElem);
	    if(_tmIds === "\"tmIds\":null"){
		errMsg("Topic Map ID");
	    }
	}
	catch(err){
	    alert(err);
	    validToBeSent = false;
	}

	// --- sending the data
	if(validToBeSent === true){
	    var _fragment = "{" + _topic + "," + _topicStubs + "," + _associations + "," + _tmIds + "}";

	    new Ajax.Request(COMMIT_URL,
	                     {
				 method: "post",
				 requestHeaders:{ "Content-Type":"application/json"},
				 onSuccess: function(xhr){ window.alert("Fragment committed successfully!"); },
				 onFailure: function(xhr){ window.alert("Something went wrong ...\n" + xhr.status + ": " + xhr.statusText); },
				 postBody: _fragment
	                     });
	}
	else {
	    alert("You have entered som invalid data - please check your input!");
	}
    }

    commitButton.observe("click", onclickHandler);

    return fragment;
}


// --- calls the getFragment function with a onSuccessHandler.
// --- the handler uses the function makeFragmentNode to create a
// --- DOM node with the jsonFragment, if the answer is null,
// --- the fragments content fields are set to "" or the are hidden.
function getAndBuildFragment(topicPsi)
{
    // --- if there was passed an invalid topicPsi or onSuccessHandler, the return value is null
    if(typeof(topicPsi) !== "string" || topicPsi.length === 0)
	$("content").insert(makeFragmentNode(null), {"position" : "content"});

    function onSuccessHandler(xhr)
    {
	try{
	    var jsonFragment = xhr.responseText.evalJSON();
	    $("content").insert(makeFragmentNode(jsonFragment), {"position" : "content"});
	}
	catch(err){
	    alert("Got bad json data from " + GET_PREFIX + topicPsi.gsub("#", "%23"));
	}
    }

    getFragment(topicPsi, onSuccessHandler);
}


// --- creates an ajax request for a fragment to the url mconcatenated of
// --- the GET_PREFIX and the topicPsi
function getFragment(topicPsi, onSuccessHandler)
{
    // --- if there was passed an invalid topicPsi or onSuccessHandler, the return value is null
    if(typeof(topicPsi) !== "string" || topicPsi.length === 0 || !onSuccessHandler)return null;


    // --- the ajax-request error handler
    function onFailureHandler(xhr)
    {
	window.alert("Something went wrong ...\n" + xhr.status + ": " + xhr.statusText);
    }
    
    // --- the real ajax request
    var fragment = null;
    var requestUrl = GET_PREFIX + topicPsi.gsub("#", "%23");
    var request = new Ajax.Request(requestUrl,
				   {"method" : "get",
				    "onSuccess" : onSuccessHandler,
				    "onFailure" : onFailureHandler
				   });
}