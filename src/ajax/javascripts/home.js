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


// --- with this object there will be set the first and last index of topics to get by the ajax request
// --- further this object handles out of range violations and some other site effects, e.g.
// --- topicsPerPage === -1 -> show all topics, ...
var __idx = {"firstIdx" : 0, "lastIdx" : 10, "lastDirectionForward" : true, "topicsPerPage" : 10, "outOfRange" : false,
	     "topicPerPageVals" : ["5", "10", "15", "25", "50", "100", "200", "300", "All"],
	     "getTopicPerPageVals" : function(){ return this.topicPerPageVals; },
	     "getFirstIdx" : function(){ return this.firstIdx; },
	     "setFirstIdx" : function(x) { if(typeof(x) === "number" && x >= 0) this.firstIdx = x; },
	     "getLastIdx" : function(){ return this.lastIdx; },
	     "setLastIdx" : function(x) { if(typeof(x) === "number" && x >= 0) this.lastIdx = x; },
	     "getLastDirectionForward" : function() { return this.lastDirectionForward; },
	     "setLastDirectionForward" : function(x) { if(typeof(x) === "boolean") this.lastDirectionForward = x; },
	     "getTopicsPerPage" : function() {return (this.topicsPerPage === -1 ? "nil" : this.topicsPerPage); },
	     "getTopicsPerPageAsNumber" : function(){ return this.topicsPerPage; },
	     "setTopicsPerPage" : function(x) {
		 if(typeof(x) === "number" && x > 0){
		     this.topicsPerPage = x;
		     this.lastIdx = this.firstIdx + this.topicsPerPage;
		 }
		 else if(typeof(x) === "number" && x === -1){
		     this.topicsPerPage = x;
		     this.lastIdx = "nil";
		 }
	     },
	     "getOutOfRange" : function() { return this.outOfRange; },
	     "setOutOfRange" : function(x){ if(typeof(x) === "boolean") this.outOfRange = x;  },
	     "next" : function() {
		 if(this.outOfRange) return;
		 this.firstIdx += this.topicsPerPage;
		 if(this.topicsPerPage !== -1){ this.lastIdx = this.firstIdx + this.topicsPerPage; }
		 else { this.lastIdx = "nil"; }
		 this.lastDirectionForward = true;
	     },
	     "prev" : function() { 
		 if(this.topicsPerPage !== -1){
		     this.firstIdx -= this.topicsPerPage;
		     if(this.firstIdx < 0)this.firstIdx = 0;
		     this.lastIdx = this.firstIdx + this.topicsPerPage;
		 }
		 else {
		     this.firstIdx = 0;
		     this.lastIdx = "nil";
		 }
		 this.lastDirectionForward = false;
	     }
	    };



// --- creates a html table with the id "tableId" and appends it on the element with the id
// --- "parentId", if the variable next ist set to true there will be shown the next
// --- topics otherwise the previous topics
// --- the table looks like the following schema:
// --- itemIdentity | subjectLocator | subjectIdentifier | instanceOf | name | occurrence
function makeHome(parentId, tableId, next)
{
    // --- create the ajax-request handlers ------------------------------------
    function onSuccessHandler(xhr)
    {
	// --- creates the navigation div-element with a forward-, backward- button and a
	// --- selection box where the user can choose the amount of topics per page
	function createTableNavi(top)
	{
	    // --- creates the backwards and forwards buttons, if they don't exist
	    if(($("naviDivTop") === null && top === true) || ($("naviDivBottom") === null && top === false) && $(parentId)){
		var div = new Element("div", {"id" : (top ? "naviDivTop" : "naviDivBottom"), "class" : "naviTopicTable " + PAGES.home});
		var lftBtn = new Element("input", {"type" : "button", "id" : (top ? "topicTableLftBtnTop" : "topicTableLftBtnBottom")});
		var rgtBtn = new Element("input", {"type" : "button", "id" : (top ? "topicTableRgtBtnTop" : "topicTableRgtBtnBottom")});
		lftBtn.value = "<<";
		rgtBtn.value = ">>";
		rgtBtn.setStyle({"float" : "right"});
		lftBtn.setStyle({"float" : "left"});
		div.insert(lftBtn, {"position" : "top"});
		div.insert(rgtBtn, {"position" : "bottom"});
		$("content").insert(div, {"position" : "top"});
		
		rgtBtn.observe("click", function(event)
			       {
				   __idx.next();
				   makeHome(parentId, tableId, true);
			       });
		lftBtn.observe("click", function(event)
			       {
				   __idx.prev();
				   makeHome(parentId, tableId, false);
			       });

		var select = new Element("select", {"id" : (top === true ? "topicTableSelectTop" : "topicTableSelectBottom"), "class" : "topicTable"});
		var selectValues = __idx.getTopicPerPageVals();
		var selectInnerHTML = "";
		selectValues.each(function(value, idx)
				  {
				      var numberValue = value;
				      numberValue = (numberValue === "All" ? "-1" : numberValue);
				      if(Number(numberValue) !== __idx.getTopicsPerPageAsNumber()){
					  select.insert(new Element("option", {"value" : numberValue}).update(value), {"position" : "bottom"});
				      }
				      else {
					  select.insert(new Element("option", {"value" : numberValue, "selected" : "selected"}).update(value), {"position" : "bottom"});
				      }
				  });
		div.insert(select, {"position" : "content"});

		select.observe("change", function(event)
			       {
				   __idx.setTopicsPerPage(Number(event.element().value));
				   makeHome(parentId, tableId, true);
			       });
	    }
	}


	try {
	    var topicSummaries = xhr.responseText.evalJSON();
	    // --- inserts or updates the topic table if there is some json data or
	    // --- if there isn't a table yet
	    if(topicSummaries !== null || $(tableId) === null){
		// --- removes the old table - if there exists an element with the id "tableId"
		if($(tableId) !== null)$(tableId).remove();
		if($("naviDivTop") !== null)$("naviDivTop").remove();
		if($("naviDivBottom") !== null)$("naviDivBottom").remove();
		
		createTableNavi(true);

		// --- creates the html table
		var topicTable = new Element("table", {"id" : "topicTable", "class" : PAGES.home});
		
		// --- creates the header row
		var header = new Element("tr");
		header.insert(new Element("th", {"id" : "itemIdentityTh"}).update("itemIdentity"), {"position" : "bottom"});
		header.insert(new Element("th", {"id" : "subjectLocatorTh"}).update("subjectLocator"), {"position" : "bottom"});
		header.insert(new Element("th", {"id" : "subjectIdentifierTh"}).update("subjectIdentifier"), {"position" : "bottom"});
		header.insert(new Element("th", {"id" : "instanceOfTh"}).update("instanceOf"), {"position" : "bottom"});
		header.insert(new Element("th", {"id" : "nameTh"}).update("name"), {"position" : "bottom"});
		header.insert(new Element("th", {"id" : "occurrenceTh"}).update("occurrence"), {"position" : "bottom"});
		topicTable.insert(header, {"position" : "top"});
		
		// --- creates the topic summary data of the json object
		if(topicSummaries !== null){
		    topicSummaries.each(function(topicSummary, idx)
					{
					    var tr = new Element("tr");

					    			    		    
					    var itemIdentity = new Element("td", {"class" : "topicSummaryTd"});
					    var ul = new Element("ul", {"class" : "topicTable"});
					    itemIdentity.insert(ul, {"position" : "top"});
					    if(topicSummary.itemIdentities){
						topicSummary.itemIdentities.each(function(itemIdentityJ, innerIdx)
										 {
										     ul.insert(new Element("li").update(itemIdentityJ), {"position" : "bottom"});
										 });
					    }
					    
					    var subjectLocator = new Element("td", {"class" : "topicSummaryTd"});
					    ul = new Element("ul", {"class" : "topicTable"});
					    subjectLocator.insert(ul, {"position" : "top"});
					    if(topicSummary.subjectLocators){
						topicSummary.subjectLocators.each(function(subjectLocatorJ, innerIdx)
										  {
										      ul.insert(new Element("li").update(subjectLocatorJ), {"position" : "bottom"});
										  });		    
					    }

					    var subjectIdentifier = new Element("td", {"class" : "topicSummaryTd"});
					    ul = new Element("ul", {"class" : "topicTable"});
					    subjectIdentifier.insert(ul, {"position" : "top"});
					    if(topicSummary.subjectIdentifiers){
						topicSummary.subjectIdentifiers.each(function(subjectIdentifierJ, innerIdx)
										     {
											 var li = new Element("li", {"class" : "clickable"}).update(subjectIdentifierJ);
											 ul.insert(li, {"position" : "bottom"});
											 li.observe("click", function(event)
												    {
													var node = event.element();
													makePage(PAGES.edit, node.textContent);
												    });
										     });
					    }
					    
					    var instanceOf = new Element("td", {"class" : "topicSummaryTd"});
					    ul = new Element("ul", {"class" : "topicTable"});
						instanceOf.insert(ul, {"position" : "top"});
					    if(topicSummary.instanceOfs){
						topicSummary.instanceOfs.each(function(instanceOfJ, innerIdx)
									      {
										  if(instanceOfJ){
											  instanceOfJ.each(function(psi, psiIdx)
													  {
													      ul.insert(new Element("li").update(psi), {"position" : "top"});
													  });
										  }
									      });
					    }

					    var name = new Element("td", {"class" : "topicSummaryTd"});
					    ul = new Element("ul", {"class" : "topicTable"});
					    name.insert(ul, {"position" : "top"});
					    if(topicSummary.names){
						topicSummary.names.each(function(nameJ, innerIdx)
									{
									    ul.insert(new Element("li").update(nameJ), {"position" : "top"});
									});
					    }

					    var occurrence = new Element("td", {"class" : "topicSummaryTd"});
					    ul = new Element("ul", {"class" : "topicTable"});
					    occurrence.insert(ul, {"position" : "top"});
					    if(topicSummary.occurrences){
						topicSummary.occurrences.each(function(occurrenceJ, innerIdx)
									      {
										  ul.insert(new Element("li").update(occurrenceJ), {"position" : "top"});
									      });
					    }

					    tr.insert(itemIdentity, {"position" : "bottom"});
					    tr.insert(subjectLocator, {"position" : "bottom"});
					    tr.insert(subjectIdentifier, {"position" : "bottom"});
					    tr.insert(instanceOf, {"position" : "bottom"});
					    tr.insert(name, {"position" : "bottom"});
					    tr.insert(occurrence, {"position" : "bottom"});

					    topicTable.insert(tr, {"position" : "bottom"});					    
					});
		}
	    }

	    // --- there was no data received or not all requested
	    // --- so it's not allowed to increment the indices of the requested topics
	    if(topicSummaries === null || topicSummaries.length != __idx.getTopicsPerPage()){
		__idx.setOutOfRange(true);
	    }
	    else {
		__idx.setOutOfRange(false);
	    }

	    // --- inserts the table in the parent element
	    if($(parentId)){
		$(parentId).insert(topicTable, {"position" : "top"});
	    }
	    createTableNavi(false);
	}
	catch(err){
	    window.alert("got bad json data from: " + SUMMARY_URL + "\n\n" + err);
	}
    }


    function onFailureHandler(xhr)
    {
	window.alert("something went wrong ...\n" + xhr.status + ": " + xhr.statusText);
    }


    // --- the real ajax request
    new Ajax.Request(SUMMARY_URL,
		     {"method" : "get",
		      "onSuccess" : onSuccessHandler,
		      "onFailure" : onFailureHandler,
		      "parameters" : {"start" : __idx.getFirstIdx(), "end" : __idx.getLastIdx()}
		     });
}