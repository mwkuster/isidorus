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


var TIMEOUT = 5000; // const TIMEOUT = 5000 --> "const" doesn't work under IE
var HOST_PREF = "http://localhost:8000/";
var GET_PREFIX = HOST_PREF + "json/get/";
var COMMIT_URL = HOST_PREF + "json/commit/";
var ALL_PSIS_URL = HOST_PREF + "json/psis/";
var OWN_URL = HOST_PREF + "isidorus";
var SUMMARY_URL = HOST_PREF + "json/summary"


// --- a kind of enum for the the different pages with an attribute and a value
var PAGES = {"home" : "home", "search" : "searchTopic", "edit" : "editTopic", "create" : "createTopic"};

// --- contains most css classes used in this project
var CLASSES = {"__addBtnHeader" : "addButton",
	       "__associationFrame" : "associationFrame",
	       "__associationsFrame" : "assocaitionsFrame",
	       "__associationsHeader" : "associationsHeaderRow",
	       "__button" : "clickable",
	       "__fragmentFrame" : "fragmentFrame",
               "__frame" : "frame",
	       "__header" : "headerRow",
	       "__headerTitle" : "title",
	       "__hideBtnHeader" : "hideButton",
	       "__inAssociationFrame" : "inAssociationFrame",
	       "__inNameFrame" : "inNameFrame",
	       "__inOccurrenceFrame" : "inOccurrenceFrame",
	       "__inRoleFrame" : "inRoleFrame",
	       "__instanceOfFrame" : "instanceOf",
	       "__inTopicFrame" : "inTopicFrame",
	       "__inTopicStubFrame" : "inTopicStubFrame",
	       "__inVariantFrame" : "inVariantFrame",
	       "__itemIdentityFrame" : "itemIdentity",
	       "__listFrame" : "listFrame",
	       "__nameFrame" : "nameFrame",
	       "__namesFrame" : "namesFrame",
	       "__occurrenceFrame" : "occurrenceFrame",
	       "__occurrencesFrame" : "occurrencesFrame",
	       "__playerFrame" : "playerFrame",
	       "__removeBtnHeader" : "removeButton",
	       "__removeBtnRow" : "rowDel",
	       "__resourceFrame" : "resourceFrame",
	       "__roleFrame" : "roleFrame",
	       "__rolesFrame" : "roleFrame",
	       "__row" : "row",
	       "__scopeFrame" : "scopeFrame",
	       "__subjectIdentifierFrame" : "subjectIdentifier",
	       "__subjectLocatorFrame" : "subjectLocator", 
	       "__textareaRow" : "rowTextArea",
	       "__textRow" : "rowTextfield",
	       "__tmIdsFrame" : "tmIdsFrame",
	       "__tmIdsHeader" : "tmIdsHeaderRow",
	       "__topicFrame" : "topicFrame",
	       "__topicHeader" : "topicHeaderRow",
	       "__topicIdFrame" : "topicId",
	       "__topicStubFrame" : "topicStubFrame",
	       "__topicStubsHeader" : "topicStubsHeaderRow",
	       "__topicStubsFrame" : "topicStubsFrame",
	       "__typeFrame" : "typeFrame",
	       "__valueFrame" : "valueFrame",
	       "__variantFrame" : "variantFrame",
	       "__variantHeader" : "variantHeaderRow",
	       "__variantsFrame" : "variantsFrame",

	       "addBtnHeader" : function(){ return this.__button + " " + this.__addBtnHeader; },
	       "associationFrame" : function(){ return this.__frame + " " + this.__associationFrame; },
	       "associationsFrame" : function(){ return this.__frame + " " + this.__associationsFrame; },
	       "associationsHeader" : function(){ return this.__associationsHeader; },
	       "button" : function(){ this.__button; },
	       "fragmentFrame" : function(){ return this.__frame + " " + this.__fragmentFrame; },
	       "frame" : function(){ return this.__frame; },
	       "header" : function(){ return this.__header; },
	       "headerTitle" : function(){ return this.__headerTitle; },
	       "hideBtnHeader" : function(){ return this.__button + " " + this.__hideBtnHeader; },
	       "inAssociationFrame" : function(){ return this.__inAssociationFrame; },
	       "inNameFrame" : function(){ return this.__inNameFrame; },
	       "inOccurrenceFrame" : function(){ return this.__inOccurrenceFrame; },
	       "inRoleFrame" : function(){ return this.__inRoleFrame; },
	       "instanceOfFrame" : function(){ return this.__instanceOfFrame; },
	       "inTopicFrame" : function(){ return this.__inTopicFrame; },
	       "inTopicStubFrame" : function(){ return this.__inTopicStubFrame; },
	       "inVariantFrame" : function(){ return this.__inVariantFrame; },
	       "itemIdentityFrame" : function(){ return this.__itemIdentityFrame; },
	       "listFrame" : function(){ return this.__frame + " " + this.__listFrame; },
	       "nameFrame" : function(){ return this.__frame + " " + this.__nameFrame; },
	       "namesFrame" : function(){ return this.__frame + " " + this.__namesFrame; },
	       "occurrenceFrame" : function(){ return this.__frame + " " + this.__occurrenceFrame; },
	       "occurrencesFrame" : function(){ return this.__frame + " " + this.__occurrencesFrame; },
	       "playerFrame" : function(){ return this.__playerFrame; },
	       "removeBtnHeader" : function(){ return this.__button + " " + this.__removeBtnHeader; },
	       "removeBtnRow" : function(){ return this.__button + " " + this.__removeBtnRow; },
	       "roleFrame" : function(){ return this.__frame + " " + this.__roleFrame; },
	       "rolesFrame" : function(){ return this.__frame + " " + this.__rolesFrame; },
	       "row" : function(){ return this.__row; },
	       "scopeFrame" : function(){ return this.__scopeFrame; },
	       "resourceFrame" : function(){ return this.__frame + " " + this.__resourceFrame; },
	       "subjectIdentifierFrame" : function(){ return this.__subjectIdentifierFrame; },
	       "subjectLocatorFrame" : function(){ return this.__subjectLocatorFrame; },
	       "textareaRow" : function(){ return this.__textareaRow; },
	       "textRow" : function(){ return this.__textRow; },
	       "tmIdsFrame" : function(){ return this.__frame + " " + this.__tmIdsFrame; },
	       "tmIdsHeader" : function(){ return this.__tmIdsHeader; },
	       "topicFrame" : function(){ return this.__frame + " " + this.__topicFrame; },
	       "topicHeader" : function(){ return this.__topicHeader; },
	       "topicIdFrame" : function(){ return this.__topicIdFrame; },
	       "topicStubFrame" : function(){ return this.__frame + " " + this.__topicStubFrame; },
	       "topicStubsHeader" : function(){ return this.__topicStubsHeader; },
	       "topicStubsFrame" : function(){ return this.__frame + " " + this.__topicStubsFrame; },
	       "typeFrame" : function(){ return this.__typeFrame; },
	       "valueFrame" : function(){ return this.__valueFrame; },
	       "variantFrame" : function(){ return this.__frame + " " + this.__variantFrame; },
	       "variantHeader" : function(){ return this.__variantHeader; },
	       "variantsFrame" : function(){ return this.__frame + " " + this.__variantsFrame; }
	      };