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


// --- Some constants fot the http connections via the XMLHttpRequest-Object
var TIMEOUT = 10000; // const TIMEOUT = 10000 --> "const" doesn't work under IE
var HOST_PREF = "http://localhost:8000/";
var GET_PREFIX = HOST_PREF + "json/get/";
var GET_STUB_PREFIX = HOST_PREF + "json/topicstubs/";
var TMCL_TYPE_URL = HOST_PREF + "json/tmcl/type/";
var TMCL_INSTANCE_URL = HOST_PREF + "json/tmcl/instance/";
var COMMIT_URL = HOST_PREF + "json/commit/";
var ALL_PSIS_URL = HOST_PREF + "json/psis/";
var TYPE_PSIS_URL = HOST_PREF + "json/tmcl/types/";
var OWN_URL = HOST_PREF + "isidorus";
var SUMMARY_URL = HOST_PREF + "json/summary"



// --- A kind of enum for the the different pages with an attribute and a value
var PAGES = {"home" : "home", "search" : "searchTopic", "edit" : "editTopic", "create" : "createTopic", "current" : ""};

var ANY_URI = "http://www.w3.org/2001/XMLSchema#anyURI";
var STRING = "http://www.w3.org/2001/XMLSchema#string";
var CURRENT_TOPIC = "**current-topic**";
var CURRENT_TOPIC_ESCAPED = "\\*\\*current-topic\\*\\*";

// --- Contains most css classes used in this project
// --- There should be called only the function to be sure to don't override
// --- the original values.
var CLASSES = {"__divPage__" : "page",
	       "__divSubPage__" : "subPage",
	       "__divContent__" : "content",
	       "__divTextrowWithRemoveButton__" : "textrowWithRemoveButton",
	       "__divTextrowWithoutRemoveButton__" : "textrowWithoutRemoveButton",
	       "__divSelectrowWithRemoveButton__" : "selectrowWithRemoveButton",
	       "__divSelectrowWithoutRemoveButton__" : "selectrowWithoutRemoveButton",
	       "__spanClickable__" : "clickable",
	       "__notVisible__" : "notvisible",
	       "__divError__" : "errorMessage",
	       "__ulFragmentFrame__" : "fragmentFrame",
	       "__tableTopicFrame__" : "topicFrame",
	       "__trTopicIdFrame__" : "topicIdFrame",
	       "__tdContent__" : "content",
	       "__tdDescription__" : "description",
	       "__divInstanceOfFrame__" : "instanceOfFrame",
	       "__divItemIdentityFrame__" : "itemIdentityFrame",
	       "__divSubjectLocatorFrame__" : "subjectLocatorFrame",
	       "__divSubjectIdentifierFrame__" : "subjectIdentifierFrame",
	       "__divNameContainer__" : "nameContainer",
	       "__divNameFrame__" : "nameFrame",
	       "__trControlColumn__" : "controlColumn",
	       "__trShowHiddenRows__" : "showHiddenRows",
	       "__trTypeFrame__" : "typeFrame",
	       "__divScopeFrame__" : "scopeFrame",
	       "__divScopeContainer__" : "scopeContainer",
	       "__divValueFrame__" : "valueFrame",
	       "__divVariantFrame__" : "variantFrame",
	       "__divVariantContainer__" : "variantContainer",
	       "__divDatatypeFrame__" : "datatypeFrame",
	       "__divOccurrenceContainer__" : "occurrenceContainer",
	       "__divOccurrenceFrame__" : "occurrenceFrame",
	       "__divAssociationContainer__" : "associationContainer",
	       "__divAssociationFrame__" : "associationFrame",
	       "__divRoleContainer__" : "roleContainer",
	       "__divRoleFrame__" : "roleFrame",
	       "__divPlayerFrame__" : "playerFrame",
	       "__spanDeselect__" : "deselect",
	       "__divFog__" : "fog",
	       "__inputCommitButton__" : "commitButton",

	       "page" : function(){ return this.__divPage__; },
	       "subPage" : function(){ return this.__divSubPage__; },
	       "content" : function(){ return this.__divContent__; },
	       "textrowWithRemoveButton" : function(){ return this.__divTextrowWithRemoveButton__; },
	       "textrowWithoutRemoveButton" : function(){ return this.__divTextrowWithoutRemoveButton__; },
	       "selectrowWithRemoveButton" : function(){ return this.__divSelectrowWithRemoveButton__; },
	       "selectrowWithoutRemoveButton" : function(){ return this.__divSelectrowWithoutRemoveButton__; },
	       "clickable" : function(){ return this.__spanClickable__; },
	       "notVisible" : function(){ return this.__notVisible__; },
	       "error" : function(){ return this.__divError__; },	       "fragmentFrame" : function(){ return this.__ulFragmentFrame__; },
	       "topicFrame" : function(){ return this.__tableTopicFrame__; },
	       "topicIdFrame" : function(){ return this.__trTopicIdFrame__; },
	       "content" : function(){ return this.__tdContent__; },
	       "description" : function(){ return this.__tdDescription__; },
	       "instanceOfFrame" : function(){ return this.__divInstanceOfFrame__; },
	       "itemIdentityFrame" : function(){ return this.__divItemIdentityFrame__; },
	       "subjectLocatorFrame" : function(){ return this.__divSubjectLocatorFrame__; },
	       "subjectIdentifierFrame" : function(){ return this.__divSubjectIdentifierFrame__; },
	       "nameContainer" : function(){ return this.__divNameContainer__; },
	       "nameFrame" : function(){ return this.__divNameFrame__; },
	       "controlColumn" : function(){ return this.__trControlColumn__; },
	       "showHiddenRows" : function(){ return this.__trShowHiddenRows__; },
	       "typeFrame" : function(){ return this.__trTypeFrame__; },
	       "scopeFrame" : function(){ return this.__divScopeFrame__; },
	       "scopeContainer" : function(){ return this.__divScopeContainer__; },
	       "valueFrame" : function(){ return this.__divValueFrame__; },
	       "variantFrame" : function(){ return this.__divVariantFrame__; },
	       "variantContainer" : function(){ return this.__divVariantContainer__; },
	       "datatypeFrame" : function(){ return this.__divDatatypeFrame__; },
	       "occurrenceContainer" : function(){ return this.__divOccurrenceContainer__; },
	       "occurrenceFrame" : function(){ return this.__divOccurrenceFrame__; },
	       "associationContainer" : function(){ return this.__divAssociationContainer__; },
	       "associationFrame" : function(){ return this.__divAssociationFrame__; },
	       "roleContainer" : function(){ return this.__divRoleContainer__; },
	       "roleFrame" : function(){ return this.__divRoleFrame__; },
	       "playerFrame" : function(){ return this.__divPlayerFrame__; },
	       "deselect" : function(){ return this.__spanDeselect__; },
	       "fog" : function(){ return this.__divFog__; },
	       "commitButton" : function(){ return this.__inputCommitButton__; }
	      };