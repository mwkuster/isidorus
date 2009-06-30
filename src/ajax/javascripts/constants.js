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
var HOST_PREF = "http://143.93.190.237:8000/"; // "http://localhost:8000/"; // of the form "http://(.+)/"
var GET_PREFIX = HOST_PREF + "json/get/";
var GET_STUB_PREFIX = HOST_PREF + "json/topicstubs/";
var TMCL_TYPE_URL = HOST_PREF + "json/tmcl/type/";
var TMCL_INSTANCE_URL = HOST_PREF + "json/tmcl/instance/";
var COMMIT_URL = HOST_PREF + "json/commit/";
var ALL_PSIS_URL = HOST_PREF + "json/psis/";
var TYPE_PSIS_URL = HOST_PREF + "json/tmcl/types/";
var INSTANCE_PSIS_URL = HOST_PREF + "json/tmcl/instances/";
var OWN_URL = HOST_PREF + "isidorus";
var SUMMARY_URL = HOST_PREF + "json/summary"
var TM_OVERVIEW = "/json/tmcl/overview/";
var TIMEOUT = 10000; // const TIMEOUT = 10000 --> "const" doesn't work under IE



// --- A kind of enum for the the different pages with an attribute and a value
var PAGES = {"home" : "home", "search" : "searchTopic", "edit" : "editTopic", "create" : "createTopic", "current" : ""};

var ANY_URI = "http://www.w3.org/2001/XMLSchema#anyURI";
var STRING = "http://www.w3.org/2001/XMLSchema#string";
var CURRENT_TOPIC = "**current-topic**";
var CURRENT_TOPIC_ESCAPED = "\\*\\*current-topic\\*\\*";
var INIT_DATE = ["If-Modified-Since", "Thu, 1 Jan 1970 00:00:00 GMT"];
var MAX_INT = "MAX_INT";
var MMAX_INT = "*";


// --- Contains most css classes used in this project
// --- There should be called only the function to be sure to don't override
// --- the original values.
var CLASSES = {"page" : function(){ return "page"; },
	       "subPage" : function(){ return "subPage"; },
	       "content" : function(){ return "content"; },
	       "textrowWithRemoveButton" : function(){ return "textrowWithRemoveButton"; },
	       "textrowWithoutRemoveButton" : function(){ return "textrowWithoutRemoveButton"; },
	       "selectrowWithRemoveButton" : function(){ return "selectrowWithRemoveButton"; },
	       "selectrowWithoutRemoveButton" : function(){ return "selectrowWithoutRemoveButton"; },
	       "clickable" : function(){ return "clickable"; },
	       "error" : function(){ return "errorMessage" },
	       "fragmentFrame" : function(){ return "fragmentFrame"; },
	       "topicFrame" : function(){ return "topicFrame"; },
	       "topicIdFrame" : function(){ return "topicIdFrame"; },
	       "content" : function(){ return "content"; },
	       "description" : function(){ return "description"; },
	       "instanceOfFrame" : function(){ return "instanceOfFrame"; },
	       "itemIdentityFrame" : function(){ return "itemIdentityFrame"; },
	       "subjectLocatorFrame" : function(){ return "subjectLocatorFrame"; },
	       "subjectIdentifierFrame" : function(){ return "subjectIdentifierFrame"; },
	       "nameContainer" : function(){ return "nameContainer"; },
	       "nameFrame" : function(){ return "nameFrame"; },
	       "controlColumn" : function(){ return "controlColumn"; },
	       "showHiddenRows" : function(){ return "showHiddenRows"; },
	       "typeFrame" : function(){ return "typeFrame"; },
	       "scopeFrame" : function(){ return "scopeFrame"; },
	       "scopeContainer" : function(){ return "scopeContainer"; },
	       "valueFrame" : function(){ return "valueFrame"; },
	       "variantFrame" : function(){ return "variantFrame"; },
	       "variantContainer" : function(){ return "variantContainer"; },
	       "datatypeFrame" : function(){ return "datatypeFrame"; },
	       "occurrenceContainer" : function(){ return "occurrenceContainer"; },
	       "occurrenceFrame" : function(){ return "occurrenceFrame"; },
	       "associationContainer" : function(){ return "associationContainer"; },
	       "associationFrame" : function(){ return "associationFrame"; },
	       "roleContainer" : function(){ return "roleContainer"; },
	       "roleFrame" : function(){ return "roleFrame"; },
	       "playerFrame" : function(){ return "playerFrame"; },
	       "commitButton" : function(){ return "commitButton"; },
	       "tmIdFrame" : function(){ return "tmIdFrame"; },
	       "load" : function(){ return "loadFrame"; },
	       "ajaxLoader" : function(){ return "ajaxLoader"; },
	       "editFrame" : function(){ return "editFrame"; },
	       "disabled" : function(){ return "disabled"; },
	       "node" : function(){ return "treeNode"; },
	       "tree" : function(){ return "tree"; },
	       "treeView" : function(){ return "treeView"; },
	       "instances" : function(){ return "instances"; },
	       "subtypes" : function(){ return "subtypes"; },
	       "topicPsis" : function(){ return "topicPsis"; }
	      };