var TIMEOUT = 5000; // const TIMEOUT = 5000 --> "const" doesn't work under IE
var HOST_PREF = "http://localhost:8000/";
var GET_PREFIX = HOST_PREF + "json/get/";
var COMMIT_URL = HOST_PREF + "json/commit/";
var ALL_PSIS_URL = HOST_PREF + "json/psis/";
var OWN_URL = HOST_PREF + "isidorus";
var SUMMARY_URL = HOST_PREF + "json/summary"


// --- a kind of enum for the the different pages with an attribute and a value
var PAGES = {"home" : "home", "search" : "searchTopic", "edit" : "editTopic", "create" : "createTopic"};