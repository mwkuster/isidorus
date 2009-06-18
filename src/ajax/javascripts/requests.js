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




// --- Sets a timeout function which alerts a message.
function setAjaxTimeout(time, url)
{
    return setTimeout(function(){
	alert("The AJAX request for \"" + url + "\" timed out. Please check your network connection!");
    }, time);
}


// --- Returns a function whihc can be used as an XHR-Handler.
// --- The returned function is the passed handler wrapped in
// --- a lambda-function which additionally clears the passed timeout
// --- function.
function createXHRHandler(handler, timeFun)
{
    function fun(xhr){
	try{
	    clearTimeout(timeFun);
	    var loading = $$("div." + CLASSES.load());
	    if(loading.length === 1) loading[0].remove();
	    var content = $$("div." + CLASSES.content());
	    if(content.length === 1) content[0].show();
	    handler(xhr);
	}
	catch(err) {alert("err: " + err); }
    }
    return fun;
}


function onLoad(text)
{
    var div = new Element("div", {"class" : CLASSES.load()}).update(content);
    var content = $$("div." + CLASSES.content());
    if(content.length === 1){
	content[0].hide();
	var load = new Element("div", {"class" : CLASSES.load()}).update(text);
	content[0].insert({"before" : load});
    }
}


// --- This is the default error handler of the used ajax.requests.
function defaultFailureHandler(xhr)
{
    window.alert("Something went wrong by calling \"" + xhr.request.url + "\"\n" + xhr.status +
		 ": " + xhr.statusText + "\n" + xhr.responseText);
}


// --- Gets all type psis from the server.
function getTypePsis(onSuccessHandler, onFailureHandler)
{
    try{
	var onFailure = onFailureHandler ? onFailureHandler : defaultFailureHandler;
	var timeFun = setAjaxTimeout(TIMEOUT, TYPE_PSIS_URL);
	onLoad("Requesting all Type PSIs");

	new Ajax.Request(TYPE_PSIS_URL, {
	    "method" : "get",
	    "requestHeaders" : INIT_DATE,
	    "onSuccess" : createXHRHandler(onSuccessHandler, timeFun),
	    "onFailure" : createXHRHandler(onFailure, timeFun)});
    }
    catch(err){
	alert("Could not request all type PSIs, please try again!\n\n" + err);
    }
}


// --- Sends a post-request to the server with the passed psis as postBody.
// --- Gets a constraint-object.
function requestConstraints(psis, onSuccessHandler, onFailureHandler)
{
    try{
	var onFailure = onFailureHandler ? onFailureHandler : defaultFailureHandler;
	var timeFun = setAjaxTimeout(TIMEOUT, TMCL_TYPE_URL);
	onLoad("Requesting all constraints for psis:\<br/>" + psis.gsub("\\[", "").gsub("\\]", ""));

	new Ajax.Request(TMCL_TYPE_URL, {
	    "method" : "post",
	    "postBody" : psis,
	    "onSuccess" : createXHRHandler(onSuccessHandler, timeFun),
	    "onFailure" : createXHRHandler(onFailure, timeFun)});
    }
    catch(err){
	alert("Could not request contraints, please try again!\n\n" + err);
    }
}


// --- gets all topicStubs information for the passed psis and
// --- executes the onSuccessHandler or the on FailureHandler
// --- if all stubs are requested or one request fails.
function getTopicStubs(psis, onSuccessHandler, onFailureHandler)
{
    if(!onSuccessHandler || !onFailureHandler) throw "From getTopicStubs(): onsuccessHandler and onFailureHandler must be set!";
    try{
	var topicStubs = new Array();

	if(psis && psis.length !== 0){
	    onLoad("Requesting topicStubs information for<br/>" + psis);
	    for(var i = 0; i !== psis.length; ++i){
		var url = GET_STUB_PREFIX + psis[i].gsub("#", "%23");
		new Ajax.Request(url, {
		    "method" : "get",
		    "requestHeaders" : INIT_DATE,
		    "onSuccess" : function(xhr){
			if(xhr.responseText.length === 0 || xhr.responseText.isJSON() === false) errorHandler("Got bad JSON-Data for \"" + psis[i] + "\"!");
			else topicStubs.push(xhr.responseText);
		    },
		    "onFailure" : function(xhr){
			alert("From getTopicStubs(): Could not equest topicStub information for \"" + xhr.request.url + "\"!!!");
			onFailureHandler();
		    }});
	    }
	}

	// --- Checks the requested value. If there are all values requested, there will be called the
	// --- onSuccessHandler - otherwise (after the maximum time out or an faild request) there will
	// --- be called the onErrorHandler.
	var maxTimeout = psis.length * TIMEOUT;
	var neededTime = 0;
	function checkRequests(){
	    var delta = 100;
	    neededTime += delta;
	    if(delta > maxTimeout && psis && psis.length !== 0){
		alert("From getTopicStubs(): Please check your network-connection - the request timed out!!!");
		var loading = $$("div." + CLASSES.load());
		if(loading.length === 1) loading[0].remove();
		var content = $$("div." + CLASSES.content());
		if(content.length === 1) content[0].show();
		onFailureHandler();
		return;
	    }

	    if(topicStubs.length === psis.length){
		var loading = $$("div." + CLASSES.load());
		if(loading.length === 1) loading[0].remove();
		var content = $$("div." + CLASSES.content());
		if(content.length === 1) content[0].show();
		onSuccessHandler(topicStubs);
	    }
	    else setTimeout(checkRequests, delta);
	}

	checkRequests();

    }
    catch(err){
	alert("From getTopicStubs(): Could not request topicStubs information for: " + psis + "\n\n" + err);
    }
}


// --- Sends a POST-Message to the server with the fragment data which hast to be committed.
function commitFragment(json, onSuccessHandler, onFailureHandler)
{
    if(!json || !onSuccessHandler) throw "From commitFragment(): json and onSuccessHandler must be set!";
    try{
	var onFailure = onFailureHandler ? onFailureHandler : defaultFailureHandler;
	var timeFun = setAjaxTimeout(TIMEOUT, COMMIT_URL);
	onLoad("Committing current fragment to " + COMMIT_URL);
	
	new Ajax.Request(COMMIT_URL, {
	    "method" : "post",
	    "postBody" : json,
	    "onSuccess" : createXHRHandler(onSuccessHandler, timeFun),
	    "onFailure" : createXHRHandler(onFailure, timeFun)});
    }
    catch(err){
	alert("From commitFragment(): " + err);
    }
}