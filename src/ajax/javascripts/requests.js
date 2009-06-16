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
	
	new Ajax.Request(TYPE_PSIS_URL, {
	    "method" : "get",
	    "requestHeaders" : ["If-Modified-Since", "Thu, 1 Jan 1970 00:00:00 GMT"],
	    "onSuccess" : onSuccessHandler,
	    "onFailure" : onFailure});
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
	
	new Ajax.Request(TMCL_TYPE_URL, {
	    "method" : "post",
	    "postBody" : psis,
	    "onSuccess" : onSuccessHandler,
	    "onFailure" : onFailure});
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

	if(psis){
	    for(var i = 0; i !== psis.length; ++i){
		var url = GET_STUB_PREFIX + psis[i].gsub("#", "%23");
		new Ajax.Request(url, {
		    "method" : "get",
		    "requestHeaders" : ["If-Modified-Since", "Thu, 1 Jan 1970 00:00:00 GMT"],
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
	    if(delta > maxTimeout){
		alert("From getTopicStubs(): Please check your network-connection - the request timed out!!!");
		onFailureHandler();
		return;
	    }

	    if(topicStubs.length === psis.length) onSuccessHandler(topicStubs);
	    else setTimeout(checkRequests, delta);
	}

	checkRequests();

    }
    catch(err){
	alert("From getTopicStubs(): Could not request topicStubs information for: " + psis + "\n\n" + err);
    }
}