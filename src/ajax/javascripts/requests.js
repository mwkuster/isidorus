//+-----------------------------------------------------------------------------
//+  Isidorus
//+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
//+
//+  Isidorus is freely distributable under the LLGPL license.
//+  This ajax module uses the frameworks PrototypeJs and Scriptaculous, both
//+  are distributed under the MIT license.
//+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt, and
//+  trunk/docs/LGPL-LICENSE.txt in
//+  trunk/src/ajax/javascripts/external/MIT-LICENSE.txt.
//+-----------------------------------------------------------------------------


// --- replaces every / character that is not prefixed by a \ character
function escapeSlashInJSON(jsonString){
    return jsonString.replace(/([^\\])\//g, '$1\\/').replace(/([^\\])\//g, '$1\\/');
}


// --- Sets a timeout function which alerts a message.
function setAjaxTimeout(time, url)
{
    return setTimeout(function(){
	alert("The AJAX request for \"" + url + "\" timed out. Please check your network connection!");
	hideLoad();
    }, time);
}


// --- Returns a function whihc can be used as an XHR-Handler.
// --- The returned function is the passed handler wrapped in
// --- a lambda-function which additionally clears the passed timeout
// --- function and call onLoad.
function createXHRHandler(handler, timeFun)
{
    function fun(xhr){
	clearTimeout(timeFun);
	hideLoad();
	handler(xhr);
    }
    return fun;
}


// --- Removes all divs with the class ajaxLoader. The inner image with the
// --- class ajaxLoader will be moved to the top of div.content and the
// --- display attribute will be set to none;
function hideLoad()
{
    var img = $$("img." + CLASSES.ajaxLoader());
    if(img.length === 1){
	img[0].setStyle({"display" : "none"})
	$("page").insert({"top" : img[0]});
    }

    var loading = $$("div." + CLASSES.load());
    if(loading.length === 1) loading[0].remove();
    var content = $$("div." + CLASSES.content());
    if(content.length === 1) content[0].show();
}


// --- The hidden image with the class ajaxLoader will be moved to the new created
// --- div with the given message. The div with the class content will be hidden and instaed
// --- of the hidden div there will be shown the new created element.
function onLoad(text)
{
    var div = new Element("div", {"class" : CLASSES.load()}).update(content);
    var content = $$("div." + CLASSES.content());
    if(content.length === 1){
	content[0].hide();
	var load = new Element("div", {"class" : CLASSES.load()}).update("<br/><br/>" + text);
	content[0].insert({"before" : load});
	var img = $$("img." + CLASSES.ajaxLoader());
	if(img.length === 1){
	    img[0].setStyle({"display" : "block"})
	    load.insert({"top" : img[0]})
	}
    }
}


// --- This is the default error handler of the used ajax.requests.
function defaultFailureHandler(xhr)
{
    window.alert("Something went wrong by calling \"" + xhr.request.url + "\"\n" + xhr.status +
		 ": " + xhr.statusText + "\n" + xhr.responseText);
}


// --- Gets all psis from the server. If typePsis is set to true
// --- there will be requested only TopicType's psis.
function getPsis(onSuccessHandler, onFailureHandler, what)
{
    try{
	var onFailure = onFailureHandler ? onFailureHandler : defaultFailureHandler;
	var timeFun = setAjaxTimeout(TIMEOUT, TYPE_PSIS_URL);
	
	var url = ALL_PSIS_URL;
	var message = "Requesting all type PSIs";
	if(what && what.types && what.types === true) url = TYPE_PSIS_URL;
	else if(what && what.instances && what.instances === true){
	    url = INSTANCE_PSIS_URL;
	    message = "Requesting all instance PSIs";
	}
	else if(what && what.all && what.all === true){
	    url = ALL_PSIS_URL;
	    message = "Requesting all PSIs";
	}

	onLoad(message);

	new Ajax.Request(url, {
	    "method" : "get",
	    "requestHeaders" : INIT_DATE,
	    "onSuccess" : createXHRHandler(onSuccessHandler, timeFun),
	    "onFailure" : createXHRHandler(onFailure, timeFun)});
    }
    catch(err){
	alert("From getTypePsis(): could not request all type PSIs, please try again!\n\n" + err);
    }
}


// --- Sends a post-request to the server with the passed psis as postBody.
// --- Gets a constraint-object.
function requestConstraints(psis, onSuccessHandler, onFailureHandler, typeConstraints)
{
    try{
	var onFailure = onFailureHandler ? onFailureHandler : defaultFailureHandler;
	var timeFun = setAjaxTimeout(TIMEOUT, TMCL_TYPE_URL);
	onLoad("Requesting all constraints for the psis:\<br/>" + psis.gsub("\\[", "").gsub("\\]", ""));

	url = TMCL_INSTANCE_URL;
	if(typeConstraints === true) url = TMCL_TYPE_URL;

	new Ajax.Request(url, {
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
			alert("From getTopicStubs(): Could not request topicStub information for \"" + xhr.request.url + "\"!!!");
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
		hideLoad();
		onFailureHandler();
		return;
	    }

	    if(topicStubs.length === psis.length){
		hideLoad();
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
	    "postBody" : escapeSlashInJSON(json),
	    "onSuccess" : createXHRHandler(onSuccessHandler, timeFun),
	    "onFailure" : createXHRHandler(onFailure, timeFun)});
    }
    catch(err){
	alert("From commitFragment(): " + err);
    }
}


// --- Sends a DELETE-Message to the server. The sent message enables the server
// --- to find the spcified object and mark it as deleted
function commitDeletedObject(json, onSuccessHandler, onFailureHandler)
{
    if(!json || !onSuccessHandler) throw "From commitDeletedObject(): json and onSuccessHandler must be set!";
    try{
	var onFailure = onFailureHandler ? onFailureHandler : defaultFailureHandler;
	var timeFun = setAjaxTimeout(TIMEOUT, COMMIT_URL);
	new Ajax.Request(MARK_AS_DELETED_URL, {
	    "method" : "delete",
	    "postBody" : escapeSlashInJSON(json),
	    "onSuccess" : createXHRHandler(onSuccessHandler, timeFun),
	    "onFailure" : createXHRHandler(onFailure, timeFun)});
    }
    catch(err){
	alert("From commitDeletedObject(): " + err);
    }
}


// --- Requests a JSON-Fragment for the passed psi and calls the onSuccessHandler function
// --- after a succeeded request.
function requestFragment(psi, onSuccessHandler, onFailureHandler)
{
    if(!psi || !onSuccessHandler) throw "From requestFragment(): psi and onSuccessHandler must be set!";

    try{
	var onFailure = onFailureHandler ? onFailureHandler : defaultFailureHandler;
	var timeFun = setAjaxTimeout(TIMEOUT, COMMIT_URL);
	onLoad("Requesting fragment for the topic " + psi);
	
	var url = GET_PREFIX + psi.gsub("#", "%23");

	new Ajax.Request(url, {
	    "method" : "get",
	    "requestHeaders" : INIT_DATE,
	    "onSuccess" : createXHRHandler(onSuccessHandler, timeFun),
	    "onFailure" : createXHRHandler(onFailure, timeFun)});
    }
    catch(err){
	alert("From requestFragment(): " + err);
    }
}


// --- Request a topic map overview object from the server and calls
// --- onSuccessHandler or OnFailureHandler.
function requestTreeView(onSuccessHandler, onFailureHandler)
{
    if(!onSuccessHandler) throw "From requestTreeView(): onSuccessHandler must be set!";

    try{
	var onFailure = onFailureHandler ? onFailureHandler : defaultFailureHandler;
	var timeFun = setAjaxTimeout(6 * TIMEOUT, COMMIT_URL);
	onLoad("Requesting a topic map overview from " + TM_OVERVIEW);
	
	new Ajax.Request(TM_OVERVIEW, {
	    "method" : "get",
	    "requestHeaders" : INIT_DATE,
	    "onSuccess" : createXHRHandler(onSuccessHandler, timeFun),
	    "onFailure" : createXHRHandler(onFailure, timeFun)});
    }
    catch(err){
	alert("From requestFragment(): " + err);
    }
}