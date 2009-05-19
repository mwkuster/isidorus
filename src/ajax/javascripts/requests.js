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