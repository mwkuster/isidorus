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

function entryPoint(){
    var elem = getElem();
    var url = window.location.href;
    var urlFrags = url.split("/");
    var newUrl = "";
    for(var i = 0; i !== urlFrags.length; ++i){
	if (newUrl.length !== 0) newUrl += "/";
	newUrl += urlFrags[i];
    }
    elem.innerHTML = " " + newUrl;
}


function getElem(){
    return document.getElementById("content");
}