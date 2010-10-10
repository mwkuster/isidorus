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