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


// --- adds some event handlers to the navigation elements
function addHandlersToNavi()
{
    $(PAGES.home).observe("click", function(event){ setNaviClasses(event.element()); makePage(PAGES.home, ""); });
    $(PAGES.search).observe("click", function(event){ setNaviClasses(event.element()); makePage(PAGES.search, ""); });
    $(PAGES.edit).observe("click", function(event){ setNaviClasses(event.element());	makePage(PAGES.edit, ""); });
    $(PAGES.create).observe("click", function(event){ setNaviClasses(event.element()); makePage(PAGES.create, ""); });

    // --- necessary for the first call of the page
    makePage(PAGES.home, "");
}


// --- Sets the classes of all navi-elements to the default class.
// --- The currently clicked element is set to "isActive".
function setNaviClasses(activeNaviElement)
{
    $(PAGES.home).writeAttribute({"class" : "clickableButton"});
    $(PAGES.search).writeAttribute({"class" : "clickableButton"});
    $(PAGES.edit).writeAttribute({"class" : "clickableButton"});
    $(PAGES.create).writeAttribute({"class" : "clickableButton"});
    activeNaviElement.writeAttribute({"class" : "isActive"});
}


// --- generates the current page depending on the variable __currentPage
function makePage(newPage, psi)
{
    // --- if there is called the subpage which is already displayed
    // --- there will be done nothing!
    if(newPage === PAGES.current) return;
    PAGES.current = newPage;

    // --- removes the old content
    hideLoad(); // to prevent the load-gif of beeing removed when it is used in the error-div
    $(CLASSES.subPage()).update();

    // --- creates the new content
    switch(newPage){
    case PAGES.home:
	makeHome();
	break;
    case PAGES.search:
	makeSearch(psi);
	break;
    case PAGES.edit:
	makeEdit(psi)
	break;
    case PAGES.create:
	makeCreate(psi);
	break;
    }
}


document.observe("dom:loaded", addHandlersToNavi);