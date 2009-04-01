// --- adds some event handlers to the navigation elements
function addHandlersToNavi()
{
    $(PAGES.home).observe("click", function(){ makePage(PAGES.home, ""); });
    $(PAGES.search).observe("click", function(){ makePage(PAGES.search, ""); });
    $(PAGES.edit).observe("click", function(){	makePage(PAGES.edit, ""); });
    $(PAGES.create).observe("click", function(){ makePage(PAGES.create, ""); });

    // --- necessary for the first call of the page
    makePage(PAGES.home);
}


// --- generates the current page depending on the variable __currentPage
function makePage(newPage, psi)
{
    // --- removes the old content
    cleanPage(newPage);

    // --- creates the new content
    switch(newPage){
    case PAGES.home:
	makeHome("content", "topicTable", true);
	break;
    case PAGES.search:
	break;
    case PAGES.edit:
	makeEdit(psi);
	break;
    case PAGES.create:
	break;
    }
}


// --- removes all old DOM-Elements - if the page to create is not
// --- the old page
function cleanPage(newPage)
{
    $("content").childElements().each(function(nodeToDelete, idx)
				      {
					  if(!nodeToDelete.hasClassName(newPage))
					      nodeToDelete.remove();
				      });
}


document.observe("dom:loaded", addHandlersToNavi);

