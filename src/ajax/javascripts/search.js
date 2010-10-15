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

function makeSearch(psi)
{
    var content = new Element("div", {"class" : CLASSES.content()});
    var header = new Element("h1").update("Search a Topic");
    content.insert({"bottom" : header});
    $(CLASSES.subPage()).insert({"bottom" : content});
}
