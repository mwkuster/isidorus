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


function makeEdit(psi)
{
    var content = new Element("div", {"class" : CLASSES.content()});
    var header = new Element("h1").update("Edit a Topic");
    $(CLASSES.subPage()).insert((content.insert(header, {"position" : "bottom"})), {"position" : "bottom"});
}
