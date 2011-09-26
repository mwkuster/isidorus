package us.isidor.gdl.anaToMia.Widgets.isidorus;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.Window;

public class IsidorusEntryPoint implements EntryPoint {

	@Override
	public void onModuleLoad() {
		try{
			String requestedTopicType = IsidorusConstants.HASH_OBJECT_PSI;
			if(Window.Location.getHref().contains("environment")) requestedTopicType = IsidorusConstants.ENVIRONMENT_PSI;
			
			GdlWebPage page = new GdlWebPage(requestedTopicType);
			page.createWebPage();
		}catch(Exception e){
			Window.alert("could not create a web page, because: (" + e.getClass() + ") " + e.getMessage());
		}
	}

}
