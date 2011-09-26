package us.isidor.gdl.anaToMia.Widgets.isidorus;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.Window;


public class EnvironmentUi implements EntryPoint {
	@Override
	public void onModuleLoad() {
		try{
			GdlWebPage page = new GdlWebPage(IsidorusConstants.ENVIRONMENT_PSI);
			page.createWebPage();
		}catch(Exception e){
			Window.alert("could not create a web page, because: (" + e.getClass() + ") " + e.getMessage());
		}
	}
}
