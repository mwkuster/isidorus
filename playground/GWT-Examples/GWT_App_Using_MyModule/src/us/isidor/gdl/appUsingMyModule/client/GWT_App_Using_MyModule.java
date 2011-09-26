package us.isidor.gdl.appUsingMyModule.client;

import us.isidor.gdl.mymodule.client.MyWidget;
import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.RootPanel;

public class GWT_App_Using_MyModule implements EntryPoint{

	@Override
	public void onModuleLoad() {
		Window.alert("Test !!!");
		RootPanel.get("GWT_Content").add(new MyWidget());
	}
}
