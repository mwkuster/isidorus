package us.isidor.gdl.anaToMia.Widgets.base;


import us.isidor.gdl.anaToMia.Widgets.isidorus.LoadScreenPanel;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.RootPanel;


public class TestClass implements EntryPoint{
	@Override
	public void onModuleLoad() {
		RootPanel.get().setHeight("1500px");
		RootPanel.get().setWidth("2500px");
		Window.enableScrolling(false);
		final LoadScreenPanel lsp = new LoadScreenPanel("Title", "message");
		RootPanel.get().add(lsp);
		
		Timer t = new Timer() {
			@Override
			public void run() {
				lsp.removeFromParent();
				Window.enableScrolling(true);
			}
		};
		
		t.schedule(5000);
		
		
	}
}
