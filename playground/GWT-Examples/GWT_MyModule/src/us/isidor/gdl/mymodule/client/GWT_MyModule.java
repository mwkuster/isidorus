package us.isidor.gdl.mymodule.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.RootPanel;


public class GWT_MyModule implements EntryPoint {
	private HorizontalPanel mainPanel = new HorizontalPanel();
	
	public void onModuleLoad() {		
		mainPanel.setBorderWidth(1);
		mainPanel.setPixelSize(1000, 500);
		DOM.setStyleAttribute(mainPanel.getElement(), "backgroundColor", "#DDDDDD");
		DOM.setStyleAttribute(mainPanel.getElement(), "marginTop", "200px");
		DOM.setStyleAttribute(mainPanel.getElement(), "marginLeft", "auto");
		DOM.setStyleAttribute(mainPanel.getElement(), "marginRight", "auto");
		RootPanel.get("GWT_Content").add(mainPanel);
		
		MyWidget myWidget = new MyWidget();
		
		mainPanel.add(myWidget);
	}
}
