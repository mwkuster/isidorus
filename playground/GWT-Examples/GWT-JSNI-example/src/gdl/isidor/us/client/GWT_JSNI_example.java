package gdl.isidor.us.client;


import gdl.isidor.us.TopicMaps.TmEngine;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.layout.client.Layout.Alignment;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.CaptionPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.LayoutPanel;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.AbsolutePanel;
import com.google.gwt.user.client.ui.ScrollPanel;
import com.google.gwt.user.client.ui.VerticalPanel;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class GWT_JSNI_example implements EntryPoint {
	private VerticalPanel mainPanel = new VerticalPanel();
	private AbsolutePanel absolutePanel = new AbsolutePanel();


	/**
	 * This is the entry point method.
	 */
	public void onModuleLoad() {
		mainPanel.setBorderWidth(1);

		int mpWidth = 500;
		int mpHeight = 500;
		mainPanel.setPixelSize(mpWidth, mpHeight);
		RootPanel.get("gwtCode").add(mainPanel);

		int apWidth = 290;
		int apHeight = 290;
		absolutePanel.setPixelSize(apWidth, apHeight);
		mainPanel.add(absolutePanel);
		DOM.setStyleAttribute(absolutePanel.getElement(), "marginLeft", "auto");
		DOM.setStyleAttribute(absolutePanel.getElement(), "marginRight", "auto");
		DOM.setStyleAttribute(absolutePanel.getElement(), "marginTop", (mpHeight - apHeight)/2 + "px");

		Button actionButton = new Button();
		actionButton.setText("process");
		absolutePanel.addStyleName("absolutePanel");

		absolutePanel.add(actionButton);
		absolutePanel.setWidgetPosition(actionButton, apWidth - actionButton.getOffsetWidth(), apHeight - actionButton.getOffsetHeight());


		actionButton.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				Window.alert("test 1");
				TmEngine tme = new TmEngine();
				Window.alert(tme.toString() + ": " + tme.createTM("http://isidor.us/test-tm").getLoator().getReference());
			}
		});

		createLayoutPanel(absolutePanel);
	} //endmethod
	
	
	
	public void createLayoutPanel(AbsolutePanel ap){
		LayoutPanel lp = new LayoutPanel();
		
		int lpWidth = 240;
		int lpHeight = 240;
		
		lp.setPixelSize(lpWidth, lpHeight);
		DOM.setStyleAttribute(lp.getElement(), "backgroundColor", "Grey");
		
		ap.add(lp,(ap.getOffsetWidth() - lpWidth)/2, 10);
		
		CaptionPanel cp = new CaptionPanel("CaptionPanel");
		lp.add(cp);
		lp.setWidgetLeftRight(cp, 5d, Unit.PX, 0d, Unit.PX);
		
		ScrollPanel sp = new ScrollPanel();
		DOM.setStyleAttribute(sp.getElement(), "backgroundColor", "Green");
		
		Label lbl = new Label();
		String lblTxt = "anyText";
		for(int i = 0; i != 10; ++i){
			lbl.setText(lbl.getText() + " " + lblTxt);
		}
		
		sp.add(lbl);
		lp.add(sp);
		
		lp.setWidgetBottomHeight(sp, 80d, Unit.PX, 50d, Unit.PX);
		lp.setWidgetLeftRight(sp, 150d, Unit.PX, 40d, Unit.PX);
	} //endmethod
}
