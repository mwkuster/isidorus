package us.isidor.gdl.anaToMia.Widgets.isidorus;

import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTMLPanel;


public class LoadScreenPanel extends Composite {
	private HTMLPanel mainPanel = null;
	private HTMLPanel content = null;
	private static long instanceCounter = 0;
	
	public LoadScreenPanel(String title, String message){
		String localTitle = title == null ? "" : title;
		String localMessage = message == null ? "" : message;
		String mainPanelId = "GDL_LoadScreenPanel_id_" +  LoadScreenPanel.instanceCounter++;
		this.mainPanel = new HTMLPanel("<div id=\"" + mainPanelId + "\" style=\"position: absolute; z-index: 2147483646; left: 0px; top: 0px; right: 0px; bottom: 0px; opacity: 0.6; background-color: rgb(0, 0, 0);\"></div>");
		this.initWidget(this.mainPanel);
		this.content = new HTMLPanel("<div style=\"position: absolute; z-index: 2147483647; left: 50px; top: 50px; width: 600px; color: rgb(255, 255, 255); font-family: verdana; text-align: left;\"><div style=\"font-size: 30px; font-weight: bold;\">" + localTitle + "</div><div style=\"font-size: 15px;\">" + localMessage + "</div></div>");
		this.mainPanel.add(this.content, mainPanelId);
	}
}
