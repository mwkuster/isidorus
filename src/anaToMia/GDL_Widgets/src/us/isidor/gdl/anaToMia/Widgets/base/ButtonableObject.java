package us.isidor.gdl.anaToMia.Widgets.base;

import java.util.ArrayList;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.button.GdlActionButton;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Widget;


public class ButtonableObject extends Composite {
	private HorizontalPanel mainPanel = new HorizontalPanel();
	
	
	private ButtonableObject(){
		initWidget(this.mainPanel);		
	}
	
	
	public ButtonableObject(Widget mainObject) throws ExecutionException {
		this();
		if(mainObject == null) throw new ExecutionException("mainObject must not be set to null!");
		if(this.mainPanel != null) this.mainPanel.add(mainObject);
	}
	
	
	public Widget getMainObject(){
		if(this.mainPanel.getWidgetCount() != 0) return this.mainPanel.getWidget(0);
		else return null;
	}
	
	
	public void addButton(GdlActionButton button){
		this.mainPanel.add(button);
	}
	
	
	public boolean removeButton(GdlActionButton button){
		return this.mainPanel.remove(button);
	}
	
	
	public void removeAllButtons(){
		while(this.mainPanel.getWidgetCount() > 1){
			this.mainPanel.remove(this.mainPanel.getWidgetCount() - 1);
		}
	}
	
	
	public boolean containsButton(Topic tmRepresentative){
		for(int i = 1; i < this.mainPanel.getWidgetCount(); ++i)
			if(((GdlActionButton)this.mainPanel.getWidget(i)).getTmRepresentative().equals(tmRepresentative)) return true;
		
		return false;
	}
	
	
	public boolean containsButton(GdlActionButton button){
		for(int i = 1; i < this.mainPanel.getWidgetCount(); ++i)
			if(this.mainPanel.getWidget(i).equals(button)) return true;
		
		return false;
	}
	
	
	public void removeObsoleteButtons(ArrayList<Topic> activeButtons){
		if(activeButtons == null) return;
		
		ArrayList<GdlActionButton> buttonsToRemove = new ArrayList<GdlActionButton>();
		for(int i = 1; i < this.mainPanel.getWidgetCount(); ++i)
			if(!activeButtons.contains(this.mainPanel.getWidget(i))) buttonsToRemove.add((GdlActionButton)this.mainPanel.getWidget(i));
		
		for (GdlActionButton buttonToRemove : buttonsToRemove)
			this.mainPanel.remove(buttonToRemove);
	}
}
