package us.isidor.gdl.anaToMia.Widgets.text;

import java.util.ArrayList;

import com.google.gwt.event.dom.client.MouseOutEvent;
import com.google.gwt.event.dom.client.MouseOutHandler;
import com.google.gwt.event.dom.client.MouseOverEvent;
import com.google.gwt.event.dom.client.MouseOverHandler;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;


public class GdlInfo extends GdlTextObject {
	GdlVisibleObject parent = null;
	boolean parentListenerSet = false;
	
	// some constructors
	protected GdlInfo(){
		super();
	}
	
	
	public GdlInfo(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		RootPanel.get().add(this);
		super.setGdlStyle();
		// TODO: create an info element for each tm construct
		this.createNewInfo().setText("Info");
		this.hide();
		this.setParentListener();
	}
	
	
	@Override
	public void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		this.createNewInfo().setText(value);
	}
	
	
	private Label createNewInfo() throws ExecutionException, InvalidGdlSchemaException{
		Label lbl = new Label();
		this.addToContainerPanel(lbl);
		this.setGdlStyle(lbl);		
		return lbl;
	}
	
	
	@Override
	@Deprecated
	public void fixValue(){
		// has no effect on this element
	}
	
	
	private void setParentListener(){
		if(this.parent != null && !this.parentListenerSet){
			parent.addMouseOverHandler(new MouseOverHandler() {
				@Override
				public void onMouseOver(MouseOverEvent event) {
					try {
						GdlInfo.this.show();
					} catch (InvalidGdlSchemaException e) {
						e.printStackTrace();
					}
				}
			});
			parent.addMouseOutHandler(new MouseOutHandler() {				
				@Override
				public void onMouseOut(MouseOutEvent event) {
					GdlInfo.this.hide();
				}
			});			
			this.parentListenerSet = true;
		}
	}
	
	
	public void setPosition(GdlVisibleObject parent) throws InvalidGdlSchemaException{
		this.parent = parent;
		int left = parent.getAbsoluteLeft() + 5;
		int top = parent.getAbsoluteTop() + parent.getOffsetHeight() + 1;
		DOM.setStyleAttribute(this.mainPanel.getElement(), "position", "absolute");
		DOM.setStyleAttribute(this.mainPanel.getElement(), "top", top + "px");
		DOM.setStyleAttribute(this.mainPanel.getElement(), "left", left + "px");
		this.setParentListener();
		this.hide();
	}
	
	
	public void show() throws InvalidGdlSchemaException{
		DOM.setStyleAttribute(this.mainPanel.getElement(), "display", this.getDisplay().getCssName());
	}
	
	
	public void hide(){
		DOM.setStyleAttribute(this.mainPanel.getElement(), "display", "none");
	}
	
	
	@Override
	@Deprecated
	public ArrayList<String> getSelectedValues(){
		return new ArrayList<String>();
	}
}
