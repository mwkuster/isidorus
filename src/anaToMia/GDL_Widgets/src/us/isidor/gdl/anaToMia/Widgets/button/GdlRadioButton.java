package us.isidor.gdl.anaToMia.Widgets.button;


import java.util.ArrayList;

import com.google.gwt.user.client.ui.RadioButton;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;


public class GdlRadioButton extends GdlInputButton {	
	protected GdlRadioButton(){
		super();
	}
	
	
	public GdlRadioButton(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		// TODO: create a radio button for each tm construct
		this.createRadioButton().setText("Radio Button 1");
		this.createRadioButton().setText("Radio Button 2");
		this.createRadioButton().setText("Radio Button 3");
		this.setNthButtons();
	}
	
	
	public RadioButton createRadioButton() throws InvalidGdlSchemaException, ExecutionException{
		RadioButton rb = new RadioButton(this.getGroupName());
		this.addToContainerPanel(rb);
		this.setGdlStyle(rb);
		return rb;
	}


	@Override
	public void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		// TODO: implement
	}
	
	
	@Override
	public ArrayList<String> getSelectedValues(){
		// TODO: implement
		return new ArrayList<String>();
	}
	
	
	@Override
	public void fixValue(){
		// TODO: implement
	}
}
