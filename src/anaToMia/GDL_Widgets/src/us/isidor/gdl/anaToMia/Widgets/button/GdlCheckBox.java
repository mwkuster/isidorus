package us.isidor.gdl.anaToMia.Widgets.button;

import java.util.ArrayList;

import com.google.gwt.user.client.ui.CheckBox;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;


public class GdlCheckBox extends GdlInputButton {
	protected GdlCheckBox(){
		super();
	}
	
	
	public GdlCheckBox(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		// TODO: create a check box for each tm construct
		this.createcheckBox().setText("Check Box 1");
		this.createcheckBox().setText("Check Box 2");
		this.createcheckBox().setText("Check Box 3");
		this.setNthButtons();
	}
	
	
	
	public CheckBox createcheckBox() throws InvalidGdlSchemaException, ExecutionException{
		CheckBox cb = new CheckBox();
		cb.setName(this.getGroupName());
		this.addToContainerPanel(cb);
		this.setGdlStyle(cb);
		return cb;
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
