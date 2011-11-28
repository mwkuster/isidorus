package us.isidor.gdl.anaToMia.Widgets.button;

import java.util.ArrayList;
import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.Widget;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.ButtonableObject;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;


public class GdlCheckBox extends GdlInputButton {
	protected GdlCheckBox(){
		super();
	}
	
	
	public GdlCheckBox(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);

		if(receivedData != null && this.getConstraint() != null) this.setReceivedData();
		else this.setDefaultValue();
		
		this.setNthButtons();
	}
	
	
	
	public CheckBox createCheckBox() throws InvalidGdlSchemaException, ExecutionException{
		CheckBox cb = new CheckBox();
		cb.setName(this.getGroupName());
		this.addToContainerPanel(cb);
		this.setGdlStyle(cb);
		return cb;
	}


	@Override
	public void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		CheckBox cb = this.createCheckBox();
		cb.setText(value);
		cb.setValue(true);
	}
	
	
	public void addUncheckedSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		this.createCheckBox().setText(value);
	}
	
	
	@Override
	public ArrayList<String> getSelectedValues(){
		ArrayList<String> result = new ArrayList<String>();
		
		for (Widget elem : this.subElements) {
			ButtonableObject bo = (ButtonableObject)elem;
			Widget wdgt = bo.getMainObject();
			if((wdgt instanceof CheckBox) && ((CheckBox)wdgt).getValue()) result.add(((CheckBox)wdgt).getText());
		}
		
		return result;
	}
	
	
	@Override
	public void fixValue(){
		for (Widget elem : this.subElements) {
			ButtonableObject bo = (ButtonableObject)elem;
			Widget wdgt = bo.getMainObject();
			if(wdgt instanceof CheckBox){
				CheckBox cb = (CheckBox)wdgt;
				cb.setEnabled(false);
			}
		}
	}
}
