package us.isidor.gdl.anaToMia.Widgets.button;


import java.util.ArrayList;
import com.google.gwt.user.client.ui.RadioButton;
import com.google.gwt.user.client.ui.Widget;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.ButtonableObject;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;


public class GdlRadioButton extends GdlInputButton {	
	protected GdlRadioButton(){
		super();
	}
	
	
	public GdlRadioButton(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		
		if(receivedData != null && this.getConstraint() != null) this.setReceivedData();
		else this.setDefaultValue();
		
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
		RadioButton rb = this.createRadioButton();
		rb.setText(value);
		rb.setValue(true);
	}
	
	public void addUncheckedSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		this.createRadioButton().setText(value);
	}
	
	
	@Override
	public ArrayList<String> getSelectedValues(){
		ArrayList<String> result = new ArrayList<String>();
		
		for (Widget elem : this.subElements) {
			ButtonableObject bo = (ButtonableObject)elem;
			Widget wdgt = bo.getMainObject();
			if((wdgt instanceof RadioButton) && ((RadioButton)wdgt).getValue()) result.add(((RadioButton)wdgt).getText());
		}
		
		return result;
	}
	
	
	@Override
	public void fixValue(){
		for (Widget elem : this.subElements) {
			ButtonableObject bo = (ButtonableObject)elem;
			Widget wdgt = bo.getMainObject();
			if(wdgt instanceof RadioButton){
				RadioButton rb = (RadioButton)wdgt;
				rb.setEnabled(false);
			}
		}
	}
}
