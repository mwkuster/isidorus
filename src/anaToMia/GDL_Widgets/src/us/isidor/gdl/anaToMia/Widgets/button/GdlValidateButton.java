package us.isidor.gdl.anaToMia.Widgets.button;

import com.google.gwt.user.client.ui.Button;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;

public class GdlValidateButton extends GdlActionButton {
	// TODO: implement
	
	protected GdlValidateButton(){
		super();
	}
	
	
	public GdlValidateButton(Topic tmRepresentative, Construct receivedData, GdlVisibleObject parent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, parent);
		((Button)this.subElements.get(0)).setText("validate");
	}
}
