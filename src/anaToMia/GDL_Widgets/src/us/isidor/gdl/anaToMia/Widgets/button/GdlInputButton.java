package us.isidor.gdl.anaToMia.Widgets.button;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;

public abstract class GdlInputButton extends GdlButton {
	protected GdlInputButton(){
		super();
	}
	
	
	public GdlInputButton(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
	}
	
	
	public String getGroupName() throws InvalidGdlSchemaException{
		return this.getId() + "__RB__GROUP";
	}
}
