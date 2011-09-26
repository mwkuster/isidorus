package us.isidor.gdl.anaToMia.Widgets.complexData;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;

public class GdlTimePicker extends GdlComplexData {
	// TODO: implement
	
	
	// some constructors
	protected GdlTimePicker(){
		super();
	}
	
	
	public GdlTimePicker(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		// create a time widget for each tm-construct
	}
	
	
	@Override
	public void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		// TODO: implement
	}
	
	
	@Override
	public void fixValue(){
		// TODO: implement
	}
}
