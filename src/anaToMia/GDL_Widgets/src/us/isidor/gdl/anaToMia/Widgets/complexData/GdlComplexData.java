package us.isidor.gdl.anaToMia.Widgets.complexData;

import java.util.ArrayList;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;


public abstract class GdlComplexData extends GdlVisibleObject {
	// some constructors
	protected GdlComplexData() {
		super();
	}


	public GdlComplexData(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
	}
	
	
	@Override
	public ArrayList<String> getSelectedValues(){
		// TODO: implement
		return new ArrayList<String>();
	}
}
