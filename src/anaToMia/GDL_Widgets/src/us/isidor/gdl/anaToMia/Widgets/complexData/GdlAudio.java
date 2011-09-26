package us.isidor.gdl.anaToMia.Widgets.complexData;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;

public class GdlAudio extends GdlComplexData {
	// some constructors
	protected GdlAudio() throws ExecutionException {
		throw new ExecutionException(PSIs.GDL.TopicType.gdlAudio + " is not implemented by anaToMia yet");
	}


	public GdlAudio(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		this();
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
