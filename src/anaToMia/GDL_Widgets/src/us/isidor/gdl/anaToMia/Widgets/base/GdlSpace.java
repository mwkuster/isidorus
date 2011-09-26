package us.isidor.gdl.anaToMia.Widgets.base;

import java.util.ArrayList;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;

public class GdlSpace extends GdlVisibleObject {
	// some constructors
	protected GdlSpace() throws InvalidGdlSchemaException, ExecutionException {
		super();
	}
	
	
	public GdlSpace(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		this.setGdlStyle(this);
	}
	
	
	@Override
	@Deprecated
	public void addSubItem(String value){
		// this method has no effect on GdlSpace instances
	}
	
	
	@Override
	@Deprecated
	public ArrayList<String> getSelectedValues(){
		return new ArrayList<String>();
	}
	
	
	@Override
	@Deprecated
	public void fixValue(){
		// has no effect on this element
	}
}
