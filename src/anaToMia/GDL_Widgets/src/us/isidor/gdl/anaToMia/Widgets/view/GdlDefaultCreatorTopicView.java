package us.isidor.gdl.anaToMia.Widgets.view;


import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlPanel;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;


public class GdlDefaultCreatorTopicView extends GdlDefaultTopicView{
	private GdlPanel rootPanel = null;
	
	public GdlDefaultCreatorTopicView(Topic tmRepresentative, GdlVisibleObject gdlParent, GdlPanel rootPanel) throws InvalidGdlSchemaException, ExecutionException {
		super(tmRepresentative, null, gdlParent);
		this.rootPanel = rootPanel;
		// TODO Auto-generated constructor stub
	}
	
	
	@Override
	public GdlPanel getRoot(){
		return this.rootPanel;
	}
	
	
	// removes and destroys the carrier topic that is created when the getContent
	// method is invoked, so duplicate data entries are not produced!
	public void resetCarrier(){
		//TODO: implement => call for this instance and each child creator instance
		
	}
}
