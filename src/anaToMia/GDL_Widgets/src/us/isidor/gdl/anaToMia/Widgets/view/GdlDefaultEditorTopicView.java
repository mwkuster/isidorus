package us.isidor.gdl.anaToMia.Widgets.view;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlPanel;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;


public class GdlDefaultEditorTopicView extends GdlDefaultTopicView {
	private GdlPanel rootPanel = null;

	public GdlDefaultEditorTopicView(Topic tmRepresentative, Topic receivedData, GdlVisibleObject gdlParent, GdlPanel rootPanel) throws InvalidGdlSchemaException, ExecutionException {
		super(tmRepresentative, receivedData, gdlParent);
		this.rootPanel = rootPanel;
		// TODO Auto-generated constructor stub
	}
	
	
	@Override
	public GdlPanel getRoot(){
		return this.rootPanel;
	}
}
