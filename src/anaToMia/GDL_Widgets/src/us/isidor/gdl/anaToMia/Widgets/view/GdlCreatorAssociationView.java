package us.isidor.gdl.anaToMia.Widgets.view;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;


public class GdlCreatorAssociationView extends GdlAssociationView {

	public GdlCreatorAssociationView(Topic tmRepresentative, Topic  receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException {
		super(tmRepresentative, receivedData, gdlParent);
		
		
		int minValues = this.getCardMin() == 0 ? 1 : this.getCardMax();
		for(int i = 0; i != minValues; ++i)
			this.addToContainerPanel(new AssociationItem(this.tmRepresentative, null, this));
	}
}
