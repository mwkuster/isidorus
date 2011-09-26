package us.isidor.gdl.anaToMia.Widgets.environment;

import java.util.ArrayList;

import com.google.gwt.http.client.RequestException;
import us.isidor.gdl.anaToMia.Widgets.base.GdlPanel;

public interface ILoadSchemaCallback {
	public void loadSchema(GdlPanel panel, Pair<String, TopicIdentifierTypes> requestedTopicToEdit, ArrayList<Pair<String, TopicIdentifierTypes>> requestedTopicsToCreate)throws RequestException;
}
