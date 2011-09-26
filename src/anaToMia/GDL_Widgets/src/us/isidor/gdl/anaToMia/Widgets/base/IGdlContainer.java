package us.isidor.gdl.anaToMia.Widgets.base;

import java.util.ArrayList;

import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;


public interface IGdlContainer {
	public GdlVisibleObject append(Topic ancestor, Topic current) throws InvalidGdlSchemaException, ExecutionException;
	public ArrayList<Topic> contains() throws InvalidGdlSchemaException;
}
