package us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel;

import java.util.ArrayList;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Association;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Name;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Role;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMap;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Variant;



public interface TmEngine{
	public TopicMap importTopicMap(String tmData, TopicMap tm) throws FormatException, MissingReference;
	public Topic importTopic(String tmData, TopicMap tm) throws FormatException, MissingReference;
	public Name importName(String tmData, TopicMap tm) throws FormatException, MissingReference;
	public Variant importVariant(String tmData, TopicMap tm) throws FormatException, MissingReference;
	public Occurrence importOccurrence(String tmData, TopicMap tm) throws FormatException, MissingReference;
	public Association importAssociation(String tmData, TopicMap tm) throws FormatException, MissingReference;
	public Role importRole(String tmData, TopicMap tm) throws FormatException, MissingReference;
	public String exportTm(TopicMap tm) throws ExporterException;
	public String exportTm(Topic topic) throws ExporterException;
	public String exportTm(Name name) throws ExporterException;
	public String exportTm(Variant variant) throws ExporterException;
	public String exportTm(Occurrence occurrence) throws ExporterException;
	public String exportTm(Association association) throws ExporterException;
	public String exportTm(Role role) throws ExporterException;
	public String exportTm(ArrayList<Topic> topics, ArrayList<Association> associations) throws ExporterException;
	public TopicMap createTopicMap(String tmLocator);
	public TopicMap getTopicMap(String tmLocator);
	public TopicMap[] getTopicMaps();
}
