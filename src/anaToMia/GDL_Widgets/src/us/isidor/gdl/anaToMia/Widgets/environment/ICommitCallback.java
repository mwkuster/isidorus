package us.isidor.gdl.anaToMia.Widgets.environment;

import java.util.ArrayList;

import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.TmEngine;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMapsTypes;



public interface ICommitCallback {
	public void commitTmConstruct(ArrayList<Pair<Construct, TopicMapsTypes>> constructs, String buttonId, TmEngine tmEngine);
}
