package us.isidor.gdl.anaToMia.Widgets.environment;

import java.util.ArrayList;
import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.TmEngine;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMapsTypes;


public interface IDeleteCallback {
	public void deleteTmConstruct(ArrayList<Pair<Object, TopicMapsTypes>> constructs, TmEngine tmEngine, String buttonId);
}
