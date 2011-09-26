package us.isidor.gdl.anaToMia.Widgets.isidorus;

import java.util.ArrayList;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.base.TmHelper;
import us.isidor.gdl.anaToMia.Widgets.base.Utils;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;
import us.isidor.gdl.anaToMia.Widgets.environment.TopicIdentifierTypes;
import com.google.gwt.core.client.JsArray;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.http.client.URL;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.user.client.Window;
import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.ExporterException;
import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.TmEngine;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Association;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Locator;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Name;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMap;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMapsTypes;
import us.isidor.gdl.anaToMia.Widgets.environment.ICommitCallback;


public class CommitCallback implements ICommitCallback{
	public final String DELETE_REUQEST_URL = IsidorusConstants.DELETE_REQUEST_URL;
	public final String COMMIT_REQUEST_URL = IsidorusConstants.COMMIT_REQUEST_URL;
	public final String GET_REQUEST_URL = IsidorusConstants.GET_FRAGMENT_REQUEST_URL;
	private GdlWebPage gdlWebPage = null;
	
	
	@SuppressWarnings("unused")
	private CommitCallback(){}
	
	
	public CommitCallback(GdlWebPage gdlWebPage){
		this.gdlWebPage = gdlWebPage;
	}
	
	
	@Override
	public void commitTmConstruct(ArrayList<Pair<Construct, TopicMapsTypes>> constructs, String buttonId, TmEngine tmEngine) {
		try{
			if("creator_hash_object_commit_button_id".equals(buttonId)){
				if(this.gdlWebPage != null) this.gdlWebPage.createLoadScreenPanel("Committing Data", "committing the currently created Hash-Object topic to " + this.COMMIT_REQUEST_URL);
				this.commitCreatedHashObject(constructs, tmEngine, false);
			}else if("editor_hash_object_commit_button_id".equals(buttonId)){
				if(this.gdlWebPage != null) this.gdlWebPage.createLoadScreenPanel("Updating Data", "updating the currently changed Hash-Object topic on " + this.COMMIT_REQUEST_URL);
				this.commitCreatedHashObject(constructs, tmEngine, true);
			} else if("environment_commit_button_id".equals(buttonId)){
				if(this.gdlWebPage != null) this.gdlWebPage.createLoadScreenPanel("Committing Data", "committing the currently created Environment topic to " + this.COMMIT_REQUEST_URL);
				this.commitCreatedEnvironment(constructs, tmEngine);
			}
		}catch(Exception e){
			e.printStackTrace();
			Window.alert("failed to commit the Topic Maps data, because: " + e.getMessage());
		}
	}
	
	
	// remove all psis from the passed topic
	private void removeSubjectIdentifiers(Topic top){
		if(top == null) return;
		JsArray<Locator> psis = top.getSubjectIdentifiers();
		ArrayList<Locator> jPsis = new ArrayList<Locator>();
		for(int i = 0; i != psis.length(); ++i)jPsis.add(psis.get(i));
		for (Locator jPsi : jPsis)top.removeSubjectIdentifier(jPsi);
	}
	
	
	private void commitCreatedEnvironment(ArrayList<Pair<Construct, TopicMapsTypes>> constructs, TmEngine tmEngine) throws ExporterException {
		final String carrierPsi = PSIs.GDL.gdl + "environment_default_creator_topic_view_id";
		ArrayList<Topic> topics = new ArrayList<Topic>();
		ArrayList<TopicMap> topicMaps = new ArrayList<TopicMap>();
		
		for (Pair<Construct, TopicMapsTypes> construct : constructs) {
			if(construct.getSecond().equals(TopicMapsTypes.Topic)) topics.add((Topic)construct.getFirst());
			else if(construct.getSecond().equals(TopicMapsTypes.TopicMap)) topicMaps.add((TopicMap)construct.getFirst());
			else throw new ExporterException("unexpected construct types: " + construct.getSecond()+ ", only Topic and Association are allowed");
		}
		
		Topic top = null;
		Locator carrierLocator = null;
		TopicMap tm = null;
		if(topicMaps.size() != 0){
			tm = topicMaps.get(0);
			carrierLocator = tm.createLocator(carrierPsi);
			top = (Topic)tm.getConstructByItemIdentifier(carrierLocator);
		}
		else if(topics.size() != 0){
			tm = topics.get(0).getTopicMap();
			top = topics.get(0);	
			carrierLocator = tm.createLocator(carrierPsi);
		}
		
		if(top != null){
			// remove old psis
			this.removeSubjectIdentifiers(top);
			
			// set the new valid psi
			Name envName = top.getNames().get(0);
			Locator psi = tm.createLocator(IsidorusConstants.ENVIRONMENT_PSI_PREFIX + envName.getValue());
			top.addSubjectIdentifier(psi);
		}
		
		String jtm = null;
		if(topicMaps.size() != 0){
			jtm = tmEngine.exportTm(topicMaps.get(0));
		} else if(topics.size() != 0){
			jtm = tmEngine.exportTm(topics, new ArrayList<Association>());
		}
		
		String url = URL.encode(this.COMMIT_REQUEST_URL);
		RequestBuilder builder = new RequestBuilder(RequestBuilder.POST, url);
		builder.setHeader("Content-type", "application/json");
		try{
			// a check for an existing environment topic is not necessary,
			// since it would be exactly the same topic.
			builder.sendRequest(jtm, new EnvironmentCommitRequest(jtm));
		}catch(RequestException e){
			e.printStackTrace();
			Window.alert("could not commit the topic: " + jtm + ", because(" + e.getClass() + "): " + e.getMessage());
		}
	}
	
	
	private void commitCreatedHashObject(ArrayList<Pair<Construct, TopicMapsTypes>> constructs, TmEngine tmEngine, boolean edited) throws ExporterException{
		final String carrierPsi = PSIs.GDL.gdl + "hash_object_default_creator_topic_view_id";
		ArrayList<Topic> topics = new ArrayList<Topic>();
		ArrayList<Association> associations = new ArrayList<Association>();
		ArrayList<TopicMap> topicMaps = new ArrayList<TopicMap>();
		
		for (Pair<Construct, TopicMapsTypes> construct : constructs) {
			if(construct.getSecond().equals(TopicMapsTypes.Association)) associations.add((Association)construct.getFirst());
			else if(construct.getSecond().equals(TopicMapsTypes.Topic)) topics.add((Topic)construct.getFirst());
			else if(construct.getSecond().equals(TopicMapsTypes.TopicMap)) topicMaps.add((TopicMap)construct.getFirst());
			else throw new ExporterException("unexpected construct types: " + construct.getSecond()+ ", only Topic and Association are allowed");
		}
		
		Topic top = null;
		Locator carrierLocator = null;
		TopicMap tm = null;
		if(topicMaps.size() != 0){
			tm = topicMaps.get(0);
			carrierLocator = tm.createLocator(carrierPsi);
			top = (Topic)tm.getConstructByItemIdentifier(carrierLocator);
		}
		else if(topics.size() != 0){
			tm = topics.get(0).getTopicMap();
			top = topics.get(0);
			carrierLocator = tm.createLocator(carrierPsi);
		}
		
		if(top != null){
			Association assoc = null;
			if(associations.size() != 0){
				assoc = associations.get(0);
			} else {
				JsArray<Association> assocs = top.getTopicMap().getAssociations();
				Topic assocType = TmHelper.getTopicByPsi(IsidorusConstants.HAS_ENVIRONMENT_PSI, tm);
				for(int i = 0; i != assocs.length(); ++i){
					if(assocs.get(i).getType().equals(assocType)){
						assoc = assocs.get(i);
						break;
					}
				}
			}
			
			Name env = assoc.getRoles(TmHelper.getTopicByPsi(IsidorusConstants.ENVIRONMENT_ROLE_TYPE_PSI, tm)).get(0).getPlayer().getNames().get(0);
			Occurrence key = top.getOccurrences(TmHelper.getTopicByPsi(IsidorusConstants.KEY_OCCURRENCE_TYPE_PSI, tm)).get(0);
			String psiValue = IsidorusConstants.HASH_OBJECT_PSI_PREFIX + env.getValue() + "/" + key.getValue();
			Locator psi = tm.createLocator(psiValue);
			this.removeSubjectIdentifiers(top);
			top.addSubjectIdentifier(psi);
		
			String jtm = null;
			if(topicMaps.size() != 0){
				jtm = tmEngine.exportTm(topicMaps.get(0));
			} else if(associations.size() != 0 || topics.size() != 0){
				jtm = tmEngine.exportTm(topics, associations);
			}
			
			String getUrl = URL.encode(this.GET_REQUEST_URL + psiValue);
			RequestBuilder getBuilder = new RequestBuilder(RequestBuilder.GET, getUrl);
			try{
				getBuilder.sendRequest(null, new HashObjectCheckRequest(jtm, psiValue, edited));
			}catch(RequestException e){
				Window.alert("could not commit the topic: " + jtm + ", because(" + e.getClass() + "): " + e.getMessage());
			}
		}
	}
	
	
	private class EnvironmentCommitRequest implements RequestCallback {
		private String objectToBeSend = null;
		
		
		@SuppressWarnings("unused")
		private EnvironmentCommitRequest(){}
		
		
		public EnvironmentCommitRequest(String objectToBeCommitted){
			this.objectToBeSend = objectToBeCommitted;
		}
		
		
		@Override
		public void onResponseReceived(Request request, Response response) {
			if (200 == response.getStatusCode()) {
				Window.Location.reload();
			} else {
				Window.alert("commit operation of the object " + this.objectToBeSend + " failed: " + response.getStatusCode() + "(" + response.getStatusText() + ")\n" + response.getText());
			}
		}
		

		@Override
		public void onError(Request request, Throwable exception) {
			Window.alert("could not commit the data: " + this.objectToBeSend);
		}
	}

	
	private class HashObjectCheckRequest implements RequestCallback {
		private String objectToBeSend = null;
		private String psi = null;
		private boolean edited = false;
		
		
		public HashObjectCheckRequest(String objectToBeSend, String psi, boolean edited){
			this.objectToBeSend = objectToBeSend;
			this.psi = psi;
			this.edited = edited;
		}
		

		@Override
		public void onResponseReceived(Request request, Response response) {
			if (200 == response.getStatusCode()) {
				if(edited){
					String[] topPsi = Utils.splitUriByLastFragment(psi);
					String[] hashValuePsi = Utils.splitUriByLastFragment(IsidorusConstants.HASH_VALUE_OCCURRENCE_TYPE);
					String[] xsdString = Utils.splitUriByLastFragment(IsidorusConstants.XSD_STRING);
					
					JSONObject delObj = new JSONObject();
					delObj.put("type", new JSONString("Occurrence"));
					JSONObject delOcc = new JSONObject();
					delObj.put("delete", delOcc);
					delOcc.put("version", new JSONString("1.1"));
					delOcc.put("item_type", new JSONString("occurrence"));
					JSONObject prefixes = new JSONObject();
					prefixes.put("pref_1", new JSONString(topPsi[0]));
					prefixes.put("pref_2", new JSONString(hashValuePsi[0]));
					prefixes.put("pref_3", new JSONString(xsdString[0]));
					delOcc.put("prefixes", prefixes);
					JSONArray jParents = new JSONArray();
					jParents.set(0, new JSONString("si:[pref_1:" + topPsi[1] + "]"));
					delOcc.put("parent", jParents);
					
					delOcc.put("value", this.getOldHashValue(response.getText(), this.psi));
					delOcc.put("datatype", new JSONString("[pref_3:" + xsdString[1] + "]"));
					delOcc.put("type", new JSONString("si:[pref_2:" + hashValuePsi[1] + "]"));
					
					try{
						String url = URL.encode(CommitCallback.this.DELETE_REUQEST_URL);
						RequestBuilder builder = new RequestBuilder(RequestBuilder.DELETE, url);
						builder.setHeader("Content-type", "application/json");
						
						builder.sendRequest(delObj.toString(), new HashObjectDeleteRequest(delObj.toString(), this.objectToBeSend));						
					}catch(RequestException e){
						Window.alert("could not update the topic " + delObj);
					}
				} else {
					Window.alert("could not commit the topic with the psi " + psi + " since it already exists");
				}
			} else if(404 == response.getStatusCode() && !edited) {
				try{
					String commitUrl = URL.encode(CommitCallback.this.COMMIT_REQUEST_URL);
					RequestBuilder commitBuilder = new RequestBuilder(RequestBuilder.POST, commitUrl);
					commitBuilder.setHeader("Content-type", "application/json");
					commitBuilder.sendRequest(this.objectToBeSend, new HashObjectCommitRequest(this.objectToBeSend));
				}catch(RequestException e){
					Window.alert("could not commit the topic: " + this.objectToBeSend + ", because(" + e.getClass() + "): " + e.getMessage());
				}
			} else {
				Window.alert("could not commit the topic with the psi " + psi + ", because: " + response.getStatusCode() + ": " + response.getStatusText());
			}
		}
	
		
		private JSONString getOldHashValue(String jtmFragment, String topicPsiToBeUpdated){
			JSONString result = new JSONString("");
			if(jtmFragment == null || topicPsiToBeUpdated == null) return result;
			
			JSONValue receivedFragment = JSONParser.parseStrict(jtmFragment);
			JSONObject fragment = receivedFragment.isObject();
			if(fragment == null) return result;
			
			JSONValue topsVal = fragment.get("topics");
			if(topsVal == null) return result;
			JSONArray topics = topsVal.isArray();
			if(topics == null) return result;
			
			JSONValue prefVal = fragment.get("prefixes");
			JSONObject prefixes = null;
			if(prefVal != null) prefixes = prefVal.isObject();
			
			for(int topicIdx = 0; topicIdx != topics.size(); ++topicIdx){
				JSONValue hashTopicVal = topics.get(topicIdx);
				if(hashTopicVal == null) continue;
				
				JSONObject hashTopic = hashTopicVal.isObject();
				if(hashTopic == null) continue;
				
				JSONValue psisVal = hashTopic.get("subject_identifiers");
				if(psisVal == null) continue;
				
				JSONArray psis = psisVal.isArray();
				if(psis == null) continue;
				for(int psiIdx = 0; psiIdx != psis.size(); ++psiIdx){
					JSONValue psiVal = psis.get(psiIdx);
					if(psiVal == null) continue;
					
					JSONString psi = psiVal.isString();
					
					if(psi != null && this.computeUri(psi.stringValue(), prefixes).equals(topicPsiToBeUpdated)){
						JSONObject occ = this.gethashValueOccurrence(hashTopic, prefixes);
						if(occ == null) continue;
							
						JSONValue occVal = occ.get("value");
						if(occVal == null) continue;
						
						JSONString occValue = occVal.isString();
						if(occValue != null) return occValue;
					}
				}
			}
			
			return result;
		}
		
		
		private JSONObject gethashValueOccurrence(JSONObject hashTopic, JSONObject prefixes){
			if(hashTopic == null) return null;
			
			JSONValue occsVal = hashTopic.get("occurrences");
			if(occsVal == null) return null;
			JSONArray occurrences = occsVal.isArray();
			if(occurrences == null) return null;
			for(int occIdx = 0; occIdx != occurrences.size(); ++occIdx){
				JSONValue occVal = occurrences.get(occIdx);
				if(occVal == null) continue;
				JSONObject occurrence = occVal.isObject();
				if(occurrence == null) continue;
				JSONValue typeVal = occurrence.get("type");
				if(typeVal == null) continue;
				JSONString typeReference = typeVal.isString();
				if(typeReference == null) continue;
				
				Pair<String, TopicIdentifierTypes> typeRef = this.computeReferenceUri(typeReference, prefixes);
				if(typeRef != null && typeRef.getSecond().equals(TopicIdentifierTypes.SubjectIdentifier) && typeRef.getFirst().equals(IsidorusConstants.HASH_VALUE_OCCURRENCE_TYPE)) return occurrence;
			}
			
			return null;
		}
		
		
		private Pair<String, TopicIdentifierTypes> computeReferenceUri(JSONString curieReference, JSONObject prefixes){
			if(curieReference == null) return null;
			
			String curieString = curieReference.stringValue();
			if(curieString == null) return null;
			
			if(curieString.startsWith("ii:")){
				return new Pair<String, TopicIdentifierTypes>(this.computeUri(curieString.substring(3), prefixes),  TopicIdentifierTypes.ItemIdentifier);
			} else if(curieString.startsWith("si:")){
				return new Pair<String, TopicIdentifierTypes>(this.computeUri(curieString.substring(3), prefixes),  TopicIdentifierTypes.SubjectIdentifier);
			} else if(curieString.startsWith("sl:")){
				return new Pair<String, TopicIdentifierTypes>(this.computeUri(curieString.substring(3), prefixes),  TopicIdentifierTypes.SubjectLocator);
			} else {
				return null;
			}
		}
		
		
		private String computeUri(String curie, JSONObject prefixes){
			if(curie == null) return "";
			
			if(curie.charAt(0) == '[' && curie.charAt(curie.length() - 1) == ']'){
				if(prefixes == null) return "";
				String rawString = curie.substring(1, curie.length() - 1);
				String[] parts = rawString.split(":");
				if(parts.length != 2) return "";
				
				JSONValue prefVal = prefixes.get(parts[0]);
				if(prefVal == null) return "";
				JSONString prefString = prefVal.isString();
				if(prefString == null) return "";
				
				return prefString.stringValue() + parts[1];
			} else {
				return curie;
			}
		}
		

		@Override
		public void onError(Request request, Throwable exception) {
			Window.alert("could not commit the data: " + this.objectToBeSend);
		}
	}
	
		
	private class HashObjectDeleteRequest implements RequestCallback{
		private String objectToBeDeleted = null;
		private String objectToBeSend = null;
		
		
		@SuppressWarnings("unused")
		private HashObjectDeleteRequest(){}
		
		
		public HashObjectDeleteRequest(String objectToBeDeleted, String objectToBeSend){
			this.objectToBeDeleted = objectToBeDeleted;
			this.objectToBeSend = objectToBeSend;
		}
		
		
		@Override
		public void onResponseReceived(Request request, Response response) {
			if (200 == response.getStatusCode()) {
				try{
					String commitUrl = URL.encode(CommitCallback.this.COMMIT_REQUEST_URL);
					RequestBuilder commitBuilder = new RequestBuilder(RequestBuilder.POST, commitUrl);
					commitBuilder.setHeader("Content-type", "application/json");
					commitBuilder.sendRequest(this.objectToBeSend, new HashObjectCommitRequest(this.objectToBeSend));
				}catch(RequestException e){
					Window.alert("could not commit the data: " + this.objectToBeSend);
				}
			} else {
				Window.alert("update operation of the object " + this.objectToBeDeleted + " failed: " + response.getStatusCode() + "(" + response.getStatusText() + ")\n" + response.getText());
			}
		}
		

		@Override
		public void onError(Request request, Throwable exception) {
			Window.alert("could not update the data: " + this.objectToBeDeleted);
		}
	}
	
	
	private class HashObjectCommitRequest implements RequestCallback {
		private String objectToBeSend = null;
		
		
		@SuppressWarnings("unused")
		private HashObjectCommitRequest(){}
		
		
		public HashObjectCommitRequest(String objectToBeCommitted){
			this.objectToBeSend = objectToBeCommitted;
		}
		
		
		@Override
		public void onResponseReceived(Request request, Response response) {
			if (200 == response.getStatusCode()) {
				Window.Location.reload();
			} else {
				Window.alert("commit operation of the object " + this.objectToBeSend + " failed: " + response.getStatusCode() + "(" + response.getStatusText() + ")\n" + response.getText());
			}
		}
		

		@Override
		public void onError(Request request, Throwable exception) {
			Window.alert("could not commit the data: " + this.objectToBeSend);
		}
	}
}
