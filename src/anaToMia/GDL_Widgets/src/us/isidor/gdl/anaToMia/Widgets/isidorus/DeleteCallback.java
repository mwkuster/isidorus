package us.isidor.gdl.anaToMia.Widgets.isidorus;

import java.util.ArrayList;
import com.google.gwt.dom.client.ButtonElement;
import com.google.gwt.dom.client.Element;
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
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Window;
import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.ExporterException;
import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.TmEngine;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMapsTypes;
import us.isidor.gdl.anaToMia.Widgets.base.TmHelper;
import us.isidor.gdl.anaToMia.Widgets.base.Utils;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.IDeleteCallback;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;


public class DeleteCallback implements IDeleteCallback {
	private final String REUQEST_URL = IsidorusConstants.DELETE_REQUEST_URL;
	private final String TM_SPARQL_URL = IsidorusConstants.TM_SPARQL_ENDPOINT;
	private final String TM_SPARQL_QUERY_PART_1 = "PREFIX types:<http://textgrid.org/serviceregistry/model/types/>\n" +
										   		  "PREFIX model:<http://textgrid.org/serviceregistry/model/>\n" +
										   		  "PREFIX env:<http://textgrid.org/serviceregistry/environment/>\n" +
										   		  "SELECT ?topics WHERE {\n" +
										   		  "?topics a types:Hash-Object.\n" +
										   		  "?topics model:environment env:";
	private final String TM_SPARQL_QUERY_PART_2 = ".\n}";
	private GdlWebPage gdlWebPage = null;
	private HashObjectPsiContainer hashObjectPsiContainer = null;
	
	
	@SuppressWarnings("unused")
	private DeleteCallback(){}
	
	
	public DeleteCallback(GdlWebPage gdlWebPage){
		this.gdlWebPage = gdlWebPage;
	}
	

	@Override
	public void deleteTmConstruct(ArrayList<Pair<Object, TopicMapsTypes>> constructs, TmEngine tmEngine, String buttonId) {
		if(buttonId != null){
			Element elem = DOM.getElementById(buttonId);
			ButtonElement btnElem = (ButtonElement)elem;
			btnElem.setDisabled(true);
		}
		this.deleteTopic(constructs, tmEngine);
	}

	
	private void deleteTopic(ArrayList<Pair<Object, TopicMapsTypes>> constructs, TmEngine tmEngine){
		for (Pair<Object, TopicMapsTypes> pair : constructs) {
			if(pair.getSecond().equals(TopicMapsTypes.Topic)){
				Topic top = (Topic)pair.getFirst();
				
				if(TmHelper.isInstanceOf(top, IsidorusConstants.ENVIRONMENT_PSI)) this.deleteEnvironmentTopic(top);
				else this.deleteNonEnvironmentTopic(top, tmEngine);
			}
		}		
	}
	
	
	private String createTmSparqlQuery(Topic environment){
		String psi = environment.getSubjectIdentifiers().get(0).getReference();
		String psiSuffix = Utils.splitUriByLastFragment(psi)[1];
		return this.TM_SPARQL_QUERY_PART_1 + psiSuffix + this.TM_SPARQL_QUERY_PART_2;
	}
	
	
	private void deleteEnvironmentTopic(Topic env){
		String url = URL.encode(this.TM_SPARQL_URL);
		RequestBuilder builder = new RequestBuilder(RequestBuilder.POST, url);
		if(env.getSubjectIdentifiers().length() == 0){
			Window.alert("illegal environment topic detected, because: the topic " + env + " has no subject identifier");
			DeleteCallback.this.gdlWebPage.resetPage();
		} else {
			try{
				this.hashObjectPsiContainer = new HashObjectPsiContainer(env, DeleteCallback.this.gdlWebPage);
				DeleteCallback.this.gdlWebPage.createLoadScreenPanel("Wating for Data", "Requesting all PSIs of instances of " + IsidorusConstants.HASH_OBJECT_PSI + " that are associated with " + env.getSubjectIdentifiers().get(0).getReference() + " from " + url);
				builder.sendRequest(this.createTmSparqlQuery(env), new GetHashObjectPsisRequest());
			}catch(RequestException e){
				e.printStackTrace();
				Window.alert("could not delete the topic " + TmHelper.getAnyIdOfTopic(env) + ", because(" + e.getClass() + "): " + e.getMessage());
				DeleteCallback.this.gdlWebPage.resetPage();
			} catch(ExecutionException e){
				e.printStackTrace();
				Window.alert("could not delete the topic " + TmHelper.getAnyIdOfTopic(env) + ", because(" + e.getClass() + "): " + e.getMessage());
				DeleteCallback.this.gdlWebPage.resetPage();
			}
		}
	}
	
	
	private class GetHashObjectPsisRequest implements RequestCallback {
		public GetHashObjectPsisRequest(){}
		

		@Override
		public void onResponseReceived(Request request, Response response) {
			if (200 == response.getStatusCode()) {
				JSONValue psiVals = JSONParser.parseStrict(response.getText());
				JSONObject resultObject = psiVals.isObject();
				if(resultObject == null) Window.alert("got bad json, a query result object was expected, but got: " + response.getText());
				
				JSONValue content = resultObject.get("topics");
				if(content == null) Window.alert("got bad json, a query result object was expected, but got: " + response.getText());
				
				JSONArray psis = null;
				if(psiVals != null) psis = content.isArray();
				if(psis != null){
					for(int psisIdx = 0; psisIdx != psis.size(); ++psisIdx){
						JSONValue psiVal = psis.get(psisIdx);
						JSONString psiString = null;
						if(psiVal != null) psiString = psiVal.isString();
						String psiValue = null;
						if(psiString != null){
							psiValue = psiString.stringValue();
							if(psiValue.startsWith("<")) psiValue = psiValue.substring(1);
							if(psiValue.endsWith(">")) psiValue = psiValue.substring(0, psiValue.length() - 1);
						}
						DeleteCallback.this.hashObjectPsiContainer.addHashObjectPsi(psiValue);
					}
				}
				
				DeleteCallback.this.gdlWebPage.setHashObjectPsiContainer(DeleteCallback.this.hashObjectPsiContainer);
			} else {
				Window.alert("could not request existing instances of " + IsidorusConstants.HASH_OBJECT_PSI + ", because(" + response.getStatusCode() + "): " + response.getStatusText());
			}
			DeleteCallback.this.gdlWebPage.removeLoadScreenPanel();
		}
		

		@Override
		public void onError(Request request, Throwable exception) {
			String message = null;
			Class<? extends Throwable> eClass = null;
			if(exception != null){
				message = exception.getMessage();
				eClass = exception.getClass();
			}
			Window.alert("could not request existing instances of " + IsidorusConstants.HASH_OBJECT_PSI + ", because(" + eClass + "): " + message);
			DeleteCallback.this.gdlWebPage.resetPage();
		}
	}
	
	
	private void deleteNonEnvironmentTopic(Topic top, TmEngine tmEngine){
		if(top.getSubjectIdentifiers().length() != 0){
			String psi = top.getSubjectIdentifiers().get(0).getReference();
			
			JSONArray psis = new JSONArray();
			psis.set(0, new JSONString(psi));
			
			JSONObject delObj = new JSONObject();
			delObj.put("type", new JSONString("Topic"));
			
			try{
				JSONValue val = JSONParser.parseStrict(tmEngine.exportTm(top));
				delObj.put("delete", val.isObject());
				
				String url = URL.encode(this.REUQEST_URL);
				RequestBuilder builder = new RequestBuilder(RequestBuilder.DELETE, url);
				builder.setHeader("Content-type", "application/json");
				
				builder.sendRequest(delObj.toString(), new DeleteRequest(delObj));
			}catch(RequestException e){
				Window.alert("could not delete the topic: " + psi + ", because(" + e.getClass() + "): " + e.getMessage());
			}catch(ExporterException e){
				Window.alert("could not delete the topic: " + psi + ", because(" + e.getClass() + "): " + e.getMessage());
			}
		}
	}
	
	
	private class DeleteRequest implements RequestCallback {
		private JSONObject objectToBeSend = null;
		
		
		@SuppressWarnings("unused")
		private DeleteRequest(){}
		
		
		public DeleteRequest(JSONObject objectToBeDeleted){
			this.objectToBeSend = objectToBeDeleted;
		}
		
		
		@Override
		public void onResponseReceived(Request request, Response response) {
			if (200 == response.getStatusCode()) {
				Window.Location.reload();
			} else {
				Window.alert("delete operation of the object " + this.objectToBeSend + " failed: " + response.getStatusCode() + "(" + response.getStatusText() + ")\n" + response.getText());
			}
		}
		

		@Override
		public void onError(Request request, Throwable exception) {
			Window.alert("could not delete the data: " + this.objectToBeSend);
		}
	}
}
