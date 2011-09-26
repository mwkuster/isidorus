package us.isidor.gdl.anaToMia.Widgets.isidorus;


import java.util.ArrayList;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.TmHelper;
import us.isidor.gdl.anaToMia.Widgets.base.Utils;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
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
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;


public class HashObjectPsiContainer extends Composite {
	private ArrayList<CheckBox> psiCheckBoxes = new ArrayList<CheckBox>();
	private VerticalPanel containerPanel = new VerticalPanel();
	private Button commitButton = new Button("delete the Environment topic and all its Hash-Object topics");
	private final String ERROR_STYLE_CLASS = "bad_input";
	private CheckBoxClickHandler cbxClickHandler = new CheckBoxClickHandler();
	private final String DELETE_URL = IsidorusConstants.DELETE_REQUEST_URL;
	private Topic environmentTopic = null;
	private GdlWebPage gdlWebPage = null;
	
	
	public HashObjectPsiContainer(Topic environmentTopic, GdlWebPage gdlWebPage) throws ExecutionException {
		if(environmentTopic == null || gdlWebPage == null) throw new ExecutionException("environmentTopic and gdlWebPage must not be null");
		this.gdlWebPage = gdlWebPage;
		this.environmentTopic = environmentTopic;
		this.initWidget(this.containerPanel);
		this.containerPanel.add(this.commitButton);
		this.commitButton.addClickHandler(new HashObjectPsiContainerClickHandler(this.environmentTopic));
		DOM.setStyleAttribute(this.commitButton.getElement(), "width", "100%");
		DOM.setStyleAttribute(this.commitButton.getElement(), "marginTop", "1em");
	}
	
	
	public void addHashObjectPsi(String psi) {
		if(psi == null) return;

		this.commitButton.removeFromParent();
		CheckBox cbx = new CheckBox(psi);
		this.psiCheckBoxes.add(cbx);
		
		int widgetIdx = 0;
		for( ; widgetIdx != this.containerPanel.getWidgetCount(); ++widgetIdx){
			Widget wdgt = this.containerPanel.getWidget(widgetIdx);
			if((wdgt instanceof Label) && ((Label)wdgt).getText().compareTo(psi) >= 0){
				this.containerPanel.insert(cbx, widgetIdx);
				break;
			}
		} 
		
		cbx.addClickHandler(this.cbxClickHandler);
		
		if(widgetIdx == this.containerPanel.getWidgetCount()) this.containerPanel.add(cbx);
		this.containerPanel.insert(this.commitButton, this.containerPanel.getWidgetCount());
	}
	
	
	private void deleteHashObject(String psi, DeleteAllHashObjectsRequest reqCallback){
		if(psi != null || reqCallback != null){
			JSONObject delObj = this.createObjectToBeDeleted(psi);
			try{
				String url = URL.encode(this.DELETE_URL);
				RequestBuilder builder = new RequestBuilder(RequestBuilder.DELETE, url);
				builder.setHeader("Content-type", "application/json");
				
				builder.sendRequest(delObj.toString(), reqCallback);
			}catch(RequestException e){
				Window.alert("could not delete the topic: " + psi + ", because(" + e.getClass() + "): " + e.getMessage());
			}
		}
	}
	
	
	private JSONObject createObjectToBeDeleted(String psi){
		JSONArray psis = new JSONArray();
		psis.set(0, new JSONString(psi));

		JSONObject delObj = new JSONObject();
		delObj.put("type", new JSONString("Topic"));

		String[] pref = Utils.splitUriByLastFragment(psi);
		JSONValue val = JSONParser.parseStrict("{\"version\":\"1.1\",\"prefixes\":{\"pref\":\"" + pref[0] + "\"},\"subject_identifiers\":[\"[pref:" + pref[1] + "]\"]}");
		delObj.put("delete", val.isObject());

		return delObj;
	}
	
	
	private class CheckBoxClickHandler implements ClickHandler{
		@Override
		public void onClick(ClickEvent event) {
			Object obj = event.getSource();
			if(obj instanceof CheckBox){
				CheckBox source = (CheckBox) obj;
				if(source.getValue()) source.removeStyleName(HashObjectPsiContainer.this.ERROR_STYLE_CLASS);
			}
		}
	}
	
	
	private class HashObjectPsiContainerClickHandler implements ClickHandler {
		private Topic environmentTopic = null;
		
		
		public HashObjectPsiContainerClickHandler(Topic environmentTopic){
			this.environmentTopic = environmentTopic;
		}
		
		
		@Override
		public void onClick(ClickEvent event) {
			boolean allValuesChecked = true;
			for (CheckBox cbx : HashObjectPsiContainer.this.psiCheckBoxes) {
				if(!cbx.getValue()){
					cbx.addStyleName(HashObjectPsiContainer.this.ERROR_STYLE_CLASS);
					allValuesChecked = false;
				}
			}
			
			if(!allValuesChecked){
				Window.alert("to delete the an environment topic, please select explicit all associated hash-object topics to be deleted!");
			} else {
				HashObjectPsiContainer.this.gdlWebPage.createLoadScreenPanel("Wating for Completing a Delete Request", "deleting all associated Hash-Object topics of the Environment Topic " + TmHelper.getAnyIdOfTopic(this.environmentTopic) + " from " + HashObjectPsiContainer.this.DELETE_URL);
				DeleteAllHashObjectsRequest reqCallback = new DeleteAllHashObjectsRequest(HashObjectPsiContainer.this.psiCheckBoxes, this.environmentTopic);
				if(HashObjectPsiContainer.this.psiCheckBoxes.size() == 0){
					JSONObject delObj = HashObjectPsiContainer.this.createObjectToBeDeleted(this.environmentTopic.getSubjectIdentifiers().get(0).getReference());
					String url = URL.encode(HashObjectPsiContainer.this.DELETE_URL);
					RequestBuilder builder = new RequestBuilder(RequestBuilder.DELETE, url);
					builder.setHeader("Content-type", "application/json");
					try{
						builder.sendRequest(delObj.toString(), new EnvironemntDeleteRequest(delObj, this.environmentTopic));
					}catch(RequestException e){
						Window.alert("delete operation of the object " + delObj + " failed, because: (" + e.getClass() + ")\n" + e.getMessage());
					}
				} else {
					for (CheckBox cbx : HashObjectPsiContainer.this.psiCheckBoxes) HashObjectPsiContainer.this.deleteHashObject(cbx.getText(), reqCallback);
				}
			}
		}
	}
	

	private class DeleteAllHashObjectsRequest implements RequestCallback {
		private ArrayList<String> hashObjectsToDelete = new ArrayList<String>();
		private int objectsDeleted = 0;
		private Topic environmentTopic = null;
		private String environmentPsi = null;
		
		
		@SuppressWarnings("unused")
		private DeleteAllHashObjectsRequest(){}
		
		
		public DeleteAllHashObjectsRequest(ArrayList<CheckBox> hashObjectToDelete, Topic environmentTopic){
			if(hashObjectToDelete != null) for (CheckBox cbx : hashObjectToDelete) this.hashObjectsToDelete.add(cbx.getText());
			this.environmentTopic = environmentTopic;
			this.environmentPsi = this.environmentTopic.getSubjectIdentifiers().get(0).getReference(); // it's ensured that the passed topic has at least one psi before
		}
		
		
		@Override
		public void onResponseReceived(Request request, Response response) {
			if (200 == response.getStatusCode()) {
				++this.objectsDeleted;
				if(this.objectsDeleted == this.hashObjectsToDelete.size()){						
					JSONObject delObj = HashObjectPsiContainer.this.createObjectToBeDeleted(this.environmentPsi);
					String url = URL.encode(HashObjectPsiContainer.this.DELETE_URL);
					RequestBuilder builder = new RequestBuilder(RequestBuilder.DELETE, url);
					builder.setHeader("Content-type", "application/json");
					try{
						builder.sendRequest(delObj.toString(), new EnvironemntDeleteRequest(delObj, this.environmentTopic));
					}catch(RequestException e){
						Window.alert("delete operation of the object " + delObj + " failed, because: (" + e.getClass() + ")\n" + e.getMessage());
					}
				}
			} else {
				Window.alert("could not delete all topics of " + Utils.arrayToString(this.hashObjectsToDelete) + ", because(" + response.getStatusCode() + "): " + response.getStatusText());
			}
		}

		
		@Override
		public void onError(Request request, Throwable exception) {
			String message = null;
			Class<? extends Throwable> eClass = null;
			if(exception != null){
				message = exception.getMessage();
				eClass = exception.getClass();
			}
			Window.alert("could not delete all topics of " + Utils.arrayToString(this.hashObjectsToDelete) + ", because(" + eClass + "): " + message);
		}
	}
	
	
	private class EnvironemntDeleteRequest implements RequestCallback {
		private JSONObject objectToBeSend = null;
		private Topic topicToBeDeleted = null;
		
		
		@SuppressWarnings("unused")
		private EnvironemntDeleteRequest(){}
		
		
		public EnvironemntDeleteRequest(JSONObject objectToBeDeleted, Topic topicToBeDeleted){
			this.objectToBeSend = objectToBeDeleted;
			this.topicToBeDeleted = topicToBeDeleted;
		}
		
		
		@Override
		public void onResponseReceived(Request request, Response response) {
			if (200 == response.getStatusCode()) {
				Window.Location.reload();
				this.topicToBeDeleted.remove();
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
