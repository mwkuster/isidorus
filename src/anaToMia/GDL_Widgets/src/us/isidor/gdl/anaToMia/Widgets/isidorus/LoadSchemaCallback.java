package us.isidor.gdl.anaToMia.Widgets.isidorus;

import java.util.ArrayList;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.http.client.URL;
import com.google.gwt.user.client.Window;
import us.isidor.gdl.anaToMia.Widgets.base.ButtonDialog;
import us.isidor.gdl.anaToMia.Widgets.base.GdlPanel;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.ILoadSchemaCallback;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;
import us.isidor.gdl.anaToMia.Widgets.environment.TopicIdentifierTypes;


// this class can be used as a callback that requests the Topic Map data
// addressable by the URI isidorusUrl
public class LoadSchemaCallback implements ILoadSchemaCallback{
	private final String isidorusUrl = URL.encode(IsidorusConstants.GET_SCHEMA_REQUEST_URL);
	private ArrayList<Pair<String, TopicIdentifierTypes>> requestedTopicsToCreate = new ArrayList<Pair<String,TopicIdentifierTypes>>(); 
	private Pair<String, TopicIdentifierTypes> requestedTopicToEdit = null;
	private RequestBuilder requestBuilder = new RequestBuilder(RequestBuilder.GET, isidorusUrl);
	private GdlWebPage gdlWebPage = null;
	
	@SuppressWarnings("unused")
	private LoadSchemaCallback(){}
	
	
	public LoadSchemaCallback(GdlWebPage gdlWebPage){
		this.gdlWebPage = gdlWebPage;
	}
	
	
	// this method is invoked as a callback method
	@Override
	public void loadSchema(GdlPanel panel, Pair<String, TopicIdentifierTypes> requestedTopicToEdit , ArrayList<Pair<String, TopicIdentifierTypes>> requestedTopicsToCreate)throws RequestException {
		this.requestedTopicsToCreate = requestedTopicsToCreate;
		this.requestedTopicToEdit = requestedTopicToEdit;
		this.gdlWebPage.createLoadScreenPanel("Requesting and Processing GDL data", "requesting a GDL-Schema from " + IsidorusConstants.GET_SCHEMA_REQUEST_URL + " and creating the corresponding UI-fragment");
		requestBuilder.sendRequest(null, new RequestCallbackImpl(panel));
	}

	
	// this method implements the actual request and a simple error handling
	private class RequestCallbackImpl implements RequestCallback{
		private GdlPanel panel = null;
		
		
		@SuppressWarnings("unused")
		private RequestCallbackImpl() {}
		
		
		public RequestCallbackImpl(GdlPanel panel){
			this.panel = panel;
		}
		
		
		@Override
		public void onResponseReceived(Request request, Response response) {
			if(Response.SC_OK == response.getStatusCode()){
				try{
					if(panel.getTmEngine() == null || panel.getSchemaTm() == null) throw new ExecutionException("no Topic Maps engine was set yet");
					panel.getTmEngine().importTopicMap(response.getText(), panel.getSchemaTm());
					panel.createView();
				}catch(ExecutionException e){
					Window.alert("Execution Error: " + e.getMessage());
				}catch(Exception e){
					Window.alert("panel: " + panel + ", tm: " + panel.getSchemaTm() + "\ncaught error: " + e.getLocalizedMessage());
					e.printStackTrace();
				}
			} else {
				final ButtonDialog dialog = new ButtonDialog("Connection Error", "The request to " + isidorusUrl + " failed\n" + response.getStatusCode() + ": " + response.getStatusText(), "retry", "cancel", null, null);
				dialog.setLeftButtonClickHandler(new ClickHandler() {	
					@Override
					public void onClick(ClickEvent event) {
						dialog.hide();
						try{
							loadSchema(panel, LoadSchemaCallback.this.requestedTopicToEdit, LoadSchemaCallback.this.requestedTopicsToCreate);
						}catch(Exception e){
							Window.alert("connection to : " + isidorusUrl + " failed: " + e.getMessage());
						}
					}
				});
				
				dialog.setRightButtonClickHandler(new ClickHandler() {
					@Override
					public void onClick(ClickEvent event) {
						dialog.hide();
					}
				});
				
				dialog.center();
			}
			LoadSchemaCallback.this.gdlWebPage.removeLoadScreenPanel();
		}
		
		
		@Override
		public void onError(Request request, Throwable exception) {
			final ButtonDialog dialog = new ButtonDialog("Connection Error", "The request to " + isidorusUrl + " failed\n" + exception.getMessage(), "retry", "cancel", null, null);
			dialog.setLeftButtonClickHandler(new ClickHandler() {	
				@Override
				public void onClick(ClickEvent event) {
					dialog.hide();
					try{
						loadSchema(panel, LoadSchemaCallback.this.requestedTopicToEdit, LoadSchemaCallback.this.requestedTopicsToCreate);
					}catch(Exception e){
						Window.alert("connection to : " + isidorusUrl + " failed: " + e.getMessage());
					}
				}
			});
			
			dialog.setRightButtonClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					dialog.hide();
				}
			});
			
			dialog.center();
			LoadSchemaCallback.this.gdlWebPage.removeLoadScreenPanel();
		}
	}
}
