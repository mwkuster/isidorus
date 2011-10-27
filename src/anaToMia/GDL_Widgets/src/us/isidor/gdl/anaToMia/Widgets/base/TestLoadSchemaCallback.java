package us.isidor.gdl.anaToMia.Widgets.base;


import java.util.ArrayList;
import com.google.gwt.core.client.GWT;
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
// addressable by the URI requestUrl
public class TestLoadSchemaCallback implements ILoadSchemaCallback{
	private final String requestUrl = URL.encode(GWT.getModuleBaseURL() + "Service_GDL_Schema.jtm");
	private ArrayList<Pair<String, TopicIdentifierTypes>> requestedTopicsToCreate = new ArrayList<Pair<String,TopicIdentifierTypes>>(); 
	private Pair<String, TopicIdentifierTypes> requestedTopicToEdit = null;
	private RequestBuilder requestBuilder = new RequestBuilder(RequestBuilder.GET, requestUrl);


	public TestLoadSchemaCallback(){}


	// this method is invoked as a callback method
	@Override
	public void loadSchema(GdlPanel panel, Pair<String, TopicIdentifierTypes> requestedTopicToEdit , ArrayList<Pair<String, TopicIdentifierTypes>> requestedTopicsToCreate)throws RequestException {
		this.requestedTopicsToCreate = requestedTopicsToCreate;
		this.requestedTopicToEdit = requestedTopicToEdit;
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
				final ButtonDialog dialog = new ButtonDialog("Connection Error", "The request to " + requestUrl + " failed\n" + response.getStatusCode() + ": " + response.getStatusText(), "retry", "cancel", null, null);
				dialog.setLeftButtonClickHandler(new ClickHandler() {   
					@Override
					public void onClick(ClickEvent event) {
						dialog.hide();
						try{
							loadSchema(panel, TestLoadSchemaCallback.this.requestedTopicToEdit, TestLoadSchemaCallback.this.requestedTopicsToCreate);
						}catch(Exception e){
							Window.alert("connection to : " + requestUrl + " failed: " + e.getMessage());
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
		}


		@Override
		public void onError(Request request, Throwable exception) {
			final ButtonDialog dialog = new ButtonDialog("Connection Error", "The request to " + requestUrl + " failed\n" + exception.getMessage(), "retry", "cancel", null, null);
			dialog.setLeftButtonClickHandler(new ClickHandler() {   
				@Override
				public void onClick(ClickEvent event) {
					dialog.hide();
					try{
						loadSchema(panel, TestLoadSchemaCallback.this.requestedTopicToEdit, TestLoadSchemaCallback.this.requestedTopicsToCreate);
					}catch(Exception e){
						Window.alert("connection to : " + requestUrl + " failed: " + e.getMessage());
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
	}
}
