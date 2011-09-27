package us.isidor.gdl.anaToMia.Widgets.isidorus;

import java.util.ArrayList;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.TextAreaElement;
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
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.VerticalPanel;
import us.isidor.gdl.anaToMia.TmEngine.jtmsBasedEngine.JtmsTmEngine;
import us.isidor.gdl.anaToMia.Widgets.base.GdlPanel;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;
import us.isidor.gdl.anaToMia.Widgets.environment.TopicIdentifierTypes;


public class GdlWebPage {
	private String topicTypePsi = null;
	private HorizontalPanel mainPanel = new HorizontalPanel();
	private VerticalPanel navigationPanel = new VerticalPanel();
	private VerticalPanel contentPanel = new VerticalPanel();
	private Button createNewButton = new Button("create new");
	private PsiContainer psiContainer = null;
	private GdlPanel gdlPanel = null;
	private LoadScreenPanel loadScreenPanel = null;
	private final String GDL_PANEL_STYLE_NAME = "gdl_panel";
	private final String CREATE_NEW_BUTTON_STYLE_NAME = "create_new_button";
	private final String HASH_OBJECT_PSI_CONTAINER_STYLE_NAME = "hash_object_psi_container";
	private final String MAIN_PANEL_STYLE_CLASS = "content_panel";
	private HashObjectPsiContainer hashObjectPsicontainer = null;
	
	
	public GdlWebPage(String topicTypePsi) throws ExecutionException{
		if(topicTypePsi == null) throw new ExecutionException("topicTypePsi must not be null");
		this.topicTypePsi = topicTypePsi;
		GdlPanel.addClickHandler("hash_object_reset_button_id", new ResetClickHandler());
	}
	
	
	public void createWebPage(){
		this.mainPanel.addStyleName(this.MAIN_PANEL_STYLE_CLASS);
		this.createNewButton.addStyleName(this.CREATE_NEW_BUTTON_STYLE_NAME);
		DOM.setStyleAttribute(this.createNewButton.getElement(), "marginBottom", "1em");
		try{
			this.psiContainer = new PsiContainer(new PsiClickHandler());
			this.createNewButton.addClickHandler(new CreateNewClickHandler());
		}catch(Exception e){
			Window.alert("could not create web page, because: (" + e.getClass() + ") " + e.getMessage());
			this.resetPage();
		}
		
		RootPanel.get("GWT_Content").add(this.mainPanel);
		this.mainPanel.add(this.navigationPanel);
		this.mainPanel.add(this.contentPanel);
		this.mainPanel.setPixelSize(1254, 700);
		this.navigationPanel.add(this.createNewButton);
		try{
			this.requestPsis();
		} catch(ExecutionException e){
			Window.alert("could not create web page, because: (" + e.getClass() + ") " + e.getMessage());
			this.resetPage();
		}
	}
	
	
	public void resetPage() {
		this.removeLoadScreenPanel();
		if(this.hashObjectPsicontainer != null) this.hashObjectPsicontainer.removeFromParent();
		if(this.gdlPanel != null) this.gdlPanel.removeFromParent();
		if(this.psiContainer != null) this.psiContainer.removeFromParent();
		if(this.createNewButton != null) this.createNewButton.removeFromParent();
		
		try{
			this.psiContainer = new PsiContainer(new PsiClickHandler());
			this.requestPsis();
			this.navigationPanel.add(this.createNewButton);
		}catch(Exception e){
			Window.alert("could not create web page, becuase: (" + e.getClass() + ") " + e.getMessage());
		}
	}
	
	
	public void setHashObjectPsiContainer(HashObjectPsiContainer psiContainer){
		if(this.hashObjectPsicontainer != null) this.hashObjectPsicontainer.removeFromParent();
		this.hashObjectPsicontainer = psiContainer;
		if(this.hashObjectPsicontainer != null){
			this.contentPanel.insert(this.hashObjectPsicontainer, this.contentPanel.getWidgetCount());
			this.hashObjectPsicontainer.addStyleName(this.HASH_OBJECT_PSI_CONTAINER_STYLE_NAME);
		}
	}
	
	
	public void setGdlPanel(GdlPanel gdlPanel){
		if(this.gdlPanel != null) this.gdlPanel.removeFromParent();
		if(this.hashObjectPsicontainer != null) this.hashObjectPsicontainer.removeFromParent();
		this.gdlPanel = gdlPanel;
		if(this.gdlPanel != null) this.contentPanel.add(this.gdlPanel);
	}
	
	
	public void removeLoadScreenPanel(){
		Window.enableScrolling(true);
		if(this.loadScreenPanel != null) this.loadScreenPanel.removeFromParent();
	}
	
	
	public void createLoadScreenPanel(String title, String message){
		Window.enableScrolling(false);
		this.removeLoadScreenPanel();
		this.loadScreenPanel = new LoadScreenPanel(title, message);
		RootPanel.get().add(this.loadScreenPanel);
	}
	
	
	private void requestPsis() throws ExecutionException {
		String url = null;
		String postData = null;
		if(IsidorusConstants.HASH_OBJECT_PSI.equals(GdlWebPage.this.topicTypePsi)){
			url = URL.encode(IsidorusConstants.GET_HASH_OBJECT_PSIS_URL);
			postData = "PREFIX pref:<http://textgrid.org/serviceregistry/model/types>\n" +
					   "SELECT ?topics WHERE {\n" +
					   "?topics a pref:Hash-Object.\n" +
					   "}";
		}
		else if(IsidorusConstants.ENVIRONMENT_PSI.equals(GdlWebPage.this.topicTypePsi)){
			url = URL.encode(IsidorusConstants.GET_ENVIRONMENT_PSIS_URL);
			postData = "PREFIX pref:<http://textgrid.org/serviceregistry/model/types>\n" +
			   "SELECT ?topics WHERE {\n" +
			   "?topics a pref:Environment.\n" +
			   "}";
		}
		else{
			throw new ExecutionException("the topic type PSI " + GdlWebPage.this.topicTypePsi + " is not supported!");
		}
		
		RequestBuilder builder = new RequestBuilder(RequestBuilder.POST, url);
		try{
			this.createLoadScreenPanel("Wating for Data", "Requesting all PSIs of instances of " + GdlWebPage.this.topicTypePsi + " from " + url);
			builder.sendRequest(postData, new PsiRequest());
		}catch(RequestException e){
			e.printStackTrace();
			Window.alert("could not request existing instances of " + GdlWebPage.this.topicTypePsi + ", because(" + e.getClass() + "): " + e.getMessage());
			GdlWebPage.this.resetPage();
		}
	}
	
	
	private class CreateNewClickHandler implements ClickHandler {
		@Override
		public void onClick(ClickEvent event) {
			try{
				ArrayList<Pair<String, TopicIdentifierTypes>> topicsToCreate = new ArrayList<Pair<String, TopicIdentifierTypes>>();
				topicsToCreate.add(new Pair<String, TopicIdentifierTypes>(GdlWebPage.this.topicTypePsi, TopicIdentifierTypes.SubjectIdentifier));
				GdlPanel gdlPanel = new GdlPanel(null, topicsToCreate, 592, 160);
				gdlPanel.setTmEngine(new JtmsTmEngine());
				gdlPanel.setLoadSchemaCallback(new LoadSchemaCallback(GdlWebPage.this));
				gdlPanel.setCommitCallback(new CommitCallback(GdlWebPage.this));
				gdlPanel.setDeleteCallback(new DeleteCallback(GdlWebPage.this));
				gdlPanel.addStyleName(GdlWebPage.this.GDL_PANEL_STYLE_NAME);
				GdlWebPage.this.setGdlPanel(gdlPanel);
				gdlPanel.loadSchema();
			}catch(Exception e){
				e.printStackTrace();
				Window.alert("could not instantiate the GdlPanel, because(" + e.getClass() + "): " + e.getMessage());
				GdlWebPage.this.resetPage();	
			}
		}
	}
	
	
	private class PsiClickHandler implements ClickHandler {
		@Override
		public void onClick(ClickEvent event) {
			Object obj = event.getSource();
			if(obj instanceof Label){
				Label source = (Label)obj;				
				try{
					GdlPanel gdlPanel = new GdlPanel(new Pair<String, TopicIdentifierTypes>(source.getText(), TopicIdentifierTypes.SubjectIdentifier), null, 592, 160);
					gdlPanel.setTmEngine(new JtmsTmEngine());
					gdlPanel.setLoadSchemaCallback(new LoadSchemaCallback(GdlWebPage.this));
					gdlPanel.setCommitCallback(new CommitCallback(GdlWebPage.this));
					gdlPanel.setDeleteCallback(new DeleteCallback(GdlWebPage.this));
					GdlWebPage.this.setGdlPanel(gdlPanel);
					gdlPanel.loadSchema();
					gdlPanel.addStyleName(GdlWebPage.this.GDL_PANEL_STYLE_NAME);
					Window.scrollTo(0, 0);
				}catch(Exception e){
					e.printStackTrace();
					Window.alert("could not instantiate the GdlPanel, because(" + e.getClass() + "): " + e.getMessage());
					GdlWebPage.this.resetPage();	
				}
			}
		}
	}
	
	
	private class ResetClickHandler implements ClickHandler {
		@Override
		public void onClick(ClickEvent event) {
			Element elem = DOM.getElementById("hash_object_text_key_id__GDL_0");
			((TextAreaElement)elem).setValue("");
			elem = DOM.getElementById("hash_object_text_value_id__GDL_0");
			((TextAreaElement)elem).setValue("");
		}
	}
	
	
	private class PsiRequest implements RequestCallback {
		public PsiRequest(){}
		

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
						GdlWebPage.this.psiContainer.addPsi(psiValue);
					}
				}
				
				GdlWebPage.this.psiContainer.insertIn(GdlWebPage.this.navigationPanel);
			} else {
				Window.alert("could not request existing instances of " + GdlWebPage.this.topicTypePsi + ", because(" + response.getStatusCode() + "): " + response.getStatusText());
			}
			GdlWebPage.this.removeLoadScreenPanel();
		}
		

		@Override
		public void onError(Request request, Throwable exception) {
			String message = null;
			Class<? extends Throwable> eClass = null;
			if(exception != null){
				message = exception.getMessage();
				eClass = exception.getClass();
			}
			Window.alert("could not request existing instances of " + GdlWebPage.this.topicTypePsi + ", because(" + eClass + "): " + message);
			GdlWebPage.this.resetPage();
		}
	}
}
