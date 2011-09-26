package us.isidor.gdl.anaToMia.Widgets.base;


import us.isidor.gdl.anaToMia.Widgets.isidorus.LoadScreenPanel;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.RootPanel;


public class TestClass implements EntryPoint{
	HorizontalPanel mainPanel = new HorizontalPanel();
	GdlPanel gdlPanel = null;
	
	@Override
	public void onModuleLoad() {
		final LoadScreenPanel lsp = new LoadScreenPanel("Title", "message");
		RootPanel.get().add(lsp);
		
		Timer t = new Timer() {
			@Override
			public void run() {
				lsp.removeFromParent();
			}
		};
		
		t.schedule(5000);
		
		/*
		try{
			RootPanel.get("GWT_Content").add(mainPanel);
			mainPanel.setBorderWidth(1);
			mainPanel.setPixelSize(1000, 600);
			
			ArrayList<Pair<String, TopicIdentifierTypes>> requestedTopicsToCreate = new ArrayList<Pair<String,TopicIdentifierTypes>>();
			requestedTopicsToCreate.add(new Pair<String, TopicIdentifierTypes>("http://textgrid.org/serviceregistry/model/types/Hash-Object", TopicIdentifierTypes.SubjectIdentifier));
			//requestedTopicsToCreate.add(new Pair<String, TopicIdentifierTypes>("http://textgrid.org/serviceregistry/model/types/Environment", TopicIdentifierTypes.SubjectIdentifier));
			//requestedTopicsToCreate.add(new Pair<String, TopicIdentifierTypes>("http://psi.test.org/gdl-test/Poet", TopicIdentifierTypes.SubjectIdentifier));
			//requestedTopicsToCreate.add(new Pair<String, TopicIdentifierTypes>("http://psi.test.org/gdl-test/Musician", TopicIdentifierTypes.SubjectIdentifier));
			
			//Pair<String, TopicIdentifierTypes> requestedTopicToEdit = new Pair<String, TopicIdentifierTypes>("http://textgrid.org/serviceregistry/test-env-1", TopicIdentifierTypes.SubjectIdentifier);
			//Pair<String, TopicIdentifierTypes> requestedTopicToEdit = new Pair<String, TopicIdentifierTypes>("http://textgrid.org/serviceregistry/test-hash-2", TopicIdentifierTypes.SubjectIdentifier);
			//Pair<String, TopicIdentifierTypes> requestedTopicToEdit = new Pair<String, TopicIdentifierTypes>("http://textgrid.org/serviceregistry/hash-object/Test Environment 1/test1", TopicIdentifierTypes.SubjectIdentifier);
			gdlPanel = new GdlPanel(null, requestedTopicsToCreate);
			 
			gdlPanel.addClickHandler("hash_object_reset_button_id", new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					Element elem = DOM.getElementById("hash_object_text_key_id__GDL_0");
					((TextAreaElement)elem).setValue("");
					elem = DOM.getElementById("hash_object_text_value_id__GDL_0");
					((TextAreaElement)elem).setValue("");
				}
			});
			
			mainPanel.add(gdlPanel);
			gdlPanel.setTmEngine(new JtmsTmEngine());
			gdlPanel.setLoadSchemaCallback(new LoadSchemaCallback());
			gdlPanel.setCommitCallback(new CommitCallback());
			gdlPanel.setDeleteCallback(new DeleteCallback());
			
			Button requestButton = new Button("load schema");
			requestButton.addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					gdlPanel.loadSchema();
				}
			});
			
			mainPanel.add(requestButton);
		}catch(Exception e){
			e.printStackTrace();
			Window.alert(">> e >> " + e.getClass() + " >> " + e.getMessage());
		}
		*/
	}
}
