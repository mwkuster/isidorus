package us.isidor.gdl.anaToMia.Widgets.base;

import us.isidor.gdl.anaToMia.TmEngine.jtmsBasedEngine.JtmsTmEngine;
import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.TmEngine;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMapsTypes;
import us.isidor.gdl.anaToMia.Widgets.isidorus.CommitCallback;
import us.isidor.gdl.anaToMia.Widgets.isidorus.DeleteCallback;
import us.isidor.gdl.anaToMia.Widgets.isidorus.LoadSchemaCallback;
import us.isidor.gdl.anaToMia.Widgets.environment.ICommitCallback;
import us.isidor.gdl.anaToMia.Widgets.environment.IDeleteCallback;
import us.isidor.gdl.anaToMia.Widgets.environment.ILoadSchemaCallback;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;
import us.isidor.gdl.anaToMia.Widgets.environment.TopicIdentifierTypes;
import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.dom.client.Element;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.dom.client.TextAreaElement;
import java.util.ArrayList;


public class TestClass implements EntryPoint{
	public final String MODEL_PREF = "http://services.org/serviceregistry/model/types/";
	public final String SERVICE_PSI = MODEL_PREF + "Service";
	public final String SORT_SERVICE = "http://service.org/serviceregistry/psis/service/sort-service";
	HorizontalPanel mainPanel = new HorizontalPanel();
	GdlPanel gdlPanel = null;

	@Override
	public void onModuleLoad() {
		try{
			RootPanel.get("GWT_Content").add(mainPanel);
			mainPanel.setBorderWidth(1);
			mainPanel.setPixelSize(1000, 600);

			ArrayList<Pair<String, TopicIdentifierTypes>> requestedTopicsToCreate = new ArrayList<Pair<String,TopicIdentifierTypes>>();
			requestedTopicsToCreate.add(new Pair<String, TopicIdentifierTypes>(SERVICE_PSI, TopicIdentifierTypes.SubjectIdentifier));
			
			Pair<String, TopicIdentifierTypes> requestedTopicToEdit = new Pair<String, TopicIdentifierTypes>(SORT_SERVICE, TopicIdentifierTypes.SubjectIdentifier);
			gdlPanel = new GdlPanel(requestedTopicToEdit, null);

			GdlPanel.addClickHandler("hash_object_reset_button_id", new ClickHandler() {
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
			gdlPanel.setLoadSchemaCallback(new TestLoadSchemaCallback());
			gdlPanel.setCommitCallback(new ICommitCallback() {
				@Override
				public void commitTmConstruct(ArrayList<Pair<Construct, TopicMapsTypes>> constructs, String buttonId, TmEngine tmEngine) {
					Window.alert("not implemented yet :-(");
				}
			});
			gdlPanel.setDeleteCallback(new IDeleteCallback() {
				@Override
				public void deleteTmConstruct(ArrayList<Pair<Construct, TopicMapsTypes>> constructs, TmEngine tmEngine, String buttonId) {
					Window.alert("not implemented yet :-(");
				}
			});

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
	}
}
