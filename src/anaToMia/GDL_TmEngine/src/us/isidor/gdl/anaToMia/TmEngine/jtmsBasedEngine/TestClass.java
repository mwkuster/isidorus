package us.isidor.gdl.anaToMia.TmEngine.jtmsBasedEngine;


import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.ExporterException;
import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.MissingReference;
import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.FormatException;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMap;
import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.TextArea;
import com.google.gwt.user.client.ui.VerticalPanel;


public class TestClass implements EntryPoint{
	JtmsTmEngine tme = new JtmsTmEngine();
	private TextArea userInputField = new TextArea();
	private Button importJsonButton = new Button();
	private Button exportJsonButton = new Button();
	private VerticalPanel mainPanel = new VerticalPanel();
	private Label topicsLabel = new Label();
	private Label associationsLabel = new Label();
	private Label exportedJTMLabel = new Label();
	private Label title = new Label();
	private HorizontalPanel statusPanel = new HorizontalPanel();
	private String tmData = "{\"version\":\"1.1\",\"prefixes\":{\"pref_1\":\"http://www.topicmaps.org/xtm/1.0/core.xtm#\",\"pref_2\":\"http://psi.topicmaps.org/iso13250/model/\",\"pref_5\":\"http://some.where/tmsparql/author/\",\"xsd\":\"http://www.w3.org/2001/XMLSchema#\",\"pref_3\":\"http://psi.topicmaps.org/tmcl/\",\"pref_6\":\"http://some.where/psis/poem/\",\"pref_4\":\"http://some.where/tmsparql/\",\"pref_7\":\"http://some.where/ii/zb/\",\"pref_8\":\"http://some.where/ii/\"},\"item_identifiers\":[\"[pref_4:jtm-tm]\"],\"topics\":[{\"subject_identifiers\":[\"[pref_1:topic]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":null,\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_1:association]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":null,\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_1:occurrence]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":null,\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_1:class-instance]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":null,\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_1:class]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":null,\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_1:supertype-subtype]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":null,\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_1:supertype]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":null,\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_1:subtype]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":null,\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_1:sort]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":null,\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_1:display]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":null,\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_2:type-instance]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":null,\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_2:type]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":null,\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_2:instance]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":null,\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_3:topic-type]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:topic-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_3:occurrence-type]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:topic-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_3:association-type]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:topic-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:written-by]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:association-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_3:role-type]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:topic-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:written]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:role-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:writer]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:role-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_3:name-type]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:topic-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_3:scope-type]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:topic-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:author]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:topic-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:poem]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:topic-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:first-name]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:name-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:last-name]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:name-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:title]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:name-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:display-name]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:scope-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:de]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:scope-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:date-of-birth]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:occurrence-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:date-of-death]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:occurrence-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:poem-content]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:occurrence-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:years]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:occurrence-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:isDead]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:occurrence-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:isAlive]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:occurrence-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_4:reifier-type]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_3:topic-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_5:goethe]\"],\"subject_locators\":null,\"item_identifiers\":[\"[pref_8:goethe]\"],\"instance_of\":[\"si:[pref_4:author]\"],\"names\":[{\"item_identifiers\":null,\"value\":\"Johann Wolfgang\",\"type\":\"si:[pref_4:first-name]\",\"scope\":null,\"variants\":null,\"reifier\":null},{\"item_identifiers\":null,\"value\":\"von Goethe\",\"type\":\"si:[pref_4:last-name]\",\"scope\":null,\"variants\":[{\"item_identifiers\":[\"[pref_8:goethe-variant]\"],\"datatype\":\"http://www.w3.org/2001/XMLSchema#string\",\"value\":\"Goethe\",\"scope\":[\"si:[pref_4:display-name]\"],\"reifier\":null}],\"reifier\":\"ii:[pref_8:goethe-name-reifier]\"},{\"item_identifiers\":[\"[pref_8:goethe-untyped-name]\"],\"value\":\"Johann Wolfgang von Goethe\",\"type\":null,\"scope\":null,\"variants\":null,\"reifier\":null}],\"occurrences\":[{\"item_identifiers\":[\"[pref_8:goethe-occ]\"],\"datatype\":\"http://www.w3.org/2001/XMLSchema#date\",\"type\":\"si:[pref_4:date-of-birth]\",\"value\":\"28.08.1749\",\"scope\":null,\"reifier\":\"ii:[pref_8:goethe-occ-reifier]\"},{\"item_identifiers\":null,\"datatype\":\"http://www.w3.org/2001/XMLSchema#date\",\"type\":\"si:[pref_4:date-of-death]\",\"value\":\"22.03.1832\",\"scope\":null,\"reifier\":null},{\"item_identifiers\":[\"[pref_8:goethe-years-occ]\"],\"datatype\":\"http://www.w3.org/2001/XMLSchema#integer\",\"type\":\"si:[pref_4:years]\",\"value\":\"82\",\"scope\":null,\"reifier\":null},{\"item_identifiers\":null,\"datatype\":\"http://www.w3.org/2001/XMLSchema#boolean\",\"type\":\"si:[pref_4:isDead]\",\"value\":\"true\",\"scope\":null,\"reifier\":null},{\"item_identifiers\":null,\"datatype\":\"http://www.w3.org/2001/XMLSchema#boolean\",\"type\":\"si:[pref_4:isAlive]\",\"value\":\"false\",\"scope\":null,\"reifier\":null}]},{\"subject_identifiers\":null,\"subject_locators\":null,\"item_identifiers\":[\"[pref_8:goethe-occ-reifier]\"],\"instance_of\":[\"si:[pref_4:reifier-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":null,\"subject_locators\":null,\"item_identifiers\":[\"[pref_8:goethe-name-reifier]\"],\"instance_of\":[\"si:[pref_4:reifier-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":[\"[pref_6:zauberlehrling]\"],\"subject_locators\":null,\"item_identifiers\":null,\"instance_of\":[\"si:[pref_4:poem]\"],\"names\":[{\"item_identifiers\":null,\"value\":\"Der Zauberlehrling\",\"type\":\"si:[pref_4:title]\",\"scope\":null,\"variants\":null,\"reifier\":null}],\"occurrences\":[{\"item_identifiers\":[\"[pref_7:occurrence]\"],\"datatype\":\"http://www.w3.org/2001/XMLSchema#string\",\"type\":\"si:[pref_4:poem-content]\",\"value\":\"Hat der alte Hexenmeister ... sich doch einmal wegbegeben! ...\",\"scope\":[\"si:[pref_4:de]\"],\"reifier\":null}]},{\"subject_identifiers\":null,\"subject_locators\":null,\"item_identifiers\":[\"[pref_8:association-reifier]\"],\"instance_of\":[\"si:[pref_4:reifier-type]\"],\"names\":null,\"occurrences\":null},{\"subject_identifiers\":null,\"subject_locators\":null,\"item_identifiers\":[\"[pref_8:role-reifier]\"],\"instance_of\":[\"si:[pref_4:reifier-type]\"],\"names\":null,\"occurrences\":null}],\"associations\":[{\"item_identifiers\":[\"[pref_8:association]\"],\"type\":\"si:[pref_4:written-by]\",\"reifier\":\"ii:[pref_8:association-reifier]\",\"scope\":null,\"roles\":[{\"item_identifiers\":null,\"type\":\"si:[pref_4:writer]\",\"reifier\":\"ii:[pref_8:role-reifier]\",\"player\":\"si:[pref_5:goethe]\"},{\"item_identifiers\":[\"[pref_8:role-2]\"],\"type\":\"si:[pref_4:written]\",\"reifier\":null,\"player\":\"si:[pref_6:zauberlehrling]\"}]}],\"item_type\":\"topicmap\",\"reifier\":null}";  

	@Override
	public void onModuleLoad() {
		TopicMap tm = tme.createTopicMap("http://my.topic.map/tm-1");
		
		RootPanel.get("GWT_Content").add(mainPanel);
		
		mainPanel.add(userInputField);
		userInputField.setPixelSize(Window.getClientWidth() - 100, Window.getClientHeight() / 6);
		userInputField.setValue(tmData);
		
		mainPanel.add(statusPanel);
		statusPanel.add(importJsonButton);
		statusPanel.add(exportJsonButton);
		statusPanel.add(topicsLabel);
		statusPanel.add(associationsLabel);
		
		topicsLabel.setText("topics: " + tm.getTopics().length());
		associationsLabel.setText("associations: " + tm.getAssociations().length());
		
		importJsonButton.setText("import JSON");
		importJsonButton.addClickHandler(new ImportJsonButtonClickHandler(tm, tme));
		
		exportJsonButton.setText("export topic map");
		exportJsonButton.addClickHandler(new ExportJsonButtonClickHandler(tm, tme));
		
		mainPanel.add(title);
		title.setText("Exported TM:");
		DOM.setStyleAttribute(title.getElement(), "fontSize", "larger");
		DOM.setStyleAttribute(title.getElement(), "fontWeight", "bold");
		DOM.setStyleAttribute(title.getElement(), "marginTop", "3%");
		
		mainPanel.add(exportedJTMLabel);
		DOM.setStyleAttribute(exportedJTMLabel.getElement(), "backgroundColor", "rgb(100, 255, 100)");
		DOM.setStyleAttribute(exportedJTMLabel.getElement(), "marginTop", "1em");
		exportedJTMLabel.setWidth(mainPanel.getOffsetWidth() + "px");
		DOM.setStyleAttribute(exportedJTMLabel.getElement(), "fontSize", "large");
		
		setStatusPanel();
	}
	
	private void setStatusPanel(){
		statusPanel.setWidth("30%");
		
		DOM.setStyleAttribute(topicsLabel.getElement(), "color", "red");
		DOM.setStyleAttribute(associationsLabel.getElement(), "color", "red");
		DOM.setStyleAttribute(topicsLabel.getElement(), "fontSize", "large");
		DOM.setStyleAttribute(associationsLabel.getElement(), "fontSize", "large");
	}
	
	
	private class ImportJsonButtonClickHandler implements ClickHandler {
		private TopicMap tm = null;
		private JtmsTmEngine tme = null;
		
		@SuppressWarnings("unused")
		private ImportJsonButtonClickHandler(){}
		
		
		public ImportJsonButtonClickHandler(TopicMap tm, JtmsTmEngine tme){
			this.tm = tm;
			this.tme = tme;
		}
		
		@Override
		public void onClick(ClickEvent event) {
			try{
				tme.importTopicMap(userInputField.getValue(), tm);
			}catch(FormatException fe){
				fe.printStackTrace();
				Window.alert("caught error: " + fe.getMessage());
			}catch(MissingReference me){
				me.printStackTrace();
				Window.alert("caught error: " + me.getMessage());
			}catch (Exception e){
				e.printStackTrace();
				Window.alert("cought error: " + e.getMessage());
			}finally {
				topicsLabel.setText("topics: " + tm.getTopics().length());
				associationsLabel.setText("associations: " + tm.getAssociations().length());
				setStatusPanel();
			}
		}
	}
	
	
	private class ExportJsonButtonClickHandler implements ClickHandler {
		private TopicMap tm = null;
		private JtmsTmEngine tme = null;
		
		@SuppressWarnings("unused")
		private ExportJsonButtonClickHandler(){}
		
		
		public ExportJsonButtonClickHandler(TopicMap tm, JtmsTmEngine tme){
			this.tm = tm;
			this.tme = tme;
		}
		
		@Override
		public void onClick(ClickEvent event) {
			String text = "JTM export failed";
			try{
				text = tme.exportTm(tm);
			}catch(ExporterException ee){
				Window.alert("caught error: " + ee.getMessage());
			}finally {
				exportedJTMLabel.setText(text);
			}
		}
	}
}
