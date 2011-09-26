package us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.core.client.JsArray;


public class Topic extends JavaScriptObject implements Construct{
	protected Topic() {}

	
	public final native void addSubjectLocator(Locator subjectLocator) /*-{
		this.addSubjectLocator(subjectLocator);
	}-*/;
	
	
	public final native void addSubjectIdentifier(Locator subjectIdentifier) /*-{
		this.addSubjectIdentifier(subjectIdentifier);
	}-*/;
	
	
	public final native void addType(Topic type) /*-{
		this.addType(type);
	}-*/;
	
	
	public final Name createName(String value, JsArray<Topic> scope){
		TopicMap tm = this.getTopicMap(); 
		Topic defaultNameType = tm.createTopicBySubjectIdentifier(tm.createLocator(Name.defaultNameTypePsi));
		return this.createName(value, defaultNameType, scope);
	}
	
	
	public final native Name createName(String value, Topic type, JsArray<Topic> scope) /*-{
		return this.createName(value, type, scope);
	}-*/;
	
	
	public final Occurrence createOccurrence(Topic type, Locator value, JsArray<Topic> scope) {
		TopicMap tm = getTopicMap();
		return createOccurrence(type, value.getReference(), tm.createLocator(DatatypeAwareStub.xsdUri), scope);
	}
	
	
	public final Occurrence createOccurrence(Topic type, String value, JsArray<Topic> scope) {
		TopicMap tm = getTopicMap();
		return createOccurrence(type, value, tm.createLocator(DatatypeAwareStub.xsdString), scope);
	}
	
	
	public final native Occurrence createOccurrence(Topic type, String value, Locator datatype, JsArray<Topic> scope) /*-{
		return this.createOccurrence(type, value, datatype, scope);
	}-*/;
	
	
	public final native JsArray<Name> getNames() /*-{
		return this.getNames();
	}-*/;
	
	
	public final native JsArray<Name> getNames(Topic type) /*-{
		return this.getNames(type);
	}-*/;
	
	
	public final native JsArray<Occurrence> getOccurrences() /*-{
		return this.getOccurrences();
	}-*/;


	public final native JsArray<Occurrence> getOccurrences(Topic type) /*-{
		return this.getOccurrences(type);
	}-*/;
	
	
	public final native Reifiable getReified() /*-{
		return this.getReified();
	}-*/;
	
	
	public final native JsArray<Role> getRolesPlayed() /*-{
		return this.getRolesPlayed();
	}-*/;
	
	
	public final native JsArray<Role> getRolesPlayed(Topic type) /*-{
		return this.getRolesPlayed(type);
	}-*/;
	
	
	public final native JsArray<Role> getRolesPlayed(Topic type, Topic assocType) /*-{
		return this.getRolesPlayed(type, assocType);
	}-*/;
	
	
	public final native JsArray<Locator> getSubjectIdentifiers() /*-{
		return this.getSubjectIdentifiers();
	}-*/;
	
	
	public final native JsArray<Locator> getSubjectLocators() /*-{
		return this.getSubjectLocators();
	}-*/;
	
	
	public final native JsArray<Topic> getTypes() /*-{
		return this.getTypes();
	}-*/;
	
	
	public final native void mergeIn(Topic other) /*-{
		this.mergeIn(other);
	}-*/;
	
	
	public final native void removeSubjectIdentifier(Locator subjectIdentifier) /*-{
		this.removeSubjectIdentifier(subjectIdentifier);
	}-*/;
	
	
	public final native void removeSubjectLocator(Locator subjectLocator) /*-{
		this.removeSubjectIdentifier(subjectLocator);
	}-*/;
	
	
	public final native void removeType(Topic type) /*-{
		this.removeType(type);		
	}-*/;
	

	public final native String getId() /*-{
		return this.getId() + "";
	}-*/;

	
	public final native void addItemIdentifier(Locator itemIdentifier) /*-{
		this.addItemIdentifier(itemIdentifier);
	}-*/;
	

	public final native JsArray<Locator> getItemIdentifiers() /*-{
		return this.getItemIdentifiers();
	}-*/;

	
	public final native TopicMap getParent() /*-{
		return this.getParent();
	}-*/;
	

	public final native TopicMap getTopicMap() /*-{
		return this.getTopicMap();
	}-*/;
	

	public final native void remove() /*-{
		this.remove();
	}-*/;
	

	public final native void removeItemIdentifier(Locator itemIdentifier) /*-{
		this.removeItemIdentifier(itemIdentifier);
	}-*/;
	
	
	public final TopicMapsTypes classType(){
		return TopicMapsTypes.Topic;
	}
}
