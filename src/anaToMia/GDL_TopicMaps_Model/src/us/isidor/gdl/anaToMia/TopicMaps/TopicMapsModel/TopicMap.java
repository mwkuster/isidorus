package us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.core.client.JsArray;


public class TopicMap extends JavaScriptObject implements Reifiable{
	protected TopicMap() {}
	
	
	public final native void close() /*-{
		this.close();
	}-*/;
	
	
	public final native Association createAssociation(Topic type, JsArray<Topic> scope) /*-{
		return this.createAssociation(type, scope);
	}-*/;
	

	public final native Topic createTopic() /*-{
		return this.createTopic();
	}-*/;
	
	
	public final native Topic createTopicByItemIdentifier(Locator itemIdentifier) /*-{
		return this.createTopicByItemIdentifier(itemIdentifier);
	}-*/;
	
	
	public final native Topic createTopicBySubjectLocator(Locator subjectLocator) /*-{
		return this.createTopicBySubjectLocator(subjectLocator);
	}-*/;
	
	
	public final native Topic createTopicBySubjectIdentifier(Locator subjectIdentifier) /*-{
		return this.createTopicBySubjectIdentifier(subjectIdentifier);
	}-*/;
	
	
	public final native JsArray<Association> getAssociations() /*-{
		return this.getAssociations();
	}-*/;
	
	
	public final native Construct getConstructById(String id) /*-{
		return this.getConstructById(id);
	}-*/;
	
	
	public final native Construct getConstructByItemIdentifier(Locator itemIdentifier) /*-{
		return this.getConstructByItemIdentifier(itemIdentifier);
	}-*/;
	
	
	public final native Locator getLocator() /*-{
		return this.getLocator();
	}-*/;
	
	
	public final native Construct getParent() /*-{
		return this.getParent();
	}-*/;
	
	
	public final native Topic getTopicBySubjectLocator(Locator subjectLocator) /*-{
		return this.getTopicBySubjectLocator(subjectLocator);
	}-*/;
	
	
	public final native Topic getTopicBySubjectIdentifier(Locator subjectIdentifier) /*-{
		return this.getTopicBySubjectIdentifier(subjectIdentifier);
	}-*/;
	
	
	public final native JsArray<Topic> getTopics() /*-{
		return this.getTopics();
	}-*/;
	
	
	public final native void mergeIn(TopicMap other) /*-{
		this.mergeIn(other);
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


	public final native TopicMap getTopicMap() /*-{
		return this.getTopicMap();
	}-*/;


	public final native void remove() /*-{
		return this.remove();
	}-*/;


	public final native void removeItemIdentifier(Locator itemIdentifier) /*-{
		this.removeItemIdentifier(itemIdentifier);
	}-*/;


	public final native Topic getReifier() /*-{
		return this.getReifier();
	}-*/;


	public final native void setReifier(Topic reifier) /*-{
		this.setReifier(reifier);
	}-*/;
	
	
	public final native Locator createLocator(String reference) /*-{
		return this.createLocator(reference);
	}-*/;
	
	
	public final TopicMapsTypes classType(){
		return TopicMapsTypes.TopicMap;
	}
}
