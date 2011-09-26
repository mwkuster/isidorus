package us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.core.client.JsArray;

public class Name extends JavaScriptObject implements Scoped, Reifiable, Typed{
	public final static String defaultNameTypePsi = "http://psi.topicmaps.org/iso13250/model/topic-name";
	
	
	protected Name() {}
	
	
	public final Variant createVariant(Locator value, JsArray<Topic> scope) {
		TopicMap tm = getTopicMap();
		return this.createVariant(value.getReference(), tm.createLocator(DatatypeAwareStub.xsdUri), scope);
	}
	
	
	public final Variant createVariant(String value, JsArray<Topic> scope) {
		TopicMap tm = getTopicMap();
		return this.createVariant(value, tm.createLocator(DatatypeAwareStub.xsdString), scope);
	}
	
	
	public final native Variant createVariant(String value, Locator datatype, JsArray<Topic> scope) /*-{
		return this.createVariant(value, datatype, scope);
	}-*/;
	
	
	public final native String getValue() /*-{
		return this.getValue();
	}-*/;
	
	
	public final native JsArray<Variant> getVariants() /*-{
		return this.getVariants();
	}-*/;
	
	
	public final native void setValue(String value) /*-{
		this.setValue(value);
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

	
	public final native Topic getParent() /*-{
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

	
	public final native Topic getType() /*-{
		return this.getType();
	}-*/;

	
	public final native void setType(Topic type) /*-{
		this.setType(type);
	}-*/;

	
	public final native Topic getReifier() /*-{
		return this.getReifier();
	}-*/;
	

	public final native void setReifier(Topic reifier) /*-{
		this.setReifier(reifier);
	}-*/;

	
	public final native void addTheme(Topic theme) /*-{
		this.addTheme(theme);
	}-*/;
	

	public final native void removeTheme(Topic theme) /*-{
		this.removeTheme(theme);
		
	}-*/;
	

	public final native JsArray<Topic> getScope() /*-{
		return this.getScope();
	}-*/;

	
	public final TopicMapsTypes classType(){
		return TopicMapsTypes.Name;
	}
}
