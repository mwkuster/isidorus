package us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.core.client.JsArray;

public class TypedStub extends JavaScriptObject implements Typed {
	protected TypedStub(){}
	
	
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
	
	
	public final native Topic getType() /*-{
		return this.getType();
	}-*/;
	
	
	public final native void setType(Topic type) /*-{
		this.setType(type);
	}-*/;
}
