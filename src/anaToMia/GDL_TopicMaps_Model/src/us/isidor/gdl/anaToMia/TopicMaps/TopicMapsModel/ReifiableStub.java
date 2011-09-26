package us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.core.client.JsArray;


public class ReifiableStub extends JavaScriptObject implements Reifiable {
	protected ReifiableStub() {}
	
	
	public final native String getId() /*-{
		return this.getId() + "";
	}-*/;
	
 	
	public final native void addItemIdentifier(Locator itemIdentifier) /*-{
		this.addItemIdentifier(itemIdentifier);
	}-*/;
	
	
	public final native JsArray<Locator> getItemIdentifiers() /*-{
		return this.getItemIdentifiers();
	}-*/;
	
	
	public final native Construct getParent() /*-{
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
	
	
	public final native Topic getReifier() /*-{
		return this.getReifier();
	}-*/;
	
	
	public final native void setReifier(Topic reifier) /*-{
		this.setReifier(reifier);
	}-*/;
	
	
	public final TopicMapsTypes classType(){
		return TopicMapsTypes.Reifiable;
	}
}
