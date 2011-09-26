package us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.core.client.JsArray;

public class ScopedStub extends JavaScriptObject implements Scoped {
	protected ScopedStub() {}


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

	
	public final native void addTheme(Topic theme) /*-{
		this.addTheme(theme);
	}-*/;


	public final native void removeTheme(Topic theme) /*-{
		this.removeTheme(theme);
	}-*/;


	public final native JsArray<Topic> getScope() /*-{
		return this.getScope();
	}-*/;
}
