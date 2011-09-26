package us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.core.client.JsArray;


public class Association extends JavaScriptObject implements Scoped, Reifiable, Typed{
	protected Association() {}
	
	
	public final native Role createRole(Topic type, Topic player) /*-{
		return this.createRole(type, player);
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


	public final native Topic getType() /*-{
		return this.getType();
	}-*/;


	public final native void setType(Topic type) /*-{
		this.setType(type);
	}-*/;
	
	
	public final native JsArray<Role> getRoles() /*-{
		return this.getRoles();
	}-*/;
	
	
	public final native JsArray<Role> getRoles(Topic type) /*-{
		return this.getRoles(type);
	}-*/;
	
	
	public final native JsArray<Topic> getRoleTypes() /*-{
		return this.getRoleTypes();
	}-*/;
	
	
	public final TopicMapsTypes classType(){
		return TopicMapsTypes.Association;
	}
}
