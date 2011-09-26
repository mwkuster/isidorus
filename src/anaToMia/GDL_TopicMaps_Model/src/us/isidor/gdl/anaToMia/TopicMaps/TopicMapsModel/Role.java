package us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel;


import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.core.client.JsArray;


public class Role extends JavaScriptObject implements Reifiable, Typed {
	protected Role() {}
	
	
	public final native Topic getPlayer() /*-{
		return this.getPlayer();
	}-*/;
	
	
	public final native void setPlayer(Topic player) /*-{
		this.setPlayer(player);
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

	
	public final native Association getParent() /*-{
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
	
	
	public final TopicMapsTypes classType(){
		return TopicMapsTypes.Role;
	}
}
