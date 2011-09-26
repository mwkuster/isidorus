package us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel;
import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.core.client.JsArray;



public class DatatypeAwareStub extends JavaScriptObject implements DatatypeAware {
	public final static String xsdString = "http://www.w3.org/2001/XMLSchema#string";
	public final static String xsdUri = "http://www.w3.org/2001/XMLSchema#anyUri";

	
	protected DatatypeAwareStub() {}
	
	
	public final native void addTheme(Topic theme) /*-{
		this.addTheme(theme);
	}-*/;
	
	
	public final native void removeTheme(Topic theme) /*-{
		this.removeTheme(theme);
	}-*/;
	
	
	public final native JsArray<Topic> getScope() /*-{
		return this.getScope();
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
		return TopicMapsTypes.DatatypeAware;
	}
}
