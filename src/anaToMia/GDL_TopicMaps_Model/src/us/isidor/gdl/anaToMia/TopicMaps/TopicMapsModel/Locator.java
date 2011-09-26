package us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel;

import com.google.gwt.core.client.JavaScriptObject;

public class Locator extends JavaScriptObject{
	protected Locator() {};

	public final native boolean equal(Object other) /*-{
		return this.equals(other);
	}-*/;


	public final native String getReference() /*-{
		return this.getReference();
	}-*/;


	public final native Locator resolve(String reference) /*-{
		return this.resolve(reference);
	}-*/;
	
	
	public final TopicMapsTypes classType(){
		return TopicMapsTypes.Locator;
	}
}
