package gdl.isidor.us.TopicMaps;

import com.google.gwt.core.client.JavaScriptObject;

public class Locator extends JavaScriptObject{
	
	protected Locator() {}
			
	public final native String getReference() /*-{
		return this.getReference();
	}-*/;
}
