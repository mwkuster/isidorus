package gdl.isidor.us.TopicMaps;

import com.google.gwt.core.client.JavaScriptObject;

public class TopicMap extends JavaScriptObject{
	
	protected TopicMap() {}
	
	public final native Locator getLoator() /*-{
		return this.getLocator();
	}-*/;
}
