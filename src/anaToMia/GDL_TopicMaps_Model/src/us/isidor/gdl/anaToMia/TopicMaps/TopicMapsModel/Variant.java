package us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel;

import java.math.BigDecimal;
import java.math.BigInteger;
import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.core.client.JsArray;


public class Variant extends JavaScriptObject implements DatatypeAware{
	protected Variant() {}

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
	

	public final native Name getParent() /*-{
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
	

	public final native BigDecimal decimalValue() /*-{
		return this.decimalValue();
	}-*/;
	

	public final native float floatValue() /*-{
		return this.floatValue();
	}-*/;
	

	public final native Locator getDatatype() /*-{
		return this.getDatatype();
	}-*/;
	

	public final native String getValue() /*-{
		return this.getValue();
	}-*/;
	

	public final native BigInteger integerValue() /*-{
		return this.integerValue();
	}-*/;
	

	public final native int intValue() /*-{
		return this.intValue();
	}-*/;
	

	public final native Locator locatorValue() /*-{
		return this.locatorValue();
	}-*/;
	
	
	public final native Long longValue() /*-{
		return this.longValue();
	}-*/;
	

	public final native void setValue(BigDecimal value) /*-{
		this.setValue(value);
	}-*/;
	

	public final native void setValue(BigInteger value) /*-{
		this.setValue(value);
	}-*/;

	
	public final native void setValue(float value) /*-{
		this.setValue(value);
	}-*/;

	
	public final native void setValue(int value) /*-{
		this.setValue(value);
	}-*/;

	
	public final native void setValue(Locator value) /*-{
		this.setValue(value);
	}-*/;
	
	
	public final native Long setValue(Long value) /*-{
	 	this.setValue(value);
	}-*/;

	
	public final native void setValue(String value) /*-{
		this.setValue(value);
	}-*/;

	
	public final native void setValue(String value, Locator datatype) /*-{
		this.setValue(value);
	}-*/;

	
	public final TopicMapsTypes classType(){
		return TopicMapsTypes.Variant;
	}
}
