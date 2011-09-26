package us.isidor.gdl.anaToMia.Widgets.environment;

import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.FocusEvent;
import com.google.gwt.event.dom.client.FocusHandler;


public class FocusStyleHandler implements FocusHandler, BlurHandler{
	GdlVisibleObject widget = null;
	
	// this class is used to set the style of focused elements
	public FocusStyleHandler(GdlVisibleObject widget) {
		this.widget = widget;
	} 

	 
	@Override
	public void onFocus(FocusEvent event) {
		widget.onFocusStart(event, this);
	}


	@Override
	public void onBlur(BlurEvent event) {
		widget.onFocusEnd(event, this);
	}
}
