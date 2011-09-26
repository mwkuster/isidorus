package us.isidor.gdl.anaToMia.Widgets.environment;


import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import com.google.gwt.event.dom.client.MouseOutEvent;
import com.google.gwt.event.dom.client.MouseOutHandler;
import com.google.gwt.event.dom.client.MouseOverEvent;
import com.google.gwt.event.dom.client.MouseOverHandler;


public class HoverStyleHandler implements MouseOverHandler, MouseOutHandler{
	GdlVisibleObject widget = null;
	
	
	public HoverStyleHandler(GdlVisibleObject widget){
		this.widget = widget;
	}

	
	@Override
	public void onMouseOut(MouseOutEvent event) {
		widget.onHoverEnd(event, this);
	}

	
	@Override
	public void onMouseOver(MouseOverEvent event) {
		widget.onHoverStart(event, this);
	}
}
