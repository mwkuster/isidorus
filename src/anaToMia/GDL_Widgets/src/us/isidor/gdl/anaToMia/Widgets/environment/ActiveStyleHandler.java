package us.isidor.gdl.anaToMia.Widgets.environment;

import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import com.google.gwt.event.dom.client.MouseDownEvent;
import com.google.gwt.event.dom.client.MouseDownHandler;
import com.google.gwt.event.dom.client.MouseUpEvent;
import com.google.gwt.event.dom.client.MouseUpHandler;


public class ActiveStyleHandler implements MouseDownHandler, MouseUpHandler{
	GdlVisibleObject widget = null;
	
	public ActiveStyleHandler(GdlVisibleObject widget) {
		this.widget = widget;
	}
		

	@Override
	public void onMouseUp(MouseUpEvent event) {
		widget.onActiveEnd(event, this);
	}

	
	@Override
	public void onMouseDown(MouseDownEvent event) {
		widget.onActiveStart(event, this);
	}
}
