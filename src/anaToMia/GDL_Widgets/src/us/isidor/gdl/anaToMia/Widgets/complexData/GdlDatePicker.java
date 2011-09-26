package us.isidor.gdl.anaToMia.Widgets.complexData;


import java.util.Date;

import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.FocusEvent;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.dom.client.HasBlurHandlers;
import com.google.gwt.event.dom.client.HasFocusHandlers;
import com.google.gwt.event.dom.client.HasMouseDownHandlers;
import com.google.gwt.event.dom.client.HasMouseOutHandlers;
import com.google.gwt.event.dom.client.HasMouseOverHandlers;
import com.google.gwt.event.dom.client.HasMouseUpHandlers;
import com.google.gwt.event.dom.client.MouseDownEvent;
import com.google.gwt.event.dom.client.MouseDownHandler;
import com.google.gwt.event.dom.client.MouseOutEvent;
import com.google.gwt.event.dom.client.MouseOutHandler;
import com.google.gwt.event.dom.client.MouseOverEvent;
import com.google.gwt.event.dom.client.MouseOverHandler;
import com.google.gwt.event.dom.client.MouseUpEvent;
import com.google.gwt.event.dom.client.MouseUpHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.datepicker.client.DatePicker;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.ButtonableObject;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ActiveStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.FocusStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.HoverStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;



public class GdlDatePicker extends GdlComplexData {

	// some constructors
	protected GdlDatePicker(){
		super();
	}
	
	
	public GdlDatePicker(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		// create a date widget for each tm-construct
		this.createNewDateWidget();
		this.setNthButtons();
	}
	
	
	
	public DateWidget createNewDateWidget() throws InvalidGdlSchemaException, ExecutionException {
		// this object is able to own only one sub-element

		DateWidget date = new DateWidget();
		ActiveStyleHandler asHandler = new ActiveStyleHandler(this);
		FocusStyleHandler fsHandler = new FocusStyleHandler(this);
		HoverStyleHandler hsHandler = new HoverStyleHandler(this);
		int idSuffix = 0;
		if(this.subElements != null) idSuffix = this.subElements.size();
		date.setId(this.getId() + "__GDL_" + idSuffix);
		date.addMouseDownHandler(asHandler);
		date.addMouseUpHandler(asHandler);
		date.addMouseOverHandler(hsHandler);
		date.addMouseOutHandler(hsHandler);
		date.addFocusHandler(fsHandler);
		date.addBlurHandler(fsHandler);
		
		super.addToContainerPanel(date);	
		this.setGdlStyle(date);
		return date;
	}
	
	
	public ButtonableObject removeDatePicker(DateWidget elem) throws InvalidGdlSchemaException, ExecutionException{
		return this.removeFromContainer(elem);
	}
	
	
	@SuppressWarnings("deprecation")
	@Override
	public void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		DateWidget dateWidget = this.createNewDateWidget();
		Date dateValue = new Date();
		dateValue.setTime(Date.parse(value));
		dateWidget.setValue(dateValue);
	}
	
	
	@Override
	public void fixValue(){
		// TODO: implement
	}
	
	
	protected class DateWidget extends DatePicker implements HasFocusHandlers, HasBlurHandlers, HasMouseDownHandlers, HasMouseOutHandlers, HasMouseOverHandlers, HasMouseUpHandlers {
		public void setId(String id){
			DOM.setElementAttribute(this.getElement(), "id", id);
		}


		@Override
		public HandlerRegistration addMouseOverHandler(MouseOverHandler handler) {
			return this.addDomHandler(handler, MouseOverEvent.getType());
		}


		@Override
		public HandlerRegistration addMouseOutHandler(MouseOutHandler handler) {
			return this.addDomHandler(handler, MouseOutEvent.getType());
		}


		@Override
		public HandlerRegistration addBlurHandler(BlurHandler handler) {
			return this.addDomHandler(handler, BlurEvent.getType());
		}


		@Override
		public HandlerRegistration addFocusHandler(FocusHandler handler) {
			return this.addDomHandler(handler, FocusEvent.getType());
		}


		@Override
		public HandlerRegistration addMouseUpHandler(MouseUpHandler handler) {
			return this.addDomHandler(handler, MouseUpEvent.getType());
		}


		@Override
		public HandlerRegistration addMouseDownHandler(MouseDownHandler handler) {
			return this.addDomHandler(handler, MouseDownEvent.getType());
		}
	}
}
