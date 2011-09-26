package us.isidor.gdl.anaToMia.Widgets.text;

import java.util.ArrayList;

import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.FocusEvent;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.dom.client.HasBlurHandlers;
import com.google.gwt.event.dom.client.HasFocusHandlers;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Label;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.environment.ActiveStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.FocusStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.HoverStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.value.ColorValue;
import us.isidor.gdl.anaToMia.Widgets.value.CursorValue;
import us.isidor.gdl.anaToMia.Widgets.value.TextDecorationValue;


public class GdlReference extends GdlTextObject{
	// some constructors
	protected GdlReference() throws InvalidGdlSchemaException, ExecutionException {
		super();
	}
	
	public GdlReference(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		// TODO: create a Reference element for each TM-elem
		ReferenceWidget ref = this.createReference(); // TODO: remove
		ref.setText("Reference"); // TODO: remove
		ref.setHref("http://www.google.de"); // TODO: remove
		this.setNthButtons();
	}
	
	
	@Override
	public void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		// TODO: implement
	}
	
	
	@Override
	@Deprecated
	public void fixValue(){
		// has no effect on this element
	}
	
	
	private ReferenceWidget createReference() throws InvalidGdlSchemaException, ExecutionException {
		ReferenceWidget reference = new ReferenceWidget();
		DOM.setElementAttribute(reference.getElement(), "id", this.getId() + "__GDL_" + this.subElements.size());
		super.addToContainerPanel(reference);
		ActiveStyleHandler asHandler = new ActiveStyleHandler(this);
		FocusStyleHandler fsHandler = new FocusStyleHandler(this);
		HoverStyleHandler hsHandler = new HoverStyleHandler(this);
		reference.addMouseDownHandler(asHandler);
		reference.addMouseUpHandler(asHandler);
		reference.addMouseOverHandler(hsHandler);
		reference.addMouseOutHandler(hsHandler);
		reference.addFocusHandler(fsHandler);
		reference.addBlurHandler(fsHandler);
		reference.addClickHandler(new ReferenceClickHandler());
		super.setGdlStyle(reference);
		return reference;
	}
	
	
	// removes the passed element
	public void removeReference(ReferenceWidget elem) throws InvalidGdlSchemaException, ExecutionException {
		this.removeFromContainer(elem);
	}
	
	
	// returns a ColorValue instance that represents the text color of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public ColorValue getColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = super.getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlColor, styleClass);
		} else {
			colorOcc = super.getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlColor);
		}

		if(colorOcc == null && (styleClass == null || styleClass.equals(PSIs.GDL.Scope.gdlFocus) || styleClass.equals(PSIs.GDL.Scope.gdlHover))){
			return new ColorValue("#0000FF");
		}else if(colorOcc == null && styleClass.equals(PSIs.GDL.Scope.gdlActive)){
			return null;
		}else {
			return new ColorValue(colorOcc.getValue());
		}
	}
	
	
	// returns a CursorValue instance that represents the cursor of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	@Override
	public CursorValue getCursor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence cursorOcc = null;
		if(styleClass != null){
			cursorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlCursor, styleClass);
		} else {
			cursorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlCursor);
		}

		if(cursorOcc == null && styleClass != null){
			return null;
		} else if(cursorOcc == null) {
			return CursorValue.POINTER;
		} else {
			try{
				return CursorValue.valueOf(cursorOcc.getValue().toUpperCase().replace("-", "_"));
			}catch(IllegalArgumentException e){
				String values = "auto, default, crosshair, pointer, move, n-resize, ne-resize," +
				"nw-resize, e-resize, se-resize, s-resize, sw-resize, w-resize," +
				"text, wait, help, or progress";
				throw new InvalidGdlSchemaException("cursor must be set to one of " + values + ", but is " + cursorOcc.getValue());
			}
		}	
	}
	
	
	// returns a TextDecoarionValue instance that represents the text-decoration of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	@Override
	public TextDecorationValue getTextDecoration(String styleClass) throws InvalidGdlSchemaException {
		Occurrence decorationOcc = null;
		if(styleClass != null){
			decorationOcc = super.getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlTextDecoration, styleClass);
		} else {
			decorationOcc = super.getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlTextDecoration);
		}

		if(decorationOcc == null && styleClass != null){
			return null;
		} else if(decorationOcc == null) {
			return TextDecorationValue.UNDERLINE;
		} else {
			try{
				return TextDecorationValue.valueOf(decorationOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlTextDecoration + " must be set to one of \"underline\", \"overline\", \"line-through\", \"blink\" or \"none\", but is \"" + decorationOcc.getValue() + "\"");
			}
		}
	}
	
	
	// this class is responsible for the default click behavior of a GdlReference instance
	protected class ReferenceClickHandler implements ClickHandler  {
		@Override
		public void onClick(ClickEvent event) {
			ReferenceWidget ref = (ReferenceWidget)event.getSource();
			Window.open(ref.getHref(), "new_window", "");
			/*
			TODO: implement
			
			If a gdl:Reference element is bound to a gdl:Literal-Value (cf. chapter 7.16.2).
			The set value must be treated as a network address, i.e. if this element is clicked
			a new tabulator or a new window must appear with the content represented
			by the given URI.
			If a gdl:Reference element is bound to a gdl:TM-Value (cf. chapter 7.16.4),
			the set value must be treated as a topic value, i.e. if a gdl:Reference
			element is clicked a new tabulator or a new window must appear with the
			gdl:Default-Editor-View bound to the topic represented by the given URI.
			If no gdl:Default-Topic-View is defined for the requested topic,
			a warning must be thrown followed by a regular proceeding of the GDL-frontend. 
			*/
		}
	}
	
	
	@Override
	@Deprecated
	public ArrayList<String> getSelectedValues(){
		return new ArrayList<String>();
	}
	
	
	// this class wraps a Label that is used as 
	protected class ReferenceWidget extends Label implements HasFocusHandlers, HasBlurHandlers {
		private String href = "";
		
		public ReferenceWidget(){
			super();
		}
		
		
		public ReferenceWidget(String text, String href) {
			super(text);
			if(href != null) this.href = href;
		}
		
		
		public String getHref(){
			return this.href;
		}
		
		
		public void setHref(String href){
			if(href != null) this.href = href;
			else this.href = "";
			
		}
		

		@Override
		public HandlerRegistration addBlurHandler(BlurHandler handler) {
			return this.addDomHandler(handler, BlurEvent.getType());
		}

		@Override
		public HandlerRegistration addFocusHandler(FocusHandler handler) {
			return this.addDomHandler(handler, FocusEvent.getType());
		}
	}
}
