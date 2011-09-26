package us.isidor.gdl.anaToMia.Widgets.text;

import java.util.ArrayList;
import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.KeyPressEvent;
import com.google.gwt.event.dom.client.KeyPressHandler;
import com.google.gwt.event.dom.client.MouseOutEvent;
import com.google.gwt.event.dom.client.MouseUpEvent;
import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.TextArea;
import com.google.gwt.user.client.ui.Widget;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.ButtonableObject;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.environment.ActiveStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.FocusStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.HoverStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;
import us.isidor.gdl.anaToMia.Widgets.value.AbsoluteNumValue;
import us.isidor.gdl.anaToMia.Widgets.value.BorderStyleValue;
import us.isidor.gdl.anaToMia.Widgets.value.ColorValue;
import us.isidor.gdl.anaToMia.Widgets.value.NumUnitValue;
import us.isidor.gdl.anaToMia.Widgets.value.ResizeValue;
import us.isidor.gdl.anaToMia.Widgets.value.TextTypeValue;



public class GdlText extends GdlTextObject {
	protected ArrayList<PasswordKeyPressHandler> passwordKeyPressHandler = null;
	protected ArrayList<HandlerRegistration> passwordKeyPressRegistrations = null;
	
	// some constructors
	protected GdlText() throws InvalidGdlSchemaException, ExecutionException {
		super();
	}
	
	
	public GdlText(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		
		// If the receivedData is set and the tmRepresentative is bound to a value-group
		// that is bound to a constraint, the received data is consumed.
		// Otherwise, the default values are consumped, since no set constraint means
		// that no data of the received construct is determined to be set by this Widget.
		if(receivedData != null && this.getConstraint() != null) this.setReceivedData();
		else this.setDefaultValue();

		this.setNthButtons();
	}
	
	
	@Override
	public void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		this.createNewTextArea().setText(value);
	}
	
	
	@Override
	public void fixValue(){
		for (Widget ctrl : this.subElements) {
			if(ctrl instanceof ButtonableObject){
				Widget mo = ((ButtonableObject) ctrl).getMainObject();
				if(mo instanceof TextArea){
					((TextArea)mo).setReadOnly(true);
				}
			}
		}
	}
	
	
	// creates a new TextArea item, adds it to the subElements array,
	// and applies the styles on it
	protected TextArea createNewTextArea() throws InvalidGdlSchemaException, ExecutionException {
		TextArea elem = new TextArea();
		DOM.setElementAttribute(elem.getElement(), "id", this.getId() + "__GDL_" + this.subElements.size());
		this.setGdlStyle(elem);
		ActiveStyleHandler asHandler = new ActiveStyleHandler(this);
		FocusStyleHandler fsHandler = new FocusStyleHandler(this);
		HoverStyleHandler hsHandler = new HoverStyleHandler(this);
		elem.addMouseDownHandler(asHandler);
		elem.addMouseUpHandler(asHandler);
		elem.addMouseOverHandler(hsHandler);
		elem.addMouseOutHandler(hsHandler);
		elem.addFocusHandler(fsHandler);
		elem.addBlurHandler(fsHandler);
		super.addToContainerPanel(elem);
		return elem;
	}
	
	
	// removes the passed element and all its handlers from the outer element
	public void removeTextArea(TextArea elem) throws InvalidGdlSchemaException, ExecutionException{
		for (Pair<Widget, ArrayList<EventHandler>> item : this.eventHandlers) {
			if(item.getFirst().equals(elem)){
				this.eventHandlers.remove(item);
				break;
			}
		}
		
		this.removeFromContainer(elem);
	}
	
	
	// returns only a value if it was set, so the browsers default value is set
	@Override
	public AbsoluteNumValue getBorderWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderWidth);
		}

		if(widthOcc == null) return null;
		else return new AbsoluteNumValue(widthOcc.getValue());
	}
	
	
	// returns only a value if it was set, so the browsers default value is set
	@Override
	public AbsoluteNumValue getBorderTopWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopWidth);
		}

		if(widthOcc == null) return null;
		else return new AbsoluteNumValue(widthOcc.getValue());
	}
	
	
	// returns only a value if it was set, so the browsers default value is set
	@Override
	public AbsoluteNumValue getBorderRightWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRightWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRightWidth);
		}

		if(widthOcc == null) return null;
		else return new AbsoluteNumValue(widthOcc.getValue());
	}

	
	// returns only a value if it was set, so the browsers default value is set
	@Override
	public AbsoluteNumValue getBorderBottomWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomWidth);
		}

		if(widthOcc == null) return null;
		else return new AbsoluteNumValue(widthOcc.getValue());
	}

	
	// returns only a value if it was set, so the browsers default value is set
	@Override
	public AbsoluteNumValue getBorderLeftWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderLeftWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderLeftWidth);
		}

		if(widthOcc == null) return null;
		else return new AbsoluteNumValue(widthOcc.getValue());
	}
	
	
	// returns only a value if it was set, so the browsers default value is set
	public NumUnitValue getBorderRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRadius, styleClass);
		} else {
			radiusOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRadius);
		}

		if(radiusOcc == null) return null;
		else return new NumUnitValue(radiusOcc.getValue());
	}


	// returns only a value if it was set, so the browsers default value is set
	public NumUnitValue getBorderTopLeftRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopLeftRadius, styleClass);
		} else {
			radiusOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopLeftRadius);
		}

		if(radiusOcc == null) return null;
		else return new NumUnitValue(radiusOcc.getValue());
	}


	// returns only a value if it was set, so the browsers default value is set
	public NumUnitValue getBorderTopRightRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopRightRadius, styleClass);
		} else {
			radiusOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopRightRadius);
		}

		if(radiusOcc == null) return null;
		else return new NumUnitValue(radiusOcc.getValue());
	}

	
	// returns only a value if it was set, so the browsers default value is set
	public NumUnitValue getBorderBottomLeftRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomLeftRadius, styleClass);
		} else {
			radiusOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomLeftRadius);
		}

		if(radiusOcc == null) return null;
		else return new NumUnitValue(radiusOcc.getValue());
	}
	
	
	// returns only a value if it was set, so the browsers default value is set
	public NumUnitValue getBorderBottomRightRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomRightRadius, styleClass);
		} else {
			radiusOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomRightRadius);
		}

		if(radiusOcc == null) return null;
		else return new NumUnitValue(radiusOcc.getValue());
	}
	
	
	// returns only a value if it was set, so the browsers default value is set
	public ColorValue getBorderColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderColor, styleClass);
		} else {
			colorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderColor);
		}

		if(colorOcc == null ) return null;
		else return new ColorValue(colorOcc.getValue());
	}


	// returns only a value if it was set, so the browsers default value is set
	public ColorValue getBorderTopColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopColor, styleClass);
		} else {
			colorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopColor);
		}

		if(colorOcc == null ) return null;
		else return new ColorValue(colorOcc.getValue());
	}


	// returns only a value if it was set, so the browsers default value is set
	public ColorValue getBorderRightColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRightColor, styleClass);
		} else {
			colorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRightColor);
		}

		if(colorOcc == null ) return null;
		else return new ColorValue(colorOcc.getValue());
	}


	// returns only a value if it was set, so the browsers default value is set
	public ColorValue getBorderBottomColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomColor, styleClass);
		} else {
			colorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomColor);
		}

		if(colorOcc == null ) return null;
		else return new ColorValue(colorOcc.getValue());
	}


	// returns only a value if it was set, so the browsers default value is set
	public ColorValue getBorderLeftColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderLeftColor, styleClass);
		} else {
			colorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderLeftColor);
		}

		if(colorOcc == null ) return null;
		else return new ColorValue(colorOcc.getValue());
	}


	// returns only a value if it was set, so the browsers default value is set
	public BorderStyleValue getBorderStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderStyle, styleClass);
		} else {
			styleOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderStyle);
		}

		if(styleOcc == null) {
			return null;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}


	// returns only a value if it was set, so the browsers default value is set
	public BorderStyleValue getBorderTopStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopStyle, styleClass);
		} else {
			styleOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopStyle);
		}

		if(styleOcc == null) {
			return null;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}


	// returns only a value if it was set, so the browsers default value is set
	public BorderStyleValue getBorderRightStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRightStyle, styleClass);
		} else {
			styleOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRightStyle);
		}

		if(styleOcc == null) {
			return null;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}


	// returns only a value if it was set, so the browsers default value is set
	public BorderStyleValue getBorderBottomStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomStyle, styleClass);
		} else {
			styleOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomStyle);
		}

		if(styleOcc == null) {
			return null;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}


	// returns only a value if it was set, so the browsers default value is set
	public BorderStyleValue getBorderLeftStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderLeftStyle, styleClass);
		} else {
			styleOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderLeftStyle);
		}

		if(styleOcc == null) {
			return null;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}
	
	
	// returns a TextType instance of a gdl:text-type occurrence.
	// If no gdl:text-type occurrence is set, the default value is returned
	public TextTypeValue getTextType() throws InvalidGdlSchemaException {
		Occurrence typeOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlTextType);

		if(typeOcc != null){
			try{
				return TextTypeValue.valueOf(typeOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlTextType + " must be set to one of \"text\" or \"password\", but is \"" + typeOcc.getValue() + "\"");
			}
		} else {
			return TextTypeValue.Text;
		}
	}
	
	
	// returns a boolean instance of a gdl:readonly occurrence.
	// If no gdl:readonly occurrence is set, the default value is returned
	public boolean getReadonly() throws InvalidGdlSchemaException {
		Occurrence readOnlyOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlReadonly);

		if(readOnlyOcc != null){
			String boolStr = readOnlyOcc.getValue().toUpperCase();
			if(boolStr.equals("TRUE") || this.fixedDefaultValue()){
				return true;
			} else if(boolStr.equals("FALSE")) {
				return false;
			} else {
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlReadonly + " must be set to one of \"true\" or \"false\", but is \"" + readOnlyOcc.getValue() + "\"");
			}
		} else {
			return true;
		}
	}
	
	
	// returns an int instance of a gdl:rows occurrence.
	// If no gdl:rows occurrence is set, the default value is returned
	public int getRows() throws InvalidGdlSchemaException {
		Occurrence rowsOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlRows);

		if(rowsOcc != null){
			try{
				int value = Integer.valueOf(rowsOcc.getValue());
				if(value < 0) throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlRows + " must be set to a positive integer, but is \"" + rowsOcc.getValue() + "\""); 
				else return value;
			}catch(NumberFormatException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlRows + " must be set to a positive integer, but is \"" + rowsOcc.getValue() + "\"");
			}
		} else {
			return 1;
		}
	}
	
	
	// returns a boolean instance of a gdl:cols occurrence.
	// If no gdl:cols occurrence is set, the default value is returned
	public int getCols() throws InvalidGdlSchemaException {
		Occurrence colsOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlCols);
		
		if(colsOcc != null){
			try{
				int value = Integer.valueOf(colsOcc.getValue());
				if(value < 0) throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlCols + " must be set to a positive integer, but is \"" + colsOcc.getValue() + "\""); 
				else return value;
			}catch(NumberFormatException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlCols + " must be set to a positive integer, but is \"" + colsOcc.getValue() + "\"");
			}
		} else {
			return 5;
		}
	}
	
	
	// returns a boolean instance of a gdl:resize occurrence.
	// If no gdl:resize occurrence is set, the default value is returned
	public ResizeValue getResize() throws InvalidGdlSchemaException {
		Occurrence resizeOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlResize);

		if(resizeOcc != null){
			try{
				return ResizeValue.valueOf(resizeOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlResize + " must be set to one of \"both\", \"vertical\", \"horizontal\" or \"none\", but is \"" + resizeOcc.getValue() + "\"");
			}
		} else {
			return ResizeValue.NONE;
		}
	}
	
	
	// sets the resize style property.
	// If gdl:readonly is set to true, this property is set to none, i.e. the passed value is ignored.
	// IF gdl:readonly is set to false, this property is set to the passed argument.
	public void setResize(Widget widget, ResizeValue value) throws InvalidGdlSchemaException, ExecutionException{
		if(value == null || getReadonly() == true){
			DOM.setStyleAttribute(widget.getElement(), "resize", "none");
			if(value == null || value == ResizeValue.NONE) DOM.setStyleAttribute(widget.getElement(), "overflow", "hidden"); // IE: hide scrollbar
		} else{
			DOM.setStyleAttribute(widget.getElement(), "resize", value.getCssValue());
			if(value.equals(ResizeValue.NONE)) DOM.setStyleAttribute(widget.getElement(), "overflow", "hidden");
		}
	}
	
	
	// sets the readonly property of this element's text are element
	public void setReadonly(Widget widget, boolean value){
		((TextArea)widget).setReadOnly(value);
	}
	
	
	// sets and registers or unsets and deregisters a PasswordKeyPressHandler
	// on this element's text area item.
	public void setTextType(TextTypeValue value){
		if(value != null && value == TextTypeValue.Password){
			if(this.passwordKeyPressHandler == null) {
				this.passwordKeyPressHandler = new ArrayList<PasswordKeyPressHandler>();
				this.passwordKeyPressRegistrations = new ArrayList<HandlerRegistration>();
				
				for (Widget item : this.subElements) {
					TextArea elem = (TextArea)((ButtonableObject)item).getMainObject();
					PasswordKeyPressHandler handler = new PasswordKeyPressHandler();
					this.passwordKeyPressRegistrations.add(elem.addKeyPressHandler(handler));
				}
			}
		}else {
			if(this.passwordKeyPressRegistrations != null){
				for (HandlerRegistration reg : this.passwordKeyPressRegistrations) {
					reg.removeHandler();
				}
				this.passwordKeyPressHandler = null;
				this.passwordKeyPressRegistrations = null;
			}
		}
	}
	
	
	// sets the rows property if gdl:width is not set
	public void setRows(Widget widget, int value) throws InvalidGdlSchemaException, ExecutionException {
		if(value < 0) throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlRows + " must be set to a positive integer, but is \"" + value + "\"");
		Occurrence heightOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlHeight);

		// rows is only treated if height is not set
		if(heightOcc == null) ((TextArea)widget).setVisibleLines(value);
	}
	
	
	// sets the cols property if gdl:height is not set
	public void setCols(Widget widget, int value) throws InvalidGdlSchemaException, ExecutionException {
		if(value < 0) throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlRows + " must be set to a positive integer, but is \"" + value + "\"");
		Occurrence widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlWidth);

		// cols is only treated if width is not set
		if(widthOcc == null) DOM.setElementAttribute(widget.getElement(), "cols", String.valueOf(value));
	}
	
	
	// calls the super applies the GdlStyle for every TextArea item of this instance
	@Override
	protected void setGdlStyle() throws InvalidGdlSchemaException, ExecutionException{
		if(this.subElements != null){
			for (Widget item : this.subElements) {
				this.setGdlStyle(((ButtonableObject)item).getMainObject());
			}
		}
	}
	
	
	// sets the css properties, by calling the super class's method and the local
	// method, which sets some specific properties for the GdlText instance
	@Override
	public void setGdlStyle(Widget widget) throws InvalidGdlSchemaException, ExecutionException {
		super.setGdlStyle(widget);

		this.setReadonly(widget, this.getReadonly());
		this.setResize(widget, this.getResize());
		this.setTextType(this.getTextType());
		this.setRows(widget, this.getRows());
		this.setCols(widget, this.getCols());
	}

	
	
	// this class catches all keypress events and transforms the visible
	// characters to *, whereas the actual characters are collected and requestable
	// via the function getValue.
	protected class PasswordKeyPressHandler implements KeyPressHandler {
		String realValue = "";
		
		@Override
		public void onKeyPress(KeyPressEvent event) {
			TextArea elem = (TextArea) event.getSource();
			elem.cancelKey();
			elem.setText(elem.getText() + "*");
			realValue += (char)event.getCharCode() + "";
		}
		

		public String getValue(){
			return this.realValue;
		}
	}
	
	
	@Override
	public ArrayList<String> getSelectedValues(){
		ArrayList<String> result = new ArrayList<String>();
		for (Widget ctrl : this.subElements)
			if(ctrl instanceof ButtonableObject)
				if(((ButtonableObject) ctrl).getMainObject() instanceof TextArea)
					result.add(((TextArea)((ButtonableObject)ctrl).getMainObject()).getValue());
		return result;
	}
	
	
	// like setGdlStyle, except the width, minwidth, maxWidth, height, minHeight and maxHeight properties
	// are not set. This method should only be invoked of the onFocusEnd, onActiveEnd and onHoverEnd
	// to avoid the resizing of resizeable textarea elements
	private void setCleanGdlStyle(TextArea source) throws InvalidGdlSchemaException, ExecutionException{
		ResizeValue resize = this.getResize();
		
		// GdlVisibleObject
		this.setMargin(source, this.getMargin(null), null);
		this.setMarginTop(source, this.getMarginTop(null), null);
		this.setMarginRight(source, this.getMarginRight(null), null);
		this.setMarginBottom(source, this.getMarginBottom(null), null);
		this.setMarginLeft(source, this.getMarginLeft(null), null);
		
		this.setPadding(source, this.getPadding(null), null);
		this.setPaddingTop(source, this.getPaddingTop(null), null);
		this.setPaddingRight(source, this.getPaddingRight(null), null);
		this.setPaddingBottom(source, this.getPaddingBottom(null), null);
		this.setPaddingLeft(source, this.getPaddingLeft(null), null);

		this.setBorderColor(source, this.getBorderColor(null), null);
		this.setBorderTopColor(source, this.getBorderTopColor(null), null);
		this.setBorderRightColor(source, this.getBorderRightColor(null), null);
		this.setBorderBottomColor(source, this.getBorderBottomColor(null), null);
		this.setBorderLeftColor(source, this.getBorderLeftColor(null), null);

		this.setBorderStyle(source, this.getBorderStyle(null), null);
		this.setBorderTopStyle(source, this.getBorderTopStyle(null), null);
		this.setBorderRightStyle(source, this.getBorderRightStyle(null), null);
		this.setBorderBottomStyle(source, this.getBorderBottomStyle(null), null);
		this.setBorderLeftStyle(source, this.getBorderLeftStyle(null), null);

		this.setBorderWidth(source, this.getBorderWidth(null), null);
		this.setBorderTopWidth(source, this.getBorderTopWidth(null), null);
		this.setBorderRightWidth(source, this.getBorderRightWidth(null), null);
		this.setBorderBottomWidth(source, this.getBorderBottomWidth(null), null);
		this.setBorderLeftWidth(source, this.getBorderLeftWidth(null), null);

		this.setBorderRadius(source, this.getBorderRadius(null), null);
		this.setBorderTopRightRadius(source, this.getBorderTopRightRadius(null), null);
		this.setBorderBottomRightRadius(source, this.getBorderBottomRightRadius(null), null);
		this.setBorderBottomLeftRadius(source, this.getBorderBottomLeftRadius(null), null);
		this.setBorderTopLeftRadius(source, this.getBorderTopLeftRadius(null), null);

		this.setCursor(source, this.getCursor(null), null);

		this.setBackgroundColor(source, this.getBackgroundColor(null), null);
		
		if(resize == ResizeValue.NONE || resize == ResizeValue.VERTICAL){
			this.setWidth(source, this.getWidth(null), null);
			this.setMaxWidth(source, this.getMaxWidth(null), null);
			this.setMinWidth(source, this.getMinWidth(null), null);
		}

		if(resize == ResizeValue.NONE || resize == ResizeValue.HORIZONTAL){
			this.setHeight(source, this.getHeight(null), null);
			this.setMaxHeight(source, this.getMaxHeight(null), null);
			this.setMinHeight(source, this.getMinHeight(null), null);
		}
		
		// GdlTextObject
		this.setColor(source, this.getColor(null), null);
		
		this.setDirection(source, this.getDirection(null), null);
		this.setTextAlign(source, this.getTextAlign(null), null);
		this.setLineHeight(source, this.getLineHeight(null), null);
		this.setTextDecoration(source, this.getTextDecoration(null), null);
		this.setFontFamily(source, this.getFontFamily(null), null);
		this.setFontStyle(source, this.getFontStyle(null), null);
		this.setFontSize(source, this.getFontSize(null), null);
		this.setFontWeight(source, this.getFontWeight(null), null);
		this.setLetterSpacing(source, this.getLetterSpacing(null), null);
		this.setWordSpacing(source, this.getWordSpacing(null), null);
	}
	
	
	// shall be called to apply the styles of the focus class
	public void onActiveEnd(MouseUpEvent event, ActiveStyleHandler handler) {
		try{
			TextArea source = (TextArea)event.getSource();
			this.removeEventHandler(source, handler);
			EventHandler lastHandler = this.getLastHandler(source);
			if(lastHandler == null){
				this.setCleanGdlStyle(source);
			}else {
				if(lastHandler.getClass().equals(HoverStyleHandler.class)) this.onHoverStart(source);
				else this.onFocusStart(source);
			}
		}catch(Exception e){
			Window.alert("could not apply the default CSS style >> " + e.getClass() + " >> " + e.getMessage());
		}
	}
	
	
	// shall be called when the focus event was fired 
	public void onHoverEnd(MouseOutEvent event, HoverStyleHandler handler) {
		try{
			TextArea source = (TextArea)event.getSource();
			this.removeEventHandler(source, handler);
			EventHandler lastHandler = this.getLastHandler(source);
			if(lastHandler == null){
				this.setCleanGdlStyle(source);
			}else {
				if(lastHandler.getClass().equals(ActiveStyleHandler.class)) this.onActiveStart(source);
				else this.onFocusStart(source);
			}
		}catch(Exception e){
			Window.alert("could not apply the default CSS style >> " + e.getClass() + " >> " + e.getMessage());
			e.printStackTrace();
		}
	}
	
	
	// shall be called when the blur event was fired 
	public void onFocusEnd(BlurEvent event, FocusStyleHandler handler) {
		try{
			TextArea source = (TextArea)event.getSource();
			this.removeEventHandler(source, handler);
			EventHandler lastHandler = this.getLastHandler(source);
			if(lastHandler == null){
				this.setCleanGdlStyle(source);
			}else {
				if(lastHandler.getClass().equals(ActiveStyleHandler.class)) this.onActiveStart(source);
				else this.onHoverStart(source);
			}
		}catch(Exception e){
			Window.alert("could not apply the default CSS style >> " + e.getClass() + " >> " + e.getMessage());
		}
	}	
	
	
	// width, minWidth, maxWidth, height, minHeight, maxHeight properties are ignore for styleClasses of resizable elements
	@Override
	protected void setCssProperty(Widget elem, String styleClass, String cssProperty, String cssValue)throws InvalidGdlSchemaException, ExecutionException{
		if(styleClass != null){
			ResizeValue resize = this.getResize();
			if(resize == ResizeValue.NONE){
				super.setCssProperty(elem, styleClass, cssProperty, cssValue);
			}else if(resize == ResizeValue.HORIZONTAL){
				if(cssProperty.equals("width") || cssProperty.equals("minWidth") || cssProperty.equals("maxWidth")) {
					// do nothing this properties are ignored for resizable textareas
				} else {
					super.setCssProperty(elem, styleClass, cssProperty, cssValue);
				}
			}else if(resize == ResizeValue.VERTICAL) {
				if(cssProperty.equals("height") || cssProperty.equals("minHeight") || cssProperty.equals("maxHeight")) {
					// do nothing this properties are ignored for resizable textareas
				} else {
					super.setCssProperty(elem, styleClass, cssProperty, cssValue);
				}
			}else {
				if(cssProperty.equals("height") || cssProperty.equals("minHeight") || cssProperty.equals("maxHeight") || cssProperty.equals("width") || cssProperty.equals("minWidth") || cssProperty.equals("maxWidth")) {
					// do nothing this properties are ignored for resizable textareas
				} else {
					super.setCssProperty(elem, styleClass, cssProperty, cssValue);
				}
			}
		}else {
			super.setCssProperty(elem, styleClass, cssProperty, cssValue);
		}
	}
	
	
	// width, minWidth, maxWidth, height, minHeight, maxHeight properties are ignore for styleClasses of resizable elements
	protected void setCssProperty(String styleClass, String cssProperty, String cssValue)throws InvalidGdlSchemaException, ExecutionException{
		if(styleClass != null){
			ResizeValue resize = this.getResize();
			if(resize == ResizeValue.NONE){
				super.setCssProperty(styleClass, cssProperty, cssValue);
			}else if(resize == ResizeValue.HORIZONTAL){
				if(cssProperty.equals("width") || cssProperty.equals("minWidth") || cssProperty.equals("maxWidth")) {
					// do nothing this properties are ignored for resizable textareas
				} else {
					super.setCssProperty(styleClass, cssProperty, cssValue);
				}
			}else if(resize == ResizeValue.VERTICAL) {
				if(cssProperty.equals("height") || cssProperty.equals("minHeight") || cssProperty.equals("maxHeight")) {
					// do nothing this properties are ignored for resizable textareas
				} else {
					super.setCssProperty(styleClass, cssProperty, cssValue);
				}
			}else {
				if(cssProperty.equals("height") || cssProperty.equals("minHeight") || cssProperty.equals("maxHeight") || cssProperty.equals("width") || cssProperty.equals("minWidth") || cssProperty.equals("maxWidth")) {
					// do nothing this properties are ignored for resizable textareas
				} else {
					super.setCssProperty(styleClass, cssProperty, cssValue);
				}
			}
		}else {
			super.setCssProperty(styleClass, cssProperty, cssValue);
		}
	}

}
