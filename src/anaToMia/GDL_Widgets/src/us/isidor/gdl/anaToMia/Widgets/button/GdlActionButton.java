package us.isidor.gdl.anaToMia.Widgets.button;

import java.util.ArrayList;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.ui.Button;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlPanel;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.value.AbsoluteNumValue;
import us.isidor.gdl.anaToMia.Widgets.value.BorderStyleValue;
import us.isidor.gdl.anaToMia.Widgets.value.ColorValue;
import us.isidor.gdl.anaToMia.Widgets.value.ContentOrientationValue;
import us.isidor.gdl.anaToMia.Widgets.value.CursorValue;
import us.isidor.gdl.anaToMia.Widgets.value.NumUnitValue;
import us.isidor.gdl.anaToMia.Widgets.value.TextAlignValue;

public class GdlActionButton extends GdlButton {
	
	protected GdlActionButton(){
		super();
	}
	
	
	public GdlActionButton(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);

		this.setDefaultValue();
		this.setClickHandlers();
	}
	
	
	public void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		this.createNewButton().setText(value);
	}
	
	
	// creates a new button if no one exist, otherwise returns the existing button
	protected Button createNewButton() throws InvalidGdlSchemaException, ExecutionException{
		if(this.subElements.size() != 0) return (Button)this.subElements.get(0);
		Button btn = new Button();
		this.subElements.add(btn);
		this.mainPanel.add(btn);
		this.setId(this.getId());
		this.setGdlStyle(btn);
		return btn;
	}
	
	
	protected void setClickHandlers() throws InvalidGdlSchemaException{
		ArrayList<ClickHandler> handlers = GdlPanel.getClickHandlers(this.getId());		
		if(handlers == null || this.subElements.size() == 0) return;
		
		for (ClickHandler handler : handlers)
			((Button)this.subElements.get(0)).addClickHandler(handler);
	}
	
	
	// sets the id property of this element by using the GWT DOM class
	@Override
	public void setId(String id){
		if(id != null && this.subElements.size() != 0)
			DOM.setElementProperty(this.subElements.get(0).getElement(), "id", id);
	}
	
	
	// content orientation has no effect on buttons
	@Override
	@Deprecated
	public void setContentOrientation(ContentOrientationValue value) throws InvalidGdlSchemaException, ExecutionException {
		return;
	}
	
	
	// returns a TextAlingvalue instance that represents the text-align of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null. The default value for gdl:Action-Button is Center
	@Override
	public TextAlignValue getTextAlign(String styleClass) throws InvalidGdlSchemaException {
		Occurrence textAlignOcc = null;
		if(styleClass != null){
			textAlignOcc = super.getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlTextAlign, styleClass);
		} else {
			textAlignOcc = super.getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlTextAlign);
		}
		
		if(textAlignOcc == null && styleClass != null){
			return null;
		} else if(textAlignOcc == null) {
			return TextAlignValue.CENTER;
		} else {
			try{
				return TextAlignValue.valueOf(textAlignOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlTextAlign + " must be set to one of \"left\", \"right\", \"center\" or \"justify\", but is \"" + textAlignOcc.getValue() + "\"");
			}
		}
	}
	
		
	// returns a AbsoluteNumValue instance that represents the width of this element's border.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AbsoluteNumValue getBorderWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderWidth);
		}

		if(widthOcc == null && styleClass != null){
			return null;
		} else if(widthOcc == null) {
			return new AbsoluteNumValue("1px");
		} else {
			return new AbsoluteNumValue(widthOcc.getValue());
		}
	} 
	
		
	// returns a NumUnitValue instance that represents the padding of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	@Override
	public NumUnitValue getPadding(String styleClass) throws InvalidGdlSchemaException {
		Occurrence paddingOcc = null;
		if(styleClass != null){
			paddingOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlPadding, styleClass);
		} else {
			paddingOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlPadding);
		}

		if(paddingOcc == null && styleClass != null){
			return null;
		} else if(paddingOcc == null) {
			return new NumUnitValue("5px");
		} else {
			return new NumUnitValue(paddingOcc.getValue());
		}		
	}	

	
	// returns a ColorValue instance that represents the color of this element's border.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	@Override
	public ColorValue getBorderColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderColor, styleClass);
		} else {
			colorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderColor);
		}

		if(colorOcc == null && styleClass != null){
			return null;
		} else if(colorOcc == null) {
			return new ColorValue("#bbbbbb");
		} else {
			return new ColorValue(colorOcc.getValue());
		}
	}
	
	
	
	// returns a ColorValue instance that represents the style of this element's border.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public BorderStyleValue getBorderStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderStyle, styleClass);
		} else {
			styleOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderStyle);
		}

		if(styleOcc == null && styleClass != null){
			return null;
		} else if(styleOcc == null) {
			return BorderStyleValue.SOLID;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}	
	
	
	// returns a CursorValue instance that represents the cursor of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
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
	
	
	
	// returns a NumUnitValue instance that represents the radius of this element's border.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public NumUnitValue getBorderRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRadius, styleClass);
		} else {
			radiusOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRadius);
		}

		if(radiusOcc == null && styleClass != null){
			return null;
		} else if(radiusOcc == null) {
			return new NumUnitValue("5px");
		} else {
			return new NumUnitValue(radiusOcc.getValue());
		}
	}
	
	
	// Returns the actual button element
	public Button getButton(){
		if(this.subElements.size() == 0) return null;
		else return (Button)this.subElements.get(0);
	}
	
	
	@Override
	@Deprecated
	public ArrayList<String> getSelectedValues(){
		return new ArrayList<String>();
	}
	
	
	@Override
	@Deprecated
	public void fixValue(){
		// has no effect on this element
	}
}
