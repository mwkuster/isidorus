package us.isidor.gdl.anaToMia.Widgets.text;

import com.google.gwt.dom.client.Style.FontStyle;
import com.google.gwt.user.client.ui.Widget;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.value.ColorValue;
import us.isidor.gdl.anaToMia.Widgets.value.DirectionValue;
import us.isidor.gdl.anaToMia.Widgets.value.FontWeightValue;
import us.isidor.gdl.anaToMia.Widgets.value.NormalNumUnitValue;
import us.isidor.gdl.anaToMia.Widgets.value.PositiveNumUnitValue;
import us.isidor.gdl.anaToMia.Widgets.value.TextAlignValue;
import us.isidor.gdl.anaToMia.Widgets.value.TextDecorationValue;


public abstract class GdlTextObject extends GdlVisibleObject {
	
	// some constructors
	protected GdlTextObject(){
		super();
	}
	
	
	public GdlTextObject(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		super.setGdlStyle();
	}
	
	
	// returns a DirectionValue instance that represents the text direction of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public DirectionValue getDirection(String styleClass) throws InvalidGdlSchemaException {
		Occurrence directionOcc = null;
		if(styleClass != null){
			directionOcc = super.getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlDirection, styleClass);
		} else {
			directionOcc = super.getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlDirection);
		}

		if(directionOcc == null && styleClass != null){
			return null;
		} else if(directionOcc == null) {
			return DirectionValue.LTR;
		} else {
			try{
				return DirectionValue.valueOf(directionOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlDirection + " must be set to one of \"ltr\" or \"rtl\", but is \"" + directionOcc.getValue() + "\"");
			}
		}
	}
	
	
	// returns a TextAlingvalue instance that represents the text-align of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
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
			return TextAlignValue.LEFT;
		} else {
			try{
				return TextAlignValue.valueOf(textAlignOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlTextAlign + " must be set to one of \"left\", \"right\", \"center\" or \"justify\", but is \"" + textAlignOcc.getValue() + "\"");
			}
		}
	}
	
	
	// returns a NormalNumUnitValue instance that represents the line-height of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public NormalNumUnitValue getLineHeight(String styleClass) throws InvalidGdlSchemaException {
		Occurrence lineHeightOcc = null;
		if(styleClass != null){
			lineHeightOcc = super.getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlLineHeight, styleClass);
		} else {
			lineHeightOcc = super.getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlLineHeight);
		}
		
		if(lineHeightOcc == null && styleClass != null){
			return null;
		} else if(lineHeightOcc == null) {
			return new NormalNumUnitValue();
		} else {
			return new NormalNumUnitValue(lineHeightOcc.getValue());
		}
	}
	
	
	// returns a TextDecoarionValue instance that represents the text-decoration of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
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
			return TextDecorationValue.NONE;
		} else {
			try{
				return TextDecorationValue.valueOf(decorationOcc.getValue().toUpperCase().replace("-", "_"));
			}catch(IllegalArgumentException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlTextDecoration + " must be set to one of \"underline\", \"overline\", \"line-through\", \"blink\" or \"none\", but is \"" + decorationOcc.getValue() + "\"");
			}
		}
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

		if(colorOcc == null && styleClass != null){
			return null;
		} else if(colorOcc == null) {
			return new ColorValue();
		} else {
			return new ColorValue(colorOcc.getValue());
		}
	}
	
	
	// returns a String instance that represents the text font-family of this element.
	public String getFontFamily(String styleClass) throws InvalidGdlSchemaException {
		Occurrence fontOcc = null;
		if(styleClass != null){
			fontOcc = super.getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlFontfamily, styleClass);
		} else {
			fontOcc = super.getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlFontfamily);
		}

		if(fontOcc == null){
			return null; // use the browser's default font
		} else {
			return fontOcc.getValue();
		}
	}
	
	
	// returns a FontStyle instance that represents the text font-style of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public FontStyle getFontStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = super.getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlFontStyle, styleClass);
		} else {
			styleOcc = super.getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlFontStyle);
		}
		
		if(styleOcc == null && styleClass != null){
			return null;
		} else if(styleOcc == null) {
			return FontStyle.NORMAL;
		} else {
			try{
				return FontStyle.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlFontStyle + " must be set to one of \"normal\", \"italic\" or \"oblique\", but is \"" + styleOcc.getValue() + "\"");
			}
		}
	}
	
	
	// returns a PositiveNumUnitValue instance that represents the text font-size of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public PositiveNumUnitValue getFontSize(String styleClass) throws InvalidGdlSchemaException {
		Occurrence sizeOcc = null;
		if(styleClass != null){
			sizeOcc = super.getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlFontSize, styleClass);
		} else {
			sizeOcc = super.getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlFontSize);
		}

		if(sizeOcc == null && styleClass != null){
			return null;
		} else if(sizeOcc == null) {
			return new PositiveNumUnitValue("12pt");
		} else {
			return new PositiveNumUnitValue(sizeOcc.getValue());
		}
	}
	
	
	// returns a FontWeightValue instance that represents the text font-weight of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public FontWeightValue getFontWeight(String styleClass) throws InvalidGdlSchemaException {
		Occurrence weightOcc = null;
		if(styleClass != null){
			weightOcc = super.getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlFontWeight, styleClass);
		} else {
			weightOcc = super.getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlFontWeight);
		}
		
		if(weightOcc == null && styleClass != null){
			return null;
		} else if(weightOcc == null) {
			return FontWeightValue.NORMAL;
		} else {
			try{
				return FontWeightValue.fromString(weightOcc.getValue());
			}catch(IllegalArgumentException e){
				String values = "normal, bold, bolder, lighter, 100, 200, 300, 400, 500, 600, 700, 800 or 900";
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlFontWeight + " must be set to one of " + values + ", but found " + weightOcc.getValue() + "\"");
			}
		}
	}
	
	
	// returns a NormalNumUnitValue instance that represents the text letter-spacing of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public NormalNumUnitValue getLetterSpacing(String styleClass) throws InvalidGdlSchemaException {
		Occurrence spacingOcc = null;
		if(styleClass != null){
			spacingOcc = super.getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlLetterSpacing, styleClass);
		} else {
			spacingOcc = super.getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlLetterSpacing);
		}
		
		if(spacingOcc == null && styleClass != null){
			return null;
		} else if(spacingOcc == null) {
			return new NormalNumUnitValue();
		} else {
			return new NormalNumUnitValue(spacingOcc.getValue());
		}
	}
	
	
	// returns a NormalNumUnitValue instance that represents the text word-spacing of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public NormalNumUnitValue getWordSpacing(String styleClass) throws InvalidGdlSchemaException {
		Occurrence spacingOcc = null;
		if(styleClass != null){
			spacingOcc = super.getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlWordSpacing, styleClass);
		} else {
			spacingOcc = super.getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlWordSpacing);
		}
		
		if(spacingOcc == null && styleClass != null){
			return null;
		} else if(spacingOcc == null) {
			return new NormalNumUnitValue();
		} else {
			return new NormalNumUnitValue(spacingOcc.getValue());
		}
	}


	// sets the direction style property of this element by using the GWT DOM class
	public void setDirection(Widget widget, DirectionValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "direction", value.getCssValue());
	}
	
	
	// sets the text-align style property of this element by using the GWT DOM class
	public void setTextAlign(Widget widget, TextAlignValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "textAlign", value.getCssValue());
	}
	
	
	// sets the line-height style property of this element by using the GWT DOM class
	public void setLineHeight(Widget widget, NormalNumUnitValue value, String styleClass) throws InvalidGdlSchemaException,	ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "lineHeight", value.getCssValue());
	}
	
	
	// sets the text-decoration style property of this element by using the GWT DOM class
	public void setTextDecoration(Widget widget, TextDecorationValue value,	String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "textDecoration", value.getCssValue());
	}
	
	
	// sets the color style property of this element by using the GWT DOM class
	public void setColor(Widget widget, ColorValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "color", value.getCssValue());
	}
	
	
	// sets the font-family style property of this element by using the GWT DOM class
	public void setFontFamily(Widget widget, String value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "fontFamily", value);
	}
	
	
	// sets the font-style style property of this element by using the GWT DOM class
	public void setFontStyle(Widget widget, FontStyle value, String styleClass)	throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "fontStyle", value.getCssName());
	}
	
	
	// sets the font-size style property of this element by using the GWT DOM class
	public void setFontSize(Widget widget, PositiveNumUnitValue value, String styleClass) throws InvalidGdlSchemaException,	ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "fontSize", value.getCssValue());
	}
	
	
	// sets the font-weight style property of this element by using the GWT DOM class
	public void setFontWeight(Widget widget, FontWeightValue value,	String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "fontWeight", value.getCssValue());
	}
	
	
	// sets the letter-spacing style property of this element by using the GWT DOM class
	public void setLetterSpacing(Widget widget, NormalNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "letterSpacing", value.getCssValue());
	}
	
	
	// sets the word-spacing style property of this element by using the GWT DOM class
	public void setWordSpacing(Widget widget, NormalNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "wordSpacing", value.getCssValue());
	}
	
	
	public void setGdlStyle(Widget widget) throws InvalidGdlSchemaException, ExecutionException {
		super.setGdlStyle(widget);
		String[] styleClasses = new String[]{null, PSIs.GDL.Scope.gdlActive, PSIs.GDL.Scope.gdlFocus, PSIs.GDL.Scope.gdlHover};
		for (String styleClass : styleClasses) {
			this.setColor(widget, this.getColor(styleClass), styleClass);
			
			this.setDirection(widget, this.getDirection(styleClass), styleClass);
			this.setTextAlign(widget, this.getTextAlign(styleClass), styleClass);
			this.setLineHeight(widget, this.getLineHeight(styleClass), styleClass);
			this.setTextDecoration(widget, this.getTextDecoration(styleClass), styleClass);
			this.setFontFamily(widget, this.getFontFamily(styleClass), styleClass);
			this.setFontStyle(widget, this.getFontStyle(styleClass), styleClass);
			this.setFontSize(widget, this.getFontSize(styleClass), styleClass);
			this.setFontWeight(widget, this.getFontWeight(styleClass), styleClass);
			this.setLetterSpacing(widget, this.getLetterSpacing(styleClass), styleClass);
			this.setWordSpacing(widget, this.getWordSpacing(styleClass), styleClass);
		}
	}
}
