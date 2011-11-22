package us.isidor.gdl.anaToMia.Widgets.base;

import com.google.gwt.dom.client.Style.Display;
import com.google.gwt.dom.client.Style.Float;
import com.google.gwt.dom.client.Style.VerticalAlign;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.value.AbsoluteNumValue;
import us.isidor.gdl.anaToMia.Widgets.value.AutoNumUnitValue;
import us.isidor.gdl.anaToMia.Widgets.value.AutoNumValue;
import us.isidor.gdl.anaToMia.Widgets.value.BorderStyleValue;
import us.isidor.gdl.anaToMia.Widgets.value.ClearValue;
import us.isidor.gdl.anaToMia.Widgets.value.ColorValue;
import us.isidor.gdl.anaToMia.Widgets.value.ContentOrientationValue;
import us.isidor.gdl.anaToMia.Widgets.value.CursorValue;
import us.isidor.gdl.anaToMia.Widgets.value.NumUnitValue;

public class GdlVisibleObjectCssService {
	private GdlVisibleObject owner = null;
	
	@SuppressWarnings("unused")
	private GdlVisibleObjectCssService() {}
	
	
	public GdlVisibleObjectCssService(GdlVisibleObject owner) throws ExecutionException{
		if(owner == null) throw new ExecutionException("owner must not be null");
		this.owner = owner;
	}
	
	
	// returns a Display instance of a gdl:display occurrence.
	// If no gdl:display occurrence is set, the default value is returned
	public Display getDisplay() throws InvalidGdlSchemaException {
		Occurrence displayOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlDisplay);

		if(displayOcc != null){
			String value = displayOcc.getValue().toLowerCase();
			if(value.equals("none")){
				return Display.NONE;
			} else if (value.equals("inline")){
				return Display.INLINE;
			} else if (value.equals("inline-block")){
				return Display.INLINE_BLOCK;
			} else if(value.equals("block")){
				return Display.BLOCK;
			} else {
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlDisplay + " must be set to one of \"none\", \"inline\", \"inline-block\" or \"block\", but is \"" + displayOcc.getValue() + "\"");
			}
		} else {
			return Display.INLINE_BLOCK;
		}
	}


	// returns an AutoNumValue instance of a gdl:z-index occurrence.
	// If no gdl:z-index occurrence is set, the default value is returned
	public AutoNumValue getZindex() throws InvalidGdlSchemaException {
		Occurrence zOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlZindex);
		if(zOcc != null){
			return new AutoNumValue(zOcc.getValue());
		} else {
			return new AutoNumValue();
		}
	}
	
	
	// returns a Float instance of a gdl:float occurrence or the default value for
	// this property if no gdl:float occurrence is available
	public Float getFloat() throws InvalidGdlSchemaException {
		Occurrence floatOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlFloat);

		if(floatOcc != null){
			String value = floatOcc.getValue().toLowerCase();
			if(value.equals("none")){
				return Float.NONE;
			} else if (value.equals("left")){
				return Float.LEFT;
			} else if (value.equals("right")){
				return Float.RIGHT;
			} else {
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlFloat + " must be set to one of \"none\", \"left\" or \"right\", but is \"" + floatOcc.getValue() + "\"");
			}
		} else {
			return Float.NONE;
		}
	}


	// returns a ClearValue instance of a gdl:clear occurrence or the default value for
	// this property if no gdl:clear occurrence is available
	public ClearValue getClear() throws InvalidGdlSchemaException {
		Occurrence clearOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlClear);

		if(clearOcc != null){
			try{
				return ClearValue.valueOf(clearOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlClear + " must be set to one of \"none\", \"left\", \"right\" or \"both\", but is \"" + clearOcc.getValue() + "\"");
			}
		} else {
			return ClearValue.NONE;
		}
	}
	
	
	// returns a ContentOrientationValue instance of a gdl:content-orientation occurrence or the default value for
	// this property if no gdl:content-orientation occurrence is available
	public ContentOrientationValue getContentOrientation() throws InvalidGdlSchemaException {
		Occurrence orientationOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlContentOrientation);

		if(orientationOcc != null){
			try{
				return ContentOrientationValue.valueOf(orientationOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlContentOrientation + " must be set to one of \"horizontal\" or \"vertical\", but is \"" + orientationOcc.getValue() + "\"");
			}
		} else {
			return ContentOrientationValue.VERTICAL;
		}
	}
	
	
	// returns a VerticalAlign instance of a gdl:vertical-align occurrence
	// or the default value for this property if no gdl:vertical-align occurrence
	// is available. The styleClass attribute is used as scope for expressing
	// a css pseudo-class, if styleClass is null the occurrence must be unscoped
	public VerticalAlign getVerticalAlign(String styleClass) throws InvalidGdlSchemaException {
		Occurrence vaOcc = null;
		if(styleClass != null){
			vaOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlVerticalAlign, styleClass);
		} else {
			vaOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlVerticalAlign);
		}

		if(vaOcc == null && styleClass != null){
			return null;
		} else if(vaOcc == null) {
			return VerticalAlign.BASELINE;
		}else {
			String value = vaOcc.getValue().toLowerCase();
			if(value.equals("baseline")){
				return VerticalAlign.BASELINE;
			} else if(value.equals("sub")){
				return VerticalAlign.SUB;
			} else if(value.equals("super")) {
				return VerticalAlign.SUPER;
			} else if(value.equals("top")) {
				return VerticalAlign.TOP;
			}else if(value.equals("text-top")) {
				return VerticalAlign.TEXT_TOP;
			}else if(value.equals("middle")) {
				return VerticalAlign.MIDDLE;
			}else if(value.equals("bottom")) {
				return VerticalAlign.BOTTOM;
			}else if(value.equals("text-bottom")) {
				return VerticalAlign.TEXT_BOTTOM;
			} else {
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlVerticalAlign + " must be set to one of \"baseline\", \"sub\", \"super\", \"top\", \"text-top\", \"middle\", \"bottom\" or \"text-bottom\", but is \"" + vaOcc.getValue() + "\"");
			}
		}
	}
	
	
	// returns a NumUnitValue instance that represents the margin of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public NumUnitValue getMargin(String styleClass) throws InvalidGdlSchemaException {
		Occurrence marginOcc = null;
		if(styleClass != null){
			marginOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMargin, styleClass);
		} else {
			marginOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMargin);
		}

		if(marginOcc == null && styleClass != null){
			return null;
		} else if(marginOcc == null) {
			return new NumUnitValue();
		} else {
			return new NumUnitValue(marginOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the margin-top of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public NumUnitValue getMarginTop(String styleClass) throws InvalidGdlSchemaException {
		Occurrence marginOcc = null;
		if(styleClass != null){
			marginOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMarginTop, styleClass);
		} else {
			marginOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMarginTop);
		}

		if(marginOcc == null){
			return null;
		} else {
			return new NumUnitValue(marginOcc.getValue());
		}
	}

	
	// returns a NumUnitValue instance that represents the margin-right of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public NumUnitValue getMarginRight(String styleClass) throws InvalidGdlSchemaException {
		Occurrence marginOcc = null;
		if(styleClass != null){
			marginOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMarginRight, styleClass);
		} else {
			marginOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMarginRight);
		}

		if(marginOcc == null){
			return null;
		} else {
			return new NumUnitValue(marginOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the margin-bottom of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public NumUnitValue getMarginBottom(String styleClass) throws InvalidGdlSchemaException {
		Occurrence marginOcc = null;
		if(styleClass != null){
			marginOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMarginBottom, styleClass);
		} else {
			marginOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMarginBottom);
		}

		if(marginOcc == null){
			return null;
		} else {
			return new NumUnitValue(marginOcc.getValue());
		}
	}

	
	// returns a NumUnitValue instance that represents the margin-left of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public NumUnitValue getMarginLeft(String styleClass) throws InvalidGdlSchemaException {
		Occurrence marginOcc = null;
		if(styleClass != null){
			marginOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMarginLeft, styleClass);
		} else {
			marginOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMarginLeft);
		}

		if(marginOcc == null){
			return null;
		} else {
			return new NumUnitValue(marginOcc.getValue());
		}
	}


	// returns a ColorValue instance that represents the color of this element's border.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public ColorValue getBorderColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderColor, styleClass);
		} else {
			colorOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderColor);
		}

		if(colorOcc == null && styleClass != null){
			return null;
		} else if(colorOcc == null) {
			return new ColorValue();
		} else {
			return new ColorValue(colorOcc.getValue());
		}
	}

	
	// returns a ColorValue instance that represents the color of this element's border-top.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public ColorValue getBorderTopColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderTopColor, styleClass);
		} else {
			colorOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderTopColor);
		}

		if(colorOcc == null ){
			return null;
		} else {
			return new ColorValue(colorOcc.getValue());
		}
	}


	// returns a ColorValue instance that represents the color of this element's border-right.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public ColorValue getBorderRightColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderRightColor, styleClass);
		} else {
			colorOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderRightColor);
		}

		if(colorOcc == null ){
			return null;
		} else {
			return new ColorValue(colorOcc.getValue());
		}
	}

	
	// returns a ColorValue instance that represents the color of this element's border-bottom.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public ColorValue getBorderBottomColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderBottomColor, styleClass);
		} else {
			colorOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderBottomColor);
		}

		if(colorOcc == null ){
			return null;
		} else {
			return new ColorValue(colorOcc.getValue());
		}
	}


	// returns a ColorValue instance that represents the color of this element's border-left.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public ColorValue getBorderLeftColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderLeftColor, styleClass);
		} else {
			colorOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderLeftColor);
		}

		if(colorOcc == null ){
			return null;
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
			styleOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderStyle, styleClass);
		} else {
			styleOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderStyle);
		}

		if(styleOcc == null && styleClass != null){
			return null;
		} else if(styleOcc == null) {
			return BorderStyleValue.NONE;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}


	// returns a ColorValue instance that represents the style of this element's border-top.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public BorderStyleValue getBorderTopStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderTopStyle, styleClass);
		} else {
			styleOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderTopStyle);
		}

		if(styleOcc == null){
			return null;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-top-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}


	// returns a ColorValue instance that represents the style of this element's border-right.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public BorderStyleValue getBorderRightStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderRightStyle, styleClass);
		} else {
			styleOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderRightStyle);
		}

		if(styleOcc == null){
			return null;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-right-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}


	// returns a ColorValue instance that represents the style of this element's border-bottom.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public BorderStyleValue getBorderBottomStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderBottomStyle, styleClass);
		} else {
			styleOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderBottomStyle);
		}

		if(styleOcc == null){
			return null;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-bottom-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}


	// returns a ColorValue instance that represents the style of this element's border-left.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public BorderStyleValue getBorderLeftStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderLeftStyle, styleClass);
		} else {
			styleOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderLeftStyle);
		}

		if(styleOcc == null){
			return null;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-left-style must be set to one of " + values + ", but is " + styleOcc.getValue());
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
			widthOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderWidth, styleClass);
		} else {
			widthOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderWidth);
		}

		if(widthOcc == null && styleClass != null){
			return null;
		} else if(widthOcc == null) {
			return new AbsoluteNumValue();
		} else {
			return new AbsoluteNumValue(widthOcc.getValue());
		}
	}


	// returns a AbsoluteNumValue instance that represents the width of this element's border-top.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public AbsoluteNumValue getBorderTopWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderTopWidth, styleClass);
		} else {
			widthOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderTopWidth);
		}

		if(widthOcc == null){
			return null;
		} else {
			return new AbsoluteNumValue(widthOcc.getValue());
		}
	}


	// returns a AbsoluteNumValue instance that represents the width of this element's border-right.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public AbsoluteNumValue getBorderRightWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderRightWidth, styleClass);
		} else {
			widthOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderRightWidth);
		}

		if(widthOcc == null){
			return null;
		} else {
			return new AbsoluteNumValue(widthOcc.getValue());
		}
	}


	// returns a AbsoluteNumValue instance that represents the width of this element's border-bottom.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public AbsoluteNumValue getBorderBottomWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderBottomWidth, styleClass);
		} else {
			widthOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderBottomWidth);
		}

		if(widthOcc == null){
			return null;
		} else {
			return new AbsoluteNumValue(widthOcc.getValue());
		}
	}


	// returns a AbsoluteNumValue instance that represents the width of this element's border-left.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public AbsoluteNumValue getBorderLeftWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderLeftWidth, styleClass);
		} else {
			widthOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderLeftWidth);
		}

		if(widthOcc == null){
			return null;
		} else {
			return new AbsoluteNumValue(widthOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the radius of this element's border.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public NumUnitValue getBorderRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderRadius, styleClass);
		} else {
			radiusOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderRadius);
		}

		if(radiusOcc == null && styleClass != null){
			return null;
		} else if(radiusOcc == null) {
			return new NumUnitValue();
		} else {
			return new NumUnitValue(radiusOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the radius of this element's border-top-left.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getBorderTopLeftRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderTopLeftRadius, styleClass);
		} else {
			radiusOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderTopLeftRadius);
		}

		if(radiusOcc == null && styleClass != null){
			return null;
		} else if(radiusOcc == null){
			return null;
		} else {
			return new NumUnitValue(radiusOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the radius of this element's border-top-right.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getBorderTopRightRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderTopRightRadius, styleClass);
		} else {
			radiusOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderTopRightRadius);
		}

		if(radiusOcc == null && styleClass != null){
			return null;
		} else if(radiusOcc == null){
			return null;
		} else {
			return new NumUnitValue(radiusOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the radius of this element's border-bottom-left.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getBorderBottomLeftRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderBottomLeftRadius, styleClass);
		} else {
			radiusOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderBottomLeftRadius);
		}

		if(radiusOcc == null && styleClass != null){
			return null;
		} else if(radiusOcc == null){
			return null;
		} else {
			return new NumUnitValue(radiusOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the radius of this element's border-bottom-right.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getBorderBottomRightRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderBottomRightRadius, styleClass);
		} else {
			radiusOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBorderBottomRightRadius);
		}

		if(radiusOcc == null && styleClass != null){
			return null;
		} else if(radiusOcc == null){
			return null;
		} else {
			return new NumUnitValue(radiusOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the padding of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public NumUnitValue getPadding(String styleClass) throws InvalidGdlSchemaException {
		Occurrence paddingOcc = null;
		if(styleClass != null){
			paddingOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlPadding, styleClass);
		} else {
			paddingOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlPadding);
		}

		if(paddingOcc == null && styleClass != null){
			return null;
		} else if(paddingOcc == null) {
			return new NumUnitValue();
		} else {
			return new NumUnitValue(paddingOcc.getValue());
		}		
	}


	// returns a NumUnitValue instance that represents the padding of this element's top.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getPaddingTop(String styleClass) throws InvalidGdlSchemaException {
		Occurrence paddingOcc = null;
		if(styleClass != null){
			paddingOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlPaddingTop, styleClass);
		} else {
			paddingOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlPaddingTop);
		}

		if(paddingOcc == null){
			return null;
		} else {
			return new NumUnitValue(paddingOcc.getValue());
		}		
	}


	// returns a NumUnitValue instance that represents the padding of this element's right.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getPaddingRight(String styleClass) throws InvalidGdlSchemaException {
		Occurrence paddingOcc = null;
		if(styleClass != null){
			paddingOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlPaddingRight, styleClass);
		} else {
			paddingOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlPaddingRight);
		}

		if(paddingOcc == null){
			return null;
		} else {
			return new NumUnitValue(paddingOcc.getValue());
		}		
	}


	// returns a NumUnitValue instance that represents the padding of this element's bottom.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getPaddingBottom(String styleClass) throws InvalidGdlSchemaException {
		Occurrence paddingOcc = null;
		if(styleClass != null){
			paddingOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlPaddingBottom, styleClass);
		} else {
			paddingOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlPaddingBottom);
		}

		if(paddingOcc == null){
			return null;
		} else {
			return new NumUnitValue(paddingOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the padding of this element's left.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getPaddingLeft(String styleClass) throws InvalidGdlSchemaException {
		Occurrence paddingOcc = null;
		if(styleClass != null){
			paddingOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlPaddingLeft, styleClass);
		} else {
			paddingOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlPaddingLeft);
		}

		if(paddingOcc == null){
			return null;
		} else {
			return new NumUnitValue(paddingOcc.getValue());
		}		
	}


	// returns an AutoNumUnitValue instance that represents the width of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AutoNumUnitValue getWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlWidth, styleClass);
		} else {
			widthOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlWidth);
		}

		if(widthOcc == null && styleClass != null){
			return null;
		} else if(widthOcc == null) {
			return new AutoNumUnitValue();
		} else {
			return new AutoNumUnitValue(widthOcc.getValue());
		}		
	}


	// returns an AutoNumUnitValue instance that represents the min-width of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AutoNumUnitValue getMinWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMinWidth, styleClass);
		} else {
			widthOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMinWidth);
		}

		if(widthOcc == null && styleClass != null){
			return null;
		} else if(widthOcc == null) {
			return new AutoNumUnitValue();
		} else {
			return new AutoNumUnitValue(widthOcc.getValue());
		}		
	}


	// returns an AutoNumUnitValue instance that represents the max-width of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AutoNumUnitValue getMaxWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMaxWidth, styleClass);
		} else {
			widthOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMaxWidth);
		}

		if(widthOcc == null && styleClass != null){
			return null;
		} else if(widthOcc == null) {
			return new AutoNumUnitValue();
		} else {
			return new AutoNumUnitValue(widthOcc.getValue());
		}		
	}


	// returns an AutoNumUnitValue instance that represents the height of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AutoNumUnitValue getHeight(String styleClass) throws InvalidGdlSchemaException {
		Occurrence heightOcc = null;
		if(styleClass != null){
			heightOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlHeight, styleClass);
		} else {
			heightOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlHeight);
		}

		if(heightOcc == null && styleClass != null){
			return null;
		} else if(heightOcc == null) {
			return new AutoNumUnitValue();
		} else {
			return new AutoNumUnitValue(heightOcc.getValue());
		}		
	}


	// returns an AutoNumUnitValue instance that represents the min-height of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AutoNumUnitValue getMinHeight(String styleClass) throws InvalidGdlSchemaException {
		Occurrence heightOcc = null;
		if(styleClass != null){
			heightOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMinHeight, styleClass);
		} else {
			heightOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMinHeight);
		}

		if(heightOcc == null && styleClass != null){
			return null;
		} else if(heightOcc == null) {
			return new AutoNumUnitValue();
		} else {
			return new AutoNumUnitValue(heightOcc.getValue());
		}		
	}


	// returns an AutoNumUnitValue instance that represents the max-height of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AutoNumUnitValue getMaxHeight(String styleClass) throws InvalidGdlSchemaException {
		Occurrence heightOcc = null;
		if(styleClass != null){
			heightOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMaxHeight, styleClass);
		} else {
			heightOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlMaxHeight);
		}

		if(heightOcc == null && styleClass != null){
			return null;
		} else if(heightOcc == null) {
			return new AutoNumUnitValue();
		} else {
			return new AutoNumUnitValue(heightOcc.getValue());
		}		
	}


	// returns a CursorValue instance that represents the cursor of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public CursorValue getCursor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence cursorOcc = null;
		if(styleClass != null){
			cursorOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlCursor, styleClass);
		} else {
			cursorOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlCursor);
		}

		if(cursorOcc == null && styleClass != null){
			return null;
		} else if(cursorOcc == null) {
			return CursorValue.AUTO;
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


	// returns a ColorValue instance that represents the background-color of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public ColorValue getBackgroundColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = TmHelper.getNoneOrOneScopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBackgroundColor, styleClass);
		} else {
			colorOcc = TmHelper.getNoneOrOneUnscopedOccurrence(this.owner.getTmRepresentative(), PSIs.GDL.OccurrenceType.gdlBackgroundColor);
		}

		if(colorOcc == null && styleClass != null){
			return null;
		} else if(colorOcc == null) {
			return new ColorValue("#ffffff");
		} else {
			return new ColorValue(colorOcc.getValue());
		}	
	}
}
