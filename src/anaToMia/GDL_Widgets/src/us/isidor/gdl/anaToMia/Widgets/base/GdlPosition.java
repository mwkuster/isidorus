package us.isidor.gdl.anaToMia.Widgets.base;

import java.util.ArrayList;

import com.google.gwt.core.client.JsArray;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.ui.Widget;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMap;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.value.AutoNumUnitValue;
import us.isidor.gdl.anaToMia.Widgets.value.PositionStyleValue;


public class GdlPosition implements GdlDescriptor {
	private Topic tmRepresentative = null;
	
	
	@SuppressWarnings("unused")
	private GdlPosition(){}
	
	
	public GdlPosition(Topic tmRepresentative){
		this.tmRepresentative = tmRepresentative;
	}
	
	
	// a helper method that returns all occurrences of the type bound to the passed PSI
	@SuppressWarnings("unchecked")
	private JsArray<Occurrence> getOccurrences(String occurrenceType){
		TopicMap tm = this.tmRepresentative.getTopicMap();
		Topic occType = tm.getTopicBySubjectIdentifier(tm.createLocator(occurrenceType));
		if(occType == null) return (JsArray<Occurrence>)JsArray.createArray();
		else return tmRepresentative.getOccurrences(occType);
	}


	// a helper method that returns one occurrence of the type bound to the passed PSI.
	// If more than one occurrence is available an InvalidGdlSchemaException is thrown.
	// If nor occurrence is available the return value is null
	private Occurrence getNoneOrOneUnscopedOccurrence(String occurrenceType) throws InvalidGdlSchemaException{
		JsArray<Occurrence> occs = getOccurrences(occurrenceType);
		ArrayList<Occurrence> unscopedOccs = new ArrayList<Occurrence>();
		for(int i = 0; i != occs.length(); ++i){
			if(occs.get(i).getScope().length() == 0) unscopedOccs.add(occs.get(i));
		}

		if(unscopedOccs.size() > 1){
			throw new InvalidGdlSchemaException("The topic " + TmHelper.getAnyIdOfTopic(this.tmRepresentative) + " must be bound to none or one unscoped occurrence of the type " + occurrenceType + ", but is bound " + unscopedOccs.size() + " times to it");
		} else if(unscopedOccs.size() == 1){
			return unscopedOccs.get(0);
		} else {
			return null;
		}
	}
	
	
	@Override
	public Topic getTmRepresentative() {
		return this.tmRepresentative;
	}

	
	public AutoNumUnitValue getTop() throws InvalidGdlSchemaException {
		Occurrence topOcc = null;
		topOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlTop);

		if(topOcc == null) return null;
		else return new AutoNumUnitValue(topOcc.getValue());
	}
	
	
	public AutoNumUnitValue getRight() throws InvalidGdlSchemaException {
		Occurrence rightOcc = null;
		rightOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlRight);

		if(rightOcc == null) return null;
		else return new AutoNumUnitValue(rightOcc.getValue());
	}
	
	
	public AutoNumUnitValue getBottom() throws InvalidGdlSchemaException {
		Occurrence bottomOcc = null;
		bottomOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBottom);

		if(bottomOcc == null) return null;
		else return new AutoNumUnitValue(bottomOcc.getValue());
	}
	
	
	public AutoNumUnitValue getLeft() throws InvalidGdlSchemaException {
		Occurrence leftOcc = null;
		leftOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlLeft);

		if(leftOcc == null) return null;
		else return new AutoNumUnitValue(leftOcc.getValue());
	}
	
	
	public PositionStyleValue getPositionStyle()throws InvalidGdlSchemaException {
		Occurrence positionStyleOcc = null;
		positionStyleOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlPositionStyle);

		if(positionStyleOcc == null) return null;
		else try{
			return PositionStyleValue.valueOf(positionStyleOcc.getValue().toUpperCase());
		}catch(IllegalArgumentException e){
			throw new InvalidGdlSchemaException("a position style value must be one of static, relative or absolute, but is " + positionStyleOcc.getValue());
		}
	}
	
	
	public void setAttributes(Widget widget) throws InvalidGdlSchemaException{
		if(widget == null) return;
		
		if(this.getPositionStyle() != null) DOM.setStyleAttribute(widget.getElement(), "position", this.getPositionStyle().getCssValue());
		else DOM.setStyleAttribute(widget.getElement(), "position", "static");
		if(this.getTop() != null) DOM.setStyleAttribute(widget.getElement(), "top", this.getTop().getCssValue());
		if(this.getRight() != null) DOM.setStyleAttribute(widget.getElement(), "right", this.getRight().getCssValue());
		if(this.getBottom() != null) DOM.setStyleAttribute(widget.getElement(), "bottom", this.getBottom().getCssValue());
		if(this.getLeft() != null) DOM.setStyleAttribute(widget.getElement(), "left", this.getLeft().getCssValue());
	}
}
