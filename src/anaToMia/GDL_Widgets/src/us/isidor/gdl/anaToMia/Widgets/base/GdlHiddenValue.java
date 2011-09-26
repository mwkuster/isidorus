package us.isidor.gdl.anaToMia.Widgets.base;


import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;


public class GdlHiddenValue {
	protected Topic constraintTopic = null;
	protected boolean constraintTopicSet = false;
	protected Topic rootConstraintTopic = null;
	protected boolean rootConstraintTopicSet = false;
	protected Topic defaultTmValueTopic = null;
	protected boolean defaultTmValueTopicSet = false;
	protected Topic defaultLiteralValueTopic = null;
	protected boolean defaultLiteralValueTopicSet = false;
	protected Topic tmRepresentative = null;
	protected boolean rawTmValuesSet = false;
	protected Topic rawTmValue = null;
	protected GdlVisibleObject owner = null;
	
	
	@SuppressWarnings("unused")
	private GdlHiddenValue(){}
	
	
	public GdlHiddenValue(Topic tmRepresentative, GdlVisibleObject owner) throws ExecutionException{
		if(tmRepresentative == null || owner == null) throw new ExecutionException("tmRepresentative and owner must not be null");
		this.tmRepresentative = tmRepresentative;
		this.owner = owner;
	}
	
	
	public Topic getTmRepresentative(){
		return this.tmRepresentative;
	}
	
	
	// returns the direct (first) constraint that is bound to the value-group
	// of this element - or null if it is unbound
	public Topic getConstraint() throws InvalidGdlSchemaException {
		if(this.constraintTopicSet){
			return this.constraintTopic;
		} else {
			this.constraintTopic = TmHelper.getConstraintOfHiddenValue(this.getTmRepresentative());
			this.constraintTopicSet = true;
			return this.constraintTopic;
		}
	}
	
	
	// returns the root (last) constraint that is bound to the value-group
	// of this element - or null if it is unbound
	public Topic getRootConstraint() throws InvalidGdlSchemaException {
		if(this.rootConstraintTopicSet){
			return this.rootConstraintTopic;
		} else {
			this.rootConstraintTopic = TmHelper.getRootConstraintOfHiddenValue(this.getTmRepresentative(), this.getConstraint());
			this.rootConstraintTopicSet = true;
			return this.rootConstraintTopic;
		}
	}
	
	
	// returns the topic that represents the default topic maps value of
	// the value-group that is bound to this element - null if it is unbound
	public Topic getDefaultTmValue() throws InvalidGdlSchemaException {
		if(this.defaultTmValueTopicSet){
			return this.defaultTmValueTopic;
		} else {
			this.defaultTmValueTopic = TmHelper.getDefaultTmValueOfHiddenValue(this.getTmRepresentative());
			this.defaultTmValueTopicSet = true;
			return this.defaultTmValueTopic;
		}
	}


	// returns the topic that represents the default literal value of the
	// value-group that is bound to this element - or null if it is unbound
	public Topic getDefaultLiteralValue() throws InvalidGdlSchemaException {
		if(this.defaultLiteralValueTopicSet){
			return this.defaultLiteralValueTopic;
		} else {
			this.defaultLiteralValueTopic = TmHelper.getDefaultLiteralValueOfHiddenValue(this.getTmRepresentative());
			this.defaultLiteralValueTopicSet = true;
			return this.defaultLiteralValueTopic;
		}
	}
	
	
	// returns the actual literal value that is set for this hidden value
	// to be used
	public String getRawDefaultLiteralValue() throws InvalidGdlSchemaException {
		if(this.getDefaultLiteralValue() == null) return null;
		
		Occurrence occurrence = TmHelper.getSingleOccurrence(this.getDefaultLiteralValue(), TmHelper.getTopicByPsi(PSIs.GDL.OccurrenceType.gdlLiteralValue, this.tmRepresentative.getTopicMap()));
		if(occurrence == null) throw new InvalidGdlSchemaException("the topic " + TmHelper.getAnyIdOfTopic(this.getDefaultLiteralValue()) + " must be bound to exactly one occurrence of the type: " + PSIs.GDL.OccurrenceType.gdlLiteralValue + ", but is: unbound");
		else return occurrence.getValue();
	}
	
	
	// returns the topic that represents the default value of
	// the value-group that is bound to this element - null if it is unbound
	public Topic getDefaultValue() throws InvalidGdlSchemaException {
		if(this.getDefaultLiteralValue() != null && this.getDefaultTmValue() != null) throw new InvalidGdlSchemaException("the topic " + TmHelper.getAnyIdOfTopic(this.getTmRepresentative()) + " must be bound to maximal one " + PSIs.GDL.TopicType.gdlDefaultValue + ", but is: 2");
		else if(this.getDefaultLiteralValue() != null) return this.getDefaultLiteralValue();
		else return this.getDefaultTmValue();
	}


	//returns the actual values represented by the tmValues 
	public Topic getRawTmValue() throws InvalidGdlSchemaException{
		if(this.rawTmValuesSet){
			return this.rawTmValue;
		} else {
			this.rawTmValuesSet = true;
			this.rawTmValue = TmHelper.getRawTmValueForHiddenValue(this.tmRepresentative, this.owner);
			return this.rawTmValue;
		}
	}
}
