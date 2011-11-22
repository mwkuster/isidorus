package us.isidor.gdl.anaToMia.Widgets.view;

import java.util.ArrayList;
import com.google.gwt.core.client.JsArray;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Association;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Role;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMapsTypes;
import us.isidor.gdl.anaToMia.Widgets.base.ButtonableObject;
import us.isidor.gdl.anaToMia.Widgets.base.GdlHiddenValue;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.base.TmHelper;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidContentException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;
import us.isidor.gdl.anaToMia.Widgets.value.ContentOrientationValue;


public abstract class GdlAssociationView extends GdlView {
	private ArrayList<Topic> associationRoleConstraints = new ArrayList<Topic>();
	private boolean associationRoleConstraintsSet = false;
	private ArrayList<Topic> topicRoleConstraints = new ArrayList<Topic>();
	private boolean topicRoleConstraintsSet = false;
	private Topic associationType = null;
	private boolean associationTypeSet = false;
	private ArrayList<Pair<Topic, Topic>> roleAndPlayerTypes = new ArrayList<Pair<Topic, Topic>>();
	private boolean roleAndPlayerTypesSet = false;
	private ArrayList<Topic> roleCombinationConstraints = new ArrayList<Topic>();
	private boolean roleCombinationConstraintsSet = false;
	private ArrayList<Pair<Pair<Topic, Topic>, Pair<Topic, Topic>>> validRoleCombinations = new ArrayList<Pair<Pair<Topic,Topic>,Pair<Topic,Topic>>>();
	private boolean validRoleCombinationsSet = false;
	
	
	protected GdlAssociationView(){
		super();
	}
	
	
	public GdlAssociationView(Topic tmRepresentative, Topic receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException {
		super(tmRepresentative, receivedData, gdlParent);
	}
	
	// content orientation has no effect on a view, so here must the original method
	// defined in GdlVisibleObject implemented
	@Override
	public void setContentOrientation(ContentOrientationValue value) throws InvalidGdlSchemaException, ExecutionException {
		if(value == ContentOrientationValue.HORIZONTAL && this.containerPanel == null) this.containerPanel = new HorizontalPanel();
		else if(this.containerPanel == null) this.containerPanel = new VerticalPanel();
		
		this.mainPanel.add(this.containerPanel);
	}
	
	
	// Searches the topic that represents the first item that is placed within this view instance
	// i.e. such an item must not have an association that is bound to it via a role of the type
	// gdl:ancestor.
	protected Topic getStartElement(ArrayList<Topic> containees) throws InvalidGdlSchemaException {
		return TmHelper.getFirstContainee(super.getTmRepresentative(), containees);
	}
	
	
	// Return the association-role-constraints this view is bound to
	public ArrayList<Topic> getAssociationRoleConstraints() throws InvalidGdlSchemaException {
		if(this.associationRoleConstraintsSet){
			return this.associationRoleConstraints;
		} else {
			this.associationRoleConstraintsSet = true;
			this.associationRoleConstraints = TmHelper.getAssociationRoleConstraintsForView(this.tmRepresentative);
			return this.associationRoleConstraints;
		}
	}
	
	
	// Return the topic-role-constraints this view is bound to
	public ArrayList<Topic> getTopicRoleConstraints() throws InvalidGdlSchemaException {
		if(this.topicRoleConstraintsSet){
			return this.topicRoleConstraints;
		} else {
			this.topicRoleConstraintsSet = true;
			this.topicRoleConstraints = TmHelper.getTopicRoleConstraintsForView(this.tmRepresentative);
			return this.topicRoleConstraints;
		}
	}
	
	
	// Returns the role-combination-constraints of the association-
	// type that is bound to this association-view via the bound
	// constraints
	public ArrayList<Topic> getRoleCombinationConstraints() throws InvalidGdlSchemaException {
		if(this.roleCombinationConstraintsSet){
			return this.roleCombinationConstraints;
		} else {
			this.roleCombinationConstraintsSet = true;
			this.roleCombinationConstraints = TmHelper.getRoleCombinationConstraints(this.getAssociationType());
			return this.roleCombinationConstraints;
		}
	}
	
	
	// returns the role-types that are valid to be used
	// in associations of the type that is bound to this view
	public ArrayList<Pair<Topic, Topic>> getRoleAndPlayerTypes() throws InvalidGdlSchemaException {
		if(this.roleAndPlayerTypesSet){
			return this.roleAndPlayerTypes;
		} else {
			this.roleAndPlayerTypesSet = true;
			ArrayList<Topic> roleTypesOfAssociationRoleConstraints = new ArrayList<Topic>();
			for (Topic constraint : this.getAssociationRoleConstraints()){
				Topic top = TmHelper.getConstraintRoleOfConstraint(constraint);
				if(!roleTypesOfAssociationRoleConstraints.contains(top)) roleTypesOfAssociationRoleConstraints.add(top);
			}
			
			ArrayList<Pair<Topic, Topic>> roleTypesOfTopicRoleConstraints = new ArrayList<Pair<Topic, Topic>>();
			for (Topic constraint : this.getTopicRoleConstraints()){
				Pair<Topic, Topic> roleAndPlayerType = TmHelper.getConstrainedRoleAndPlayerTypeOfConstraint(constraint);
				if(!roleTypesOfTopicRoleConstraints.contains(roleAndPlayerType)) roleTypesOfTopicRoleConstraints.add(roleAndPlayerType);
			}
			
			ArrayList<Pair<Topic, Topic>> result = new ArrayList<Pair<Topic,Topic>>();
			for (Pair<Topic, Topic> roleAndPlayerType : roleTypesOfTopicRoleConstraints) {
				if(roleTypesOfAssociationRoleConstraints.contains(roleAndPlayerType.getFirst())) result.add(roleAndPlayerType);
			}
			
			this.roleAndPlayerTypes = result;
			return this.roleAndPlayerTypes;
		}
	}
	
	
	// returns the association type that is valid with respect to the
	// topic- and the association-role-constraints that are bound to this view
	public Topic getAssociationType() throws InvalidGdlSchemaException {
		if(this.associationTypeSet){
			return this.associationType;
		} else {
			this.associationTypeSet = true;
			ArrayList<Topic> assocTypes = new ArrayList<Topic>();
			for (Topic constraint : this.getAssociationRoleConstraints()){
				Topic top = TmHelper.getConstrainedStatement(constraint);
				if(!assocTypes.contains(top)) assocTypes.add(top);
			}
			
			for (Topic constraint : this.getTopicRoleConstraints()){
				Topic top = TmHelper.getConstrainedStatement(constraint);
				if(!assocTypes.contains(top)) assocTypes.add(top);
			}
			
			if(assocTypes.size() != 1){
				String bindings = "[";
				for (Topic topic : assocTypes)
					bindings += TmHelper.getAnyIdOfTopic(topic) + ", ";
				if(bindings.length() == 1) bindings = "[ ]";
				else bindings = bindings.substring(0, bindings.length() - 2) + "]";
				throw new InvalidGdlSchemaException("the association-view " + TmHelper.getAnyIdOfTopic(this.tmRepresentative) + " must be bound to topic-role-constraints and association-rle-constraints that own the same constrained-statement, but found the constrained-statements: " + bindings);
			}
			
			this.associationType = assocTypes.get(0);
			return this.associationType;
		}
	}
	
	
	// Returns all valid role-player combinations for this association-view
	public ArrayList<Pair<Pair<Topic, Topic>, Pair<Topic, Topic>>> getValidRoleCombinations() throws InvalidGdlSchemaException {
		if(this.validRoleCombinationsSet){
			return this.validRoleCombinations;
		} else {
			this.validRoleCombinationsSet = true;
			ArrayList<Pair<Pair<Topic, Topic>, Pair<Topic, Topic>>> roleCombinations = new ArrayList<Pair<Pair<Topic,Topic>,Pair<Topic,Topic>>>();
			for (Topic constraint : this.getRoleCombinationConstraints()) {
				Pair<Pair<Topic, Topic>, Pair<Topic, Topic>> combination = TmHelper.getRoleCombinationsOfConstraint(constraint);
				if(!roleCombinations.contains(combination)) roleCombinations.add(combination);
			}
			this.validRoleCombinations = roleCombinations;			
			return this.validRoleCombinations;
		}
	}

	
	@Override
	public ArrayList<Pair<Object, TopicMapsTypes>> getContent(Construct carrier, boolean validate) throws InvalidGdlSchemaException, ExecutionException, InvalidContentException{
		ArrayList<Pair<Object, TopicMapsTypes>> result = new ArrayList<Pair<Object,TopicMapsTypes>>();
		
		for (Widget ctrl : this.subElements) {
			if(ctrl instanceof ButtonableObject){
				if(((ButtonableObject)ctrl).getMainObject() instanceof AssociationItem){
					for (Pair<Object, TopicMapsTypes> pair : ((AssociationItem)((ButtonableObject)ctrl).getMainObject()).getContent(null, validate)) {
						result.add(pair);
					}
				}
			}
		}
		
		// process hidden values
		for (Topic hd : this.getHiddenValues()) {
			GdlHiddenValue hdv = new GdlHiddenValue(hd, this);
			Topic player = hdv.getRawTmValue();
			if(player == null) throw new InvalidGdlSchemaException("the hidden value " + TmHelper.getAnyIdOfTopic(hd) + " must be bound to exactly one topic, but is unbound");
			Topic roleType = TmHelper.getConstrainedRoleAndPlayerTypeOfConstraint(hdv.getRootConstraint()).getFirst();
			
			if(TmHelper.isInstanceOf(hdv.getConstraint(), PSIs.GDL.TopicType.gdlRolePlayer)){
				if(!TmHelper.isInstanceOf(hdv.getRootConstraint(), PSIs.TMCL.tmclTopicRoleConstraint)) throw new InvalidGdlSchemaException("the constraint " + TmHelper.getAnyIdOfTopic(hdv.getConstraint()) + " must be bound to an instance of " + PSIs.TMCL.tmclTopicRoleConstraint + ", but is: " + TmHelper.getAnyIdOfTopic(hdv.getRootConstraint()));
				for (Pair<Object, TopicMapsTypes> pair : result) {
					if(pair.getSecond().equals(TopicMapsTypes.Association)){
						Association assoc = (Association)pair.getFirst();
						JsArray<Role> roles = assoc.getRoles(roleType);
						int i = 0;
						for( ; i != roles.length(); ++i) if(roles.get(i).getPlayer().equals(player)) break;
						if(i == roles.length()) assoc.createRole(roleType, player);
					}
				}
			}
			
			// TODO: implement
			// reifier => topic-role
			// item-identifier => topic-role
			// type => association-role
			// reifier => association-role
			// item-identifier => association-role
			// scope => association-role
		}
		
		return result;
	}
	

	// This class represents a single association that is displayed.
	// Further elements are created by caling the GdlInstantiator.instantiate
	// method, the passed received argument is set ot the association that is
	// represented by this class
	public class AssociationItem extends GdlView {
		private int indexInParent = -1;
		private Association representedAssociation = null;
		
		
		@SuppressWarnings("unchecked")
		public Association getRepresentedAssociation() throws InvalidGdlSchemaException{
			if(this.receivedData != null){
				return (Association)this.receivedData;
			} else if (this.representedAssociation != null){
				return this.representedAssociation;
			} else{
				this.representedAssociation = this.tmRepresentative.getTopicMap().createAssociation(((GdlAssociationView)this.getGdlParent()).getAssociationType(), (JsArray<Topic>)JsArray.createArray());
				return this.representedAssociation;
			}
		}
		
		
		@SuppressWarnings("unused")
		private AssociationItem(){
			super();
		}
		
		
		public AssociationItem(Topic tmRepresentative, Association receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
			super(tmRepresentative, receivedData, gdlParent);
			this.indexInParent = this.getGdlParent().getSubElementsCount();
			
			ArrayList<Topic> objectsContained = this.contains();
			
			Topic currentTopic = this.getStartElement(objectsContained);
			Topic lastTopic = super.getTmRepresentative();
			while(currentTopic != null){
				this.append(lastTopic, currentTopic);
				lastTopic = currentTopic;
				currentTopic = TmHelper.getNextContainee(currentTopic, objectsContained);
			}
		}
		
		
		@Override
		public ArrayList<Pair<Object, TopicMapsTypes>> getContent(Construct carrier, boolean validate) throws InvalidGdlSchemaException, ExecutionException, InvalidContentException {
			ArrayList<Pair<Object, TopicMapsTypes>> result = new ArrayList<Pair<Object,TopicMapsTypes>>();
			
			Association localCarrier = this.getRepresentedAssociation();
			
			for (Widget ctrl : this.subElements) {
				if(ctrl instanceof GdlVisibleObject){
					((GdlVisibleObject)ctrl).getContent(localCarrier, validate);
				}
			}
			result.add(new Pair<Object, TopicMapsTypes>(localCarrier, TopicMapsTypes.Association));
			return result;
		}
		
		
		// this method should be invoked if a new sub-element is added to this instance.
		// instances of GdlView does not organize their sub-elements in
		// tables, sub-elements are placed directly on the mainpanel
		@Override
		protected ButtonableObject addToContainerPanel(Widget widget){
			this.subElements.add(widget);
			this.mainPanel.add(widget);
			return null;
		}
		
		
		// returns the string value of a gdl:id occurrence
		public String getId() throws InvalidGdlSchemaException {
			return super.getId() + "__GDL_" + this.indexInParent;
		}
		
		
		// sets all GDL styles that are defined by the topic map representative
		@Override
		protected void setGdlStyle() throws InvalidGdlSchemaException, ExecutionException {
			// do nothing: all properties are set in the parent element
			// and are ignored for this element
		}
		
		
		// sets all GDL styles that are defined by the topic map representative to tha passed widget
		@Override
		public void setGdlStyle(Widget widget) throws InvalidGdlSchemaException, ExecutionException {
			String[] styleClasses = new String[]{null, PSIs.GDL.Scope.gdlActive, PSIs.GDL.Scope.gdlFocus, PSIs.GDL.Scope.gdlHover};
			for (String styleClass : styleClasses) {
				this.setVerticalAlign(widget, this.getVerticalAlign(styleClass), styleClass);
				
				this.setBorderColor(widget, this.getBorderColor(styleClass), styleClass);
				this.setBorderTopColor(widget, this.getBorderTopColor(styleClass), styleClass);
				this.setBorderRightColor(widget, this.getBorderRightColor(styleClass), styleClass);
				this.setBorderBottomColor(widget, this.getBorderBottomColor(styleClass), styleClass);
				this.setBorderLeftColor(widget, this.getBorderLeftColor(styleClass), styleClass);
				
				this.setBorderStyle(widget, this.getBorderStyle(styleClass), styleClass);
				this.setBorderTopStyle(widget, this.getBorderTopStyle(styleClass), styleClass);
				this.setBorderRightStyle(widget, this.getBorderRightStyle(styleClass), styleClass);
				this.setBorderBottomStyle(widget, this.getBorderBottomStyle(styleClass), styleClass);
				this.setBorderLeftStyle(widget, this.getBorderLeftStyle(styleClass), styleClass);
				
				this.setBorderWidth(widget, this.getBorderWidth(styleClass), styleClass);
				this.setBorderTopWidth(widget, this.getBorderTopWidth(styleClass), styleClass);
				this.setBorderRightWidth(widget, this.getBorderRightWidth(styleClass), styleClass);
				this.setBorderBottomWidth(widget, this.getBorderBottomWidth(styleClass), styleClass);
				this.setBorderLeftWidth(widget, this.getBorderLeftWidth(styleClass), styleClass);
				
				this.setBorderRadius(widget, this.getBorderRadius(styleClass), styleClass);
				this.setBorderTopRightRadius(widget, this.getBorderTopRightRadius(styleClass), styleClass);
				this.setBorderBottomRightRadius(widget, this.getBorderBottomRightRadius(styleClass), styleClass);
				this.setBorderBottomLeftRadius(widget, this.getBorderBottomLeftRadius(styleClass), styleClass);
				this.setBorderTopLeftRadius(widget, this.getBorderTopLeftRadius(styleClass), styleClass);
				
				this.setCursor(widget, this.getCursor(styleClass), styleClass);
				
				this.setWidth(widget, this.getWidth(styleClass), styleClass);
				this.setMaxWidth(widget, this.getMaxWidth(styleClass), styleClass);
				this.setMinWidth(widget, this.getMinWidth(styleClass), styleClass);
				
				this.setHeight(widget, this.getHeight(styleClass), styleClass);
				this.setMaxHeight(widget, this.getMaxHeight(styleClass), styleClass);
				this.setMinHeight(widget, this.getMinHeight(styleClass), styleClass);
				
				this.setBackgroundColor(widget, this.getBackgroundColor(styleClass), styleClass);
			}
		}
	}
}
