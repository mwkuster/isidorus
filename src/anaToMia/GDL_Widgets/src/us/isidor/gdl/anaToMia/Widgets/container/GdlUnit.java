package us.isidor.gdl.anaToMia.Widgets.container;

import java.util.ArrayList;
import com.google.gwt.user.client.Element;
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
import com.google.gwt.user.client.ui.AbsolutePanel;
import com.google.gwt.user.client.ui.CaptionPanel;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMap;
import us.isidor.gdl.anaToMia.Widgets.base.ButtonableObject;
import us.isidor.gdl.anaToMia.Widgets.base.GdlPosition;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.base.IGdlContainer;
import us.isidor.gdl.anaToMia.Widgets.base.TmHelper;
import us.isidor.gdl.anaToMia.Widgets.base.Utils;
import us.isidor.gdl.anaToMia.Widgets.environment.ActiveStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.FocusStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.GdlInstantiator;
import us.isidor.gdl.anaToMia.Widgets.environment.HoverStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.text.GdlTextObject;
import us.isidor.gdl.anaToMia.Widgets.value.BorderStyleValue;
import us.isidor.gdl.anaToMia.Widgets.value.TextDecorationValue;


public class GdlUnit extends GdlTextObject implements IGdlContainer {
	private Topic constraintTopic = null;
	private boolean constraintTopicSet = false;
	
	
	// some constructors
	protected GdlUnit() throws InvalidGdlSchemaException, ExecutionException {
		super();
	}
	
	
	public GdlUnit(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		// create a unit for each tm-element and set the current element, e.g. current topic, occurrence, name, ...
		this.createUnit(null);
		this.setNthButtons();
	}
	
	
	// creates a unit element => UnitWidget
	private UnitWidget createUnit(Construct currentElement) throws InvalidGdlSchemaException, ExecutionException{
		UnitWidget unit = new UnitWidget(this.getTmRepresentative(), currentElement);
		ActiveStyleHandler asHandler = new ActiveStyleHandler(this);
		FocusStyleHandler fsHandler = new FocusStyleHandler(this);
		HoverStyleHandler hsHandler = new HoverStyleHandler(this);
		int idSuffix = 0;
		if(this.subElements != null) idSuffix = this.subElements.size(); 
		unit.setId(this.getId() + "__GDL_" + idSuffix);
		unit.addMouseDownHandler(asHandler);
		unit.addMouseUpHandler(asHandler);
		unit.addMouseOverHandler(hsHandler);
		unit.addMouseOutHandler(hsHandler);
		unit.addFocusHandler(fsHandler);
		unit.addBlurHandler(fsHandler);
		super.addToContainerPanel(unit);
		this.setGdlStyle(unit);
		
		// set the actual content of a unit
		ArrayList<Topic> objectsContained = this.contains();
		Topic currentTopic = this.getStartElement(objectsContained);
		Topic lastTopic = super.getTmRepresentative();
		while(currentTopic != null){
			unit.append(lastTopic, currentTopic);
			lastTopic = currentTopic;
			currentTopic = TmHelper.getNextContainee(currentTopic, objectsContained);
		}
		
		return unit;
	}
	
	
	public ButtonableObject removeUnit(UnitWidget elem) throws InvalidGdlSchemaException, ExecutionException{
		return this.removeFromContainer(elem);
	}
	
	
	// returns a String instance that represents the literal value of this elements name.
	public String getUnitName() throws InvalidGdlSchemaException {
		Occurrence nameOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlUnitName);

		if(nameOcc == null){
			return "";
		} else {
			return nameOcc.getValue();
		}
	}

	
	// sets the passed string a caption/unit-name of the group item
	public void setUnitName(Widget widget, String value){
		((UnitWidget)widget).setCaption(value);
	}
	
	
	// returns a ColorValue instance that represents the style of this element's border.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	@Override
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
			return BorderStyleValue.GROOVE;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}
	
	
	// sets the text-decoration style property of this element by using the GWT DOM class
	@Override
	public void setTextDecoration(Widget widget, TextDecorationValue value,	String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(UnitWidget.class)){
			for(int i = 0; i != DOM.getChildCount(((UnitWidget)widget).basePanel.getElement()); ++i){
				if(DOM.getChild(((UnitWidget)widget).basePanel.getElement(), i).getNodeName().toLowerCase().equals("legend")){
					Element legend = DOM.getChild(((UnitWidget)widget).basePanel.getElement(), i);
					this.setCssProperty(legend, styleClass, "textDecoration", value.getCssValue());
				}
			}
		}
	}


	// sets all GDL styles that are defined by the topic map representative to tha passed widget
	@Override
	public void setGdlStyle(Widget widget) throws InvalidGdlSchemaException, ExecutionException {
		super.setGdlStyle(widget);
		this.setUnitName(widget, this.getUnitName());
	}
	

	@Override
	@Deprecated
	public GdlVisibleObject append(Topic ancestor, Topic current) throws InvalidGdlSchemaException, ExecutionException {
		// do nothing this method has no effect on the mainpanle
		return null;
	}
	
	
	// returns all topics that are bound to this tm representative topic via a
	// contains association
	@Override
	public ArrayList<Topic> contains() throws InvalidGdlSchemaException {
		return TmHelper.topicContains(this.tmRepresentative);
	}
	
	
	// Searches the topic that represents the first item that is placed within this view instance
	// i.e. such an item must not have an association that is bound to it via a role of the type
	// gdl:ancestor.
	protected Topic getStartElement(ArrayList<Topic> containees) throws InvalidGdlSchemaException {
		return TmHelper.getFirstContainee(super.getTmRepresentative(), containees);
	}
	
	
	// returns the constraint that the GdlUnit instance is bound to or null
	// if it is not bound to a constraint topic
	public Topic getConstraint() throws InvalidGdlSchemaException {
		if(this.constraintTopicSet){
			return this.constraintTopic;
		} else {
			this.constraintTopicSet = true;
			TopicMap tm = this.tmRepresentative.getTopicMap();
			Topic descriptorRoleType = TmHelper.getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, tm);
			Topic tmBindingAssocType = TmHelper.getTopicByPsi(PSIs.GDL.AssociationType.gdlTmBinding, tm);
			Topic tmConstructRoleType = TmHelper.getTopicByPsi(PSIs.GDL.RoleType.gdlTmConstruct, tm);
			Topic tmclTopicNameConstraintType = TmHelper.getTopicByPsi(PSIs.TMCL.tmclTopicNameConstraint, tm);
			Topic tmclVariantNameConstraintType = TmHelper.getTopicByPsi(PSIs.TMCL.tmclVariantNameConstraint, tm);
			Topic tmclTopicOccurrenceConstraintType = TmHelper.getTopicByPsi(PSIs.TMCL.tmclTopicOccurrenceConstraint, tm);
			
			ArrayList<Topic> nameConstraints = TmHelper.getOtherPlayerOfBinaryAssociation(this.tmRepresentative, descriptorRoleType, tmBindingAssocType, null, tmclTopicNameConstraintType, tmConstructRoleType);
			ArrayList<Topic> variantConstraints = TmHelper.getOtherPlayerOfBinaryAssociation(this.tmRepresentative, descriptorRoleType, tmBindingAssocType, null, tmclVariantNameConstraintType, tmConstructRoleType);
			ArrayList<Topic> occurrenceConstraints = TmHelper.getOtherPlayerOfBinaryAssociation(this.tmRepresentative, descriptorRoleType, tmBindingAssocType, null, tmclTopicOccurrenceConstraintType, tmConstructRoleType);
			ArrayList<Topic> allConstraints = Utils.union(Utils.union(nameConstraints, variantConstraints), occurrenceConstraints);
			if(allConstraints.size() > 1)
				throw new InvalidGdlSchemaException("the topic " + TmHelper.getAnyIdOfTopic(this.tmRepresentative) + " can be bound only to one constraint, but is " + allConstraints.size());
		
			if(allConstraints.size() == 0) return null;
			else return allConstraints.get(0);
		}
	}
	
	
	@Override
	@Deprecated
	public void fixValue(){
		// has no effect on this element
	}
	
	
	@Override
	public void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		// this method has no effect on this class
	}
	
	
	@Override
	public ArrayList<String> getSelectedValues(){
		// TODO: implement
		return new ArrayList<String>();
	}
	
	
	protected class UnitWidget extends Composite implements HasFocusHandlers, HasBlurHandlers, HasMouseOutHandlers, HasMouseOverHandlers, HasMouseDownHandlers, HasMouseUpHandlers{
		private CaptionPanel basePanel = new CaptionPanel();
		private AbsolutePanel containerPanel = new AbsolutePanel();
		private Topic tmRepresentative = null;
		private ArrayList<Widget> subElements = new ArrayList<Widget>();
		private Construct currentElement = null; 
		
		private UnitWidget(){
			initWidget(this.basePanel);
			this.basePanel.add(this.containerPanel);
			DOM.setStyleAttribute(this.basePanel.getElement(), "textDecoration", "none");
		}
		
		
		public UnitWidget(Topic tmRepresentative, Construct currentElement){
			this();
			this.tmRepresentative = tmRepresentative;
			this.currentElement = currentElement;
		}
		
		
		public Topic getTmRepresentative(){
			return this.tmRepresentative;
		}
				
		
		// appends a new element in form of a topic to this unit-widget
		public GdlVisibleObject append(Topic ancestor, Topic current) throws ExecutionException, InvalidGdlSchemaException {
			if(ancestor == null || current == null) throw new ExecutionException("to append \"" + TmHelper.getAnyIdOfTopic(current) + "\" on \"" + TmHelper.getAnyIdOfTopic(ancestor) + "\" both topics must be present");
			GdlPosition position = new GdlPosition(TmHelper.getPositionOf(ancestor, current));
			
			GdlVisibleObject newObj = GdlInstantiator.instantiate(current, this.currentElement, GdlUnit.this);
			GdlVisibleObject oldObj = null;
			for (Widget widget : this.subElements){
				if(((GdlVisibleObject)widget).getTmRepresentative().equals(ancestor)){
					oldObj = (GdlVisibleObject)widget;
					break;
				}
			}
			
			this.subElements.add(newObj);
			if(this.getTmRepresentative().equals(ancestor)){
				this.containerPanel.add(newObj);
			}
			else{
				Element elemBefore = oldObj.getElement();
				Element elemAfter = DOM.getNextSibling(elemBefore);
				// insert before seems to be a more accepted and stable
				// way of inserting new child elements than insertAfter
				if(elemAfter == null) this.containerPanel.add(newObj);
				else this.containerPanel.getElement().insertBefore(newObj.getElement(), elemAfter);
			}			
			position.setAttributes(newObj);
			return newObj;
		}
		
		
		public void setId(String id){
			DOM.setElementAttribute(this.basePanel.getElement(), "id", id);
		}
		
		
		public String getCaption() {
			return this.basePanel.getCaptionText();
		}
		
		
		// sets the unit's name. If the name is "" or null,
		// the legend element is set to non-visible
		public void setCaption(String caption) {
			if(caption == null || caption.length() == 0){
				this.basePanel.setCaptionText(caption);
			} else {
				this.basePanel.setCaptionText(caption);
			}
		}
	
		
		@Override
		public HandlerRegistration addMouseUpHandler(MouseUpHandler handler) {
			return this.addDomHandler(handler, MouseUpEvent.getType());
		}


		@Override
		public HandlerRegistration addMouseDownHandler(MouseDownHandler handler) {
			return this.addDomHandler(handler, MouseDownEvent.getType());
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
		public void onAttach(){
			super.onAttach();
			try{
				String[] styleClasses = new String[]{null, PSIs.GDL.Scope.gdlActive, PSIs.GDL.Scope.gdlFocus, PSIs.GDL.Scope.gdlHover};
				for (String styleClass : styleClasses) {
					GdlUnit.this.setTextDecoration(this, GdlUnit.this.getTextDecoration(styleClass), styleClass);
				}
			}catch(Exception e){
				e.printStackTrace();
			}
		}
	}
}