package us.isidor.gdl.anaToMia.Widgets.container;

import java.util.ArrayList;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Element;
import com.google.gwt.user.client.ui.AbsolutePanel;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;
import com.google.gwt.dom.client.Node;
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
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.ButtonableObject;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.base.IGdlContainer;
import us.isidor.gdl.anaToMia.Widgets.base.TmHelper;
import us.isidor.gdl.anaToMia.Widgets.environment.ActiveStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.FocusStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.GdlInstantiator;
import us.isidor.gdl.anaToMia.Widgets.environment.HoverStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;
import us.isidor.gdl.anaToMia.Widgets.value.ListStylePositionValue;
import us.isidor.gdl.anaToMia.Widgets.value.ListStyleTypeValue;


public class GdlList extends GdlVisibleObject implements IGdlContainer {
	private ArrayList<Pair<Topic, Integer>> storedItems = null;
	
	
	// some constructors
	protected GdlList() throws InvalidGdlSchemaException, ExecutionException {
		super();
		this.createNewList();
		this.setNthButtons();
	}
	
	
	public GdlList(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		this.createNewList();
		
		ArrayList<Topic> objectsContained = this.contains();
		
		Topic lastTopic = null;
		
		if(objectsContained.size() != 0){
			for(int i = 0; i != objectsContained.size(); ++i){
				this.append(lastTopic, objectsContained.get(i));
				lastTopic = objectsContained.get(i);
			}
		}
	}
	
	
	@Override
	public void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		// this method has no effect on this class
	}
	
	
	// sets the list Element to either ol or ul depending on the gdl:ordered property
	public ListWidget createNewList() throws InvalidGdlSchemaException, ExecutionException {
		// this object is able to own only one sub-element
		if(this.subElements != null && this.subElements.size() == 1) return (ListWidget)this.subElements.get(0);
		
		ListWidget list = new ListWidget(this.getOrdered());
		ActiveStyleHandler asHandler = new ActiveStyleHandler(this);
		FocusStyleHandler fsHandler = new FocusStyleHandler(this);
		HoverStyleHandler hsHandler = new HoverStyleHandler(this);
		int idSuffix = 0;
		if(this.subElements != null) idSuffix = this.subElements.size(); 
		list.setId(this.getId() + "__GDL_" + idSuffix);
		list.addMouseDownHandler(asHandler);
		list.addMouseUpHandler(asHandler);
		list.addMouseOverHandler(hsHandler);
		list.addMouseOutHandler(hsHandler);
		list.addFocusHandler(fsHandler);
		list.addBlurHandler(fsHandler);
		super.addToContainerPanel(list);	
		this.setGdlStyle(list);
		return list;
	}
	
	
	// returns the property of a gdl:ordered occurrence, otherwise the default value if no occurrence is set
	public boolean getOrdered() throws InvalidGdlSchemaException{
		Occurrence orderedOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlOrdered);

		if(orderedOcc != null){
			String boolStr = orderedOcc.getValue().toUpperCase();
			if(boolStr.equals("TRUE")){
				return true;
			} else if(boolStr.equals("FALSE")) {
				return false;
			} else {
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlOrdered + " must be set to one of \"true\" or \"false\", but is \"" + orderedOcc.getValue() + "\"");
			}
		} else {
			return false;
		}
	}
	
	
	// returns a ListStyleTypeValeu instance that describes this instance's list style type property
	public ListStyleTypeValue getListStyleType() throws InvalidGdlSchemaException {
		Occurrence typeOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlListStyleType);

		if(typeOcc != null){
			try{
				return ListStyleTypeValue.fromString(typeOcc.getValue());
			}catch(IllegalArgumentException e){
				String values = "deciaml, decimal-leading-zero, lower-greek, lower-roman, armenian, georgian, upper-roman, " +
								"lower-alpha, upper-alpha, lower-latin, upper-latin, disc, circle, square or none";
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlListStyleType + " must be set to one of " + values + " , but is \"" + typeOcc.getValue() + "\"");
			}
		} else if(this.getOrdered()){
			return ListStyleTypeValue.DECIMAL;
		} else {
			return ListStyleTypeValue.DISC;
		}
	}

	
	// returns a ListStylePositionValue instance that describes this instance's list style position property
	public ListStylePositionValue getListStylePosition() throws InvalidGdlSchemaException {
		Occurrence positionOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlListStylePosition);

		if(positionOcc != null){
			try{
				return ListStylePositionValue.valueOf(positionOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlListStylePosition + " must be set to one of inside or outside, but is \"" + positionOcc.getValue() + "\"");
			}
		} else{
			return ListStylePositionValue.OUTSIDE;
		}
		
	}

	
	// sets the css property ordered
	@Deprecated
	public void setOrdered(Widget widget, boolean value) throws InvalidGdlSchemaException, ExecutionException {
		// do nothing this property has to be set in the initList mehtod
	}
	
	
	// sets the css property list-style-type
	public void setListStyleType(ListWidget widget, ListStyleTypeValue value) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) DOM.setStyleAttribute(widget.getListElement(), "listStyleType", value.getCssValue());
	}
	
	
	// sets the css property list-style-position
	public void setListStylePositionValue(ListWidget widget, ListStylePositionValue value) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) DOM.setStyleAttribute(widget.getListElement(), "listStylePosition", value.getCssValue());
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
	// method, which sets some specific properties for the GdlList instance
	@Override
	public void setGdlStyle(Widget widget) throws InvalidGdlSchemaException, ExecutionException {
		super.setGdlStyle(widget);

		// this may be only called if the ListWidget is the passed argument
		if(widget.getClass().equals(ListWidget.class)){
			this.setListStylePositionValue((ListWidget)widget, this.getListStylePosition());
			this.setListStyleType((ListWidget)widget, this.getListStyleType());
		}
	}
	
	
	// inserts the passed item on the given index
	public void insert(GdlVisibleObject item, int position){
		if(this.subElements != null && this.subElements.size() != 0){
			((ListWidget)((ButtonableObject)this.subElements.get(0)).getMainObject()).insertWidget(item, position);
		}
	}

	
	// this class wraps a list item, i.e. a div element within a li element
	protected class ListItem{
		private Element liElement = null;
		private Element divElement = null;
		private ArrayList<GdlVisibleObject> contents = new ArrayList<GdlVisibleObject>();
		
		
		public ListItem(){
			this.liElement = DOM.createElement("li");
			this.divElement = DOM.createElement("div");
			this.liElement.insertFirst(this.divElement);
		}
		
		public ListItem(Element parentList){
			this();
			this.appendToList(parentList);
		}
		
		// inserts the this instance into the passed list element,
		// if this instance is not bound yet
		public void appendToList(Element parentList){
			if(parentList != null && this.liElement.getParentNode() == null){
				Node lastChild = parentList.getLastChild();
				if(lastChild != null){
					parentList.insertAfter(this.liElement, lastChild);
				} else {
					parentList.insertFirst(this.liElement);
				}
			}
		}
		
		
		// inserts the this instance into the passed list element
		// after the passed ListItem, if this instance is not bound yet
		public void appendToList(Element parentList, ListItem previous){
			if(parentList == null) return;
			
			if(previous == null){
				this.appendToList(parentList);
			}else{
				parentList.insertAfter(this.liElement, previous.liElement);
			}
		}
		
		
		// removes this instance from the set list
		public void removeFromList(){
			this.liElement.removeFromParent();
		}
		
		// appends the content element to the inner div item
		public void appendContentToListItem(GdlVisibleObject content){
			if(content != null){
				Node lastChild = this.divElement.getLastChild();
				if(lastChild != null){
					this.divElement.insertAfter(content.getElement(), lastChild);
				} else {
					this.divElement.insertFirst(content.getElement());
				}
				
				contents.add(content);
			}
		}
	}
	

	@Override
	public GdlVisibleObject append(Topic ancestor, Topic current) throws InvalidGdlSchemaException, ExecutionException {
		// TODO Auto-generated method stub
		if(this.subElements.size() == 0) return null;
		if(this.storedItems == null) this.storedItems = this.containsItems();
		
		// the ancestor element can be ignore, since the current idx can be looked up
		// from the storedItem array list directly
		int idx = -1;
		for (Pair<Topic, Integer> pair : this.storedItems) if(pair.getFirst().equals(current)) idx = pair.getSecond();
		
		ListWidget list = (ListWidget)((ButtonableObject)this.subElements.get(0)).getMainObject();
		GdlVisibleObject newObj = GdlInstantiator.instantiate(current, this.receivedData, GdlList.this);
		list.insertWidget(newObj, idx);
		
		return newObj;
	}
	

	// returns all topics that are bound to this tm representative topic via a
	// contains association i an ordered list
	@Override
	public ArrayList<Topic> contains() throws InvalidGdlSchemaException {
		ArrayList<Topic> result = new ArrayList<Topic>();
		for (Pair<Topic, Integer> pair : TmHelper.listContains(super.getTmRepresentative()))
			result.add(pair.getFirst());
		
		return result;
	}
	
	
	// returns an ordered list of items with their index
	public ArrayList<Pair<Topic, Integer>> containsItems() throws InvalidGdlSchemaException{
		return TmHelper.listContains(super.getTmRepresentative());
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
	
	
	// Wraps a ul an ol element as a widget based on a SimplePanel
	protected class ListWidget extends Composite implements HasMouseDownHandlers, HasMouseUpHandlers, HasFocusHandlers, HasBlurHandlers, HasMouseOutHandlers, HasMouseOverHandlers{
		private AbsolutePanel basePanel = new AbsolutePanel();
		private Element listElement = null;
		// note: index is not the actual index of the item in the array list,
		// it is the user's passed index when inserting this element
		private ArrayList<Pair<ListItem, Integer>> itemsAndIndexes = new ArrayList<Pair<ListItem, Integer>>();
		
		public ListWidget(){
			initWidget(this.basePanel);
			DOM.setStyleAttribute(this.basePanel.getElement(), "overflow", "visible");
			this.listElement = DOM.createElement("ul");
			this.basePanel.getElement().insertFirst(this.listElement);
		}
		
		
		public ListWidget(boolean ordered){
			initWidget(this.basePanel);
			DOM.setStyleAttribute(this.basePanel.getElement(), "overflow", "visible");
			if(ordered) this.listElement = DOM.createElement("ol");
			else this.listElement = DOM.createElement("ul");
			this.basePanel.getElement().insertFirst(this.listElement);
		}
		
		
		// inserts the passed list item as the last child to this list element
		public void appendListItem(ListItem item){
			if(item == null) return;
			item.appendToList(this.listElement);
		}
		
		
		// inserts the passed instance item to this list directly after the intance previous
		public void appendListItem(ListItem item, ListItem previous){
			if(item == null) return;
			item.appendToList(this.listElement, previous);
		}
		
		
		// inserts the passed widget directly before the first item that
		// has a greater position index
		public void insertWidget(GdlVisibleObject widget, int position){
			ListItem previosItem = this.getItemBeforeIndex(position);
			ListItem item = new ListItem(this.listElement);
			item.appendContentToListItem(widget);
			this.appendListItem(item, previosItem);
		}
		
		
		// returns the first item that has a smaller position index 
		public ListItem getItemBeforeIndex(int idx){
			Pair<ListItem, Integer> result = null;
			for (Pair<ListItem, Integer> item : this.itemsAndIndexes) {
				if(item.getSecond() < idx && (result == null || result.getSecond() < item.getSecond()))result = item;
			}
		
			if(result != null) result.getFirst();
			return null;
		}
		
		
		// returns the acutal DOM element
		public Element getListElement(){
			return this.listElement;
		}
		
		// returns all ListItems
		public ArrayList<ListItem> getItems(){
			ArrayList<ListItem> items = new ArrayList<GdlList.ListItem>();
			for (Pair<ListItem, Integer> pair : this.itemsAndIndexes) {
				items.add(pair.getFirst());
			}
			return items;
		}
		
		
		// returns the position index of the item, that was specified when
		// inserting the passed ListItem
		public int indexOfItem(ListItem item){
			for (Pair<ListItem, Integer> pair : this.itemsAndIndexes) {
				if(pair.getFirst().equals(item)) return pair.getSecond();
			}
			
			return -1;
		}
		
		
		public void setId(String id){
			DOM.setElementAttribute(this.basePanel.getElement(), "id", id);
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
