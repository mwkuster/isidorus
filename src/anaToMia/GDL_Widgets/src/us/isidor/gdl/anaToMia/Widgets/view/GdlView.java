package us.isidor.gdl.anaToMia.Widgets.view;


import java.util.ArrayList;
import com.google.gwt.core.client.JsArray;
import com.google.gwt.user.client.ui.Widget;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Name;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlPosition;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.base.IGdlContainer;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.base.TmHelper;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.GdlInstantiator;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.value.ContentOrientationValue;


public abstract class GdlView extends GdlVisibleObject implements IGdlContainer {
	protected boolean hiddenValuesSet = false;
	protected ArrayList<Topic> hiddenValues = new ArrayList<Topic>();
	
	
	protected GdlView(){
		super();
	}
	
	
	public GdlView(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		
	}
	
	
	@Override
	@Deprecated
	public void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		//this method has no effect on this class
	}
	
	
	@Override
	@Deprecated
	public void fixValue(){
		// has no effect on this element
	}
	
	
	@Override
	@Deprecated
	public ArrayList<String> getSelectedValues(){
		// has no effect on this element
		return new ArrayList<String>();
	}
	
	
	public String getViewName()throws InvalidGdlSchemaException {
		JsArray<Name> viewNames = super.tmRepresentative.getNames(tm.getTopicBySubjectIdentifier(tm.createLocator(PSIs.GDL.NameType.gdlViewName)));
		if(viewNames.length() != 1){
			throw new InvalidGdlSchemaException("A view must be bound to a " + PSIs.GDL.NameType.gdlViewName + " instance");
		}
		return viewNames.get(0).getValue();
	}
	
	
	// content orientation has no effect on a view
	@Override
	@Deprecated
	public void setContentOrientation(ContentOrientationValue value) throws InvalidGdlSchemaException, ExecutionException {
		// do nothing, no table is set for organizing content widgets, since a view does not organize
		// its content in this manner
	}
		
	
	// Searches the topic that represents the first item that is placed within this view instance
	// i.e. such an item must not have an association that is bound to it via a role of the type
	// gdl:ancestor.
	protected Topic getStartElement(ArrayList<Topic> containees) throws InvalidGdlSchemaException {
		return TmHelper.getFirstContainee(super.getTmRepresentative(), containees);
	}
	
	
	// creates a GdlVisbleObject instance of the passed topic current and inserts it
	// by using it's defined position style depending on the GdlVisibleObject
	// represented by ancestor. If ancestor is null current is the first topic
	@Override
	public GdlVisibleObject append(Topic ancestor, Topic current) throws InvalidGdlSchemaException, ExecutionException {
		if(ancestor == null || current == null) throw new ExecutionException("to append \"" + TmHelper.getAnyIdOfTopic(current) + "\" on \"" + TmHelper.getAnyIdOfTopic(ancestor) + "\" both topics must be present");
		GdlPosition position = new GdlPosition(TmHelper.getPositionOf(ancestor, current));
		
		GdlVisibleObject newObj = GdlInstantiator.instantiate(current, this.receivedData, this);
		GdlVisibleObject oldObj = null;
		for (Widget widget : super.subElements){
			if(((GdlVisibleObject)widget).getTmRepresentative().equals(ancestor)){
				oldObj = (GdlVisibleObject)widget;
				break;
			}
		}
		
		this.subElements.add(newObj);
		if(super.getTmRepresentative().equals(ancestor))this.mainPanel.add(newObj);
		else this.mainPanel.insert(newObj, this.mainPanel.getWidgetIndex(oldObj) + 1);
		position.setAttributes(newObj);
		return newObj;
	}
	
	
	// returns all topics that are bound to this tm representative topic via a
	// contains association
	@Override
	public ArrayList<Topic> contains() throws InvalidGdlSchemaException{
		return TmHelper.topicContains(this.tmRepresentative);
	}
	
	
	// returns all hidden values that are bound to this view
	public ArrayList<Topic> getHiddenValues() throws InvalidGdlSchemaException {
		if(this.hiddenValuesSet) {
			return this.hiddenValues;
		} else {
			this.hiddenValuesSet = true;
			this.hiddenValues = TmHelper.getHiddenValueOf(this.tmRepresentative);
			return this.hiddenValues;
		}
	}
}