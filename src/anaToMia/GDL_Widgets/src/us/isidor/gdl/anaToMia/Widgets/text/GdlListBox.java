package us.isidor.gdl.anaToMia.Widgets.text;

import java.util.ArrayList;
import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.Widget;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.ButtonableObject;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.base.TmHelper;
import us.isidor.gdl.anaToMia.Widgets.base.Utils;
import us.isidor.gdl.anaToMia.Widgets.environment.ActiveStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.FocusStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.HoverStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;


public class GdlListBox extends GdlTextObject {
	// some constructors
	protected GdlListBox() throws InvalidGdlSchemaException, ExecutionException {
		super();
	}
	
	
	public GdlListBox(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		
		if(receivedData != null && this.getConstraint() != null) this.setReceivedData();
		else this.setDefaultValue();
		
		this.setNthButtons();
	}
	
	
	@Override
	public void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		ArrayList<String> options = new ArrayList<String>();
		
		// get the explicitly set TM-Values and Literal-Values.
		// if no values were set explicitly, get the values for the constraint
		ArrayList<String> exLiteralValues = this.getLiterals();
		ArrayList<Topic> exTmValues = this.getTmValues();
		if(exLiteralValues.size() != 0 && exTmValues.size() != 0){
			throw new InvalidGdlSchemaException("found literal values and tm values for the value group " + TmHelper.getAnyIdOfTopic(this.getValueGroup()) + ", but mixing literal and tm-values for a value group is not allowed!");
		} else if(exLiteralValues.size() != 0){
			options = exLiteralValues;
		} else if(exTmValues.size() != 0){
			ArrayList<Topic> validValues = new ArrayList<Topic>();
			
			for (Topic tmValue : exTmValues){
				for (Topic topicInstance : TmHelper.getValuesForTmValue(tmValue)){
					if(!validValues.contains(topicInstance)){
						validValues.add(topicInstance);
						options.add(this.getTopicRepresentation(topicInstance, this.getDisplayByOfTmValue(tmValue), this.getPreferredScopeOfTmValue(tmValue)));
					}
				}
			}
		} else {
			ArrayList<Topic> tmValues = this.getTmValuesForConstraint();
			if(tmValues.size() != 0){
				for (Topic topic : tmValues) options.add(this.getTopicRepresentation(topic, this.getDisplayByOfValueGroup(), this.getPreferredScopeOfValueGroup()));
			} else {
				throw new InvalidGdlSchemaException("found no value to display for the value-group " + TmHelper.getAnyIdOfTopic(this.getValueGroup()));
			}					
		}		
		
		ListBox lb = this.createNewListBox();
		options = Utils.sort(options);
		for(String item : options) lb.addItem(item);
		
		for(int i = 0; i != lb.getItemCount(); ++i){
			if(lb.getItemText(i).equals(value)){
				lb.setSelectedIndex(i);
				break;
			}
		}
	}
	
	
	// creates a new ListBox item, adds it to the subElements array,
	// and applies the styles on it
	protected ListBox createNewListBox() throws InvalidGdlSchemaException, ExecutionException {
		ListBox elem = new ListBox(this.getMultiple());
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
	protected void removeListBox(ListBox elem) throws InvalidGdlSchemaException, ExecutionException{
		for (Pair<Widget, ArrayList<EventHandler>> item : this.eventHandlers) {
			if(item.getFirst().equals(elem)){
				this.eventHandlers.remove(item);
				break;
			}
		}
		
		this.removeFromContainer(elem);
	}
	
	
	// returns the gdl:multiple property - if no value is set the default value is returned
	public boolean getMultiple() throws InvalidGdlSchemaException {
		Occurrence multipleOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlMultiple);

		if(multipleOcc != null){
			String boolStr = multipleOcc.getValue().toUpperCase();
			if(boolStr.equals("TRUE")){
				return true;
			} else if(boolStr.equals("FALSE")) {
				return false;
			} else {
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlMargin + " must be set to one of \"true\" or \"false\", but is \"" + multipleOcc.getValue() + "\"");
			}
		} else {
			return false;
		}
	}
	
	
	// returns an int instance of a gdl:size occurrence.
	// If no gdl:size occurrence is set, the default value is returned
	public int getSize() throws InvalidGdlSchemaException {
		Occurrence rowsOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlSize);

		if(rowsOcc != null){
			try{
				int value = Integer.valueOf(rowsOcc.getValue());
				if(value < 0) throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlSize + " must be set to a positive integer, but is \"" + rowsOcc.getValue() + "\""); 
				else return value;
			}catch(NumberFormatException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlSize + " must be set to a positive integer, but is \"" + rowsOcc.getValue() + "\"");
			}
		} else {
			return 1;
		}
	}
	
	
	// sets the size property of this element by using the GWT DOM class
	public void setSize(Widget widget, int value){
		((ListBox)widget).setVisibleItemCount(value);
	}
	
	
	// this method has not effect, it is defined only for consistency reasons
	@Deprecated
	public void setMultiple(Widget widget, boolean value){
		// Do nothing the gdl:multiple property is set in the createListBox method
		// directly when calling the ListBox constructor
	}
	
	
	@Override
	public void fixValue(){
		for (Widget ctrl : this.subElements) {
			if(ctrl instanceof ButtonableObject){
				Widget lb = ((ButtonableObject) ctrl).getMainObject();
				if(lb instanceof ListBox){
					((ListBox)lb).setEnabled(false);
				}
			}
		}
	}
	
		
	// sets the css properties, by calling the super class's method and the local
	// method, which sets some specific properties for the GdlText instance
	@Override
	public void setGdlStyle(Widget widget) throws InvalidGdlSchemaException, ExecutionException {
		super.setGdlStyle(widget);

		this.setSize(widget, this.getSize());
	}
	
	
	@Override
	public ArrayList<String> getSelectedValues(){
		ArrayList<String> result = new ArrayList<String>();
		for (Widget ctrl : this.subElements) {
			if(ctrl instanceof ButtonableObject){
				Widget lb = ((ButtonableObject) ctrl).getMainObject();
				if(lb instanceof ListBox){
					ListBox lbi = (ListBox)lb;
					result.add(lbi.getValue(lbi.getSelectedIndex()));
				}
			}
		}
		return result;
	}
}