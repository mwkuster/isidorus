package us.isidor.gdl.anaToMia.Widgets.view;


import java.util.ArrayList;
import com.google.gwt.core.client.JsArray;
import com.google.gwt.user.client.ui.Widget;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Name;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Association;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Role;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMapsTypes;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Variant;
import us.isidor.gdl.anaToMia.Widgets.base.ButtonableObject;
import us.isidor.gdl.anaToMia.Widgets.base.GdlHiddenValue;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.base.TmHelper;
import us.isidor.gdl.anaToMia.Widgets.base.Utils;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidContentException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;
import us.isidor.gdl.anaToMia.Widgets.environment.TopicIdentifierTypes;
import us.isidor.gdl.anaToMia.Widgets.value.ContentOrientationValue;


public abstract class GdlTopicView extends GdlView {
	protected Topic representedTopic = null; 


	public GdlTopicView(Topic tmRepresentative, Topic receivedData, GdlVisibleObject gdlParent)  throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		
		ArrayList<Topic> objectsContained = this.contains();
		
		Topic currentTopic = this.getStartElement(objectsContained);
		Topic lastTopic = super.getTmRepresentative();
		while(currentTopic != null){
			this.append(lastTopic, currentTopic);
			lastTopic = currentTopic;
			currentTopic = TmHelper.getNextContainee(currentTopic, objectsContained);
		}
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
	
	
	// content orientation has no effect on a view
	@Override
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
	
	
	// sets all GDL styles that are defined by the topic map representative.
	// In contrast to visible-object, the styles are applied only on the main
	// panel, since the view's sub-elements must be formatted with their own
	// style properties
	@Override
	protected void setGdlStyle() throws InvalidGdlSchemaException, ExecutionException {
		this.setDisplay(this.getDisplay());
		this.setZindex(this.getZindex());
		this.setFloat(this.getFloat());
		this.setClear(this.getClear());
		this.setGdlStyle(this.mainPanel);
	}
	
	
	// returns true if the construct that is contained in the first member of pair
	// is also indirectly contained in the container
	private boolean indirectlyContained(Pair<Construct, TopicMapsTypes> pair, ArrayList<Pair<Construct, TopicMapsTypes>> container){
		if(pair == null) return true;
		if(container == null) return false;
		
		TopicMapsTypes newType = pair.getSecond();
		if(newType.equals(TopicMapsTypes.Name)){
			Name name = (Name)pair.getFirst();
			for (Pair<Construct, TopicMapsTypes> item : container) {
				if(item.getSecond().equals(TopicMapsTypes.Topic)){
					JsArray<Name> names = ((Topic)item.getFirst()).getNames();
					if(Utils.contains(names, name)) return true;
				}
			}
		} else if (newType.equals(TopicMapsTypes.Variant)){
			Variant variant = (Variant)pair.getFirst();
			for (Pair<Construct, TopicMapsTypes> item : container) {
				if(item.getSecond().equals(TopicMapsTypes.Name)){
					JsArray<Variant> variants = ((Name)item.getFirst()).getVariants();
					if(Utils.contains(variants, variant)) return true;
				}
			}
			
			for (Pair<Construct, TopicMapsTypes> item : container) {
				if(item.getSecond().equals(TopicMapsTypes.Topic)){
					JsArray<Name> names = ((Topic)item.getFirst()).getNames();
					for(int i = 0; i != names.length(); ++i){
						JsArray<Variant> variants = names.get(i).getVariants();
						if(Utils.contains(variants, variant)) return true;
					}
				}
			}
		} else if (newType.equals(TopicMapsTypes.Occurrence)){
			Occurrence occurrence = (Occurrence)pair.getFirst();
			for (Pair<Construct, TopicMapsTypes> item : container) {
				if(item.getSecond().equals(TopicMapsTypes.Topic)){
					JsArray<Occurrence> occurrences = ((Topic)item.getFirst()).getOccurrences();
					if(Utils.contains(occurrences, occurrence)) return true;
				}
			}
		} else if (newType.equals(TopicMapsTypes.Role)){
			Role role = (Role)pair.getFirst();
			for (Pair<Construct, TopicMapsTypes> item : container) {
				if(item.getSecond().equals(TopicMapsTypes.Association)){
					JsArray<Role> roles = ((Association)item.getFirst()).getRoles();
					if(Utils.contains(roles, role)) return true;
				}
			}
		}

		return false;
	}
	
	
	@SuppressWarnings("unchecked")
	@Override
	public ArrayList<Pair<Construct, TopicMapsTypes>> getContent(Construct carrier, boolean validate) throws InvalidGdlSchemaException, ExecutionException, InvalidContentException {
		ArrayList<Pair<Construct, TopicMapsTypes>> result = new ArrayList<Pair<Construct,TopicMapsTypes>>();
		
		Topic localCarrier = this.getRepresentedTopic();
				
		for (Widget ctrl : this.subElements) {
			int i = 0;
			for( ; i != result.size(); ++i) if(result.get(i).getFirst().equals(localCarrier)) break;
			if(i == result.size()) result.add(new Pair<Construct, TopicMapsTypes>(localCarrier, TopicMapsTypes.Topic));
			
			if(ctrl instanceof GdlVisibleObject){
				for (Pair<Construct, TopicMapsTypes> pair : ((GdlVisibleObject)ctrl).getContent(localCarrier, validate)) {
					if((this.receivedData != null || (ctrl instanceof GdlView)) && !this.indirectlyContained(pair, result))result.add(pair);
				}
			}
		}
		
		
		for (Topic hd : this.getHiddenValues()) {
			GdlHiddenValue hdv = new GdlHiddenValue(hd, this);
			if(TmHelper.isInstanceOf(hdv.getConstraint(), PSIs.TMCL.tmclTopicOccurrenceConstraint)){
				if(!hdv.getConstraint().equals(hdv.getRootConstraint())) throw new InvalidGdlSchemaException("the constraint " + TmHelper.getAnyIdOfTopic(hdv.getConstraint()) + " must not be bound to another constraint, but is bound to:  " + TmHelper.getAnyIdOfTopic(hdv.getRootConstraint()));
				Topic top = this.getRepresentedTopic();
				Topic occurrenceType = TmHelper.getConstrainedStatement(hdv.getConstraint());
				if(occurrenceType == null) throw new InvalidContentException("the constraint " + TmHelper.getAnyIdOfTopic(hdv.getConstraint()) + " must be bound to an occurrence type via a " + PSIs.TMCL.tmclConstrainedStatement + " association");
				String value = hdv.getRawDefaultLiteralValue();
				if(value == null) throw new InvalidGdlSchemaException("the topic " + TmHelper.getAnyIdOfTopic(hdv.getTmRepresentative()) + " must be bound to an instance of " + PSIs.GDL.TopicType.gdlHiddenValue + ", but is unbound");
				JsArray<Occurrence> occurrences = top.getOccurrences(occurrenceType);
				// set the occurrence value only if it does not exist - don't override an existing occurrence
				int i = 0;
				for( ; i != occurrences.length(); ++i) if(occurrences.get(i).getValue().equals(value)) break;
				
				if(i == occurrences.length()){
					Occurrence occ = top.createOccurrence(occurrenceType, value, (JsArray<Topic>)JsArray.createArray());
					Pair<Construct, TopicMapsTypes> newItem = new Pair<Construct, TopicMapsTypes>(occ, TopicMapsTypes.Occurrence);
					if(!this.indirectlyContained(newItem, result)) result.add(newItem);
				}
			}
			
			
			// TODO: process hidden values
			// subject-identifier
			// subject-locator
			// item-identifier
			// topic-name
			// variant-name
			// *topic-occurrence
			// type => topic-name
			// type => topic-occurrence
			// datatype => topic-occurrence
			// scope => topic-name
			// scope => topic-occurrence
		}
		
		return result;
	}
	
	
	// returns the received topic or creates a topic stub
	public Topic getRepresentedTopic() throws InvalidGdlSchemaException {
		if(this.receivedData ==null){
			if(this.representedTopic == null){
				this.representedTopic = this.tmRepresentative.getTopicMap().createTopicBySubjectIdentifier(this.tm.createLocator(PSIs.GDL.gdl + this.getId()));
				
				if((this instanceof GdlDefaultCreatorTopicView) || (this instanceof GdlSpecialCreatorTopicView)){
					ArrayList<Topic> topicTypes = new ArrayList<Topic>();
					ArrayList<Pair<String, TopicIdentifierTypes>> ids = this.getRoot().getReqeustedTopicsToCreate();
					if(ids != null)
						for (Pair<String, TopicIdentifierTypes> pair : ids) topicTypes.add(TmHelper.getTopicByAnyIdentifier(pair, this.tmRepresentative.getTopicMap()));
						
					for (Topic topType : topicTypes) this.representedTopic.addType(topType);
				}				
				
				return this.representedTopic;
			} else {
				return this.representedTopic;
			}
		} else {
			return (Topic)this.receivedData;
		}
	}
}
