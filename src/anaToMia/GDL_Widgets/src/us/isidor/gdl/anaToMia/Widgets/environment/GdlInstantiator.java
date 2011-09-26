package us.isidor.gdl.anaToMia.Widgets.environment;


import java.util.ArrayList;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Association;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Name;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Role;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMap;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Variant;
import us.isidor.gdl.anaToMia.Widgets.base.GdlPanel;
import us.isidor.gdl.anaToMia.Widgets.base.GdlSpace;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.base.TmHelper;
import us.isidor.gdl.anaToMia.Widgets.base.Utils;
import us.isidor.gdl.anaToMia.Widgets.button.GdlActionButton;
import us.isidor.gdl.anaToMia.Widgets.button.GdlCheckBox;
import us.isidor.gdl.anaToMia.Widgets.button.GdlCommitButton;
import us.isidor.gdl.anaToMia.Widgets.button.GdlCreateButton;
import us.isidor.gdl.anaToMia.Widgets.button.GdlDeleteButton;
import us.isidor.gdl.anaToMia.Widgets.button.GdlRadioButton;
import us.isidor.gdl.anaToMia.Widgets.button.GdlValidateButton;
import us.isidor.gdl.anaToMia.Widgets.complexData.GdlAudio;
import us.isidor.gdl.anaToMia.Widgets.complexData.GdlDatePicker;
import us.isidor.gdl.anaToMia.Widgets.complexData.GdlDateTimePicker;
import us.isidor.gdl.anaToMia.Widgets.complexData.GdlImage;
import us.isidor.gdl.anaToMia.Widgets.complexData.GdlTimePicker;
import us.isidor.gdl.anaToMia.Widgets.complexData.GdlVideo;
import us.isidor.gdl.anaToMia.Widgets.container.GdlList;
import us.isidor.gdl.anaToMia.Widgets.container.GdlUnit;
import us.isidor.gdl.anaToMia.Widgets.text.GdlInfo;
import us.isidor.gdl.anaToMia.Widgets.text.GdlListBox;
import us.isidor.gdl.anaToMia.Widgets.text.GdlReference;
import us.isidor.gdl.anaToMia.Widgets.text.GdlText;
import us.isidor.gdl.anaToMia.Widgets.text.GdlTitle;
import us.isidor.gdl.anaToMia.Widgets.view.GdlCreatorAssociationView;
import us.isidor.gdl.anaToMia.Widgets.view.GdlDefaultCreatorTopicView;
import us.isidor.gdl.anaToMia.Widgets.view.GdlDefaultEditorTopicView;
import us.isidor.gdl.anaToMia.Widgets.view.GdlEditorAssociationView;
import us.isidor.gdl.anaToMia.Widgets.view.GdlSpecialCreatorTopicView;
import us.isidor.gdl.anaToMia.Widgets.view.GdlSpecialEditorTopicView;


public class GdlInstantiator {	
	// returns a java instance of a GdlVisibleObject that corresponds to the
	// set topic type of the passed topic instance
	public static GdlVisibleObject instantiate(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		if(!(receivedData instanceof Topic) && !(receivedData instanceof Association) && !(receivedData instanceof Name) && !(receivedData instanceof Variant) && !(receivedData instanceof Occurrence) && !(receivedData instanceof Role) && receivedData != null) throw new ExecutionException("receivedData must be either a Topic, Association, Topic-Name, Name-Variant, Topic-Occurrence or Association-Role, but is: " + receivedData.getClass());
		
		if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlSpace)){
			return new GdlSpace(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlTitle)){
			return new GdlTitle(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlInfo)){
			return new GdlInfo(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlReference)){
			return new GdlReference(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlRadioButton)){
			return new GdlRadioButton(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlCheckBox)){
			return new GdlCheckBox(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlValidateButton)){
			return new GdlValidateButton(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlCreateButton)){
			return new GdlCreateButton(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlDeleteButton)){
			return new GdlDeleteButton(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlCommitButton)){
			return new GdlCommitButton(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlActionButton)){
			return new GdlActionButton(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlImage)){
			return new GdlImage(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlVideo)){
			return new GdlVideo(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlAudio)){
			return new GdlAudio(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlDatePicker)){
			return new GdlDatePicker(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlTimePicker)){
			return new GdlTimePicker(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlDateTimePicker)){
			return new GdlDateTimePicker(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlUnit)){
			return new GdlUnit(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlText)){
			return new GdlText(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlList)){
			return new GdlList(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlListBox)){
			return new GdlListBox(tmRepresentative, receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlDefaultCreatorTopicView)){
			return new GdlDefaultCreatorTopicView(tmRepresentative, gdlParent, null);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlDefaultEditorTopicView)){
			return new GdlDefaultEditorTopicView(tmRepresentative, (Topic)receivedData, gdlParent, null);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlSpecialCreatorTopicView)){
			return new GdlSpecialCreatorTopicView(tmRepresentative, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlSpecialEditorTopicView)){
			return new GdlSpecialEditorTopicView(tmRepresentative, (Topic)receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlCreatorAssociationview)){
			return new GdlCreatorAssociationView(tmRepresentative, (Topic)receivedData, gdlParent);
		}else if(TmHelper.isInstanceOf(tmRepresentative, PSIs.GDL.TopicType.gdlEditorAssociationView)){
			return new GdlEditorAssociationView(tmRepresentative, (Topic)receivedData, gdlParent);
		}else{
			String values = "";
			for(int i = 0; i != tmRepresentative.getTypes().length(); ++i){
				if(i != tmRepresentative.getTypes().length() - 1){
					values += TmHelper.getAnyIdOfTopic(tmRepresentative.getTypes().get(i)) + ", ";
				}else {
					values += TmHelper.getAnyIdOfTopic(tmRepresentative.getTypes().get(i));	
				}
			}
			throw new InvalidGdlSchemaException("the topic " + TmHelper.getAnyIdOfTopic(tmRepresentative) + " is an instance of an unsupported visible topic type or an abstract topic type: " + values);
		}
	}

	
	// returns a GdlDefaultCreatorTopicView that corresponds to the passed arguments
	public static GdlDefaultCreatorTopicView instantiateDefaultCreatorTopicView(GdlPanel panel) throws InvalidGdlSchemaException, ExecutionException{
		if(panel == null) return null;
		ArrayList<Topic> views = getViewTopics(panel.getReqeustedTopicsToCreate(), TmHelper.getTopicByPsi(PSIs.GDL.TopicType.gdlDefaultCreatorTopicView, panel.getSchemaTm()), panel.getSchemaTm());
		
		if(views.size() != 1){
			String values = "";
			for (Pair<String, TopicIdentifierTypes> pair : panel.getReqeustedTopicsToCreate())
				values += ", " + pair.getFirst();
			if(values.length() >= 2 )values = values.substring(2);
			String bindings = Utils.topicArrayToString(views);
			if(bindings.length() >= 2)bindings = bindings.substring(2);
			else bindings = "[]";
			throw new InvalidGdlSchemaException("the combination of topics requested (" + values + ") must be bound exactly once to a " + PSIs.GDL.TopicType.gdlDefaultCreatorTopicView + " but is bound to " + bindings);
		}
		
		return new GdlDefaultCreatorTopicView(views.get(0), null, panel);
	}
	
	
	// returns the topics that are bound to the corresponding user topics and corresponds to the
	// given view super type. Note only topics that are bound to TM-Single-Type-Value and
	// TM-Multiple-Type-Value are taken into account.
	public static ArrayList<Topic> getViewTopics(ArrayList<Pair<String, TopicIdentifierTypes>> requestedTopics, Topic viewSupertype, TopicMap schemaTm) throws InvalidGdlSchemaException{
		if(requestedTopics == null || requestedTopics.size() == 0 || schemaTm == null) return null;
		
		// request all topics that are passed by the user
		ArrayList<Topic> requestedTops = new ArrayList<Topic>();
		for (Pair<String, TopicIdentifierTypes> topId : requestedTopics){
			Topic top = TmHelper.getTopicByAnyIdentifier(topId, schemaTm);
			if(top == null) throw new InvalidGdlSchemaException("the topic " + topId.getFirst() + " was not found!");
			if(!requestedTops.contains(top)) requestedTops.add(top);
		}

		// get all TM-Values, i.e. TM-Multiple-Type-Value and TM-Single-Type-Value (only if requstedTops.sie() == 1)
		ArrayList<Topic> tmValues = new ArrayList<Topic>();
		Topic tmConstruct = TmHelper.getTopicByPsi(PSIs.GDL.RoleType.gdlTmConstruct, schemaTm);
		Topic tmBinding = TmHelper.getTopicByPsi(PSIs.GDL.AssociationType.gdlTmBinding, schemaTm);
		Topic descriptor = TmHelper.getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, schemaTm);
		Topic tmMultipleTypeValue = TmHelper.getTopicByPsi(PSIs.GDL.TopicType.gdlTmMultipleTypeValue, schemaTm);
		Topic tmSingleTypeValue = TmHelper.getTopicByPsi(PSIs.GDL.TopicType.gdlTmSingleTypeValue, schemaTm);
		for (Topic requestedTopic : requestedTops) {
			ArrayList<Topic> allTmValues = TmHelper.getOtherPlayerOfBinaryAssociation(requestedTopic, tmConstruct, tmBinding, null, descriptor);
			// filter only the valid tm-values for this scenario 
			for (Topic tmValue : allTmValues){
				if(TmHelper.isInstanceOf(tmValue, tmMultipleTypeValue) || (requestedTops.size() == 1 && TmHelper.isInstanceOf(tmValue, tmSingleTypeValue))){
					// check all user topics that are associated with the tmValue, i.e. a tmValue is only valid
					// if exactly those topics are associated to it through a tm-binding association, which are
					// requested by the user
					ArrayList<Topic> allInstanceValues = TmHelper.getOtherPlayerOfBinaryAssociation(tmValue, descriptor, tmBinding, null, tmConstruct);
					if(Utils.compareLists(allInstanceValues, requestedTops) && !tmValues.contains(tmValue)) tmValues.add(tmValue);
				}
			}
		}		

		// get all views that are bound to the found tm-values
		ArrayList<Topic> views = new ArrayList<Topic>();
		Topic tmValueRoleType = TmHelper.getTopicByPsi(PSIs.GDL.RoleType.gdlTmValue, schemaTm);
		Topic topicViewBinding = TmHelper.getTopicByPsi(PSIs.GDL.AssociationType.gdlTopicViewBinding, schemaTm);
		for (Topic tmValue : tmValues) {
			ArrayList<Topic> allViews = TmHelper.getOtherPlayerOfBinaryAssociation(tmValue, tmValueRoleType, topicViewBinding, null, viewSupertype, descriptor);
			for (Topic view : allViews)
				if(!views.contains(view) && TmHelper.isInstanceOf(view, viewSupertype)) views.add(view);
		}
		
		return views;
	}
	
	
	// returns a GdlDefaultEditorTopicView that corresponds to the passed arguments
	public static GdlDefaultEditorTopicView instantiateDefaultEditorTopicView(GdlPanel panel) throws InvalidGdlSchemaException, ExecutionException{
		if(panel == null) return null;
		
		
		// request the topic that is passed by the user
		Topic requestedTop = TmHelper.getTopicByAnyIdentifier(panel.getRequestedTopicToEdit(), panel.getSchemaTm());
		if(requestedTop == null) throw new InvalidGdlSchemaException("the topic " + panel.getRequestedTopicToEdit().getFirst() + " was not found!");
		
		// get all tm-instance-value topics bound to the requested topic
		Topic tmConstruct = TmHelper.getTopicByPsi(PSIs.GDL.RoleType.gdlTmConstruct, panel.getSchemaTm());
		Topic tmBinding = TmHelper.getTopicByPsi(PSIs.GDL.AssociationType.gdlTmBinding, panel.getSchemaTm());
		Topic descriptor = TmHelper.getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, panel.getSchemaTm());
		ArrayList<Topic> tmInstanceValues = TmHelper.getOtherPlayerOfBinaryAssociation(requestedTop, tmConstruct, tmBinding, null, descriptor);
		
		// get all views bound to the found tm-instance-values
		Topic defaultEditorTopicView = TmHelper.getTopicByPsi(PSIs.GDL.TopicType.gdlDefaultEditorTopicView, panel.getSchemaTm());
		Topic tmValue = TmHelper.getTopicByPsi(PSIs.GDL.RoleType.gdlTmValue, panel.getSchemaTm());
		Topic topicViewBinding = TmHelper.getTopicByPsi(PSIs.GDL.AssociationType.gdlTopicViewBinding, panel.getSchemaTm());
		ArrayList<Topic> views = new ArrayList<Topic>();
		for (Topic tmInstanceValue : tmInstanceValues) {
			ArrayList<Topic> tmpViews = TmHelper.getOtherPlayerOfBinaryAssociation(tmInstanceValue, tmValue, topicViewBinding, null, defaultEditorTopicView, descriptor);
			for (Topic tmpView : tmpViews) if(!views.contains(tmpView))views.add(tmpView);
		}
		
		if(views.size() == 1){
			return new GdlDefaultEditorTopicView(views.get(0), requestedTop,  null, panel);
		}else if(views.size() > 1){
			String bindings = Utils.topicArrayToString(views);
			if(bindings.length() >= 2)bindings = bindings.substring(2);
			throw new InvalidGdlSchemaException("the topic " + panel.getRequestedTopicToEdit().getFirst() + " requested for editing must be bound to exaclty one " + PSIs.GDL.TopicType.gdlDefaultEditorTopicView + ", but is bound to " + bindings);
		}else {
			ArrayList<Pair<String, TopicIdentifierTypes>> typesOfRequestedTopic = new ArrayList<Pair<String,TopicIdentifierTypes>>();
			for(int i = 0; i != requestedTop.getTypes().length(); ++i)
				typesOfRequestedTopic.add(TmHelper.getAnyIdenditfierOfTopic(requestedTop.getTypes().get(i)));
						
			views = getViewTopics(typesOfRequestedTopic, defaultEditorTopicView, panel.getSchemaTm());
			
			if(views.size() != 1){
				String values = "";
				for (Pair<String, TopicIdentifierTypes> pair : typesOfRequestedTopic)
					values += ", " + pair.getFirst();
				if(values.length() >= 2)values = values.substring(2);
				String bindings = Utils.topicArrayToString(views);
				if(bindings.length() >= 2)bindings = bindings.substring(2) + "]";
				else bindings = "[ ]";
				throw new InvalidGdlSchemaException("the combination of topic types (" + values + ") for the requested topic " + panel.getRequestedTopicToEdit().getFirst() + " must be bound exactly once to a " + PSIs.GDL.TopicType.gdlDefaultEditorTopicView + " but is bound to " + bindings);
			}
			
			return new GdlDefaultEditorTopicView(views.get(0), requestedTop, null, panel);
		}
	}
}
