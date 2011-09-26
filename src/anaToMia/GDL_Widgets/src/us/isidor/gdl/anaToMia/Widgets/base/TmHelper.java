package us.isidor.gdl.anaToMia.Widgets.base;

import java.util.ArrayList;
import java.util.List;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Association;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Locator;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Name;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Role;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Scoped;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.ScopedStub;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMap;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;
import us.isidor.gdl.anaToMia.Widgets.environment.Pattern;
import us.isidor.gdl.anaToMia.Widgets.environment.TopicIdentifierTypes;
import us.isidor.gdl.anaToMia.Widgets.view.GdlTopicView;
import us.isidor.gdl.anaToMia.Widgets.view.GdlAssociationView.AssociationItem;
import us.isidor.gdl.anaToMia.Widgets.view.GdlView;

import com.google.gwt.core.client.JsArray;


public class TmHelper {	
	// a helper that returns the topic bound to the identifier via a subject identifier
	public static Topic getTopicByPsi(String subjectIdentifier, TopicMap tm){
		if(subjectIdentifier == null || tm == null) return null;
		
		Locator loc = tm.createLocator(subjectIdentifier);
		return tm.getTopicBySubjectIdentifier(loc);
	}
	
	
	// a helper that returns the topic bound to the identifier via a subject locator
	public static Topic getTopicBySl(String subjectLocator, TopicMap tm){
		if(subjectLocator == null || tm == null) return null;
		
		Locator loc = tm.createLocator(subjectLocator);
		return tm.getTopicBySubjectLocator(loc);
	}
	
	
	// a helper that returns the topic bound to the identifier via an item identifier
	public static Topic getTopicByIi(String itemIdentifier, TopicMap tm){
		if(itemIdentifier == null || tm == null) return null;
		
		Locator loc = tm.createLocator(itemIdentifier);
		return (Topic)tm.getConstructByItemIdentifier(loc);
	}
	
	
	// returns a topic that is identified by ref
	public static Topic getTopicByAnyIdentifier(Pair<String, TopicIdentifierTypes> ref, TopicMap tm){
		if(ref.getSecond() == TopicIdentifierTypes.ItemIdentifier){
			return getTopicByIi(ref.getFirst(), tm);
		}else if(ref.getSecond() == TopicIdentifierTypes.SubjectLocator){
			return getTopicBySl(ref.getFirst(), tm);
		}else {
			return getTopicByPsi(ref.getFirst(), tm);
		}
	}	
	
	
	// a helper method to return any identifier string of the topic
	public static String getAnyIdOfTopic(Topic topic){
		if(topic == null) return "";
		if(topic.getSubjectIdentifiers().length() != 0){
			return topic.getSubjectIdentifiers().get(0).getReference();
		}else if(topic.getSubjectLocators().length() != 0) {
			return topic.getSubjectLocators().get(0).getReference();
		}if(topic.getItemIdentifiers().length() != 0){
			return topic.getItemIdentifiers().get(0).getReference();
		} else {
			return null;
		}
	}
	
	
	// a helper method to return an identifier string and a TopicIdentifierTypes instance
	// of the topic contained in a Pair object
	public static Pair<String, TopicIdentifierTypes> getAnyIdenditfierOfTopic(Topic topic){
		if(topic.getSubjectIdentifiers().length() != 0){
			return new Pair<String, TopicIdentifierTypes>(topic.getSubjectIdentifiers().get(0).getReference(), TopicIdentifierTypes.SubjectIdentifier);
		}else if(topic.getSubjectLocators().length() != 0) {
			return new Pair<String, TopicIdentifierTypes>(topic.getSubjectLocators().get(0).getReference(), TopicIdentifierTypes.SubjectLocator);
		}if(topic.getItemIdentifiers().length() != 0){
			return new Pair<String, TopicIdentifierTypes>(topic.getItemIdentifiers().get(0).getReference(), TopicIdentifierTypes.ItemIdentifier);
		} else {
			return null;
		}
	}
	
	
	// returns all instances of the topic type
	public static ArrayList<Topic> getInstanceOf(Topic type, boolean multipleTypesAllowed){
		ArrayList<Topic> result = new ArrayList<Topic>();
		TopicMap tm = type.getTopicMap();
		
		for(int i = 0; i != tm.getTopics().length(); ++i){
			if(multipleTypesAllowed){
				if(isInstanceOf(tm.getTopics().get(i), type)) result.add(tm.getTopics().get(i));
			} else {
				if(isInstanceOf(tm.getTopics().get(i), type) && tm.getTopics().get(i).getTypes().length() == 1) result.add(tm.getTopics().get(i));
			}
		}
		
		return result;
	}
	
	
	// returns true if the topic instance if an instance of the topic type
	public static boolean isInstanceOf(Topic instance, Topic type){
		if(instance == null || (instance.getTypes().length() != 0 && type == null)) return false;
		if(instance.getTypes().length() == 0 && type == null) return false; // type must be always set!

		JsArray<Topic> types = instance.getTypes();
		for(int i = 0; i != types.length(); ++i){
			ArrayList<Topic> superTypes = getAllSuperTypes(types.get(i), null);
			superTypes.add(types.get(i));	
			if(superTypes.contains(type)) return true;
		}		
		return false;
	}
	
	
	// returns an array list with all direct and indirect supertypes of top
	public static ArrayList<Topic> getAllSuperTypes(Topic top, ArrayList<Topic> collectedSupertypes){
		if(top == null) return new ArrayList<Topic>();
		
		ArrayList<Topic> localCollectedSuperTypes = new ArrayList<Topic>();
		if(collectedSupertypes != null) localCollectedSuperTypes = collectedSupertypes;
			
		ArrayList<Topic> direcSupertypes = getDirectSuperTypes(top);
		for (Topic topic : localCollectedSuperTypes) direcSupertypes.remove(topic); //avoid duplicates
		for (Topic topic : direcSupertypes)localCollectedSuperTypes.add(topic);
		
		ArrayList<Topic> collectedIndirectSupertypes = new ArrayList<Topic>();
		for (Topic directSuperType : direcSupertypes) {
			ArrayList<Topic> indirectSuperTypes = getAllSuperTypes(directSuperType, localCollectedSuperTypes);
			for (Topic indirectSupertype : indirectSuperTypes)
				if(!collectedIndirectSupertypes.contains(indirectSupertype)) collectedIndirectSupertypes.add(indirectSupertype);
		}
		
		for (Topic collectedIndirectSupertype : collectedIndirectSupertypes)
			if(!localCollectedSuperTypes.contains(collectedIndirectSupertype)) localCollectedSuperTypes.add(collectedIndirectSupertype);
				
		return localCollectedSuperTypes;
	}
	
	
	// returns true if the passed topic subtype is a subtype of the passed
	// topic supertype
	public static boolean isSupertypeOf(Topic subtype, Topic supertype){
		return getAllSuperTypes(subtype, null).contains(supertype);
	}
	
	
	// returns true if the passed topic subtype is a subtype of the passed
	// topic supertype, that is represented by the supertype psi
	public static boolean isSupertypeOf(Topic subtype, String supertype){
		if(subtype == null) return false;
		return isSupertypeOf(subtype, getTopicByPsi(supertype, subtype.getTopicMap()));
	}
	
	
	// returns the direct supertypes of the passed topic
	public static ArrayList<Topic> getDirectSuperTypes(Topic top){
		ArrayList<Topic> result = new ArrayList<Topic>();
		if(top == null) return result;
		TopicMap tm = top.getTopicMap();
		Topic subtype = getTopicByPsi(PSIs.TMDM.subtype, tm);
		Topic supertype = getTopicByPsi(PSIs.TMDM.supertype, tm);
		Topic supertypeSubtype = getTopicByPsi(PSIs.TMDM.supertypeSubtype, tm);
		
		if(subtype == null || supertype == null || supertypeSubtype == null) return result;
		
		JsArray<Role> validRoles = top.getRolesPlayed(subtype, supertypeSubtype);
		for(int i = 0; i != validRoles.length(); ++i){
			Association parent = validRoles.get(i).getParent();
			if(parent.getRoles().length() == 2 && parent.getRoles(supertype).length() == 1)result.add(parent.getRoles(supertype).get(0).getPlayer());
		}
		return result;
	}
	
	
	// returns true if a corresponding association exists
	public static boolean hasBinaryAssociation(Topic thisTopic, Topic thisRoleType, Topic assocType, Topic otherPlayer, Topic otherRoleType){
		if(thisTopic == null || thisRoleType == null || assocType == null || otherPlayer == null || otherRoleType == null) return false;
		
		JsArray<Role> roles = thisTopic.getRolesPlayed(thisRoleType, assocType);
		for(int i = 0; i != roles.length(); ++i){
			Association parent = roles.get(i).getParent();
			JsArray<Role> otherRoles = parent.getRoles(otherRoleType);
			for(int j = 0; j != otherRoles.length(); ++j)
				if(otherRoles.get(j).getPlayer().equals(otherPlayer)) return true;
		}
		
		return false;
	}
	
	
	// returns the other player of an association with two roles and the correct values
	public static ArrayList<Topic> getOtherPlayerOfBinaryAssociation(Topic thisTopic, Topic thisRoleType, Topic assocType, ArrayList<Topic> scope, Topic otherRoleType){
		ArrayList<Topic> result = new ArrayList<Topic>();
		if(thisTopic == null || thisRoleType == null || assocType == null || otherRoleType == null) return result;
		
		JsArray<Role> roles = thisTopic.getRolesPlayed(thisRoleType, assocType);
		for(int i = 0; i != roles.length(); ++i){
			Association parent = roles.get(i).getParent();
			if(parent.getRoles().length() != 2) return result;
			if(thisRoleType.equals(otherRoleType) && parent.getRoles(thisTopic).length() == 2){
				if(parent.getRoles(thisRoleType).get(0).getPlayer().equals(thisTopic)) result.add(parent.getRoles(thisRoleType).get(1).getPlayer()); 
				else result.add(parent.getRoles(thisRoleType).get(0).getPlayer()); 
			}else if(parent.getRoles(otherRoleType).length() == 1){
				result.add(parent.getRoles(otherRoleType).get(0).getPlayer());
			}
		}
		return result;
	}
	
	
	// returns the other player of an association with two roles and the correct values
	public static ArrayList<Topic> getOtherPlayerOfBinaryAssociation(Topic thisTopic, Topic thisRoleType, Topic assocType, ArrayList<Topic> scope, Topic otherPlayerType, Topic otherRoleType){
		ArrayList<Topic> allPlayers = getOtherPlayerOfBinaryAssociation(thisTopic, thisRoleType, assocType, scope, otherRoleType);
		if(otherRoleType == null) return allPlayers;
		
		ArrayList<Topic> result = new ArrayList<Topic>();
		for (Topic player : allPlayers) if(isInstanceOf(player, otherPlayerType)) result.add(player);
		
		return result;
	}

	
	// returns true if the instance topic is an instance of the topic bound to typeSubectIdentifier
	public static boolean isInstanceOf(Topic instance, String typeSubjectIdentifier){
		if(instance == null || typeSubjectIdentifier == null) return false;
		TopicMap tm = instance.getTopicMap();
		Topic type = tm.getTopicBySubjectIdentifier(tm.createLocator(typeSubjectIdentifier));
		return isInstanceOf(instance, type);
	}
	
	
	// returns true if the passed association has exactly the corresponding roles to
	// the array list rolePlayersAndTypes
	public static boolean hasRoles(Association assoc, ArrayList<Pair<Topic, Topic>> rolePlayertypesAndTypes){
		if(assoc == null) return false;
		if(rolePlayertypesAndTypes == null) return assoc.getRoles().length() == 0;
		if(assoc.getRoles().length() != rolePlayertypesAndTypes.size()) return false;
		
		for (Pair<Topic, Topic> pair : rolePlayertypesAndTypes) {
			JsArray<Role> selection = assoc.getRoles(pair.getSecond());
			int i = 0;
			for(; i != selection.length(); ++i)
				if(pair.getFirst() == null || isInstanceOf(selection.get(i).getPlayer(), pair.getFirst())) break;
			
			if(i == selection.length()) return false;
		}
		
		// check the reverse way => duplicates
		JsArray<Role> roles = assoc.getRoles();
		for(int i = 0; i != roles.length(); ++i){
			Role currentRole = roles.get(i);
			int j = 0;
			for(; j != rolePlayertypesAndTypes.size(); ++j)
				if(currentRole.getType().equals(rolePlayertypesAndTypes.get(j).getSecond()) && (isInstanceOf(currentRole.getPlayer(), rolePlayertypesAndTypes.get(j).getFirst()) || rolePlayertypesAndTypes.get(j).getFirst() == null)) break;
			
			if(j == rolePlayertypesAndTypes.size()) return false;
		}
		
		return true;
	}
	
	
	// returns true if the topics of themes are all equal to the scope of construct
	public static boolean hasThemes(Scoped construct, ArrayList<Topic> themes){
		if(construct == null) return false;
		if(themes == null) return ((ScopedStub)construct).getScope().length() == 0;
		
		ScopedStub scoped = (ScopedStub)construct;
		for(int i = 0; i != scoped.getScope().length(); ++i){
			if(!themes.contains(scoped.getScope().get(i))) return false;
		}
		
		return true;
	}
	
	
	// returns the associations that are bound to the topic "topic" and have the passed scope and roles
	public static ArrayList<Association> getAssociationsOfTopic(Topic topic, Topic roleType, Topic assocType, ArrayList<Topic> scope, ArrayList<Pair<Topic, Topic>> rolePlayertypesAndTypes){
		ArrayList<Association> result = new ArrayList<Association>();
		if(topic == null || assocType == null || rolePlayertypesAndTypes == null) return result;
		
		ArrayList<Association> allPotentialAssocs = new ArrayList<Association>();
		for(int i = 0; i != topic.getRolesPlayed(roleType, assocType).length(); ++i) allPotentialAssocs.add(topic.getRolesPlayed(roleType, assocType).get(i).getParent());
		
		ArrayList<Association> assocsWoScope = new ArrayList<Association>();
		@SuppressWarnings("unchecked")
		ArrayList<Pair<Topic, Topic>> localRolePlayertypesAndTypes = (ArrayList<Pair<Topic, Topic>>)rolePlayertypesAndTypes.clone();
		Topic typeOfTopic = topic.getTypes().length() != 0 ? topic.getTypes().get(0) : null;
		localRolePlayertypesAndTypes.add(new Pair<Topic, Topic>(typeOfTopic, roleType));
			
		for (Association assoc : allPotentialAssocs)
			if(hasRoles(assoc, localRolePlayertypesAndTypes)) assocsWoScope.add(assoc);
		
		for (Association assoc : assocsWoScope) if(hasThemes(assoc, scope)) result.add(assoc);
		
		return result;
	}


	// returns all topics that are related to the passed topic via a contains association
	public static ArrayList<Topic> topicContains(Topic container) {
		if(container == null) return new ArrayList<Topic>();	
		TopicMap tm = container.getTopicMap();
		Topic containsAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlContains, tm);
		Topic containerRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlContainer, tm);
		Topic visibleObject = getTopicByPsi(PSIs.GDL.TopicType.gdlVisibleObject, tm);
		Topic containeeRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlContainee, tm);

		return getOtherPlayerOfBinaryAssociation(container, containerRoleType, containsAssocType, null, visibleObject, containeeRoleType);
	}
	
	
	// returns all instances of gdl:Info that are related to the passed topic via a contains association
	public static ArrayList<Topic> topicContainsInfo(Topic container) {
		if(container == null) return new ArrayList<Topic>();	
		TopicMap tm = container.getTopicMap();
		Topic containsAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlContains, tm);
		Topic containerRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlContainer, tm);
		Topic info = getTopicByPsi(PSIs.GDL.TopicType.gdlInfo, tm);
		Topic containeeRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlContainee, tm);
		
		return getOtherPlayerOfBinaryAssociation(container, containerRoleType, containsAssocType, null, info, containeeRoleType);
	}
	
	
	// Returns all pairs of topics and string that are of type gdl:Action-Button and are
	// bound to the passed topic via a gdl:contains and via a gdl:button-position association.
	// The topic is the gdl:Action-Button instance, whereas the string is the value of the
	// instance of gdl:Nth-Element.
	// The integer contains the index specified by the nth-value occurrence, a -1 indicates
	// the value last.
	public static ArrayList<Pair<Topic, Integer>> topicContainsNthButtons(Topic container) throws InvalidGdlSchemaException{
		ArrayList<Pair<Topic, Integer>> result = new ArrayList<Pair<Topic,Integer>>();
		if(container == null) return result;
		
		TopicMap tm = container.getTopicMap();
		Topic containsAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlContains, tm);
		Topic containerRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlContainer, tm);
		Topic actionButtonType = getTopicByPsi(PSIs.GDL.TopicType.gdlActionButton, tm);
		Topic containeeRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlContainee, tm);
		ArrayList<Topic> actionButtons = getOtherPlayerOfBinaryAssociation(container, containerRoleType, containsAssocType, null, actionButtonType, containeeRoleType);
		
		Topic actionButtonRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlActionButton, tm);
		Topic buttonPosition = getTopicByPsi(PSIs.GDL.AssociationType.gdlButtonPosition, tm);
		Topic nthElementType = getTopicByPsi(PSIs.GDL.TopicType.gdlNthElement, tm);
		Topic nthElementRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlNthElement, tm);
		Topic nthValueOccType = getTopicByPsi(PSIs.GDL.OccurrenceType.gdlNthValue, tm);
		ArrayList<Pair<Topic, Topic>> rolePlayertypesAndTypes = new ArrayList<Pair<Topic,Topic>>();
		rolePlayertypesAndTypes.add(new Pair<Topic, Topic>(nthElementType, nthElementRoleType));
		for (Topic actionButton : actionButtons) {
			ArrayList<Association> assocs = getAssociationsOfTopic(actionButton, actionButtonRoleType, buttonPosition, null, rolePlayertypesAndTypes);
			if(assocs.size() == 0) continue;
			if(assocs.size() > 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(actionButton) + " must be bound none or once to a " + PSIs.GDL.AssociationType.gdlButtonPosition + " association, but is: " + assocs.size());
			if(assocs.size() == 1) {
				Topic nthElem = assocs.get(0).getRoles(nthElementRoleType).get(0).getPlayer();
				JsArray<Occurrence> occs = nthElem.getOccurrences(nthValueOccType);
				if(occs.length() != 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(nthElem) + " must be bound exactly once to a " + PSIs.GDL.OccurrenceType.gdlNthValue + " occurrence, but is: " + occs.length());
				int val = 0;
				try{
					if(occs.get(0).getValue().toLowerCase().equals("last")){
						val = -1;
					} else {
						Integer.valueOf(occs.get(0).getValue());
					}
				}catch(NumberFormatException e){
					throw new InvalidGdlSchemaException("The topic " + getAnyIdOfTopic(nthElem) + " must contain a value of the form <positive-integer>|\"last\" in the occurrence " + PSIs.GDL.OccurrenceType.gdlNthValue + ", but it contains: " + occs.get(0).getValue());
				}
				
				result.add(new Pair<Topic, Integer>(actionButton, val));
			}
		}
		
		return result;
	}
	
	
	// returns an ordered list of the list items and their index stored in a pair instance
	public static ArrayList<Pair<Topic, Integer>> listContains(Topic list) throws InvalidGdlSchemaException {
		if(list == null) return new ArrayList<Pair<Topic,Integer>>();
		TopicMap tm = list.getTopicMap();
		Topic containsAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlContains, tm);
		Topic containerRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlContainer, tm);
		Topic visibleObject = getTopicByPsi(PSIs.GDL.TopicType.gdlVisibleObject, tm);
		Topic containeeRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlContainee, tm);
		Topic nthElement = getTopicByPsi(PSIs.GDL.TopicType.gdlNthElement, tm);
		Topic nthElementRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlNthElement, tm);
		
		ArrayList<Pair<Topic, Topic>> rolePlayertypesAndTypes = new ArrayList<Pair<Topic,Topic>>();
		rolePlayertypesAndTypes.add(new Pair<Topic, Topic>(visibleObject, containeeRoleType));
		rolePlayertypesAndTypes.add(new Pair<Topic, Topic>(nthElement, nthElementRoleType));
		ArrayList<Association> validAssociations = getAssociationsOfTopic(list, containerRoleType, containsAssocType, null, rolePlayertypesAndTypes);
		
		List<Pair<Topic, Integer>> result = new ArrayList<Pair<Topic,Integer>>();
		for (Association validAssoc : validAssociations) {
			Topic idx = validAssoc.getRoles(nthElementRoleType).get(0).getPlayer();
			JsArray<Occurrence> idxOccs = idx.getOccurrences(tm.getTopicBySubjectIdentifier(tm.createLocator(PSIs.GDL.OccurrenceType.gdlNthValue)));
			ArrayList<Occurrence> validIdxOccs = new ArrayList<Occurrence>();
			for(int i = 0; i != idxOccs.length(); ++i) if(idxOccs.get(i).getScope().length() == 0) validIdxOccs.add(idxOccs.get(i));
			
			if(validIdxOccs.size() != 1){
				throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(idx) + " must be bound exaclty once to an occurrence of the type " + PSIs.GDL.OccurrenceType.gdlNthValue + " but is bound " + validIdxOccs.size() + " times");
			}else{
				Occurrence validOcc = validIdxOccs.get(0);
				if(!validOcc.getValue().toUpperCase().equals("LAST") && !Utils.isDecNumber(validOcc.getValue())){
					throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(idx) + " must be bound exaclty once to an occurrence of the type " + PSIs.GDL.OccurrenceType.gdlNthValue + " with a the value \"last\" or <positive integer>, but is " + validOcc.getValue());
				}
			
				int idxValue = -1; // -1 represents last
				if(!validOcc.getValue().toUpperCase().equals("LAST")) idxValue = Integer.valueOf(validOcc.getValue());
				Topic item = validAssoc.getRoles(containeeRoleType).get(0).getPlayer();	
				
				if(result.size() == 0){
					result.add(new Pair<Topic, Integer>(item, idxValue));
				}else {
					for(int i = 0; i != result.size(); ++i){
						if (result.get(i).getSecond() == idxValue){
							String idxStr = idxValue == -1 ? "last" : "" + idxValue;
							throw new InvalidGdlSchemaException("the list " + getAnyIdOfTopic(list) + " contains more than one item with the idx " + idxStr);
						}
						
						if(result.get(i).getSecond() > idxValue){
							result.add(i, new Pair<Topic, Integer>(item, idxValue));
							break;
						}
						
						if(i == result.size() - 1){
							result.add(new Pair<Topic, Integer>(item, idxValue));
							break;
						}
					}
				}
			}
		}
		
		ArrayList<Pair<Topic, Integer>> orderedResult = new ArrayList<Pair<Topic,Integer>>(result);
		return orderedResult;
	}


	// returns the topic that represents the first item of a container, that means this item is
	// bound to the container via a position association and plays the role descendant, whereas
	// the container plays the role ancestor
	public static Topic getFirstContainee(Topic container, ArrayList<Topic> containees) throws InvalidGdlSchemaException {
		if(containees == null || containees.size() == 0 || container == null) return null;
		
		TopicMap tm = containees.get(0).getTopicMap();
		Topic descendantRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlDescendant, tm);
		Topic positionAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlPosition, tm);
		Topic position = getTopicByPsi(PSIs.GDL.TopicType.gdlPosition, tm);
		Topic visibleObject = getTopicByPsi(PSIs.GDL.TopicType.gdlVisibleObject, tm);
		Topic ancestorRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlAncestor, tm);
		Topic descriptorRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, tm);
		
		ArrayList<Pair<Topic, Topic>> rolePlayertypesAndTypes = new ArrayList<Pair<Topic,Topic>>();
		rolePlayertypesAndTypes.add(new Pair<Topic, Topic>(visibleObject, descendantRoleType));
		rolePlayertypesAndTypes.add(new Pair<Topic, Topic>(position, descriptorRoleType));
		ArrayList<Association> descendantAssocsOfContainer = TmHelper.getAssociationsOfTopic(container, ancestorRoleType, positionAssocType, null, rolePlayertypesAndTypes);
				
		ArrayList<Topic> validDescendants = new ArrayList<Topic>();
		for (Association descendantAssociation : descendantAssocsOfContainer) {
			JsArray<Role> descendantRoles = descendantAssociation.getRoles(descendantRoleType);
			for(int i = 0; i != descendantRoles.length(); ++i){
				Topic player =  descendantRoles.get(i).getPlayer();
				if(isInstanceOf(player, visibleObject) && !validDescendants.contains(player) && containees.contains(player)) validDescendants.add(player);
			}
		}
		
		if(validDescendants.size() != 1){
			String top = getAnyIdOfTopic(container);
			String bindings = "";
			for (Topic topic : validDescendants) bindings += ", " + getAnyIdOfTopic(topic);
			if(bindings.length() == 0) bindings = "[]";
			else bindings = bindings.substring(2);
			throw new InvalidGdlSchemaException("the topic " + top + " must be bound to exactly one descendant element, but found " + bindings);
		}else {
			return validDescendants.get(0);
		}
	}
	
	
	// returns the descendant of the current topic related via an contains association
	public static Topic getNextContainee(Topic current, ArrayList<Topic> containees) throws InvalidGdlSchemaException{
		if (current == null || containees == null || containees.size() == 0) return null;
		
		TopicMap tm = current.getTopicMap();
		Topic descendantRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlDescendant, tm);
		Topic positionAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlPosition, tm);
		Topic visibleObject = getTopicByPsi(PSIs.GDL.TopicType.gdlVisibleObject, tm);
		Topic ancestorRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlAncestor, tm);
		Topic position = getTopicByPsi(PSIs.GDL.TopicType.gdlPosition, tm);
		Topic descriptorRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, tm);
		ArrayList<Pair<Topic, Topic>> rolePlayertypesAndTypes = new ArrayList<Pair<Topic,Topic>>();
		rolePlayertypesAndTypes.add(new Pair<Topic, Topic>(visibleObject, descendantRoleType));
		rolePlayertypesAndTypes.add(new Pair<Topic, Topic>(position, descriptorRoleType));
		ArrayList<Association> positionAssocs = getAssociationsOfTopic(current, ancestorRoleType, positionAssocType, null, rolePlayertypesAndTypes);
		
		ArrayList<Topic> descendantsContainedInContainees = new ArrayList<Topic>();
		for (Association assoc : positionAssocs) {
			Role role = assoc.getRoles(descendantRoleType).get(0); // it is ensured that exaclty one role is existent, due to getAssociationsOfTopic
			if(containees.contains(role.getPlayer()) && !descendantsContainedInContainees.contains(role.getPlayer())) descendantsContainedInContainees.add(role.getPlayer());
		}
				
		if(descendantsContainedInContainees.size() > 1){
			String top = getAnyIdOfTopic(current);
			String bindings = "";
			for (Topic topic : descendantsContainedInContainees) bindings += ", " + getAnyIdOfTopic(topic);
			if(bindings.length() >= 2) bindings = bindings.substring(2);
			throw new InvalidGdlSchemaException("the topic " + top + " must be bound to none or one descendant topics, but is bound to " + bindings);
		}else if(descendantsContainedInContainees.size() == 1){
			return descendantsContainedInContainees.get(0);
		}else{
			return null;
		}
	}
	
	
	// returns the topic that represents the position between the ancestor
	// and descendant topics
	public static Topic getPositionOf(Topic ancestor, Topic descendant) throws InvalidGdlSchemaException {
		if(ancestor == null || descendant == null) return null;
		
		// get all potential valid associations that models a position association
		TopicMap tm = ancestor.getTopicMap();
		Topic ancestorRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlAncestor, tm);
		Topic positionAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlPosition, tm);
		ArrayList<Pair<Topic, Topic>> rolePlayertypesAndTypes = new ArrayList<Pair<Topic,Topic>>();
		Topic descriptor = getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, tm);
		Topic descendantRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlDescendant, tm);
		Topic visibleObject = getTopicByPsi(PSIs.GDL.TopicType.gdlVisibleObject, tm);
		Topic position = getTopicByPsi(PSIs.GDL.TopicType.gdlPosition, tm);
		rolePlayertypesAndTypes.add(new Pair<Topic, Topic>(position, descriptor));
		rolePlayertypesAndTypes.add(new Pair<Topic, Topic>(visibleObject, descendantRoleType));
		ArrayList<Association> possiblePosAssocs = getAssociationsOfTopic(ancestor, ancestorRoleType, positionAssocType, null, rolePlayertypesAndTypes);
		ArrayList<Association> posAssocs = new ArrayList<Association>();
		for (Association candidate : possiblePosAssocs) {
			JsArray<Role> descendantRole = candidate.getRoles(descendantRoleType);
			if(descendantRole.length() == 1 && descendantRole.get(0).getPlayer().equals(descendant)) posAssocs.add(candidate);
		}
		
		if(posAssocs.size() != 1){
			String top1 = getAnyIdOfTopic(ancestor);
			String top2 = getAnyIdOfTopic(descendant);
			String bindings = "";
			for (Association assoc : posAssocs)
				if(assoc.getRoles(descriptor).length() != 0)
					for(int i = 0; i != assoc.getRoles(descriptor).length(); ++i)
						bindings += ", " + getAnyIdOfTopic(assoc.getRoles(descriptor).get(i).getPlayer());
			if(bindings.length() == 0) bindings = "[]";
			else bindings = bindings.substring(2);
			throw new InvalidGdlSchemaException("the topics " + top1 + " and " + top2 + " must be bound to exactly one position topic, but is bound to " + bindings);
		}else {
			ArrayList<Topic> positions = new ArrayList<Topic>();
			for (Association assoc : posAssocs) {
				JsArray<Role> validRoles = assoc.getRoles(descriptor);
				for(int i = 0; i != validRoles.length(); ++i)
					if(isInstanceOf(validRoles.get(i).getPlayer(), position) &&	!positions.contains(validRoles.get(i).getPlayer())) positions.add(validRoles.get(i).getPlayer());
				
			}
			
			if(positions.size() != 1){
				String top1 = getAnyIdOfTopic(ancestor);
				String top2 = getAnyIdOfTopic(descendant);
				String bindings = "";
				for (Topic pos : positions) bindings += ", " + getAnyIdOfTopic(pos);
				if(bindings.length() == 0) bindings = "[]";
				else bindings = bindings.substring(2);
				throw new InvalidGdlSchemaException("the topics " + top1 + " and " + top2 + " must be bound to exaclty one position topic, but is bound to " + bindings);				
			}else {
				return positions.get(0);
			}
		}
	}

	
	// returns binary associations that corresponds to the given parameters
	public static ArrayList<Association> getBinaryAssociations(Topic topic, Topic roleType, Topic assocType, ArrayList<Topic> scope, Topic otherPlayerType, Topic otherRoleType){
		ArrayList<Pair<Topic, Topic>> rolePlayertypesAndTypes = new ArrayList<Pair<Topic,Topic>>();
		rolePlayertypesAndTypes.add(new Pair<Topic, Topic>(otherPlayerType, otherRoleType));
		return getAssociationsOfTopic(topic, roleType, assocType, scope, rolePlayertypesAndTypes);	
	}

	
	// returns the hidden values that are bound to the representative
	// of a GdlView instance
	public static ArrayList<Topic> getHiddenValueOf(Topic viewTopic) {
		ArrayList<Topic> result = new ArrayList<Topic>();
		if(viewTopic == null) return result;
		
		TopicMap tm = viewTopic.getTopicMap();
		Topic containerRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlContainer, tm);
		Topic containeeRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlContainee, tm);
		Topic containsAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlContains, tm);
		Topic hiddenValueType = getTopicByPsi(PSIs.GDL.TopicType.gdlHiddenValue, tm);
		return getOtherPlayerOfBinaryAssociation(viewTopic, containerRoleType, containsAssocType, null, hiddenValueType, containeeRoleType);
	}
	
	
	// returns the first constraint of the passed hidden value topic
	public static Topic getConstraintOfHiddenValue(Topic hiddenValue) throws InvalidGdlSchemaException {
		if(hiddenValue == null) return null;
		
		TopicMap tm = hiddenValue.getTopicMap();
		Topic hiddenValueRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlHiddenValue, tm);
		Topic descriptorType = getTopicByPsi(PSIs.GDL.TopicType.gdlDescriptor, tm);
		Topic tmBindingAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlTmBinding, tm);
		Topic tmclConstraintType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm);
		Topic tmConstructRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlTmConstruct, tm);
		ArrayList<Topic> tmclConstraints = getOtherPlayerOfBinaryAssociation(hiddenValue, hiddenValueRoleType, tmBindingAssocType, null, tmclConstraintType, tmConstructRoleType);
		
		ArrayList<Topic> gdlConstraints = getOtherPlayerOfBinaryAssociation(hiddenValue, hiddenValueRoleType, tmBindingAssocType, null, descriptorType, tmConstructRoleType);
		
		if(tmclConstraints.size() > 1 || gdlConstraints.size() > 1 || (tmclConstraints.size() == 1 && gdlConstraints.size() == 1))
			throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(hiddenValue) + " can be bound only to one constraint, but is " + (tmclConstraints.size() + gdlConstraints.size()));
		
		if(tmclConstraints.size() == 1) return tmclConstraints.get(0);
		else if (gdlConstraints.size() == 1) return gdlConstraints.get(0);
		else return null;
	}
	
	
	// returns the last/root constraint that is bound to the passed
	// hidden value topic
	public static Topic getRootConstraintOfHiddenValue(Topic hiddenValue, Topic currentConstraint) throws InvalidGdlSchemaException {
		if(hiddenValue == null) return null;
		
		Topic localCurrentConstraint = currentConstraint == null ? getConstraintOfHiddenValue(hiddenValue) : currentConstraint;
		TopicMap tm = hiddenValue.getTopicMap();
		Topic tmclConstraintType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm); 
		
		if(isInstanceOf(localCurrentConstraint, tmclConstraintType)) return localCurrentConstraint;
		else {
			// get next constraint and invoke this method recursively
			Topic descriptorRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, tm);
			Topic tmBindingAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlTmBinding, tm);
			Topic tmConstructRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlTmConstruct, tm);
			Topic gdlDescriptor = getTopicByPsi(PSIs.GDL.TopicType.gdlDescriptor, tm);
			
			ArrayList<Topic> nextTmclConstraints = getOtherPlayerOfBinaryAssociation(localCurrentConstraint, descriptorRoleType, tmBindingAssocType, null, tmclConstraintType, tmConstructRoleType);
			ArrayList<Topic> nextGdlConstraints = getOtherPlayerOfBinaryAssociation(localCurrentConstraint, descriptorRoleType, tmBindingAssocType, null, gdlDescriptor, tmConstructRoleType);
			
			if(nextTmclConstraints.size() > 1 || nextGdlConstraints.size() > 1 || (nextTmclConstraints.size() == 1 && nextGdlConstraints.size() == 1))
				throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(localCurrentConstraint) + " can be bound only to one constraint, but is " + (nextTmclConstraints.size() + nextGdlConstraints.size()));
			
			if(nextTmclConstraints.size() == 1) return nextTmclConstraints.get(0);
			else if(nextGdlConstraints.size() == 1) return getRootConstraintOfValueGroup(hiddenValue, nextGdlConstraints.get(0));
			else throw new InvalidGdlSchemaException("the hidden value " + getAnyIdOfTopic(hiddenValue) + " must be bound to exaclty one root constraint of the type " + PSIs.TMCL.tmclConstraint + " but is unbound");
		}
	}
	
	
	// returns the default value of a hidden value, i.e. the actual value representative
	public static Topic getDefaultTmValueOfHiddenValue(Topic hiddenValue) throws InvalidGdlSchemaException {
		if(hiddenValue == null) return null;
		
		TopicMap tm = hiddenValue.getTopicMap();
		Topic valueBindingAssocTopic = getTopicByPsi(PSIs.GDL.AssociationType.gdlValueBinding, tm);
		Topic hiddenValueRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlHiddenValue, tm);
		Topic valueRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlValue, tm);
		Topic defaultTmValueType = getTopicByPsi(PSIs.GDL.TopicType.gdlDefaultTmValue, tm);
		
		ArrayList<Topic> defaultTmValues = getOtherPlayerOfBinaryAssociation(hiddenValue, hiddenValueRoleType, valueBindingAssocTopic, null, defaultTmValueType, valueRoleType);
		
		if(defaultTmValues.size() == 1) return defaultTmValues.get(0);
		else if(defaultTmValues.size() == 0) return null;
		else throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(hiddenValue) + " must be bound none or once to a " + PSIs.GDL.TopicType.gdlDefaultTmValue + ", but is: " + defaultTmValues.size());
	}
	
	
	// returns the default literal value representative of the hidden value
	public static Topic getDefaultLiteralValueOfHiddenValue(Topic hiddenValue) throws InvalidGdlSchemaException {
		if(hiddenValue == null) return null;
		
		TopicMap tm = hiddenValue.getTopicMap();
		Topic valueBindingAssocTopic = getTopicByPsi(PSIs.GDL.AssociationType.gdlValueBinding, tm);
		Topic hiddenValueRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlHiddenValue, tm);
		Topic valueRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlValue, tm);
		Topic defaultLiteralValueType = getTopicByPsi(PSIs.GDL.TopicType.gdlDefaultLiteralValue, tm);		
		ArrayList<Topic> defaultTmValues = getOtherPlayerOfBinaryAssociation(hiddenValue, hiddenValueRoleType, valueBindingAssocTopic, null, defaultLiteralValueType, valueRoleType);

		if(defaultTmValues.size() == 1) return defaultTmValues.get(0);
		else if(defaultTmValues.size() == 0) return null;
		else throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(hiddenValue) + " must be bound none or once to a " + PSIs.GDL.TopicType.gdlDefaultLiteralValue + ", but is: " + defaultTmValues.size());
	}

	
	// returns a topic that is represented by the nearest instance of GdlTopicView
	public static Topic getNearestTopic(GdlVisibleObject elem) throws InvalidGdlSchemaException{
		if(elem == null) return null;
		
		GdlTopicView view = null;
		GdlVisibleObject parent = elem;
		
		do{
			if(parent instanceof GdlTopicView) view = (GdlTopicView)parent;
			else parent = parent.getGdlParent();
		}while(view == null && parent != null);
		
		if(view == null) return null;
		else return view.getRepresentedTopic();
	}
	
	
	// returns a topic or associations that is represented by the nearest instance of GdlView
	public static Construct getNearestTopicOrAssociation(GdlVisibleObject elem) throws InvalidGdlSchemaException{
		if(elem == null) return null;
		
		GdlView view = null;
		GdlVisibleObject parent = elem;
		
		do{
			if(parent instanceof GdlView) view = (GdlView)parent;
			else parent = parent.getGdlParent();
		}while(view == null && parent != null);
		
		if(view == null) return null;
		else if(view instanceof GdlTopicView) return ((GdlTopicView)view).getRepresentedTopic();
		else if(view instanceof AssociationItem) return ((AssociationItem)view).getRepresentedAssociation();
		else return null;
	}
	
	
	// returns the topic that represents the value group that is bound to the passed
	// topic via a gdl:view-binding association
	public static Topic getValueGroupOf(Topic visibleElement) throws InvalidGdlSchemaException{
		if(visibleElement == null) throw null;
		
		TopicMap tm = visibleElement.getTopicMap();
		Topic descriptorRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, tm);
		Topic viewBindingAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlViewBinding, tm);
		Topic valueGroupType = getTopicByPsi(PSIs.GDL.TopicType.gdlValueGroup, tm);
		Topic valueGroupRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlValueGroup, tm);
		ArrayList<Topic> valueGroups = getOtherPlayerOfBinaryAssociation(visibleElement, descriptorRoleType, viewBindingAssocType, null, valueGroupType, valueGroupRoleType);
		
		if(valueGroups.size() == 1) return valueGroups.get(0);
		else if(valueGroups.size() == 0) return null;
		else throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(visibleElement) + " must be bound none or once to a value group, but is: " + valueGroups.size());
	}


	// returns the direct constraint a value group is bound to
	public static Topic getConstraintOfValueGroup(Topic valueGroup) throws InvalidGdlSchemaException{
		if(valueGroup == null) return null;
				
		TopicMap tm = valueGroup.getTopicMap();
		Topic valueGroupRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlValueGroup, tm);
		Topic descriptorType = getTopicByPsi(PSIs.GDL.TopicType.gdlDescriptor, tm);
		Topic tmBindingAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlTmBinding, tm);
		Topic tmclConstraintType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm);
		Topic tmConstructRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlTmConstruct, tm);
		ArrayList<Topic> tmclConstraints = getOtherPlayerOfBinaryAssociation(valueGroup, valueGroupRoleType, tmBindingAssocType, null, tmclConstraintType, tmConstructRoleType);
		
		ArrayList<Topic> gdlConstraints = getOtherPlayerOfBinaryAssociation(valueGroup, valueGroupRoleType, tmBindingAssocType, null, descriptorType, tmConstructRoleType);
		
		if(tmclConstraints.size() > 1 || gdlConstraints.size() > 1 || (tmclConstraints.size() == 1 && gdlConstraints.size() == 1))
			throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(valueGroup) + " can be bound only to one constraint, but is " + (tmclConstraints.size() + gdlConstraints.size()));
		
		if(tmclConstraints.size() == 1) return tmclConstraints.get(0);
		else if (gdlConstraints.size() == 1) return gdlConstraints.get(0);
		else return null;
	}

	
	// returns the constrained role-types of the passed constraint
	public static Topic getConstraintRoleOfConstraint(Topic topicRoleOrAssociationRoleConstraint) throws InvalidGdlSchemaException {
		if(topicRoleOrAssociationRoleConstraint == null) return null;
		
		TopicMap tm = topicRoleOrAssociationRoleConstraint.getTopicMap();
		Topic constraintRoleType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm);
		Topic constrainedRoleAssocType = getTopicByPsi(PSIs.TMCL.tmclConstrainedRole, tm);
		Topic constrainedRoleType = getTopicByPsi(PSIs.TMCL.tmclConstrained, tm);
		Topic roleType = getTopicByPsi(PSIs.TMCL.tmclRoleType, tm);
		ArrayList<Topic> result = getOtherPlayerOfBinaryAssociation(topicRoleOrAssociationRoleConstraint, constraintRoleType, constrainedRoleAssocType, null, roleType, constrainedRoleType);
		
		if(result.size() != 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(topicRoleOrAssociationRoleConstraint) + " must be bound exactly once to a role-type via a " + PSIs.TMCL.tmclConstrainedRole + " association, but is bound: " + result.size());
		else return result.get(0);
	}
	
	
	// returns the constrained role and player types of the passed constraint
	public static Pair<Topic, Topic> getConstrainedRoleAndPlayerTypeOfConstraint(Topic topicRoleConstraint) throws InvalidGdlSchemaException {
		if(topicRoleConstraint == null) return null;
		
		TopicMap tm = topicRoleConstraint.getTopicMap();
		Topic constraintRoleType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm);
		Topic constrainedTopicTypeAssocType = getTopicByPsi(PSIs.TMCL.tmclConstrainedTopicType, tm);
		Topic constrainedRoleType = getTopicByPsi(PSIs.TMCL.tmclConstrained, tm);
		ArrayList<Topic> result = getOtherPlayerOfBinaryAssociation(topicRoleConstraint, constraintRoleType, constrainedTopicTypeAssocType, null, constrainedRoleType);
		
		if(result.size() != 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(topicRoleConstraint) + " must be bound exactly once to a topic-type via a " + PSIs.TMCL.tmclConstrainedTopicType + " association, but is bound: " + result.size());
		else return new Pair<Topic, Topic>(getConstraintRoleOfConstraint(topicRoleConstraint), result.get(0));
	}
	
	
	// return a pair that contains again two pair instances, whereas each pair
	// represents a role type and a role player. The outer pair represents a valid
	// combination of roles of a role-combination-constraint.
	public static Pair<Pair<Topic, Topic>, Pair<Topic, Topic>> getRoleCombinationsOfConstraint(Topic roleCombinationConstraint) throws InvalidGdlSchemaException {
		if(roleCombinationConstraint == null) return null;
		
		TopicMap tm = roleCombinationConstraint.getTopicMap();
		Topic constraintRoleType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm);
		Topic constrainedRoleAssocType = getTopicByPsi(PSIs.TMCL.tmclConstrainedRole, tm);
		Topic constrainedRoleType = getTopicByPsi(PSIs.TMCL.tmclConstrained, tm);
		ArrayList<Topic> roleTypes = getOtherPlayerOfBinaryAssociation(roleCombinationConstraint, constraintRoleType, constrainedRoleAssocType, null, constrainedRoleType);
		if(roleTypes.size() != 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(roleCombinationConstraint) + " must be bound exactly once to a role-type via a " + PSIs.TMCL.tmclConstrainedRole + ", but is: " + roleTypes.size());
		Topic constrainedTopicTypeAssocType = getTopicByPsi(PSIs.TMCL.tmclConstrainedTopicType, tm);
		ArrayList<Topic> playerTypes = getOtherPlayerOfBinaryAssociation(roleCombinationConstraint, constraintRoleType, constrainedTopicTypeAssocType, null, constrainedRoleType);
		if(playerTypes.size() != 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(roleCombinationConstraint) + " must be bound exactly once to a topic-type via a " + PSIs.TMCL.tmclConstrainedTopicType + ", but is: " + playerTypes.size());
		
		Topic otherConstrainedRoleAssocType = getTopicByPsi(PSIs.TMCL.tmclOtherConstrainedRole, tm);
		ArrayList<Topic> otherRoleTypes = getOtherPlayerOfBinaryAssociation(roleCombinationConstraint, constraintRoleType, otherConstrainedRoleAssocType, null, constrainedRoleType);
		if(otherRoleTypes.size() != 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(roleCombinationConstraint) + " must be bound exactly once to an other-role-type via a " + PSIs.TMCL.tmclOtherConstrainedRole + ", but is: " + otherRoleTypes.size());
		Topic otherConstrainedTopicTypeRoleType = getTopicByPsi(PSIs.TMCL.tmclOtherConstrainedTopicType, tm);
		ArrayList<Topic> otherPlayerType = getOtherPlayerOfBinaryAssociation(roleCombinationConstraint, constraintRoleType, otherConstrainedTopicTypeRoleType, null, constrainedRoleType);
		if(otherPlayerType.size() != 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(roleCombinationConstraint) + " must be bound exactly once to an other-topic-type via a " + PSIs.TMCL.tmclOtherConstrainedTopicType + ", but is: " + otherPlayerType.size());
		
		Pair<Topic, Topic> firstRole = new Pair<Topic, Topic>(roleTypes.get(0), playerTypes.get(0));
		Pair<Topic, Topic> secondRole = new Pair<Topic, Topic>(otherRoleTypes.get(0), otherPlayerType.get(0));
		return new Pair<Pair<Topic,Topic>, Pair<Topic,Topic>>(firstRole, secondRole);
	}
	
	
	// returns the topic that plays the role of tmcl:constrained in an association
	// of the type tmcl:constrained-statement that is bound to the passed topic
	// constrinatTopic that plays the role of tmcl:constraint
	public static Topic getConstrainedStatement(Topic constraintTopic) throws InvalidGdlSchemaException{
		if(constraintTopic == null) return null;

		TopicMap tm = constraintTopic.getTopicMap();
		Topic constraintRoleType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm);
		Topic constrainedStatementAssocType = getTopicByPsi(PSIs.TMCL.tmclConstrainedStatement, tm);
		Topic constrainedRoleType = getTopicByPsi(PSIs.TMCL.tmclConstrained, tm);
		Topic nameType = getTopicByPsi(PSIs.TMCL.tmclNameType, tm);
		Topic occurrenceType = getTopicByPsi(PSIs.TMCL.tmclOccurrenceType, tm);
		Topic associationType = getTopicByPsi(PSIs.TMCL.tmclAssociationType, tm);
		ArrayList<Topic> constrainedNameTypes = getOtherPlayerOfBinaryAssociation(constraintTopic, constraintRoleType, constrainedStatementAssocType, null, nameType, constrainedRoleType);
		ArrayList<Topic> constrainedOccurrenceTypes = getOtherPlayerOfBinaryAssociation(constraintTopic, constraintRoleType, constrainedStatementAssocType, null, occurrenceType, constrainedRoleType);
		ArrayList<Topic> constrainedAssociationTypes = getOtherPlayerOfBinaryAssociation(constraintTopic, constraintRoleType, constrainedStatementAssocType, null, associationType, constrainedRoleType);
		ArrayList<Topic> constrainedStatements = Utils.union(Utils.union(constrainedNameTypes, constrainedOccurrenceTypes), constrainedAssociationTypes); 
		
		if(constrainedStatements.size() != 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(constraintTopic) + " must be bound exactly once to a statement topic via a " + PSIs.TMCL.tmclConstrainedStatement + " association, but is: " + constrainedStatements.size());
		else return constrainedStatements.get(0);
	}
	
	
	// Returns the role-combination-constraints for the passed association-type
	public static ArrayList<Topic> getRoleCombinationConstraints(Topic associationType){
		if(associationType == null) return new ArrayList<Topic>();
		
		TopicMap tm = associationType.getTopicMap();
		Topic constrainedRoleType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm);
		Topic constrainedStatementAssocType = getTopicByPsi(PSIs.TMCL.tmclConstrainedStatement, tm);
		Topic constraintRoleType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm);
		Topic roleCombinationConstraintType = getTopicByPsi(PSIs.TMCL.tmclRoleCombinationConstraint, tm);
		return getOtherPlayerOfBinaryAssociation(associationType, constrainedRoleType, constrainedStatementAssocType, null, roleCombinationConstraintType, constraintRoleType);
	}
	
	
	// Returns the tmcl:association-role-constraints that are bound to the passed topic, that is
	// the Topic Maps representative of an gdl:association-view
	public static ArrayList<Topic> getAssociationRoleConstraintsForView(Topic viewRepresentative) throws InvalidGdlSchemaException {
		if(viewRepresentative == null) new ArrayList<Topic>();
		
		TopicMap tm = viewRepresentative.getTopicMap();
		Topic descriptorRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, tm);
		Topic associationViewBindingAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlAssociationViewBinding, tm);
		Topic tmConstructRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlTmConstruct, tm);
		Topic associationRoleConstraintType = getTopicByPsi(PSIs.TMCL.tmclAssociationRoleConstraint, tm);
		ArrayList<Topic> result = getOtherPlayerOfBinaryAssociation(viewRepresentative, descriptorRoleType, associationViewBindingAssocType, null, associationRoleConstraintType, tmConstructRoleType);
		
		if(result.size() == 0) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(viewRepresentative) + " must be bound to at least one " + PSIs.TMCL.tmclAssociationRoleConstraint + ", but is unbound");
		else return result;
	}
	
	
	// Returns the tmcl:topic-role-constraints that are bound to the passed topic, that is
	// the Topic Maps representative of an gdl:association-view
	public static ArrayList<Topic> getTopicRoleConstraintsForView(Topic viewRepresentative) throws InvalidGdlSchemaException {
		if(viewRepresentative == null) new ArrayList<Topic>();
		
		TopicMap tm = viewRepresentative.getTopicMap();
		Topic descriptorRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, tm);
		Topic associationViewBindingAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlAssociationViewBinding, tm);
		Topic tmConstructRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlTmConstruct, tm);
		Topic topicRoleConstraintType = getTopicByPsi(PSIs.TMCL.tmclTopicRoleConstraint, tm);
		ArrayList<Topic> result = getOtherPlayerOfBinaryAssociation(viewRepresentative, descriptorRoleType, associationViewBindingAssocType, null, topicRoleConstraintType, tmConstructRoleType);
				
		if(result.size() == 0) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(viewRepresentative) + " must be bound to at least one " + PSIs.TMCL.tmclTopicRoleConstraint + ", but is unbound");
		else return result;
	}
	

	// returns the root constraint of a gdl:Value-Group instance, i.e. a tmcl constraint.
	// E.g. a value group may be bound to a gdl:Type constraint and this constraint may be bound
	// to a tmcl:constraint, the root constraint is the instance of the tmcl:constraint
	public static Topic getRootConstraintOfValueGroup(Topic valueGroup, Topic currentConstraint) throws InvalidGdlSchemaException {
		if(valueGroup == null || currentConstraint == null) return null;
		
		Topic localCurrentConstraint = currentConstraint == null ? getConstraintOfValueGroup(valueGroup) : currentConstraint;
		TopicMap tm = valueGroup.getTopicMap();
		Topic tmclConstraintType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm); 
		
		if(isInstanceOf(localCurrentConstraint, tmclConstraintType)) return localCurrentConstraint;
		else {
			// get next constraint and invoke this method recursively
			Topic descriptorRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, tm);
			Topic tmBindingAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlTmBinding, tm);
			Topic tmConstructRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlTmConstruct, tm);
			Topic gdlDescriptor = getTopicByPsi(PSIs.GDL.TopicType.gdlDescriptor, tm);
			
			ArrayList<Topic> nextTmclConstraints = getOtherPlayerOfBinaryAssociation(localCurrentConstraint, descriptorRoleType, tmBindingAssocType, null, tmclConstraintType, tmConstructRoleType);
			ArrayList<Topic> nextGdlConstraints = getOtherPlayerOfBinaryAssociation(localCurrentConstraint, descriptorRoleType, tmBindingAssocType, null, gdlDescriptor, tmConstructRoleType);
			
			if(nextTmclConstraints.size() > 1 || nextGdlConstraints.size() > 1 || (nextTmclConstraints.size() == 1 && nextGdlConstraints.size() == 1))
				throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(localCurrentConstraint) + " can be bound only to one constraint, but is " + (nextTmclConstraints.size() + nextGdlConstraints.size()));
			
			if(nextTmclConstraints.size() == 1) return nextTmclConstraints.get(0);
			else if(nextGdlConstraints.size() == 1) return getRootConstraintOfValueGroup(valueGroup, nextGdlConstraints.get(0));
			else throw new InvalidGdlSchemaException("the value group " + getAnyIdOfTopic(valueGroup) + " must be bound to exaclty one root constraint of the type " + PSIs.TMCL.tmclConstraint + " but is unbound");
		}
	}


	// returns the topic that represents the default tm value for the passed value group
	public static Topic getDefaultTmValue(Topic valueGroup) throws InvalidGdlSchemaException {
		if(valueGroup == null) return null;
		
		TopicMap tm = valueGroup.getTopicMap();
		Topic valueBindingAssocTopic = getTopicByPsi(PSIs.GDL.AssociationType.gdlValueBinding, tm);
		Topic valueGroupRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlValueGroup, tm);
		Topic valueRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlValue, tm);
		Topic defaultTmValueType = getTopicByPsi(PSIs.GDL.TopicType.gdlDefaultTmValue, tm);
		
		ArrayList<Topic> defaultTmValues = getOtherPlayerOfBinaryAssociation(valueGroup, valueGroupRoleType, valueBindingAssocTopic, null, defaultTmValueType, valueRoleType);
		
		if(defaultTmValues.size() == 1) return defaultTmValues.get(0);
		else if(defaultTmValues.size() == 0) return null;
		else throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(valueGroup) + " must be bound none or once to a " + PSIs.GDL.TopicType.gdlDefaultTmValue + ", but is: " + defaultTmValues.size());
	}
	
	
	// returns the topic that represents the default literal value for the passed value group
	public static Topic getDefaultLiteralValue(Topic valueGroup) throws InvalidGdlSchemaException{
		if(valueGroup == null) return null;
		
		TopicMap tm = valueGroup.getTopicMap();
		Topic valueBindingAssocTopic = getTopicByPsi(PSIs.GDL.AssociationType.gdlValueBinding, tm);
		Topic valueGroupRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlValueGroup, tm);
		Topic valueRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlValue, tm);
		Topic defaultLiteralValueType = getTopicByPsi(PSIs.GDL.TopicType.gdlDefaultLiteralValue, tm);		
		ArrayList<Topic> defaultTmValues = getOtherPlayerOfBinaryAssociation(valueGroup, valueGroupRoleType, valueBindingAssocTopic, null, defaultLiteralValueType, valueRoleType);

		if(defaultTmValues.size() == 1) return defaultTmValues.get(0);
		else if(defaultTmValues.size() == 0) return null;
		else throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(valueGroup) + " must be bound none or once to a " + PSIs.GDL.TopicType.gdlDefaultLiteralValue + ", but is: " + defaultTmValues.size());
	}
	
	
	// returns the topics that represent the tm values for the passed value group
	public static ArrayList<Topic> getTmValues(Topic valueGroup) throws InvalidGdlSchemaException {
		ArrayList<Topic> result = new ArrayList<Topic>();
		if(valueGroup == null) return result;
				
		TopicMap tm = valueGroup.getTopicMap();
		Topic valueBindingAssocTopic = getTopicByPsi(PSIs.GDL.AssociationType.gdlValueBinding, tm);
		Topic valueGroupRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlValueGroup, tm);
		Topic valueRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlValue, tm);
		Topic tmValueType = getTopicByPsi(PSIs.GDL.TopicType.gdlTmValue, tm);
		
		return getOtherPlayerOfBinaryAssociation(valueGroup, valueGroupRoleType, valueBindingAssocTopic, null, tmValueType, valueRoleType);
	}
	
	
	// returns the topics that represent the literal values for the passed value group
	public static ArrayList<Topic> getLiteralValues(Topic valueGroup){
		ArrayList<Topic> result = new ArrayList<Topic>();
		if(valueGroup == null) return result;
				
		TopicMap tm = valueGroup.getTopicMap();
		Topic valueBindingAssocTopic = getTopicByPsi(PSIs.GDL.AssociationType.gdlValueBinding, tm);
		Topic valueGroupRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlValueGroup, tm);
		Topic valueRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlValue, tm);
		Topic literalValueType = getTopicByPsi(PSIs.GDL.TopicType.gdlLiteralValue, tm);
		
		return getOtherPlayerOfBinaryAssociation(valueGroup, valueGroupRoleType, valueBindingAssocTopic, null, literalValueType, valueRoleType);
	}

	
	// returns a string that is the literal value of the literal-value-topic
	public static String getLiteral (Topic literalValue) throws InvalidGdlSchemaException {
		if(literalValue == null) return null;
		
		Occurrence occ = getSingleOccurrence(literalValue, getTopicByPsi(PSIs.GDL.OccurrenceType.gdlLiteralValue, literalValue.getTopicMap()));
		if(occ == null) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(literalValue) + " must be bound exactly once to an occurrence of the type " + PSIs.GDL.OccurrenceType.gdlLiteralValue + ", but is: 0");
		else return occ.getValue();
	}
	
	
	// returns an ArrayList of strings that are set to a value group as literal values
	public static ArrayList<String> getLiterals(Topic valueGroup) throws InvalidGdlSchemaException {
		ArrayList<Topic> literalTopics = getLiteralValues(valueGroup);
		
		ArrayList<String> result = new ArrayList<String>();
		for (Topic topic : literalTopics) result.add(getLiteral(topic));
		
		return result;
	}
	
	
	// returns the topic that can be used to satisfy the passed constraint.
	public static ArrayList<Topic> getTmValuesForConstraint(Topic constraint, Topic valueGroup) throws InvalidGdlSchemaException, ExecutionException {
		ArrayList<Topic> result = new ArrayList<Topic>();
		if(constraint == null || valueGroup == null) return result;
		
		if(isInstanceOf(constraint, PSIs.GDL.TopicType.gdlType)){
			return getTmValuesForTypeConstraint(constraint, valueGroup);
		} else if(isInstanceOf(constraint, PSIs.GDL.TopicType.gdlVariantNameReifier)){
			return getTmValuesForVariantNameReifierConstraint(constraint);
		} else if(isInstanceOf(constraint, PSIs.GDL.TopicType.gdlVariantNameScope)){
			return getTmValuesForVariantNameScopeConstraint(constraint);
		} else if(isInstanceOf(constraint, PSIs.GDL.TopicType.gdlRolePlayer)){
			return getTmValuesForRolePlayerConstraint(constraint, valueGroup);
		} else if(isInstanceOf(constraint, PSIs.TMCL.tmclReifierConstraint)){
			return getTmValuesForReifierConstraint(constraint);
		} else if(isInstanceOf(constraint, PSIs.TMCL.tmclScopeConstraint)){
			return getTmValuesForScopeConstraint(constraint);
		}
		
		return result;
	}

	
	// returns the topic that can be used to satisfy the passed role-player-constraint.
	public static ArrayList<Topic> getTmValuesForScopeConstraint(Topic scopeConstraint) throws InvalidGdlSchemaException {
		ArrayList<Topic> result = new ArrayList<Topic>();
		if(scopeConstraint == null) return result;

		TopicMap tm = scopeConstraint.getTopicMap();
		Topic constraintRoleType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm);
		Topic constrainedScopeAssocType = getTopicByPsi(PSIs.TMCL.tmclConstrainedScope, tm);
		Topic constrainedRoleType = getTopicByPsi(PSIs.TMCL.tmclConstrained, tm);
		ArrayList<Topic> scopeTypes = getOtherPlayerOfBinaryAssociation(scopeConstraint, constraintRoleType, constrainedScopeAssocType, null, constrainedRoleType);
				
		if(scopeTypes.size() != 1){
			throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(scopeConstraint) + " must be bound to exaclty one topic type that is used as scope-type, but is: " + scopeTypes.size());
		} else {
			JsArray<Topic> allTopics = tm.getTopics();
			for(int i = 0; i != allTopics.length(); ++i) if(isInstanceOf(allTopics.get(i), scopeTypes.get(0))) result.add(allTopics.get(i));
		}
		return result;
	}
	
	
	// returns the topic that can be used to satisfy the passed role-player-constraint.
	public static ArrayList<Topic> getTmValuesForReifierConstraint(Topic reifierConstraint) throws InvalidGdlSchemaException{
		ArrayList<Topic> result = new ArrayList<Topic>();
		if(reifierConstraint == null) return result;

		TopicMap tm = reifierConstraint.getTopicMap();
		Topic allowsRoleType = getTopicByPsi(PSIs.TMCL.tmclAllows, tm);
		Topic allowedRoleType = getTopicByPsi(PSIs.TMCL.tmclAllowed, tm);
		Topic allowedReifierAssocType = getTopicByPsi(PSIs.TMCL.tmclAllowedReifier, tm);
		Topic topicType = getTopicByPsi(PSIs.TMCL.tmclTopictype, tm);
		ArrayList<Topic> reifierTypes = getOtherPlayerOfBinaryAssociation(reifierConstraint, allowsRoleType, allowedReifierAssocType, null, topicType, allowedRoleType);
		
		if(reifierTypes.size() != 1){
			throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(reifierConstraint) + " must be bound to exaclty one topic type that is used as reifier, but is: " + reifierTypes.size());
		} else {
			JsArray<Topic> allTopics = tm.getTopics();
			for(int i = 0; i != allTopics.length(); ++i) if(isInstanceOf(allTopics.get(i), reifierTypes.get(0))) result.add(allTopics.get(i));
		}
		return result;
	}
	
	
	// returns the topic that can be used to satisfy the passed role-player-constraint.
	public static ArrayList<Topic> getTmValuesForRolePlayerConstraint(Topic rolePlayerConstraint, Topic valueGroup) throws ExecutionException, InvalidGdlSchemaException {
		ArrayList<Topic> result = new ArrayList<Topic>();
		if(rolePlayerConstraint == null || valueGroup == null) return result;
		
		Topic topicRoleConstraint = getRootConstraintOfValueGroup(valueGroup, rolePlayerConstraint);
		if(!isInstanceOf(topicRoleConstraint, PSIs.TMCL.tmclTopicRoleConstraint)) throw new InvalidGdlSchemaException("the root constraint of the topic " + getAnyIdOfTopic(rolePlayerConstraint) + " must be an instance of " + PSIs.TMCL.tmclTopicRoleConstraint + ", but is: " + getAnyIdOfTopic(topicRoleConstraint));
		
		Pair<Topic, Topic> roleAndPlayerType = getConstrainedRoleAndPlayerTypeOfConstraint(topicRoleConstraint);
		Topic playerType = roleAndPlayerType.getSecond();
		JsArray<Topic> allTopics = rolePlayerConstraint.getTopicMap().getTopics();
		for(int i = 0; i != allTopics.length(); ++i)
			if(isInstanceOf(allTopics.get(i), playerType)) result.add(allTopics.get(i));
		
		return result;
	}
	
	
	// returns the topic that can be used to satisfy the passed type-constraint.
	public static ArrayList<Topic> getTmValuesForTypeConstraint(Topic typeConstraint, Topic valueGroup) throws InvalidGdlSchemaException, ExecutionException {
		ArrayList<Topic> result = new ArrayList<Topic>();
		if(typeConstraint == null || valueGroup == null) return result;
		
		Topic rootConstraint = getRootConstraintOfValueGroup(valueGroup, typeConstraint);
		if(rootConstraint == null) return result;
				
		if(isInstanceOf(rootConstraint, PSIs.TMCL.tmclTopicNameConstraint) || isInstanceOf(rootConstraint, PSIs.TMCL.tmclTopicOccurrenceConstraint)){
			TopicMap tm = rootConstraint.getTopicMap();
			Topic constraintRoleType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm);
			Topic constrainedRoleType = getTopicByPsi(PSIs.TMCL.tmclConstrained, tm);
			Topic nameType = getTopicByPsi(PSIs.TMCL.tmclNameType, tm);
			Topic occurrenceType = getTopicByPsi(PSIs.TMCL.tmclOccurrenceType, tm);
			Topic constrainedStatementAssocType = getTopicByPsi(PSIs.TMCL.tmclConstrainedStatement, tm);
			ArrayList<Topic> nameTypeTopics = getOtherPlayerOfBinaryAssociation(rootConstraint, constraintRoleType, constrainedStatementAssocType, null, nameType, constrainedRoleType);
			ArrayList<Topic> occurrenceTypeTopics = getOtherPlayerOfBinaryAssociation(rootConstraint, constraintRoleType, constrainedStatementAssocType, null, occurrenceType, constrainedRoleType);
			ArrayList<Topic> typeTopics = Utils.union(nameTypeTopics, occurrenceTypeTopics);
						
			if(typeTopics.size() != 1){
				throw new InvalidGdlSchemaException("the constraint " + getAnyIdOfTopic(rootConstraint) + " must be bound extactly once to an occurrence or name type, but is: " + typeTopics.size());
			} else {
				// add the direct specified type
				result.add(typeTopics.get(0));
				
				// get subtypes of typeTopic
				JsArray<Topic> allTopics = tm.getTopics();
				for(int i = 0; i != allTopics.length(); ++i) if(isSupertypeOf(allTopics.get(i), typeTopics.get(0))) result.add(allTopics.get(i));
			}
		} else if(isInstanceOf(rootConstraint, PSIs.TMCL.tmclAssociationRoleConstraint)){
			// TODO: implement
			throw new ExecutionException(PSIs.TMCL.tmclAssociationRoleConstraint + " is not implemented yet");
		} else if(isInstanceOf(rootConstraint, PSIs.TMCL.tmclTopicRoleConstraint)){
			// TODO: implement
			throw new ExecutionException(PSIs.TMCL.tmclTopicRoleConstraint + " is not implemented yet");
		}
		
		return result;
	}
	
	
	// returns the topic that can be used to satisfy the passed variant-name-reifier-constraint.
	public static ArrayList<Topic> getTmValuesForVariantNameReifierConstraint(Topic variantNameReifierConstraint) throws ExecutionException {
		// TODO: implement
		throw new ExecutionException("this mehtod is currently not implemented");
	}
	
	
	// returns the topic that can be used to satisfy the passed variant-name-scope-constraint.
	public static ArrayList<Topic> getTmValuesForVariantNameScopeConstraint(Topic variantNameScopeConstraint) throws ExecutionException{
		// TODO: implement
		throw new ExecutionException("this method is currently not implemented");
	}
	
	
	// returns the regular expression for a constraint. If no regular-expression constraint is set for the
	// constrained-statement of the passed constraint, the value ".*" is returned
	public static String getLiteralValueForConstraint(Topic constraint) throws InvalidGdlSchemaException {
		String result = ".*";
		if(constraint == null) return result; 
		
		TopicMap tm = constraint.getTopicMap();
		Topic constraintRoleType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm);
		Topic constrainedStatementAssocType = getTopicByPsi(PSIs.TMCL.tmclConstrainedStatement, tm);
		Topic constrainedRoleType = getTopicByPsi(PSIs.TMCL.tmclConstrained, tm);
		Topic nameType = getTopicByPsi(PSIs.TMCL.tmclNameType, tm);
		Topic occurrenceType = getTopicByPsi(PSIs.TMCL.tmclOccurrenceType, tm);
		ArrayList<Topic> nameTypes = getOtherPlayerOfBinaryAssociation(constraint, constraintRoleType, constrainedStatementAssocType, null, nameType, constrainedRoleType);
		ArrayList<Topic> occurrenceTypes = getOtherPlayerOfBinaryAssociation(constraint, constraintRoleType, constrainedStatementAssocType, null, occurrenceType, constrainedRoleType);
		ArrayList<Topic> nameOrOccurrenceTypes = Utils.union(nameTypes, occurrenceTypes);
		
		if(nameOrOccurrenceTypes.size() == 0){
			return result;
		} else if(nameOrOccurrenceTypes.size() > 1){
			throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(constraint) + " must be bound maximum once to a topic via a " + PSIs.TMCL.tmclConstrainedStatement + ", but is: " + nameOrOccurrenceTypes.size());
		} else {
			Topic regularExpressionConstraintType = getTopicByPsi(PSIs.TMCL.tmclRegularExpressionConstraint, tm);
			ArrayList<Topic> regularExpressionConstraints = getOtherPlayerOfBinaryAssociation(nameOrOccurrenceTypes.get(0), constrainedRoleType, constrainedStatementAssocType, null, regularExpressionConstraintType, constraintRoleType);
			
			if(regularExpressionConstraints.size() == 0) {
				return ".*";
			} else if(regularExpressionConstraints.size() == 1){
				Topic regexpOccurrenceType = getTopicByPsi(PSIs.TMCL.tmclRegexp, tm);
				Occurrence rex = getSingleOccurrence(regularExpressionConstraints.get(0), regexpOccurrenceType);
				return rex.getValue();
			} else {
				throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(nameOrOccurrenceTypes.get(0)) + " must be bound maximum once to a " + PSIs.TMCL.tmclRegularExpressionConstraint + ", but is: " + regularExpressionConstraints.size());
			}
		}
	}
	
	
	// Returns all tmcl:topic-role-constraint that the types of this topic
	// are bound to
	public static ArrayList<Topic> getTopicRoleConstraintsForTopicInstance(Topic topicInstance){
		ArrayList<Topic> result = new ArrayList<Topic>();
		if(topicInstance == null) return result;
		
		JsArray<Topic> topicTypes = topicInstance.getTypes();
		for(int i = 0; i != topicTypes.length(); ++i){
			ArrayList<Topic> resultForType = getTopicRoleConstraintsForTopicType(topicTypes.get(i));
			result = Utils.union(result, resultForType);
		}
		
		return result;
	}
	
	
	// Returns all tmcl:topic-role-constraints the the passed topic is bound to
	public static ArrayList<Topic> getTopicRoleConstraintsForTopicType(Topic topicType){
		if(topicType == null) return new ArrayList<Topic>();
		
		TopicMap tm = topicType.getTopicMap();
		Topic constrainedRoleType = getTopicByPsi(PSIs.TMCL.tmclConstrained, tm);
		Topic constrainedTopicTypeAssocType = getTopicByPsi(PSIs.TMCL.tmclConstrainedTopicType, tm);
		Topic constraintRoleType = getTopicByPsi(PSIs.TMCL.tmclConstraint, tm);
		Topic topicRoleConstraintType = getTopicByPsi(PSIs.TMCL.tmclTopicRoleConstraint, tm);
		return getOtherPlayerOfBinaryAssociation(topicType, constrainedRoleType, constrainedTopicTypeAssocType, null, topicRoleConstraintType, constraintRoleType);
	}
	
	
	// return the occurrence of the passed type, if the topic contains more than one such occurrences, 
	// this function throws a InvalidgdlSchemaException 
	public static Occurrence getSingleOccurrence(Topic topic, Topic occurrenceType) throws InvalidGdlSchemaException {
		if(topic == null) return null;
		
		JsArray<Occurrence> occs = topic.getOccurrences(occurrenceType);
		if(occs.length() > 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(topic) + " must contains maximum one occurrence of the type " + getAnyIdOfTopic(occurrenceType) + ", but conaints: " + occs.length());
		else if(occs.length() == 0) return null;
		else return occs.get(0);
	}

	
	// Returns an array of topics that represent the preffered scope that is set
	// to a tm-value or value-group instance
	public static ArrayList<Topic> getPrefferedScopesForTopicOf(Topic tmValueOrValueGroup){
		if(tmValueOrValueGroup == null) return new ArrayList<Topic>();
		
		TopicMap tm = tmValueOrValueGroup.getTopicMap();
		Topic descriptorRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, tm);
		Topic preferredScopeAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlPrefferedScope, tm);
		Topic tmConstructRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlTmConstruct, tm);
		return getOtherPlayerOfBinaryAssociation(tmValueOrValueGroup, descriptorRoleType, preferredScopeAssocType, null, tmConstructRoleType);
	}


	// Returns the topic that is bound to the passed tm-value or value-group topic
	// and represents the representation schema of the topics that must be displayed 
	public static Topic getDisplayByTopicOf(Topic tmValueOrValueGroup) throws InvalidGdlSchemaException{
		if(tmValueOrValueGroup == null) return null;
		
		TopicMap tm = tmValueOrValueGroup.getTopicMap();
		Topic displayByAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlDisplayBy, tm);
		Topic descriptorRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, tm);
		Topic tmConstruct = getTopicByPsi(PSIs.GDL.RoleType.gdlTmConstruct, tm);
		Topic subjectIdentifierConstraintType = getTopicByPsi(PSIs.TMCL.tmclSubjectIdentifierConstraint, tm);
		Topic subjectLocatorConstraintType = getTopicByPsi(PSIs.TMCL.tmclSubjectLocatorConstraint, tm);
		Topic itemIdentifierConstraintType = getTopicByPsi(PSIs.TMCL.tmclItemIdentifierConstraint, tm);
		Topic topicNameConstraintType = getTopicByPsi(PSIs.TMCL.tmclTopicNameConstraint, tm);
		ArrayList<Topic> resultPsi = getOtherPlayerOfBinaryAssociation(tmValueOrValueGroup, descriptorRoleType, displayByAssocType, null, subjectIdentifierConstraintType, tmConstruct);
		ArrayList<Topic> resultSl = getOtherPlayerOfBinaryAssociation(tmValueOrValueGroup, descriptorRoleType, displayByAssocType, null, subjectLocatorConstraintType, tmConstruct);
		ArrayList<Topic> resultIi = getOtherPlayerOfBinaryAssociation(tmValueOrValueGroup, descriptorRoleType, displayByAssocType, null, itemIdentifierConstraintType, tmConstruct);
		ArrayList<Topic> resultName = getOtherPlayerOfBinaryAssociation(tmValueOrValueGroup, descriptorRoleType, displayByAssocType, null, topicNameConstraintType, tmConstruct);
		
		ArrayList<Topic> result = Utils.union(Utils.union(Utils.union(resultPsi, resultSl), resultIi), resultName);
		
		if(result.size() > 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(tmValueOrValueGroup) + " must be bound none or once to a tmcl:constraint via a " + PSIs.GDL.AssociationType.gdlDisplayBy + " assciation, but is: " + result.size());
		else if(result.size() == 1) return result.get(0);
		else return null;
	}


	// Returns a string that represents the topic. The returned string must
	// correspond to the passed diplayBy and preferredScopes data. 
	public static String getTopicRepresentation(Topic topicToRepresent, Topic displayBy, ArrayList<Topic> preferredScopes) throws InvalidGdlSchemaException {
		if(topicToRepresent == null) return null;
		
		if(displayBy == null){
			if(topicToRepresent.getSubjectIdentifiers().length() == 0) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(topicToRepresent) + " is not bound to a " + PSIs.GDL.AssociationType.gdlDisplayBy + " association and is not bound to a Subject-Identifier - but one of them must be set");
			else return topicToRepresent.getSubjectIdentifiers().get(0).getReference(); 
		} else {
			TopicMap tm = topicToRepresent.getTopicMap();
			Topic subjectIdentifierConstraint = getTopicByPsi(PSIs.TMCL.tmclSubjectIdentifierConstraint, tm);
			Topic subjectLocatorConstraint = getTopicByPsi(PSIs.TMCL.tmclSubjectLocatorConstraint, tm);
			Topic itemIdentifierConstraint = getTopicByPsi(PSIs.TMCL.tmclItemIdentifierConstraint, tm);
			Topic topicNameConstraint = getTopicByPsi(PSIs.TMCL.tmclTopicNameConstraint, tm);
			
			if(isInstanceOf(displayBy, topicNameConstraint)){
				Topic nameType = TmHelper.getConstrainedStatement(displayBy);
				for(int i = 0; i != topicToRepresent.getNames(nameType).length(); ++i){
					Name name = topicToRepresent.getNames(nameType).get(i);
					if(preferredScopes == null || preferredScopes.size() == 0){
						return name.getValue();
					} else {						
						for(int j = 0; j != name.getScope().length(); ++j){
							if(preferredScopes.contains(name.getScope().get(j))) return name.getValue();
						}
					}
				}
				throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(topicToRepresent) + " does not contain any name that can be used for its " + PSIs.GDL.AssociationType.gdlDisplayBy + " schema");
				
			} else if(isInstanceOf(displayBy, subjectIdentifierConstraint)){
				Pattern rex = new Pattern(TmHelper.getLiteralValueForConstraint(displayBy));
				for(int i = 0; i != topicToRepresent.getSubjectIdentifiers().length(); ++i){
					String psi = topicToRepresent.getSubjectIdentifiers().get(i).getReference();
					if(rex.matches(psi)) return psi;
				}
				throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(topicToRepresent) + " does not contain any subject-identifier that can be used for its " + PSIs.GDL.AssociationType.gdlDisplayBy + " schema");
			} else if(isInstanceOf(displayBy, subjectLocatorConstraint)){
				Pattern rex = new Pattern(TmHelper.getLiteralValueForConstraint(displayBy));
				for(int i = 0; i != topicToRepresent.getSubjectLocators().length(); ++i){
					String sl = topicToRepresent.getSubjectLocators().get(i).getReference();
					if(rex.matches(sl)) return sl;
				}
				throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(topicToRepresent) + " does not contain any subject-locator that can be used for its " + PSIs.GDL.AssociationType.gdlDisplayBy + " schema");
			} else if(isInstanceOf(displayBy, itemIdentifierConstraint)){
				Pattern rex = new Pattern(TmHelper.getLiteralValueForConstraint(displayBy));
				for(int i = 0; i != topicToRepresent.getItemIdentifiers().length(); ++i){
					String ii = topicToRepresent.getItemIdentifiers().get(i).getReference();
					if(rex.matches(ii)) return ii;
				}
				throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(topicToRepresent) + " does not contain any item-identifier that can be used for its " + PSIs.GDL.AssociationType.gdlDisplayBy + " schema");
			} else {
				throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(displayBy) + " is not a valid representation schema type");
			}
		}
	}

	
	// returns an integer representing the tmcl:card-min value
	public static int getCardMin(Topic constraint) throws InvalidGdlSchemaException {
		if(constraint == null) return 0;
		Occurrence occ = getSingleOccurrence(constraint, getTopicByPsi(PSIs.TMCL.tmclCardMin, constraint.getTopicMap()));
		if(occ == null){
			return 0;
		} else {
			try{
				return Integer.parseInt(occ.getValue());
			}catch(NumberFormatException e){
				throw new InvalidGdlSchemaException("the occurrence " + PSIs.TMCL.tmclCardMin + " of the constraint " + getAnyIdOfTopic(constraint) + " must be an integer, but is: " + occ.getValue());
			}
		}
	}
	
	
	// returns an integer representing the tmcl:card-max value or -1
	// if infinite
	public static int getCardMax(Topic constraint) throws InvalidGdlSchemaException {
		if(constraint == null) return 0;
		Occurrence occ = getSingleOccurrence(constraint, getTopicByPsi(PSIs.TMCL.tmclCardMax, constraint.getTopicMap()));
		if(occ == null){
			return 0;
		} else {
			try{
				return Integer.parseInt(occ.getValue());
			}catch(NumberFormatException e){
				throw new InvalidGdlSchemaException("the occurrence " + PSIs.TMCL.tmclCardMax + " of the constraint " + getAnyIdOfTopic(constraint) + " must be an integer, but is: " + occ.getValue());
			}
		}
	}
	

	// returns the actual list of values for a TM-Value instance, i.e.
	// *(Default-)TM-Type-Value: returns all topic instances that are
	//		of the type bound to the TM-Type-Value topic
	// *(Default-)TM-Single-Type-Value: returns all topics that are
	//		of the type bound to the TM-Single-Type-Value and are not
	//      of the type of any other topic
	// *(Default-)TM-Multiple-Type-Value: returns all topics that are
	//      instances of all types that are bound to the
	//      TM-Multiple-Type-Value topic
	// *(Default-)TM-Instance-Value: returns the topic that is bound
	//		to the TM-Instance-Value topics
	public static ArrayList<Topic> getValuesForTmValue(Topic tmValue) throws InvalidGdlSchemaException {
		ArrayList<Topic> result = new ArrayList<Topic>();
		if(tmValue == null) return result;
				
		TopicMap tm = tmValue.getTopicMap();
		Topic descriptorRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlDescriptor, tm);
		Topic tmBindingAssocType = getTopicByPsi(PSIs.GDL.AssociationType.gdlTmBinding, tm);
		Topic tmConstructRoleType = getTopicByPsi(PSIs.GDL.RoleType.gdlTmConstruct, tm);
		if(isInstanceOf(tmValue, PSIs.GDL.TopicType.gdlTmTypeValue) || isInstanceOf(tmValue, PSIs.GDL.TopicType.gdlDefaultTmTypeValue)){
			ArrayList<Topic> topicTypes = getOtherPlayerOfBinaryAssociation(tmValue, descriptorRoleType, tmBindingAssocType, null, tmConstructRoleType);
			if(topicTypes.size() != 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(tmValue) + " must be bound exactly once to a topic type value via a " + PSIs.GDL.AssociationType.gdlTmBinding + " association, but is: " + topicTypes.size());
			Topic topicType = topicTypes.get(0);
			
			JsArray<Topic> allTopics = tm.getTopics();
			for(int i = 0; i != allTopics.length(); ++i)
				if(isInstanceOf(allTopics.get(i), topicType)) result.add(allTopics.get(i));
		} else if(isInstanceOf(tmValue, PSIs.GDL.TopicType.gdlTmSingleTypeValue) || isInstanceOf(tmValue, PSIs.GDL.TopicType.gdlDefaultTmSingleTypeValue)){
			ArrayList<Topic> topicTypes = getOtherPlayerOfBinaryAssociation(tmValue, descriptorRoleType, tmBindingAssocType, null, tmConstructRoleType);
			if(topicTypes.size() != 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(tmValue) + " must be bound exactly once to a topic type value via a " + PSIs.GDL.AssociationType.gdlTmBinding + " association, but is: " + topicTypes.size());
			Topic topicType = topicTypes.get(0);
			
			JsArray<Topic> allTopics = tm.getTopics();
			for(int i = 0; i != allTopics.length(); ++i)
				if(isInstanceOf(allTopics.get(i), topicType) && allTopics.get(i).getTypes().length() == 1) result.add(allTopics.get(i));
		} else if(isInstanceOf(tmValue, PSIs.GDL.TopicType.gdlTmMultipleTypeValue) || isInstanceOf(tmValue, PSIs.GDL.TopicType.gdlDefaultTmMultipleTypeValue)){
			ArrayList<Topic> topicTypes = getOtherPlayerOfBinaryAssociation(tmValue, descriptorRoleType, tmBindingAssocType, null, tmConstructRoleType);
			if(topicTypes.size() == 0) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(tmValue) + " must be bound at least once to a topic type value via a " + PSIs.GDL.AssociationType.gdlTmBinding + " association, but is: 0");
			
			JsArray<Topic> allTopics = tm.getTopics();
			for(int i = 0; i != allTopics.length(); ++i){
				int j = 0;
				for( ; j != topicTypes.size(); ++j)
					if(!isInstanceOf(allTopics.get(i), topicTypes.get(j)) || allTopics.get(i).getTypes().length() > topicTypes.size()) break;
				
				if(j == topicTypes.size()) result.add(allTopics.get(i));
			}
		} else if(isInstanceOf(tmValue, PSIs.GDL.TopicType.gdlTmInstanceValue) || isInstanceOf(tmValue, PSIs.GDL.TopicType.gdlDefaultTmInstanceValue)){
			result = getOtherPlayerOfBinaryAssociation(tmValue, descriptorRoleType, tmBindingAssocType, null, tmConstructRoleType);
			if(result.size() != 1) throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(tmValue) + " must be bound exactly once to a topic value via a " + PSIs.GDL.AssociationType.gdlTmBinding + " association, but is: " + result.size());
		} else {
			throw new InvalidGdlSchemaException("the topic " + getAnyIdOfTopic(tmValue) + " must be an instance of " + PSIs.GDL.TopicType.gdlTmValue);
		}
		
		return result;
	}

	
	// Returns the topic that is rperesentated by the passed string corresponding
	// to the passed representationSchema
	public static Topic getTopicFromStringRepresentation(String representation, Topic valueGroup) throws ExecutionException, InvalidGdlSchemaException {
		if(representation == null || valueGroup == null) return null;
		
		ArrayList<Topic> tmValues = getTmValuesForConstraint(getConstraintOfValueGroup(valueGroup), valueGroup);
		for (Topic tmValue : tmValues) {
			if(getTopicRepresentation(tmValue, getDisplayByTopicOf(valueGroup), getPrefferedScopesForTopicOf(valueGroup)).equals(representation)) return tmValue;
		}
		
		tmValues = getTmValues(valueGroup);
		for (Topic tmValue : tmValues) {
			ArrayList<Topic> rawValues = getValuesForTmValue(tmValue);
			for (Topic rawValue : rawValues) {
				if(getTopicRepresentation(tmValue, getDisplayByTopicOf(valueGroup), getPrefferedScopesForTopicOf(valueGroup)).equals(representation)) return rawValue;
			}
		}
		
		throw new ExecutionException("no topic found for the representation string: " + representation);
	}


	// Returns the actual default tm value for the corresponding hidden-value topic 
	public static Topic getRawTmValueForHiddenValue(Topic hiddenValue, GdlVisibleObject elem) throws InvalidGdlSchemaException{
		ArrayList<Topic> result = new ArrayList<Topic>();
		if(hiddenValue == null) return null;
		
		Topic defaultTmValue = getDefaultTmValueOfHiddenValue(hiddenValue);
		if(defaultTmValue != null){
			result = getValuesForTmValue(defaultTmValue);
		}else if(getDefaultLiteralValueOfHiddenValue(hiddenValue) != null){
			return null;
		} else {
			if(elem == null) return null;
			else result.add(getNearestTopic(elem));
		}
		
		if(result.size() > 1) throw new InvalidGdlSchemaException("the hidden value " + getAnyIdOfTopic(hiddenValue) + " represents more than one particular value: " + result.size());
		else if(result.size() == 1) return result.get(0);
		else return null;
	}
}
