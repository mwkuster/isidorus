package us.isidor.gdl.anaToMia.Widgets.base;

import java.util.ArrayList;
import com.google.gwt.core.client.JsArray;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Association;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Locator;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Name;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Reifiable;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.ReifiableStub;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Role;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.ScopedStub;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMap;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMapsTypes;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Variant;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidContentException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;
import us.isidor.gdl.anaToMia.Widgets.environment.Pattern;


public class GdlVisibleObjectTmService {
	private GdlVisibleObject owner = null;
	private Topic valueGroupTopic = null;
	private boolean valueGroupTopicSet = false;
	private Topic tmRepresentative = null;
	private Topic constraintTopic = null;
	private boolean constraintTopicSet = false;
	private Topic rootConstraintTopic = null;
	private boolean rootConstraintTopicSet = false;
	private Topic defaultTmValueTopic = null;
	private boolean defaultTmValueTopicSet = false;
	private Topic defaultLiteralValueTopic = null;
	private boolean defaultLiteralValueTopicSet = false;
	private ArrayList<Topic> tmValueTopics = new ArrayList<Topic>();
	private boolean tmValueTopicsSet = false;
	private ArrayList<Topic> literalValueTopics = new ArrayList<Topic>();
	private boolean literalValueTopicsSet = false;
	private ArrayList<String> literals = new ArrayList<String>();
	private boolean literalsSet = false;
	private int cardMin = 0;
	private boolean cardMinSet = false;
	private int cardMax = 0;
	private boolean cardMaxSet = false;
	private Topic displayByConstraint = null;
	private boolean displayByConstraintSet = false;
	private ArrayList<Topic> preferredScopes = new ArrayList<Topic>();
	private boolean prefferedScopesSet = false;
	private ArrayList<Topic> rawTmValues = new ArrayList<Topic>();
	private boolean rawTmValuesSet = false;
	
	
	@SuppressWarnings("unused")
	private GdlVisibleObjectTmService() {}
	
	
	public GdlVisibleObjectTmService(GdlVisibleObject owner) throws ExecutionException {
		if(owner == null) throw new ExecutionException("owner must not be null");
		this.owner = owner;
		this.tmRepresentative = this.owner.getTmRepresentative();
	}
	
	
	// returns the topic instance of gdlt:Value-Group that is bound to this
	// visible element, or null if it is unbound
	public Topic getValueGroup() throws InvalidGdlSchemaException {
		if(this.valueGroupTopicSet){
			return this.valueGroupTopic;
		} else {
			this.valueGroupTopic = TmHelper.getValueGroupOf(this.tmRepresentative);
			this.valueGroupTopicSet = true;
			return this.valueGroupTopic;
		}
	}
	
	
	// returns the direct (first) constraint that is bound to the value-group
	// of this element - or null if it is unbound
	public Topic getConstraint() throws InvalidGdlSchemaException {
		if(this.constraintTopicSet){
			return this.constraintTopic;
		} else {
			this.constraintTopic = TmHelper.getConstraintOfValueGroup(this.getValueGroup());
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
			this.rootConstraintTopic = TmHelper.getRootConstraintOfValueGroup(this.getValueGroup(), this.getConstraint());
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
			this.defaultTmValueTopic = TmHelper.getDefaultTmValue(this.getValueGroup());
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
			this.defaultLiteralValueTopic = TmHelper.getDefaultLiteralValue(this.getValueGroup());
			this.defaultLiteralValueTopicSet = true;
			return this.defaultLiteralValueTopic;
		}
	}
	
	
	// returns the topic that represents the default value of
	// the value-group that is bound to this element - null if it is unbound
	public Topic getDefaultValue() throws InvalidGdlSchemaException {
		if(this.getDefaultLiteralValue() != null && this.getDefaultTmValue() != null) throw new InvalidGdlSchemaException("the topic " + TmHelper.getAnyIdOfTopic(this.getValueGroup()) + " must be bound to maximal one " + PSIs.GDL.TopicType.gdlDefaultValue + ", but is: 2");
		else if(this.getDefaultLiteralValue() != null) return this.getDefaultLiteralValue();
		else return this.getDefaultTmValue();
	}
	
	
	// returns true if the default value is fixed
	// otherwise the return value is false
	public boolean fixedDefaultValue() throws InvalidGdlSchemaException{
		Topic defVal = this.getDefaultValue();

		if(defVal == null) return false;

		TopicMap tm = defVal.getTopicMap();
		Occurrence fixedOcc = TmHelper.getSingleOccurrence(defVal, TmHelper.getTopicByPsi(PSIs.GDL.OccurrenceType.gdlFixed, tm));

		if(fixedOcc != null){
			try{
				return Boolean.valueOf(fixedOcc.getValue().toLowerCase());
			}catch(Exception e){
				throw new InvalidGdlSchemaException("the occurrence of type " + PSIs.GDL.OccurrenceType.gdlFixed + " bound to the topic " + TmHelper.getAnyIdOfTopic(defVal) + " must be set to either true or false, but is: " + fixedOcc.getValue());
			}
		} else {
			return false;
		}
	}
	
	
	// returns all topic maps values represented by topics of the type gdlt:Tm-Value
	// that are valid and declared for the value-group of this element - or
	// an empty ArrayList
	public ArrayList<Topic> getTmValues() throws InvalidGdlSchemaException {
		if(this.tmValueTopicsSet){
			return this.tmValueTopics;
		} else {
			this.tmValueTopics = TmHelper.getTmValues(this.getValueGroup());
			this.tmValueTopicsSet = true;
			return this.tmValueTopics;
		}
	}
	
	
	// returns all topics that represents literal values for this value-group - or
	// an empty ArrayList
	public ArrayList<Topic> getLiteralValues() throws InvalidGdlSchemaException {
		if(this.literalValueTopicsSet){
			return this.literalValueTopics;
		} else {
			this.literalValueTopics = TmHelper.getLiteralValues(this.getValueGroup());
			this.literalValueTopicsSet = true;
			return this.literalValueTopics;
		}
	}
	
	
	// returns an ArrayList of strings that are set to a value group as literal values
	public ArrayList<String> getLiterals() throws InvalidGdlSchemaException {
		if(this.literalsSet){
			return this.literals;
		} else {
			this.literalsSet = true;
			this.literals = TmHelper.getLiterals(this.getValueGroup());
			return this.literals;
		}
	}
	
	
	public int getCardMin() throws InvalidGdlSchemaException {
		if(this.cardMinSet){
			return this.cardMin;
		} else {
			this.cardMaxSet = true;
			return TmHelper.getCardMin(this.getRootConstraint());
		}
	}


	public int getCardMax() throws InvalidGdlSchemaException {
		if(this.cardMaxSet){
			return this.cardMax;
		} else {
			this.cardMaxSet = true;
			return TmHelper.getCardMax(this.getRootConstraint());
		}
	}
	
	
	// returns the valid topic maps value for the constraint bound
	// to the value-group that is bound to this element - or an empty ArrayList
	public ArrayList<Topic> getTmValuesForConstraint() throws InvalidGdlSchemaException, ExecutionException {
		return TmHelper.getTmValuesForConstraint(this.getConstraint(), this.getValueGroup());
	}
	
	
	// returns the regular expression that is set for the constraint bound to the
	// value-group of this element
	public String getLiteralValueForConstraint() throws InvalidGdlSchemaException {
		return TmHelper.getLiteralValueForConstraint(this.getConstraint());
	}
	
	
	// returns the display-by schema that is defined for the value-group that
	// is bound to this element
	public Topic getDisplayByOfValueGroup() throws InvalidGdlSchemaException {
		if(this.displayByConstraintSet){
			return this.displayByConstraint;
		} else {
			this.displayByConstraintSet = true;
			this.displayByConstraint = TmHelper.getDisplayByTopicOf(this.getValueGroup());
			return this.displayByConstraint;
		}
	}
	
	
	// returns all locators contained in the passed locators array, which match the
	// passed regular expression value
	public ArrayList<Locator> filterLocators(String pattern, JsArray<Locator> locators){
		ArrayList<Locator> result = new ArrayList<Locator>();
		if(locators == null || locators.length() == 0) return result;
		Pattern patternObject = new Pattern(pattern == null ? ".*" : pattern);
		
		for(int i = 0; i != locators.length(); ++i){
			if(patternObject.matches(locators.get(i).getReference())) result.add(locators.get(i));
		}
		
		return result;
	}
	
	
	// returns the preferred scope that is bound to the value-group of
	// this element - or an empty ArrayList
	public ArrayList<Topic> getPreferredScopeOfValueGroup() throws InvalidGdlSchemaException {
		if(this.prefferedScopesSet){
			return this.preferredScopes;
		} else {
			this.prefferedScopesSet = true;
			this.preferredScopes = TmHelper.getPrefferedScopesForTopicOf(this.getValueGroup());
			return this.preferredScopes;
		}
	}
	
	
	// validates names, occurrences and identifiers for the passed value
	public void validateLiteralValue(String selectedValue) throws InvalidContentException, InvalidGdlSchemaException{
		if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclSubjectIdentifierConstraint) || TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclSubjectLocatorConstraint) || TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclItemIdentifierConstraint)){
			Pattern pattern = new Pattern(TmHelper.getRegExp(this.getConstraint()));
			if(!pattern.matches(selectedValue)) throw new InvalidContentException("The value \"" + selectedValue + "\" does not satisfy the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " with the regular expression: " + TmHelper.getRegExp(this.getConstraint()));
		} else {
			ArrayList<Pattern> validLiteralValues = new ArrayList<Pattern>();
			for (String literal : this.getLiterals()) validLiteralValues.add(new Pattern(literal));
			if(validLiteralValues.size() == 0) validLiteralValues.add(new Pattern(this.getLiteralValueForConstraint()));	
			int i = 0;
			for( ; i != validLiteralValues.size(); ++i) if(validLiteralValues.get(i).matches(selectedValue)) break;

			if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclTopicNameConstraint)){
				if(i == validLiteralValues.size()) throw new InvalidContentException("the user data " + selectedValue + " for the topic-name " + TmHelper.getAnyIdOfTopic(TmHelper.getConstrainedStatement(this.getConstraint())) + " does not satisfy any of the constraints: " + Utils.arrayToString(validLiteralValues));
			} else if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclTopicOccurrenceConstraint)){
				if(i == validLiteralValues.size()) throw new InvalidContentException("the user data " + selectedValue + " for the topic-occurrence " + TmHelper.getAnyIdOfTopic(TmHelper.getConstrainedStatement(this.getConstraint())) + " does not satisfy any of the constraints: " + Utils.arrayToString(validLiteralValues));
			}
		}
	}
	
	
	// validates the the tm values of a constraint
	public void validateTmValue(Topic selectedTopic) throws InvalidContentException, InvalidGdlSchemaException, ExecutionException {
		if(this.getRawTmValues().size() != 0 && !this.getRawTmValues().contains(selectedTopic)){
			throw new InvalidContentException("the topic " + TmHelper.getAnyIdOfTopic(selectedTopic) + " does not satisfy the contraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " and the topic values: " + Utils.topicArrayToString(this.getRawTmValues()));
		} else {
			if(!this.getTmValuesForConstraint().contains(selectedTopic)){
				throw new InvalidContentException("the topic " + TmHelper.getAnyIdOfTopic(selectedTopic) + " does not satisfy the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " and topic values: " + Utils.topicArrayToString(this.getTmValuesForConstraint()));
			}
		}
	}
	
	
	// returns the actual topics that are set as possible tm-values
	public ArrayList<Topic> getRawTmValues() throws InvalidGdlSchemaException{
		if(this.rawTmValuesSet){
			return this.rawTmValues;
		} else {
			this.rawTmValuesSet = true;
			ArrayList<Topic> tmValues = this.getTmValues();
			for (Topic tmValue : tmValues) this.rawTmValues = Utils.union(this.rawTmValues, TmHelper.getValuesForTmValue(tmValue));
			return this.rawTmValues;
		}
	}

	
	// handles the getContent call for subject identifiers and subject locators
	public void getTopicIdentifierContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Topic carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(!(carrier instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + carrier.getClass());
		JsArray<Locator> identifiers = null;
			
		ArrayList<Locator> filteredIdentifiers = null;
		boolean isPsiConstraint = false;
		if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclSubjectIdentifierConstraint)){
			isPsiConstraint = true;
			identifiers = carrier.getSubjectIdentifiers();
			filteredIdentifiers = this.filterLocators(TmHelper.getRegExp(this.getConstraint()), identifiers);
		} else if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclSubjectLocatorConstraint)){
			identifiers = carrier.getSubjectLocators();
			filteredIdentifiers = this.filterLocators(TmHelper.getRegExp(this.getConstraint()), identifiers);
		} else {
			throw new ExecutionException("Only the constraints " + PSIs.TMCL.tmclSubjectIdentifierConstraint + " and " + PSIs.TMCL.tmclSubjectLocatorConstraint + " are supported by the function getTopicIdentifierContent");
		}
		
		Locator changedIdentifier = null;
		if(validate) this.validateLiteralValue(this.owner.getSelectedValues().get(selectedValueIndex));
		
		if(filteredIdentifiers.size() > selectedValueIndex){
			changedIdentifier = filteredIdentifiers.get(selectedValueIndex);
			if(isPsiConstraint) carrier.removeSubjectIdentifier(changedIdentifier);
			else carrier.removeSubjectLocator(changedIdentifier);
		}
		
		changedIdentifier = carrier.getTopicMap().createLocator(this.owner.getSelectedValues().get(selectedValueIndex));
		if(isPsiConstraint) carrier.addSubjectIdentifier(changedIdentifier);
		else carrier.addSubjectLocator(changedIdentifier);
		contents.add(new Pair<Object, TopicMapsTypes>(changedIdentifier, TopicMapsTypes.Locator));
	}

	
	// handles the getContent call for item identifiers
	public void getItemIdentifierContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Construct carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		// get type
		Topic constrainedTopicType = TmHelper.getConstrainedTopicType(this.getConstraint());

		int typeIdx = -1;
		JsArray<Topic> types = null;
		if((carrier instanceof Topic)){
			types = ((Topic)carrier).getTypes();
			if(types.length() != 0){
				for(typeIdx = 0; typeIdx != types.length(); ++typeIdx) if(types.get(typeIdx).equals(constrainedTopicType)) break;
			}
		}

		JsArray<Locator> identifiers = null;
		ArrayList<Locator> filteredIdentifiers = null;
		if((carrier instanceof Topic) && types != null && typeIdx != types.length()){
			identifiers = ((Topic)carrier).getItemIdentifiers();
			filteredIdentifiers = this.filterLocators(TmHelper.getRegExp(this.getConstraint()), identifiers);
			
			Locator changedIdentifier = null;
			if(validate) this.validateLiteralValue(this.owner.getSelectedValues().get(selectedValueIndex));
			
			if(filteredIdentifiers.size() > selectedValueIndex){
				changedIdentifier = filteredIdentifiers.get(selectedValueIndex);
				((Topic)carrier).removeItemIdentifier(changedIdentifier);
			}
			
			changedIdentifier = ((Topic)carrier).getTopicMap().createLocator(this.owner.getSelectedValues().get(selectedValueIndex));
			((Topic)carrier).addItemIdentifier(changedIdentifier);
			contents.add(new Pair<Object, TopicMapsTypes>(changedIdentifier, TopicMapsTypes.Locator));
		} else {			
			// search for the topic type
			Reifiable ref = null;

			// search for the characteristics type
			if(carrier instanceof Topic){
				JsArray<Name> names = ((Topic)carrier).getNames(constrainedTopicType);
				if(names.length() != 0){
					ref = names.get(0);
				} else {
					JsArray<Occurrence> occs = ((Topic)carrier).getOccurrences(constrainedTopicType);
					if(occs.length() != 0) ref = occs.get(0);
				}
			} else if(carrier instanceof Association){
				JsArray<Role> roles = ((Association)carrier).getRoles(constrainedTopicType);
				if(roles.length() != 0) ref = roles.get(0);
			}
			if(ref == null) return;
			
			// search for item-identifiers of the found topic type or characteristics
			identifiers = ((ReifiableStub)ref).getItemIdentifiers();
			filteredIdentifiers = this.filterLocators(TmHelper.getRegExp(this.getConstraint()), identifiers);
			
			Locator changedIdentifier = null;
			if(validate) this.validateLiteralValue(this.owner.getSelectedValues().get(selectedValueIndex));
			
			if(filteredIdentifiers.size() > selectedValueIndex){
				changedIdentifier = filteredIdentifiers.get(selectedValueIndex);
				((ReifiableStub)carrier).removeItemIdentifier(changedIdentifier);
			}
			
			changedIdentifier = ((ReifiableStub)carrier).getTopicMap().createLocator(this.owner.getSelectedValues().get(selectedValueIndex));
			((ReifiableStub)carrier).addItemIdentifier(changedIdentifier);
			contents.add(new Pair<Object, TopicMapsTypes>(changedIdentifier, TopicMapsTypes.Locator));
		}
	}

	
	// handles the getContent call for item identifiers of variant-names
	public void getVariantIdentifierContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Topic carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(!(carrier instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + carrier.getClass());
		ArrayList<Variant> possibleVariants = TmHelper.getVariantsForConstraint(carrier, this.getRootConstraint());		
		if(possibleVariants.size() != 0){
			Variant variant = possibleVariants.get(0);
			JsArray<Locator> identifiers = null;

			ArrayList<Locator> filteredIdentifiers = null;

			identifiers = variant.getItemIdentifiers();
			filteredIdentifiers = this.filterLocators(TmHelper.getRegExp(this.getConstraint()), identifiers);

			Locator changedIdentifier = null;
			if(validate) this.validateLiteralValue(this.owner.getSelectedValues().get(selectedValueIndex));

			if(filteredIdentifiers.size() > selectedValueIndex){
				changedIdentifier = filteredIdentifiers.get(selectedValueIndex);
				variant.removeItemIdentifier(changedIdentifier);
			}

			changedIdentifier = variant.getTopicMap().createLocator(this.owner.getSelectedValues().get(selectedValueIndex));
			variant.addItemIdentifier(changedIdentifier);
			contents.add(new Pair<Object, TopicMapsTypes>(changedIdentifier, TopicMapsTypes.Locator));
		}
	}

	
	// handles the getContent call for item identifiers of variant-names
	public void getVariantReifierContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Topic carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(!(carrier instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + carrier.getClass());
		ArrayList<Variant> possibleVariants = TmHelper.getVariantsForConstraint(carrier, this.getRootConstraint());		
		if(possibleVariants.size() != 0){
			Variant variant = possibleVariants.get(0);
			Topic reifier = TmHelper.getTopicFromStringRepresentation(this.owner.getSelectedValues().get(selectedValueIndex), this.getValueGroup());
			
			if(reifier != null && !variant.getReifier().equals(reifier)) variant.setReifier(reifier);
			contents.add(new Pair<Object, TopicMapsTypes>(variant, TopicMapsTypes.Variant));
		}
	}

	
	// handles the getContent call for occurrence and name values
	public void getTopicCharacteristicContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Topic carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(!(carrier instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + carrier.getClass());
		Topic characteristicType = TmHelper.getConstrainedStatement(this.getConstraint());
		
		boolean isOccConstraint = true;
		if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclTopicOccurrenceConstraint)){
			isOccConstraint = true;
		} else if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclTopicNameConstraint)){
			isOccConstraint = false;
		} else {
			throw new ExecutionException("the function getTopicCharacteristicContent can operate only on constraints of the type " + PSIs.TMCL.tmclTopicOccurrenceConstraint + " or " + PSIs.TMCL.tmclTopicNameConstraint + ", but is called with " + TmHelper.getAnyIdOfTopic(this.getConstraint()));
		}
		
		JsArray<Name> names = null;
		JsArray<Occurrence> occurrences = null;
		if(isOccConstraint) occurrences = carrier.getOccurrences(characteristicType);
		else names = carrier.getNames(characteristicType);
		

		Construct changedConstruct = null;
		if(validate) this.validateLiteralValue(this.owner.getSelectedValues().get(selectedValueIndex)); 

		if(isOccConstraint){
			if(occurrences.length() > selectedValueIndex){
				changedConstruct = occurrences.get(selectedValueIndex);
				((Occurrence)changedConstruct).setValue(this.owner.getSelectedValues().get(selectedValueIndex));
			}else {
				changedConstruct = carrier.createOccurrence(characteristicType, this.owner.getSelectedValues().get(selectedValueIndex), null);
			}
		} else {
			if(names.length() > selectedValueIndex){
				changedConstruct = names.get(selectedValueIndex);
				((Name)changedConstruct).setValue(this.owner.getSelectedValues().get(selectedValueIndex));
			}else {
				changedConstruct = carrier.createName(this.owner.getSelectedValues().get(selectedValueIndex), characteristicType, null);
			}
		}
		contents.add(new Pair<Object, TopicMapsTypes>(changedConstruct, isOccConstraint ? TopicMapsTypes.Occurrence : TopicMapsTypes.Name));
	}
	
	
	// handles the getContent call for subject identifiers and subject locators
	public void getVariantNameContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Topic carrier, int selectedValueIndex, ArrayList<Variant> variants) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(variants == null) return;
		
		Variant changedVariant = null;
		if(validate) this.validateLiteralValue(this.owner.getSelectedValues().get(selectedValueIndex)); 

		if(variants.size() > selectedValueIndex){
			changedVariant = variants.get(selectedValueIndex);
			changedVariant.setValue(this.owner.getSelectedValues().get(selectedValueIndex));
		}else {
			Topic nameType = TmHelper.getConstrainedStatement(this.getConstraint());
			JsArray<Name> names = carrier.getNames(nameType);
			Name owner = names.get(0);
			Topic scope = TmHelper.getConstrainedScopeTopic(this.getConstraint());
			@SuppressWarnings("unchecked")
			JsArray<Topic> scopes = (JsArray<Topic>) JsArray.createArray();
			scopes.push(scope);
			changedVariant = owner.createVariant(this.owner.getSelectedValues().get(selectedValueIndex), scopes);
		}
		contents.add(new Pair<Object, TopicMapsTypes>(changedVariant, TopicMapsTypes.Variant));
	}
	
	
	// handles the getContent call for role players
	public void getRolePlayerContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Association carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(!(carrier instanceof Association)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to an Association, but is: " + carrier.getClass());
		
		if(!TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclTopicRoleConstraint))throw new InvalidGdlSchemaException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a root constraint of the type " + PSIs.TMCL.tmclTopicRoleConstraint + ", but is: " + TmHelper.getAnyIdOfTopic(this.getRootConstraint()));
		Pair<Topic, Topic> roleAndPlayerType = TmHelper.getConstrainedRoleAndPlayerTypeOfConstraint(this.getRootConstraint());
		Topic roleType = roleAndPlayerType.getFirst();
		Topic playerType = roleAndPlayerType.getSecond();

		JsArray<Role> typedRoles = carrier.getRoles(roleType);
		ArrayList<Role> roles = new ArrayList<Role>();
		for(int i = 0; i != typedRoles.length(); ++i)
			if(TmHelper.isInstanceOf(typedRoles.get(i).getPlayer(), playerType)) roles.add(typedRoles.get(i));

		Role changedRole = null;

		Topic player = TmHelper.getTopicFromStringRepresentation(this.owner.getSelectedValues().get(selectedValueIndex), this.getValueGroup());
		if(validate) this.validateTmValue(player);
		if(roles.size() > selectedValueIndex){
			changedRole = roles.get(selectedValueIndex);
			changedRole.setPlayer(player);
		} else {
			changedRole = carrier.createRole(roleType, player);
		}
		contents.add(new Pair<Object, TopicMapsTypes>(changedRole, TopicMapsTypes.Role));
	}

	
	// handles the getContent call for scope topics of variant-names
	public void getVariantNameScopeContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Topic carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(!(carrier instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + carrier.getClass());
		ArrayList<Variant> possibleVariants = TmHelper.getVariantsForConstraint(carrier, this.getRootConstraint());		
		if(possibleVariants.size() != 0){
			Variant variant = possibleVariants.get(0);
			JsArray<Topic> scopes = variant.getScope();
			
			if(validate) this.validateLiteralValue(this.owner.getSelectedValues().get(selectedValueIndex));

			if(scopes.length() > selectedValueIndex){
				Topic oldScope = scopes.get(selectedValueIndex);
				Topic newScope = TmHelper.getTopicFromStringRepresentation(this.owner.getSelectedValues().get(selectedValueIndex), this.getValueGroup());
				
				if(!oldScope.equals(newScope)){
					variant.removeTheme(oldScope);
					variant.addTheme(newScope);
				}
			}
			contents.add(new Pair<Object, TopicMapsTypes>(variant, TopicMapsTypes.Variant));
		}
	}
	
	
	// handles the getContent call for scope topics
	public void getScopeContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Construct carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		Topic type = TmHelper.getConstrainedStatement(this.getConstraint());

		JsArray<Topic> scopes = null;
		Construct owner = null;
		TopicMapsTypes ownerType = null;
		if(carrier instanceof Topic){
			JsArray<Name> names = ((Topic)carrier).getNames(type);
			if(names.length() != 0){
				scopes = names.get(0).getScope();
				owner = names.get(0);
				ownerType = TopicMapsTypes.Name;
			}
			if(scopes == null){
				JsArray<Occurrence> occs = ((Topic)carrier).getOccurrences(type);
				if(occs.length() != 0){
					scopes = occs.get(0).getScope();
					owner = occs.get(0);
					ownerType = TopicMapsTypes.Occurrence;
				}
			}
		} else if(carrier instanceof Association){
			if(((Association)carrier).getType().equals(type)){
				scopes = ((Association)carrier).getScope();
				owner = carrier;
				ownerType = TopicMapsTypes.Association;
			}
		} else {
			throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a topic or association, but is: " + carrier.getClass());
		}
		
		Topic newScope = TmHelper.getTopicFromStringRepresentation(this.owner.getSelectedValues().get(selectedValueIndex), this.getValueGroup());
		Topic oldScope = null;
		if(scopes.length() > selectedValueIndex){
			oldScope = scopes.get(selectedValueIndex);
			if(!newScope.equals(oldScope)){
				((ScopedStub)owner).removeTheme(oldScope);
				((ScopedStub)owner).addTheme(newScope);
			}
		} else {
			((ScopedStub)owner).addTheme(newScope);
		}
		contents.add(new Pair<Object, TopicMapsTypes>(owner, ownerType));
	}
	
	
	// handles the getContent call for a reifier topic
	public void getReifierContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Construct carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		Topic type = TmHelper.getConstrainedStatement(this.getConstraint());

		Construct owner = null;
		TopicMapsTypes ownerType = null;
		if(carrier instanceof Topic){
			JsArray<Name> names = ((Topic)carrier).getNames(type);
			if(names.length() != 0){
				owner = names.get(0);
				ownerType = TopicMapsTypes.Name;
			} else {
				JsArray<Occurrence> occs = ((Topic)carrier).getOccurrences(type);
				if(occs.length() != 0){
					owner = occs.get(0);
					ownerType = TopicMapsTypes.Occurrence;
				}
			}
		} else if(carrier instanceof Association){
			if(((Association)carrier).getType().equals(type)){
				owner = carrier;
				ownerType = TopicMapsTypes.Association;
			} else {
				JsArray<Role> roles = ((Association)carrier).getRoles(type);
				if(roles.length() != 0){
					owner = roles.get(0);
					ownerType = TopicMapsTypes.Role;
				}
			}
		} else {
			throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a topic or association, but is: " + carrier.getClass());
		}
		
		Topic newReifier = TmHelper.getTopicFromStringRepresentation(this.owner.getSelectedValues().get(selectedValueIndex), this.getValueGroup());
		if(!newReifier.equals(((ReifiableStub)owner).getReifier())) ((ReifiableStub)owner).setReifier(newReifier);
		contents.add(new Pair<Object, TopicMapsTypes>(owner, ownerType));
	}
	
	
	// handles the getContent call for a Datatye value
	public void getDatatypeContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Construct carrier, int selectedValueIndex, ArrayList<Variant> variants, ArrayList<Occurrence> occurrences) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		Construct owner = null;
		TopicMapsTypes ownerType = null;
		if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclVariantNameConstraint)){
			if(variants.size() != 0){
				owner = variants.get(0);
				variants.get(0).setValue(variants.get(0).getValue(), variants.get(0).getTopicMap().createLocator(this.owner.getSelectedValues().get(selectedValueIndex)));
				ownerType = TopicMapsTypes.Variant;
			}
		} else if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclOccurrenceConstraint)){
			if(occurrences.size() != 0){
				owner = occurrences.get(0);
				occurrences.get(0).setValue(occurrences.get(0).getValue(), occurrences.get(0).getTopicMap().createLocator(this.owner.getSelectedValues().get(selectedValueIndex)));
				ownerType = TopicMapsTypes.Occurrence;
			}
		} else {
			throw new InvalidGdlSchemaException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a constraint of the type " + PSIs.TMCL.tmclVariantNameConstraint + " or " + PSIs.TMCL.tmclOccurrenceConstraint + ",  but is bound to: " + TmHelper.getAnyIdOfTopic(this.getRootConstraint()));
		}		
		contents.add(new Pair<Object, TopicMapsTypes>(owner, ownerType));
	}
}
