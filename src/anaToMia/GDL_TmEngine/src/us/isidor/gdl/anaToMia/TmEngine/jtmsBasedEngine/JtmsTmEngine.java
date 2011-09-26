package us.isidor.gdl.anaToMia.TmEngine.jtmsBasedEngine;

import java.util.ArrayList;
import com.google.gwt.core.client.JsArray;
import com.google.gwt.core.client.JsonUtils;
import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.json.client.JSONValue;
import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.ExporterException;
import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.FormatException;
import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.MissingReference;
import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.TmEngine;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Variant;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMap;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Role;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.ReifiableStub;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Reifiable;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Name;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Locator;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Association;


public class JtmsTmEngine implements TmEngine{
	private final String xsd = "http://www.w3.org/2001/XMLSchema#";
	private final String xsdString = "http://www.w3.org/2001/XMLSchema#string";
	private final String defaultNameTypePsi = "http://psi.topicmaps.org/iso13250/model/topic-name";
	private ArrayList<TopicMap> tms = new ArrayList<TopicMap>();


	public Topic createDefaultNameType(TopicMap tm){
		Locator psi = tm.createLocator(defaultNameTypePsi);
		return tm.createTopicBySubjectIdentifier(psi);
	}
	
	
	// this method merges all duplicates in the passed topic map
	private final native void sanitize(TopicMap tm) /*-{
		// tmjs seems to have a bug when merging duplicates, i.e.
		// some invocations of sanitize does not have an effect, whereas
		// the next invacation has, so this function calls at least 5
		// times the method sanitize, although the tm engine merging
		// does not merge any construct 
		
		var tops = tm.getTopics().length;
		var assocs = tm.getAssociations().length;
	
	    var count = 0;
	    while(count != 5){
	   		tm.sanitize();
	   		var loopTops = tm.getTopics().length;
	   		var loopAssocs = tm.getTopics().length;
	
	   		if(tops === loopTops && assocs === loopAssocs) ++count;
	   		else count = 0;
	   		tops = loopTops;
	   		assocs = loopAssocs;
	    }	
	}-*/;


	private final native TopicMap jsCreateTopicMap(String tmLocator) /*-{
		var factory, sys, tmid, tm;
		factory = $wnd.TopicMapSystemFactory.newInstance();
		factory.setProperty('com.semanticheadache.tmjs.backend', 'memory');
		sys = factory.newTopicMapSystem();
		tmid = sys.createLocator(tmLocator);
		tm = sys.createTopicMap(tmid);

		return tm;
	}-*/;


	@Override
	public TopicMap createTopicMap(String tmLocator){
		for (TopicMap tm : tms) {
			if(tm.getLocator().getReference().equals(tmLocator)){
				return tm;
			}
		}

		TopicMap tm = jsCreateTopicMap(tmLocator);
		tms.add(tm);
		return tm;
	}


	@Override
	public TopicMap getTopicMap(String tmLocator) {
		TopicMap match = null;

		for (TopicMap tm : tms) {
			if(tm.getLocator().getReference().equals(tmLocator)){
				match = tm;
				break;
			}
		}

		return match;
	}


	@Override
	public TopicMap[] getTopicMaps() {
		TopicMap[] tmsArray = new TopicMap[tms.size()];
		tms.toArray(tmsArray);
		return tmsArray;
	}


	@Override
	public TopicMap importTopicMap(String tmData, TopicMap tm) throws FormatException, MissingReference {
		TopicMap importedTm = importJTM11TopicMap(tmData, tm);
		this.sanitize(importedTm);
		return importedTm;
	}


	@Override
	public Topic importTopic(String tmData, TopicMap tm) throws FormatException, MissingReference {
		Topic top = importJTM11Topic(tmData, tm);
		this.sanitize(tm);
		return top;
	}


	@Override
	public Name importName(String tmData, TopicMap tm) throws FormatException, MissingReference {
		Name name = importJTM11Name(tmData, tm);
		this.sanitize(tm);
		return name;
	}


	@Override
	public Variant importVariant(String tmData, TopicMap tm) throws FormatException, MissingReference {
		Variant var =  importJTM11Variant(tmData, tm);
		this.sanitize(tm);
		return var;
	}


	@Override
	public Occurrence importOccurrence(String tmData, TopicMap tm) throws FormatException, MissingReference {
		Occurrence occ = importJTM11Occurrence(tmData, tm);
		this.sanitize(tm);
		return occ;
	}


	@Override
	public Association importAssociation(String tmData, TopicMap tm) throws FormatException, MissingReference {
		Association assoc =  importJTM11Association(tmData, tm);
		return assoc;
	}


	@Override
	public Role importRole(String tmData, TopicMap tm) throws FormatException, MissingReference {
		Role role = importJTM11Role(tmData, tm);
		this.sanitize(tm);
		return role;
	}


	@Override
	public String exportTm(TopicMap tm) throws ExporterException {
		return exportJTM11(tm, new Prefixes(tm), true);
	}


	@Override
	public String exportTm(Topic topic) throws ExporterException {
		return exportJTM11(topic, new Prefixes(topic), true);
	}


	@Override
	public String exportTm(Name name) throws ExporterException {
		return exportJTM11(name, new Prefixes(name), true);
	}


	@Override
	public String exportTm(Variant variant) throws ExporterException {
		return exportJTM11(variant, new Prefixes(variant), true);
	}


	@Override
	public String exportTm(Occurrence occurrence) throws ExporterException {
		return exportJTM11(occurrence, new Prefixes(occurrence), true);
	}


	@Override
	public String exportTm(Association association) throws ExporterException {
		return exportJTM11(association, new Prefixes(association), true);
	}


	@Override
	public String exportTm(Role role) throws ExporterException {
		return exportJTM11(role, new Prefixes(role), true);
	}


	private JSONObject checkJTMFormat(String jtm) throws FormatException{
		// create a JSONObject from the json string
		JSONValue jsonValue = JSONParser.parseStrict(jtm);

		JSONObject jsonObject = jsonValue.isObject();
		if(jsonObject == null){
			throw new FormatException("expected a JSON object");
		}

		// get the JTM version
		String version = null;
		if(jsonObject.containsKey("version")){
			version = getStringValue(jsonObject, "version");
		}

		if(!version.equals("1.1")){
			throw new FormatException("\"version\" field must be set to \"1.1\"");
		}

		return jsonObject;
	}


	private TopicMap importJTM11TopicMap(String jtm, TopicMap tm) throws FormatException, MissingReference {
		JSONObject jsonObject = checkJTMFormat(jtm);

		// get the item_type
		String item_type = null;
		if(jsonObject.containsKey("item_type")){
			item_type = getStringValue(jsonObject, "item_type");
		}

		// get all prefixes
		Prefixes prefixes;
		if(jsonObject.containsKey("prefixes")){
			prefixes = new Prefixes(jsonObject.get("prefixes").isObject());
		} else {
			prefixes = new Prefixes();
		}

		if(item_type == null || !item_type.equals("topicmap")){
			throw new FormatException("expected a topicmap object, but \"item_type\" is not set to \"topicmap\"");
		}else {
			return importJTM11TopicMap(jsonObject, tm, prefixes);
		}
	}


	private Topic importJTM11Topic(String jtm, TopicMap tm) throws FormatException, MissingReference {
		JSONObject jsonObject = checkJTMFormat(jtm);

		// get the item_type
		String item_type = null;
		if(jsonObject.containsKey("item_type")){
			item_type = getStringValue(jsonObject, "item_type");
		}

		// get all prefixes
		Prefixes prefixes;
		if(jsonObject.containsKey("prefixes")){
			prefixes = new Prefixes(jsonObject.get("prefixes").isObject());
		} else {
			prefixes = new Prefixes();
		}

		if(item_type == null || !item_type.equals("topic")){
			throw new FormatException("expected a topic object, but \"item_type\" is not set to \"topic\"");
		}else {
			return importJTM11Topic(jsonObject, tm, prefixes);
		}
	}


	private Name importJTM11Name(String jtm, TopicMap tm) throws FormatException, MissingReference {
		JSONObject jsonObject = checkJTMFormat(jtm);

		// get the item_type
		String item_type = null;
		if(jsonObject.containsKey("item_type")){
			item_type = getStringValue(jsonObject, "item_type");
		}

		// get all prefixes
		Prefixes prefixes;
		if(jsonObject.containsKey("prefixes")){
			prefixes = new Prefixes(jsonObject.get("prefixes").isObject());
		} else {
			prefixes = new Prefixes();
		}

		if(item_type == null || !item_type.equals("name")){
			throw new FormatException("expected a name object, but \"item_type\" is not set to \"name\"");
		}else {
			return importJTM11Name(null, jsonObject, tm, prefixes);
		}
	}


	private Variant importJTM11Variant(String jtm, TopicMap tm) throws FormatException, MissingReference {
		JSONObject jsonObject = checkJTMFormat(jtm);

		// get the item_type
		String item_type = null;
		if(jsonObject.containsKey("item_type")){
			item_type = getStringValue(jsonObject, "item_type");
		}

		// get all prefixes
		Prefixes prefixes;
		if(jsonObject.containsKey("prefixes")){
			prefixes = new Prefixes(jsonObject.get("prefixes").isObject());
		} else {
			prefixes = new Prefixes();
		}

		if(item_type == null || !item_type.equals("variant")){
			throw new FormatException("expected a variant object, but \"item_type\" is not set to \"variant\"");
		}else {
			return importJTM11Variant(null, jsonObject, tm, prefixes);
		}
	}


	private Occurrence importJTM11Occurrence(String jtm, TopicMap tm) throws FormatException, MissingReference {
		JSONObject jsonObject = checkJTMFormat(jtm);

		// get the item_type
		String item_type = null;
		if(jsonObject.containsKey("item_type")){
			item_type = getStringValue(jsonObject, "item_type");
		}

		// get all prefixes
		Prefixes prefixes;
		if(jsonObject.containsKey("prefixes")){
			prefixes = new Prefixes(jsonObject.get("prefixes").isObject());
		} else {
			prefixes = new Prefixes();
		}

		if(item_type == null || !item_type.equals("occurrence")){
			throw new FormatException("expected an occurrence object, but \"item_type\" is not set to \"occurrence\"");
		}else {
			return importJTM11Occurrence(null, jsonObject, tm, prefixes);
		}
	}


	private Association importJTM11Association(String jtm, TopicMap tm) throws FormatException, MissingReference {
		JSONObject jsonObject = checkJTMFormat(jtm);

		// get the item_type
		String item_type = null;
		if(jsonObject.containsKey("item_type")){
			item_type = getStringValue(jsonObject, "item_type");
		}

		// get all prefixes
		Prefixes prefixes;
		if(jsonObject.containsKey("prefixes")){
			prefixes = new Prefixes(jsonObject.get("prefixes").isObject());
		} else {
			prefixes = new Prefixes();
		}

		if(item_type == null || !item_type.equals("association")){
			throw new FormatException("expected an association object, but \"item_type\" is not set to \"association\"");
		}else {
			return importJTM11Association(jsonObject, tm, prefixes);
		}
	}


	private Role importJTM11Role(String jtm, TopicMap tm) throws FormatException, MissingReference {
		JSONObject jsonObject = checkJTMFormat(jtm);

		// get the item_type
		String item_type = null;
		if(jsonObject.containsKey("item_type")){
			item_type = getStringValue(jsonObject, "item_type");
		}

		// get all prefixes
		Prefixes prefixes;
		if(jsonObject.containsKey("prefixes")){
			prefixes = new Prefixes(jsonObject.get("prefixes").isObject());
		} else {
			prefixes = new Prefixes();
		}

		if(item_type == null || !item_type.equals("role")){
			throw new FormatException("expected a role object, but \"item_type\" is not set to \"role\"");
		}else {
			return importJTM11Role(null, jsonObject, tm, prefixes);
		}
	}


	private TopicMap importJTM11TopicMap(JSONObject jtm, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference{
		// the received topic map's item identifiers are ignored, the tm's locator is used instead

		// import all topics
		if(jtm.containsKey("topics")){
			JSONValue jsonValue = jtm.get("topics");
			JSONArray jsonTopics = jsonValue.isArray();
			if(jsonTopics != null){
				ArrayList<Topic> topics = new ArrayList<Topic>(); 
				for(int i = 0; i != jsonTopics.size(); ++i){
					JSONObject topicObject = jsonTopics.get(i).isObject();
					if(topicObject == null){
						throw new FormatException("the field \"topics\" must be an array of topic objects");
					} else {
						topics.add(importTopicStubFromTm(topicObject, tm, prefixes));
					}
				}

				// get all instance_of topics of all topics
				for(int i = 0; i != topics.size(); ++i){
					importTopicCharacteristicsFromTm(topics.get(i), jsonTopics.get(i).isObject(), tm, prefixes);
				}
			}
		}

		// import all associations
		if(jtm.containsKey("associations")){
			JSONValue jsonValue = jtm.get("associations");
			JSONArray associations = jsonValue.isArray();
			if(associations != null){
				for(int i = 0; i != associations.size(); ++i){
					importJTM11Association(associations.get(i).isObject(), tm, prefixes);
				}
			}
		}

		// set the topic map's reifier
		if(jtm.containsKey("reifier")){
			Topic reifier = getReferenced(jtm.get("reifier"), tm, prefixes);
			tm.setReifier(reifier);
		}

		return tm;
	}


	private Topic getReferenced(JSONValue reference, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference{
		if(reference.isNull() != null) return null;
		
		JSONString stringValue = reference.isString();
		if(stringValue == null){
			throw new FormatException("expected a string value as topic reference, but found \"" + reference.toString() + "\"");
		}

		return getReferenced(stringValue.stringValue(), tm, prefixes);
	}


	private Topic getReferenced(String reference, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference {
		Topic match = null;
		if(reference.startsWith("si:")){
			String si = unCurie(reference.substring(3), prefixes);
			match = tm.getTopicBySubjectIdentifier(tm.createLocator(si));
		} else if(reference.startsWith("sl:")){
			String sl = unCurie(reference.substring(3), prefixes);
			match = tm.getTopicBySubjectLocator(tm.createLocator(sl));
		} else if(reference.startsWith("ii:")){
			String ii = unCurie(reference.substring(3), prefixes);
			match = (Topic)tm.getConstructByItemIdentifier(tm.createLocator(ii));
		} else {
			throw new FormatException("expected a string value as topic reference that starts with one of \"si:\", \"sl:\", \"ii:\"");
		}

		if(match == null){
			throw new MissingReference("cannot find the Topic: \"" + reference + "\"");
		}

		return match;
	}


	private ArrayList<Topic> getReferenceds(JSONValue references, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference {
		ArrayList<String> stringReferences = new ArrayList<String>();

		JSONArray stringValues = references.isArray();
		if(references.isNull() != null){
			return getReferenceds(stringReferences, tm, prefixes);
		}
		
		if(stringValues == null){
			throw new FormatException("expected an array of strings as topic references, but found \"" + references.toString() + "\"");
		}

		for(int i = 0; i != stringValues.size(); ++i){
			JSONValue stringValue = stringValues.get(i);
			JSONString string = stringValue.isString();
			if(string == null){
				throw new FormatException("expected an array of strings as topic references, but found \"" + references.toString() + "\"");
			} else {
				stringReferences.add(string.stringValue());
			}
		}

		return getReferenceds(stringReferences, tm, prefixes);
	}


	private ArrayList<Topic> getReferenceds(ArrayList<String> references, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference {
		ArrayList<Topic> values = new ArrayList<Topic>();

		for(String reference : references){
			values.add(getReferenced(reference, tm, prefixes));
		}

		return values;
	}


	private String unCurie(String curie, Prefixes prefixes) throws FormatException{
		if(curie.startsWith("[") && curie.endsWith("]")){
			String[] strs = curie.substring(1).split(":");
			if(strs.length != 2){
				throw new FormatException("expected a CURIE reference, but found: \"" + curie + "\"");
			}

			return prefixes.getFullUri(strs[0], strs[1].substring(0, strs[1].length() - 1));
		} else {
			return curie;
		}
	}


	private Topic importTopicStubFromTm(JSONObject topic, TopicMap tm, Prefixes prefixes) throws FormatException {	
		// get all subject_identifiers
		ArrayList<String> subjectIdentifiers = new ArrayList<String>();
		for (String si : getStringsOfJSONArray(topic, "subject_identifiers")) {
			subjectIdentifiers.add(unCurie(si, prefixes));
		}

		// get all subject_locators
		ArrayList<String> subjectLocators = new ArrayList<String>();
		for (String sl : getStringsOfJSONArray(topic, "subject_locators")) {
			subjectLocators.add(unCurie(sl, prefixes));
		}

		// get all item_identifiers
		ArrayList<String> itemIdentifiers = new ArrayList<String>();
		for (String ii : getStringsOfJSONArray(topic, "item_identifiers")) {
			itemIdentifiers.add(unCurie(ii, prefixes));
		}
		
		Topic top = null;
		if(subjectIdentifiers.size() != 0){
			top = tm.createTopicBySubjectIdentifier(tm.createLocator(subjectIdentifiers.get(0)));
			for(int i = 1; i != subjectIdentifiers.size(); ++i){
				top.addSubjectIdentifier(tm.createLocator(subjectIdentifiers.get(i)));
			}

			for (String itemIdentifier : itemIdentifiers) {
				top.addItemIdentifier(tm.createLocator(itemIdentifier));
			}
			for (String subjectLocator : subjectLocators) {
				top.addSubjectLocator(tm.createLocator(subjectLocator));
			}
		} else if (subjectLocators.size() != 0) {
			top = tm.createTopicBySubjectLocator(tm.createLocator(subjectLocators.get(0)));
			for(int i = 1; i != subjectLocators.size(); ++i){
				top.addSubjectLocator(tm.createLocator(subjectLocators.get(i)));
			}
			for (String itemIdentifier : itemIdentifiers) {
				top.addItemIdentifier(tm.createLocator(itemIdentifier));
			}
		} else if (itemIdentifiers.size() != 0) {
			top = tm.createTopicByItemIdentifier(tm.createLocator(itemIdentifiers.get(0)));
			for(int i = 1; i != itemIdentifiers.size(); ++i){
				top.addItemIdentifier(tm.createLocator(itemIdentifiers.get(i)));
			}
		} else {
			throw new FormatException("a topic must have at least one identifier");
		}
		
		return top;
	}


	private ArrayList<String> getStringsOfJSONArray(JSONObject object, String key) throws FormatException{
		ArrayList<String> values = new ArrayList<String>();

		if(object.containsKey(key)){
			JSONValue jsonValue = object.get(key);
			if(jsonValue.isNull() != null){
				// the value must be null, so return an emtpy ArrayList
				return values;
			} else {
				JSONArray jsonArray = jsonValue.isArray();
				if(jsonArray == null){
					throw new FormatException("the field \"" + key + "\" must be a string array, but is " + jsonValue);
				} else {
					for(int i = 0; i != jsonArray.size(); ++i){
						JSONValue itemValue = jsonArray.get(i);
						JSONString itemString = itemValue.isString();
						if(itemString == null){
							throw new FormatException("the field \"" + key + "\" must be a string array, but is " + jsonValue);
						} else {
							values.add(itemString.stringValue());
						}
					}
				}
			}
		}

		return values;
	}


	private ArrayList<JSONObject> getObjectsOfJSONArray(JSONObject object, String key) throws FormatException {
		ArrayList<JSONObject> values = new ArrayList<JSONObject>();

		if(object.containsKey(key)){
			JSONValue jsonValues = object.get(key);
			JSONArray jsonArray = jsonValues.isArray();
			if(jsonValues.isNull() != null){
				// the value must be null, so return an emtpy ArrayList
				return values;
			} else {
				if(jsonArray == null){
					throw new FormatException("the field \"" + key + "\" must be an array of JSON objects");
				} else {
					for(int i = 0; i != jsonArray.size(); ++i){
						JSONValue jsonValue = jsonArray.get(i);
						JSONObject jsonObject = jsonValue.isObject();
						if(jsonObject == null){
							throw new FormatException("the field \"" + key + "\" must be an array of JSON objects");
						} else {
							values.add(jsonObject);
						}
					}
				}
			}
		}

		return values;
	}


	private void importTopicCharacteristicsFromTm(Topic topic, JSONObject jsonTopic, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference{
		// set instance_of of this topic
		for (String instanceOf : getStringsOfJSONArray(jsonTopic, "instance_of")) {
			topic.addType(getReferenced(instanceOf, tm, prefixes));
		}

		// set all names of this topic
		for (JSONObject name : getObjectsOfJSONArray(jsonTopic, "names")) {
			importJTM11Name(topic, name, tm, prefixes);
		}

		// set all occurrences of this topic
		for (JSONObject occurrence : getObjectsOfJSONArray(jsonTopic, "occurrences")) {
			importJTM11Occurrence(topic, occurrence, tm, prefixes);
		}
	}


	private Topic importJTM11Topic(JSONObject jtm, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference{
		// the parent item is ignored, the passed tm is set as parent item
		Topic topic = importTopicStubFromTm(jtm, tm, prefixes);
		importTopicCharacteristicsFromTm(topic, jtm, tm, prefixes);
		return topic;
	}


	private static JsArray<Topic> toJsArray(ArrayList<Topic> topics){
		JsArray<Topic> tops = null;

		if(topics == null || topics.size() == 0){
			tops = appendToJsArray(null, null);
		} else {
			for (Topic topic : topics) {
				tops = appendToJsArray(tops, topic);
			}
		}

		return tops;
	}


	private static final native JsArray<Topic> appendToJsArray(JsArray<Topic> topics, Topic topic) /*-{
		var array;
		if(topics === null){
			array = new $wnd.Array();
		} else {
			array = topics;
		}
		if(topic){
			array.push(topic);
		}
		return array;
	}-*/;


	private Topic getParent(JSONValue parentValue, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference {
		JSONArray parents = parentValue.isArray();
		if(parents == null){
			throw new FormatException("the field \"parent\" must be an array of topic identifiers");
		} else if (parents.size() == 1){
			return getReferenced(parents.get(0), tm, prefixes);
		} else {
			throw new FormatException("the \"parent\" field must contain one identfier, if referenceing a topic");
		}
	}
	
	
	private ArrayList<Construct> getParents(JSONValue parentValue, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference{
		ArrayList<Construct> values = new ArrayList<Construct>();

		JSONArray parents = parentValue.isArray();
		if(parents == null){
			throw new FormatException("the field \"parent\" must be an array of item identifiers");
		} else {
			for(int i = 0; i != parents.size(); ++i){
				JSONValue iiValue = parents.get(i);
				JSONString iiString = iiValue.isString();
				if(iiString == null){
					throw new FormatException("the field \"parent\" must be an array of item identifiers");
				}else {
					Construct construct = tm.getConstructByItemIdentifier(tm.createLocator(unCurie(iiString.stringValue(), prefixes)));
					if(construct == null){
						throw new MissingReference("cannot find the construct \"" + unCurie(iiString.stringValue(), prefixes) + "\"");
					}
					if(!values.contains(construct)){
						values.add(construct);
					}
				}
			}
		}

		return values;
	}


	private Name importJTM11Name(Topic parent, JSONObject jtm, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference{
		Topic topic = null;
		if(parent != null){
			topic = parent;
		}else {
			if(jtm.containsKey("parent")){
				Topic parentTopic = getParent(jtm.get("parent"), tm, prefixes);
				if(parentTopic == null){
					throw new FormatException("a name must contain the field \"parent\" with an array of exactly one topic reference");
				} else {
					topic = parentTopic;
				}
			} else {
				throw new FormatException("a name must contain the field \"parent\" with an array of string values");
			}
		}

		// get name value
		String value = "";
		if(jtm.containsKey("value")){
			value = getStringValue(jtm, "value");
		} else {
			throw new FormatException("a name must contain the field \"value\" with a string value");
		}

		// get type
		Topic type = null;
		if(jtm.containsKey("type")){
			type = getReferenced(jtm.get("type"), tm, prefixes);
		}

		// get scope
		ArrayList<Topic> scope = new ArrayList<Topic>();
		if(jtm.containsKey("scope")){
			scope = getReferenceds(jtm.get("scope"), tm, prefixes);
		}
		JsArray<Topic> jsScope = toJsArray(scope);

		Name name = null;
		if(type != null){
			name = topic.createName(value, type, jsScope);
		} else {
			name = topic.createName(value, jsScope);
		}

		// get item_identifiers
		for (String ii : getStringsOfJSONArray(jtm, "item_identifiers")) {
			name.addItemIdentifier(tm.createLocator(unCurie(ii, prefixes)));
		}

		// set reifier
		if(jtm.containsKey("reifier")){
			Topic reifier = getReferenced(jtm.get("reifier"), tm, prefixes);
			name.setReifier(reifier);
		}

		// get variants
		for (JSONObject variant : getObjectsOfJSONArray(jtm, "variants")) {
			importJTM11Variant(name, variant, tm, prefixes);
		}

		return name;
	}


	private String getStringValue(JSONObject jtm, String key) throws FormatException{
		if(jtm.containsKey(key)){
			JSONValue stringValue = jtm.get(key);
			JSONString stringString = stringValue.isString();
			if(stringString == null){
				throw new FormatException("the field \"" + key + "\" must contain a string value, but is: \"" + stringValue.toString() + "\"");
			} else {
				return stringString.stringValue();
			}
		} else {
			return null;
		}
	}


	private Variant importJTM11Variant(Name parent, JSONObject jtm, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference{
		// set the variant parent
		Name name = null;
		if(parent != null){
			name = parent;
		}else {
			if(jtm.containsKey("parent")){
				ArrayList<Construct> parents = getParents(jtm.get("parent"), tm, prefixes);
				if(parents.size() != 1){
					name = (Name)parents.get(0);
				} else {
					throw new FormatException("a name must contain the field \"parent\" with an array of exactly one topic reference");
				}
			} else {
				throw new FormatException("a name must contain the field \"parent\" with an array of string values");
			}
		}

		// get variant value
		String value = null;
		if(jtm.containsKey("value")){
			value = getStringValue(jtm, "value");
		} else {
			throw new FormatException("a name must contain the field \"value\" with a string value");
		}

		// get variant datatype
		String datatype = xsdString;
		if(jtm.containsKey("datatype")){
			datatype = unCurie(getStringValue(jtm, "datatype"), prefixes);
		}

		// get scope
		ArrayList<Topic> scope = new ArrayList<Topic>();
		if(jtm.containsKey("scope")){
			scope = getReferenceds(jtm.get("scope"), tm, prefixes);
		}
		if(scope.size() == 0){
			throw new FormatException("a variant must have at least on scope topic set");
		}
		JsArray<Topic> jsScope = toJsArray(scope);

		Variant variant = name.createVariant(value, tm.createLocator(datatype), jsScope);

		// get item_identifiers
		for (String ii : getStringsOfJSONArray(jtm, "item_identifiers")) {
			variant.addItemIdentifier(tm.createLocator(unCurie(ii, prefixes)));
		}

		// set reifier
		if(jtm.containsKey("reifier")){
			Topic reifier = getReferenced(jtm.get("reifier"), tm, prefixes);
			variant.setReifier(reifier);
		}

		return variant;
	}


	private Occurrence importJTM11Occurrence(Topic parent, JSONObject jtm, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference{
		// set occurrence parent
		Topic topic = null;
		if(parent != null){
			topic = parent;
		}else {
			if(jtm.containsKey("parent")){
				Topic parentTopic = getParent(jtm.get("parent"), tm, prefixes);
				if(parentTopic == null){
					throw new FormatException("an occurrence must contain the field \"parent\" with an array of exactly one topic reference");
				} else {
					topic = parentTopic;
				}
			} else {
				throw new FormatException("an occurrence must contain the field \"parent\" with an array of string values");
			}
		}

		// get occurrence value
		String value = "";
		if(jtm.containsKey("value")){
			value = getStringValue(jtm, "value");
		} else {
			throw new FormatException("an occurrence must contain the field \"value\" with a string value");
		}

		// get occurrence datatype
		String datatype = xsdString;
		if(jtm.containsKey("datatype")){
			datatype = unCurie(getStringValue(jtm, "datatype"), prefixes);
		}

		// get occurrence type
		Topic type = null;
		if(jtm.containsKey("type")){
			type = getReferenced(jtm.get("type"), tm, prefixes);
		}
		
		// get occurrence scope
		ArrayList<Topic> scope = new ArrayList<Topic>();
		if(jtm.containsKey("scope")){
			scope = getReferenceds(jtm.get("scope"), tm, prefixes);
		}
		JsArray<Topic> jsScope = toJsArray(scope);
		
		Occurrence occurrence = null;
		if(type == null){
			throw new FormatException("an occurrence must contain the field \"type\" with a topic reference");
		} else {
			occurrence = topic.createOccurrence(type, value, tm.createLocator(datatype), jsScope);
		}

		// get item_identifiers
		for (String ii : getStringsOfJSONArray(jtm, "item_identifiers")) {
			occurrence.addItemIdentifier(tm.createLocator(unCurie(ii, prefixes)));
		}
		
		// set reifier
		if(jtm.containsKey("reifier")){
			Topic reifier = getReferenced(jtm.get("reifier"), tm, prefixes);
			occurrence.setReifier(reifier);
		}
		return occurrence;
	}


	private Association importJTM11Association(JSONObject jtm, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference{
		// the parent is ignored, instead the passed tm is used as parent item

		// get association type
		Topic type = null;
		if(jtm.containsKey("type")){
			type = getReferenced(jtm.get("type"), tm, prefixes);
		}

		// get association scope
		ArrayList<Topic> scope = new ArrayList<Topic>();
		if(jtm.containsKey("scope")){
			scope = getReferenceds(jtm.get("scope"), tm, prefixes);
		}
		JsArray<Topic> jsScope = toJsArray(scope);

		Association association = null;
		if(type == null){
			throw new FormatException("an occurrence must contain the field \"type\" with a topic reference");
		} else {
			try{
			association = tm.createAssociation(type, jsScope);
			}catch(Exception e){
				System.err.println(">> e >> " + e.getLocalizedMessage() + " >> " + jsScope);
			}
		}

		// get association roles
		for (JSONObject role : getObjectsOfJSONArray(jtm, "roles")) {
			importJTM11Role(association, role, tm, prefixes);
		}

		// get item_identifiers
		for (String ii : getStringsOfJSONArray(jtm, "item_identifiers")) {
			association.addItemIdentifier(tm.createLocator(unCurie(ii, prefixes)));
		}

		// set reifier
		if(jtm.containsKey("reifier")){
			Topic reifier = getReferenced(jtm.get("reifier"), tm, prefixes);
			association.setReifier(reifier);
		}

		return association;
	}


	private Role importJTM11Role(Association parent, JSONObject jtm, TopicMap tm, Prefixes prefixes) throws FormatException, MissingReference{
		// set the variant parent
		Association association = null;
		if(parent != null){
			association = parent;
		}else {
			if(jtm.containsKey("parent")){
				ArrayList<Construct> parents = getParents(jtm.get("parent"), tm, prefixes);
				if(parents.size() != 1){
					association = (Association)parents.get(0);
				} else {
					throw new FormatException("a role must contain the field \"parent\" with an array of exactly one association reference");
				}
			} else {
				throw new FormatException("a role must contain the field \"parent\" with an array of string values");
			}
		}

		// get role player
		Topic player = null;
		if(jtm.containsKey("player")){
			player = getReferenced(jtm.get("player"), tm, prefixes);
		}

		// get role type
		Topic type = null;
		if(jtm.containsKey("type")){
			type = getReferenced(jtm.get("type"), tm, prefixes);
		}

		Role role = association.createRole(type, player);

		// get item_identifiers
		for (String ii : getStringsOfJSONArray(jtm, "item_identifiers")) {
			role.addItemIdentifier(tm.createLocator(unCurie(ii, prefixes)));
		}

		// set reifier
		if(jtm.containsKey("reifier")){
			Topic reifier = getReferenced(jtm.get("reifier"), tm, prefixes);
			role.setReifier(reifier);
		}

		return role;
	}


	private String exportItemType(TopicMap tm) throws ExporterException{
		return exportValue("topicmap", "item_type");
	}


	private String exportItemType(Topic topic) throws ExporterException{
		return exportValue("topic", "item_type");
	}


	private String exportItemType(Name name) throws ExporterException{
		return exportValue("name", "item_type");
	}


	private String exportItemType(Variant varian) throws ExporterException{
		return exportValue("variant", "item_type");
	}


	private String exportItemType(Occurrence occurrence) throws ExporterException{
		return exportValue("occurrence", "item_type");
	}


	private String exportItemType(Association association) throws ExporterException{
		return exportValue("association", "item_type");
	}


	private String exportItemType(Role role) throws ExporterException{
		return exportValue("role", "item_type");
	}


	private String exportVersion11(){
		return "\"version\":\"1.1\"";
	}


	private String exportValue(String value, String key){
		if(value == null) return "\"" + key + "\":null";
		else return "\"" + key + "\":" + escapeJSON(value);
	}


	private String exportJsonValue(String jsonValue, String key){
		return "\"" + key + "\":" + jsonValue;
	}


	private String exportArrayValues(ArrayList<String> values, String key){
		if(values == null || values.size() == 0){
			return "\"" + key + "\":" + "null";
		} else {
			String localValues = "\"" + key + "\":[";
			for(int i = 0; i != values.size(); ++i){
				localValues += escapeJSON(values.get(i));
				if(i != values.size() - 1){
					localValues += ",";
				}
			}
			return localValues + "]";
		}
	}


	private String exportJsonArrayValues(ArrayList<String> jsonValues, String key){
		if(jsonValues == null || jsonValues.size() == 0){
			return "\"" + key + "\":" + "null";
		} else {
			String localValues = "\"" + key + "\":[";
			for(int i = 0; i != jsonValues.size(); ++i){
				localValues += jsonValues.get(i);
				if(i != jsonValues.size() - 1){
					localValues += ",";
				}
			}
			return localValues + "]";
		}
	}


	private String curie(Locator uri, Prefixes prefixes) throws ExporterException{
		return curie(uri.getReference(), prefixes);
	}


	private String curie(String uri, Prefixes prefixes) throws ExporterException {
		String qualifier = prefixes.getQualifier(uri);
		if(qualifier == null || qualifier.length() == uri.length()) return uri;		
		String suffix = uri.substring(prefixes.getNs(qualifier).length());
		return "[" + qualifier + ":" + suffix + "]";
	}


	private String exportReference(Topic topic, String key, Prefixes prefixes) throws ExporterException {
		return "\"" + key + "\":" + exportReference(topic, prefixes);
	}


	private String exportReferences(ArrayList<Topic> topics, String key, Prefixes prefixes) throws ExporterException {
		return "\"" + key + "\":" + exportReferences(topics, prefixes);
	}


	private String exportReference(Topic topic, Prefixes prefixes) throws ExporterException{
		if(topic != null){
			JsArray<Locator> sis = topic.getSubjectIdentifiers();
			JsArray<Locator> sls = topic.getSubjectLocators();
			JsArray<Locator> iis = topic.getItemIdentifiers();
			if(sis.length() != 0){
				return escapeJSON("si:" + curie(sis.get(0), prefixes));
			} else if(sls.length() != 0){
				return escapeJSON("sl: " + curie(sls.get(0), prefixes));
			} else if(iis.length() != 0){
				return escapeJSON("ii: " + curie(iis.get(0), prefixes));
			} else {
				throw new ExporterException("topics must have at least one identifier set");
			}
		} else {
			return "null";
		}
	}


	private String exportReferences(ArrayList<Topic> topics, Prefixes prefixes) throws ExporterException{
		if(topics == null || topics.size() == 0){
			return "null";
		} else {
			String value = "[";
			for(int i = 0; i != topics.size(); ++i){
				value += exportReference(topics.get(i), prefixes);
				if(i != topics.size() - 1){
					value += ",";
				} 
			}

			return value + "]";
		}
	}


	private String exportJTM11(TopicMap tm, Prefixes prefixes, boolean standalone) throws ExporterException{
		// standalone is ignored, i.e. a TopicMap object mustis always exported as a
		// stand alone item.

		String json = "{";

		// version
		json += exportVersion11() + ",";

		// prefixes
		json += exportJsonValue(prefixes.toJSON(), "prefixes") + ",";

		// item_type
		json += exportItemType(tm) + ",";

		// item_identifiers
		json += exportIdentifiers(tm.getItemIdentifiers(), "item_identifiers", prefixes) + ",";

		// reifier
		json += exportReference(tm.getReifier(), "reifier", prefixes) + ",";

		// topics
		ArrayList<String> topicStrings = new ArrayList<String>();
		for(int i = 0; i != tm.getTopics().length(); ++i){
			topicStrings.add(exportJTM11(tm.getTopics().get(i), prefixes, false));
		}
		json += exportJsonArrayValues(topicStrings, "topics") + ",";

		// associations
		ArrayList<String> associationStrings = new ArrayList<String>();
		for(int i = 0; i != tm.getAssociations().length(); ++i){
			associationStrings.add(exportJTM11(tm.getAssociations().get(i), prefixes, false));
		}
		json += exportJsonArrayValues(associationStrings, "associations") + "}";

		return json;
	}
	
	
	@Override
	public String exportTm(ArrayList<Topic> topics, ArrayList<Association> associations) throws ExporterException {
		Prefixes prefixes = new Prefixes(topics, associations);
		String json = "{";

		// version
		json += exportVersion11() + ",";

		// prefixes
		json += exportJsonValue(prefixes.toJSON(), "prefixes") + ",";

		// item_type
		json += exportValue("topicmap", "item_type") + ",";

		// item_identifiers
		json += exportValue(null, "item_identifiers") + ",";

		// reifier
		json += exportValue(null, "reifier") + ",";

		// topics
		ArrayList<String> topicStrings = new ArrayList<String>();
		for (Topic topic : topics)
			topicStrings.add(exportJTM11(topic, prefixes, false));
		json += exportJsonArrayValues(topicStrings, "topics") + ",";

		// associations
		ArrayList<String> associationStrings = new ArrayList<String>();
		for (Association assoc : associations)
			associationStrings.add(exportJTM11(assoc, prefixes, false));
		json += exportJsonArrayValues(associationStrings, "associations") + "}";

		return json;
	}


	private String exportJTM11(Topic topic, Prefixes prefixes, boolean standalone) throws ExporterException{
		String json = "{";

		// the fields version, item_type, parent and prefixes are exported only if this
		// construct is exported as a standalone construct
		if(standalone){
			json += exportVersion11() + ",";
			json += exportJsonValue(prefixes.toJSON(), "prefixes") + ",";
			json += exportItemType(topic) + ",";
			json += exportParent(topic.getParent(), prefixes) + ",";
		}

		// the fields subject_identifiers, subject_locators, item_identifiers, names, occurrences
		// and instance_of are always exported

		// identifiers
		json += exportIdentifiers(topic.getSubjectIdentifiers(), "subject_identifiers", prefixes) + ",";
		json += exportIdentifiers(topic.getSubjectLocators(), "subject_locators", prefixes) + ",";
		json += exportIdentifiers(topic.getItemIdentifiers(), "item_identifiers", prefixes) + ",";

		// names
		ArrayList<String> nameStrings = new ArrayList<String>();
		for(int i = 0; i != topic.getNames().length(); ++i){
			nameStrings.add(exportJTM11(topic.getNames().get(i), prefixes, false));
		}
		json += exportJsonArrayValues(nameStrings, "names") + ",";

		// occurrences
		ArrayList<String> occurrenceStrings = new ArrayList<String>();
		for(int i = 0; i != topic.getOccurrences().length(); ++i){
			occurrenceStrings.add(exportJTM11(topic.getOccurrences().get(i), prefixes, false));
		}
		json += exportJsonArrayValues(occurrenceStrings, "occurrences") + ",";

		// instance_of
		ArrayList<Topic> instanceOfs = new ArrayList<Topic>();
		for(int i = 0; i != topic.getTypes().length(); ++i){
			instanceOfs.add(topic.getTypes().get(i));
		}
		json += exportReferences(instanceOfs, "instance_of", prefixes) + "}";

		return json;
	}


	private String exportJTM11(Name name, Prefixes prefixes, boolean standalone) throws ExporterException{
		String json = "{";

		// the fields version, item_type, parent and prefixes are exported only if this
		// construct is exported as a standalone construct
		if(standalone){
			json += exportVersion11() + ",";
			json += exportJsonValue(prefixes.toJSON(), "prefixes") + ",";
			json += exportItemType(name) + ",";
			json += exportParent(name.getParent(), prefixes) + ",";
		}

		// the fields item_identifiers, reifier, scope, variants, value and type are always exported
		json += exportIdentifiers(name.getItemIdentifiers(), "item_identifiers", prefixes) + ",";

		// reifier
		json += exportReference(name.getReifier(), "reifier", prefixes) + ",";

		// scope
		ArrayList<Topic> scopeTopics = new ArrayList<Topic>();
		for(int i = 0; i != name.getScope().length(); ++i){
			scopeTopics.add(name.getScope().get(i));
		}
		json += exportReferences(scopeTopics, "scope", prefixes) + ",";

		// variants
		ArrayList<String> variantStrings = new ArrayList<String>();
		for(int i = 0; i != name.getVariants().length(); ++i){
			variantStrings.add(exportJTM11(name.getVariants().get(i), prefixes, false));
		}
		json += exportJsonArrayValues(variantStrings, "variants") + ",";

		// value
		json += exportValue(name.getValue(), "value") + ",";

		// type - a name my have null as a type
		json += exportReference(name.getType(), "type", prefixes) + "}";

		return json;
	}


	private String exportJTM11(Variant variant, Prefixes prefixes, boolean standalone) throws ExporterException{
		String json = "{";

		// the fields version, item_type, parent and prefixes are exported only if this
		// construct is exported as a standalone construct
		if(standalone){
			json += exportVersion11() + ",";
			json += exportJsonValue(prefixes.toJSON(), "prefixes") + ",";
			json += exportItemType(variant) + ",";
			json += exportParent(variant.getParent(), prefixes) + ",";
		}

		// the fields item_identifiers, reifier, scope, datatype, value are always exported
		json += exportIdentifiers(variant.getItemIdentifiers(), "item_identifiers", prefixes) + ",";

		// reifier
		json += exportReference(variant.getReifier(), "reifier", prefixes) + ",";

		// scope
		ArrayList<Topic> scopeTopics = new ArrayList<Topic>();
		for(int i = 0; i != variant.getScope().length(); ++i){
			int j = 0;
			for( ; j != variant.getParent().getScope().length(); ++j){
				if(variant.getParent().getScope().get(j).equals(variant.getScope().get(i))){
					break;
				}
			}

			if(j == variant.getParent().getScope().length()){
				scopeTopics.add(variant.getScope().get(i));
			}
		}
		json += exportReferences(scopeTopics, "scope", prefixes) + ",";

		// datatype
		json += exportValue(curie(variant.getDatatype(), prefixes), "datatype") + ",";

		// value
		json += exportValue(variant.getValue(), "value") + "}";

		return json;
	}


	private String exportJTM11(Occurrence occurrence, Prefixes prefixes, boolean standalone) throws ExporterException{
		String json = "{";

		// the fields version, item_type, parent and prefixes are exported only if this
		// construct is exported as a standalone construct
		if(standalone){
			json += exportVersion11() + ",";
			json += exportJsonValue(prefixes.toJSON(), "prefixes") + ",";
			json += exportItemType(occurrence) + ",";
			json += exportParent(occurrence.getParent(), prefixes) + ",";
		}

		// the fields item_identifiers, reifier, type, scope, datatype, value are always exported
		json += exportIdentifiers(occurrence.getItemIdentifiers(), "item_identifiers", prefixes) + ",";

		// reifier
		json += exportReference(occurrence.getReifier(), "reifier", prefixes) + ",";

		// type
		if(occurrence.getType() == null){
			throw new ExporterException("a role must have a player set");
		}
		json += exportReference(occurrence.getType(), "type", prefixes) + ",";

		// scope
		ArrayList<Topic> scopeTopics = new ArrayList<Topic>();
		for(int i = 0; i != occurrence.getScope().length(); ++i){
			scopeTopics.add(occurrence.getScope().get(i));
		}
		json += exportReferences(scopeTopics, "scope", prefixes) + ",";

		// datatype
		json += exportValue(curie(occurrence.getDatatype(), prefixes), "datatype") + ",";

		// value
		json += exportValue(occurrence.getValue(), "value") + "}";

		return json;
	}


	private String exportParent(TopicMap parent, Prefixes prefixes) throws ExporterException {
		Locator parentIdentifier = null;

		if(parent.getItemIdentifiers().length() != 0){
			parentIdentifier = parent.getItemIdentifiers().get(0);
		} else {
			parentIdentifier = parent.getLocator();
		}

		if(parentIdentifier == null){
			throw new ExporterException("a parent must have at least one identifier to be serailised");
		} else {
			return "\"parent\":[\"ii:"  + curie(parentIdentifier, prefixes) + "\"]";
		}
	}
	
	
	private String exportParent(Topic parent, Prefixes prefixes) throws ExporterException {
		Locator parentIdentifier = null;
		String idType = "ii:";

		if(parent.getSubjectIdentifiers().length() != 0){
			parentIdentifier = parent.getSubjectIdentifiers().get(0);
			idType = "si:";
		} else if(parent.getSubjectLocators().length() != 0){
			parentIdentifier = parent.getSubjectLocators().get(0);
			idType = "sl:";
		} else if(parent.getItemIdentifiers().length() != 0){
			parentIdentifier = parent.getItemIdentifiers().get(0);
		}

		if(parentIdentifier == null){
			throw new ExporterException("a parent must have at least one identifier to be serailised");
		} else {
			return "\"parent\":[\"" + idType + curie(parentIdentifier, prefixes) + "\"]";
		}
	}


	private String exportParent(Reifiable parent, Prefixes prefixes) throws ExporterException {
		Locator parentIdentifier = null;

		ReifiableStub parentStub = (ReifiableStub)parent;

		if(parentStub.getItemIdentifiers().length() != 0){
			parentIdentifier = parentStub.getItemIdentifiers().get(0);
		}

		if(parentIdentifier == null){
			throw new ExporterException("a parent must have at least one identifier to be serailised");
		} else {
			return "\"parent\":[\"ii:"  + curie(parentIdentifier, prefixes) + "\"]";
		}
	}


	private String exportJTM11(Association association, Prefixes prefixes, boolean standalone) throws ExporterException{
		String json = "{";

		// the fields version, item_type, parent and prefixes are exported only if this
		// construct is exported as a standalone construct
		if(standalone){
			json += exportVersion11() + ",";
			json += exportJsonValue(prefixes.toJSON(), "prefixes") + ",";
			json += exportItemType(association) + ",";
			json += exportParent(association.getParent(), prefixes) + ",";
		}

		// the fields item_identifiers, reifier, scope, roles and type are always exported
		json += exportIdentifiers(association.getItemIdentifiers(), "item_identifiers", prefixes) + ",";

		// reifier
		json += exportReference(association.getReifier(), "reifier", prefixes) + ",";

		// scope
		ArrayList<Topic> scopeTopics = new ArrayList<Topic>();
		for(int i = 0; i != association.getScope().length(); ++i){
			scopeTopics.add(association.getScope().get(i));
		}
		json += exportReferences(scopeTopics, "scope", prefixes) + ",";

		// roles
		ArrayList<String> roleStrings = new ArrayList<String>();
		for(int i = 0; i != association.getRoles().length(); ++i){
			roleStrings.add(exportJTM11(association.getRoles().get(i), prefixes, false));
		}
		json += exportJsonArrayValues(roleStrings, "roles") + ",";

		// type
		if(association.getType() == null){
			throw new ExporterException("an association must have a player set");
		}
		json += exportReference(association.getType(), "type", prefixes) + "}";

		return json;
	}


	private String exportIdentifiers(JsArray<Locator> identifiers, String key, Prefixes prefixes) throws ExporterException{
		ArrayList<String> curies = new ArrayList<String>();
		for(int i = 0; i != identifiers.length(); ++i){
			curies.add(escapeJSON(curie(identifiers.get(i), prefixes)));
		}

		return exportJsonArrayValues(curies, key);
	}


	private String exportJTM11(Role role, Prefixes prefixes, boolean standalone) throws ExporterException{
		String json = "{";

		// the fields version, item_type, parent and prefixes are exported only if this
		// construct is exported as a standalone construct
		if(standalone){
			json += exportVersion11() + ",";
			json += exportJsonValue(prefixes.toJSON(), "prefixes") + ",";
			json += exportItemType(role) + ",";
			json += exportParent(role.getParent(), prefixes) + ",";
		}

		// the fields item_identifiers, reifier, player and type are always exported
		json += exportIdentifiers(role.getItemIdentifiers(), "item_identifiers", prefixes) + ",";

		// reifier
		json += exportReference(role.getReifier(), "reifier", prefixes) + ",";

		// player
		if(role.getPlayer() == null){
			throw new ExporterException("a role must have a player set");
		}
		json += exportReference(role.getPlayer(), "player", prefixes) + ",";

		// type
		if(role.getType() == null){
			throw new ExporterException("a role must have a player set");
		}
		json += exportReference(role.getType(), "type", prefixes) + "}";

		return json;
	}


	private static String escapeJSON(String json){
		return JsonUtils.escapeValue(json).replaceAll("/", "\\/");
	}


	private class Prefixes {
		private ArrayList<String[]> prefixes = new ArrayList<String[]>();


		public Prefixes(){
			this.prefixes.add(new String[]{"xsd", xsd});	
		}


		public Prefixes(JSONObject prefixes) throws FormatException{
			this();
			if(prefixes != null){
				for (String key : prefixes.keySet()) {
					JSONString ns = prefixes.get(key).isString();
					
					if(ns == null){
						throw new FormatException("a string value is expected as name space for \"" + key +"\"");
					} else if (!ns.stringValue().equals("xsd")){
						for (String[] item : this.prefixes) {
							if(item[0].equals(key) && !item[1].equals(ns.stringValue())){
								throw new FormatException("the prefix \"" + key + "\" is a duplicate");
							} else if(!item[0].equals(key)){
								this.prefixes.add(new String[]{key, ns.stringValue()});
								break;
							}
						}
					}
				}
			}
		}


		public Prefixes (Role role) throws ExporterException {
			this();
			initPrefixes(prefixes, role);
		}


		public Prefixes (Association association) throws ExporterException {
			this();
			initPrefixes(prefixes, association);
		}


		public Prefixes (Occurrence occurrence) throws ExporterException {
			this();
			initPrefixes(prefixes, occurrence);
		}


		public Prefixes (Variant variant) throws ExporterException {
			this();
			initPrefixes(prefixes, variant);
		}


		public Prefixes (Name name) throws ExporterException {
			this();
			initPrefixes(prefixes, name);
		}


		public Prefixes (Topic topic) throws ExporterException {
			this();
			initPrefixes(prefixes, topic);
		}


		public Prefixes (TopicMap tm) throws ExporterException {
			this();
			initPrefixes(prefixes, tm);
		}

		
		public Prefixes (ArrayList<Topic> topics, ArrayList<Association> associations) throws ExporterException {
			this();
			if(topics != null){
				for (Topic topic : topics)
					this.initPrefixes(prefixes, topic);
			}
			
			if(associations != null){
				for (Association assoc : associations) {
					this.initPrefixes(prefixes, assoc);
				}
			}
		}
		

		private void initPrefixes(ArrayList<String[]> prefixes, TopicMap tm) throws ExporterException{
			// item_identifiers
			addPrefixesOfIdentifiers(prefixes, tm.getItemIdentifiers());

			// reifier
			addPrefixOfTopicReference(prefixes, tm.getReifier());

			// topics
			for(int i = 0; i != tm.getTopics().length(); ++i){
				initPrefixes(prefixes, tm.getTopics().get(i));
			}

			// associations
			for(int i = 0; i != tm.getAssociations().length(); ++i){
				initPrefixes(prefixes, tm.getAssociations().get(i));
			}
		}


		private void initPrefixes(ArrayList<String[]> prefixes, Topic topic) throws ExporterException{
			// parent
			addPrefixOfTopicMapReference(prefixes, topic.getTopicMap());

			// subject_identifiers
			addPrefixesOfIdentifiers(prefixes, topic.getSubjectIdentifiers());

			// subject_locators 
			addPrefixesOfIdentifiers(prefixes, topic.getSubjectLocators());

			// item_identifeirs
			addPrefixesOfIdentifiers(prefixes, topic.getItemIdentifiers());

			// names
			for(int i = 0; i != topic.getNames().length(); ++i){
				initPrefixes(prefixes, topic.getNames().get(i));
			}

			// occurrences
			for(int i = 0; i != topic.getOccurrences().length(); ++i){
				initPrefixes(prefixes, topic.getOccurrences().get(i));
			}
		}


		private void initPrefixes(ArrayList<String[]> prefixes, Name name) throws ExporterException{
			if(null == name.getParent()){
				throw new ExporterException("a name must have a parent set"); 
			}

			// parent
			addPrefixOfTopicReference(prefixes, name.getParent());

			// item_idenifiers
			addPrefixesOfIdentifiers(prefixes, name.getItemIdentifiers());

			// reifier
			addPrefixOfTopicReference(prefixes, name.getReifier());

			// scope
			addPrefixesOfTopicReferences(prefixes, name.getScope());

			// type
			addPrefixOfTopicReference(prefixes, name.getType());

			// variants
			for(int i = 0; i != name.getVariants().length(); ++i){
				initPrefixes(prefixes, name.getVariants().get(i));
			}
		}


		private void initPrefixes(ArrayList<String[]> prefixes, Variant variant) throws ExporterException{
			if(null == variant.getParent() || null == variant.getScope() || variant.getScope().length() == 0){
				throw new ExporterException("a variant must have a parent and at least one scope set"); 
			}

			// parent
			addPrefixOfReifiableReference(prefixes, variant.getParent());

			// item_idenifiers
			addPrefixesOfIdentifiers(prefixes, variant.getItemIdentifiers());

			// reifier
			addPrefixOfTopicReference(prefixes, variant.getReifier());

			// scope
			addPrefixesOfTopicReferences(prefixes, variant.getScope());
		}


		private void initPrefixes(ArrayList<String[]> prefixes, Occurrence occurrence) throws ExporterException{
			if(null == occurrence.getParent() || null == occurrence.getType()){
				throw new ExporterException("an occurrence must have a parent and type set"); 
			}

			// parent
			addPrefixOfTopicReference(prefixes, occurrence.getParent());

			// item_idenifiers
			addPrefixesOfIdentifiers(prefixes, occurrence.getItemIdentifiers());

			// reifier
			addPrefixOfTopicReference(prefixes, occurrence.getReifier());

			// scope
			addPrefixesOfTopicReferences(prefixes, occurrence.getScope());

			// type
			addPrefixOfTopicReference(prefixes, occurrence.getType());
		}


		private void initPrefixes(ArrayList<String[]> prefixes, Association association) throws ExporterException{
			if(null == association.getParent() || null == association.getType() ||
					null == association.getRoles() || association.getRoles().length() == 0){
				throw new ExporterException("an association must have a parent, type and role set"); 
			}

			// parent
			addPrefixOfTopicMapReference(prefixes, association.getParent());

			// item_idenifiers
			addPrefixesOfIdentifiers(prefixes, association.getItemIdentifiers());

			// reifier
			addPrefixOfTopicReference(prefixes, association.getReifier());

			// scope
			addPrefixesOfTopicReferences(prefixes, association.getScope());

			// type
			addPrefixOfTopicReference(prefixes, association.getType());

			// roles
			for(int i = 0; i != association.getRoles().length(); ++i){
				initPrefixes(prefixes, association.getRoles().get(i));
			}
		}


		private void initPrefixes(ArrayList<String[]> prefixes, Role role) throws ExporterException{
			if(null == role.getParent() || null == role.getType() || null == role.getPlayer()){
				throw new ExporterException("a role must have a parent, type and player set"); 
			}

			// item_identifiers
			addPrefixesOfIdentifiers(prefixes, role.getItemIdentifiers());

			// reifier
			addPrefixOfTopicReference(prefixes, role.getReifier());

			// player
			addPrefixOfTopicReference(prefixes, role.getPlayer());

			// parent
			addPrefixOfReifiableReference(prefixes, role.getParent());

			// type
			addPrefixOfTopicReference(prefixes, role.getType());
		}


		private void addPrefixOfReifiableReference(ArrayList<String[]> prefixes, Reifiable reifiable){
			if(reifiable != null){
				ReifiableStub reifiableStub = (ReifiableStub)reifiable;

				if(reifiableStub.getItemIdentifiers().length() != 0) {
					Locator loc = reifiableStub.getItemIdentifiers().get(0);
					addPrefixOfIdentifier(prefixes, loc);
				}
			}
		}
		
		
		private void addPrefixOfTopicMapReference(ArrayList<String[]> prefixes, TopicMap tm){
			if(tm != null){
				Locator loc = null;
				if(tm.getItemIdentifiers().length() != 0) {
					loc = tm.getItemIdentifiers().get(0);
				} else {
					loc = tm.getLocator();
				}
				addPrefixOfIdentifier(prefixes, loc);
			}
		}


		private void addPrefixOfTopicReference(ArrayList<String[]> prefixes, Topic topic) throws ExporterException{
			if(topic != null){
				Locator loc = null;

				if (topic.getSubjectIdentifiers().length() != 0){
					loc = topic.getSubjectIdentifiers().get(0);
				} else if(topic.getSubjectLocators().length() != 0){
					loc = topic.getSubjectLocators().get(0);
				} else if(topic.getItemIdentifiers().length() != 0) {
					loc = topic.getItemIdentifiers().get(0);
				} else {
					throw new ExporterException("a topic must have at least on identifier set");
				}

				addPrefixOfIdentifier(prefixes, loc);
			}
		}


		private void addPrefixesOfTopicReferences(ArrayList<String[]> prefixes, JsArray<Topic> topics) throws ExporterException {
			if(topics != null) {
				for(int i = 0; i != topics.length(); ++i){
					addPrefixOfTopicReference(prefixes, topics.get(i));
				}
			}
		}


		private void addIfNew(ArrayList<String[]> prefixes, String uriPrefix){
			if(prefixes != null){
				for (String[] item : prefixes) {
					if(item[1].equals(uriPrefix)){
						return;
					}
				}
				prefixes.add(new String[]{"pref_" + prefixes.size(), uriPrefix});
			}
		}


		private void addPrefixOfIdentifier(ArrayList<String[]> prefixes, Locator loc){
			String uriPrefix = splitUriByLastFragment(loc.getReference())[0];
			if(uriPrefix.length() != loc.getReference().length()) addIfNew(prefixes, uriPrefix);
		}


		private void addPrefixesOfIdentifiers(ArrayList<String[]> prefixes, JsArray<Locator> locs){
			if(locs != null){
				for(int i = 0; i != locs.length(); ++i) {
					addPrefixOfIdentifier(prefixes, locs.get(i));
				}
			}
		}


		public String getNs(String qualifier){
			for (String[] item : this.prefixes){
				if(item[0].equals(qualifier)){
					return item[1];
				}
			}

			return null;
		}


		private String[] splitUriByLastFragment(String uri) {
			if(uri == null) return new String[]{null, null};
			
			int idxSlash = uri.lastIndexOf("/");
			int idxSharp = uri.lastIndexOf("#");
			int lastPos = idxSlash < idxSharp ? idxSharp : idxSlash;
			if(lastPos <= 0) return new String[]{uri, null}; 
			
			String prefix = uri.substring(0, lastPos + 1);
			String suffix = uri.substring(lastPos);
			suffix = suffix == null ? "" : suffix;

			return new String[]{prefix, suffix};
		}


		public String getQualifier(String uri){
			String prefix = splitUriByLastFragment(uri)[0];

			if(prefix.length() != uri.length()){			
				for (String[] item : prefixes) {
					if(item[1].equals(prefix)){
						return item[0];
					}
				}
			}
			
			return null;
		}


		public String getFullUri(String qualifier, String suffix){
			String ns = getNs(qualifier);
			if(ns != null){
				return ns + suffix;
			} else {
				return null;
			}
		}


		public String toJSON(){
			if(prefixes.size() == 0) {
				return "null";
			} else {
				String value = "{";
				for(int i = 0; i != prefixes.size(); ++i){
					value += "\"" + prefixes.get(i)[0] + "\":" + escapeJSON(prefixes.get(i)[1]);
					if(i == prefixes.size() - 1){
						value += "}";
					} else {
						value += ",";
					}
				}

				return value;
			}
		}
	}
}
