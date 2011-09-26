package us.isidor.gdl.anaToMia.TmEngine.jtmsBasedEngine;

import java.util.ArrayList;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Association;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Name;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Role;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMap;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Variant;

import com.google.gwt.core.client.JsArray;
import com.google.gwt.junit.client.GWTTestCase;



public class JtmsTmEngineTest extends GWTTestCase{
	final String tmLocator1 = "http://us.isidor.gdl.anaToMia/tm-1";
	final String tmLocator2 = "http://us.isidor.gdl.anaToMia/tm-2";
	final String siLocator1 = "http'://us.isidor.gdl.anaToMia/si-1";
	final String siLocator2 = "http'://us.isidor.gdl.anaToMia/si-2";
	final String siLocator3 = "http'://us.isidor.gdl.anaToMia/si-3";
	final String slLocator1 = "http'://us.isidor.gdl.anaToMia/sl-1";
	final String slLocator2 = "http'://us.isidor.gdl.anaToMia/sl-2";
	final String iiLocator1 = "http'://us.isidor.gdl.anaToMia/ii-1";
	final String iiLocator2 = "http'://us.isidor.gdl.anaToMia/ii-2";
	final String xsdString = "http://www.w3.org/2001/XMLSchema#string";
	final String xsdUri = "http://www.w3.org/2001/XMLSchema#anyUri";
	final String xsdDate = "http://www.w3.org/2001/XMLSchema#date";
	final String jtmTop = "{\"version\":\"1.1\"," +
			 			   "\"prefixes\":{\"tmdm\":\"http://psi.topicmaps.org/iso13250/model/\"," +
                           "\"tns\":\"http://psi.topincs.com/\"}," +
                           "\"item_type\":\"topic\"," + 
                           "\"subject_identifiers\":[\"[tns:people/thomas-vinterberg]\"]," +
                           "\"instance_of\":[\"si:[tns:person]\"]," +
                           "\"names\":[" +
                          "{\"value\":\"Thomas Vinterberg\"," +
                           "\"type\":\"si:[tmdm:topic-name]\"}]}";
	final String jtmOccurrence = "{\"version\":\"1.1\","+
			 				      "\"prefixes\":{\"tns\":\"http://psi.topincs.com/\"}," +
			 				      "\"item_type\":\"occurrence\"," +
			 				      "\"parent\":[\"si:[tns:people/thomas-vinterberg]\"]," +
			 				      "\"value\":\"1969-05-19\"," +
			 				      "\"datatype\":\"[xsd:date]\"," +
			 				      "\"type\":\"si:[tns:date-of-birth]\"}";
	final String jtmTm = "{\"version\":\"1.1\"," + 
			              "\"prefixes\":{\"country\":\"http://www.topicmaps.org/xtm/1.0/country.xtm#\"," +
                                        "\"tns\":\"http://psi.topincs.com/\"}," +
                          "\"item_type\":\"topicmap\"," + 
                          "\"topics\":[{\"item_identifiers\":[\"[tns:movies/reifier-1]\"]}," +
                                      "{\"subject_identifiers\":[\"[tns:movies/dear-wendy]\"]," +
                          	           "\"subject_locators\":[\"[tns:movies/sl-dear-wendy]\"," +
                          	                                 "\"[tns:movies/slDearWendy]\"]," +
                                       "\"instance_of\":[\"si:[tns:movie]\"]," +
                                       "\"names\":[{\"item_identifiers\":[\"[tns:movies/ii-1]\"," +
                          	                                             "\"[tns:movies/ii-2]\"]," +
                                                   "\"value\":\"Dear Wendy\"," +
                                                   "\"type\":\"si:[tns:title]\"," +
                                                   "\"scope\":[\"si:[country:US]\",\"si:[country:DE]\"]," +
                                                   "\"variants\":[{\"item_identifiers\":[\"[tns:movies/variant-ii]\"]," +
                                                                  "\"scope\":[\"si:[country:PL]\"]," +
                                                                  "\"value\":\"Dear Wendy (PL)\"}]}]," +
                                       "\"occurrences\":[{\"value\":\"2005\"," +
                                                         "\"type\":\"si:[tns:publication-year]\"," +
                                                         "\"datatype\":\"[xsd:gYear]\"," +
                                                         "\"reifier\":\"ii:[tns:movies/reifier-1]\"}]}," +
                                      "{\"subject_identifiers\":[\"[country:US]\"]}," +
                                      "{\"subject_identifiers\":[\"[country:PL]\"]}," +
                                      "{\"subject_identifiers\":[\"[tns:publication-year]\"]}," + 
                                      "{\"subject_identifiers\":[\"[country:DE]\"]}]," +
                          "\"associations\":[{\"type\":\"si:[tns:director]\"," +
                          	                 "\"roles\":[{\"player\":\"si:[tns:movies/dear-wendy]\"," +
                          	                 "\"type\":\"si:[tns:work]\"}," +
                          	                "{\"player\":\"si:[tns:people/thomas-vinterberg]\"," +
                                             "\"type\":\"si:[tns:author]\"}]}]}";
	JtmsTmEngine jtme = null;


	@Override
	public String getModuleName() {
		return "us.isidor.gdl.anaToMia.TmEngine.GDL_TmEngine";
	}


	public void testCreateTopicMap(){
		jtme = new JtmsTmEngine();

		TopicMap tm1 = jtme.createTopicMap(tmLocator1);
		assertNotNull(tm1);
		assertEquals(tmLocator1, tm1.getLocator().getReference());
		assertEquals(0, tm1.getItemIdentifiers().length());
		assertEquals(1, jtme.getTopicMaps().length);
		assertEquals(tm1, jtme.getTopicMaps()[0]);

		TopicMap tm2 = jtme.createTopicMap(tmLocator2);
		assertNotNull(tm2);
		assertEquals(tmLocator2, tm2.getLocator().getReference());
		assertEquals(0, tm2.getItemIdentifiers().length());
		assertEquals(2, jtme.getTopicMaps().length);
		assertEquals(tm1, jtme.getTopicMaps()[0]);
		assertEquals(tm2, jtme.getTopicMaps()[1]);


		TopicMap tm3 = jtme.createTopicMap(tmLocator2);
		assertNotNull(tm3);
		assertEquals(tmLocator2, tm3.getLocator().getReference());
		assertEquals(0, tm3.getItemIdentifiers().length());
		assertEquals(2, jtme.getTopicMaps().length);
		assertEquals(tm1, jtme.getTopicMaps()[0]);
		assertEquals(tm3, jtme.getTopicMaps()[1]);
	}


	public void testTopic(){
		jtme = new JtmsTmEngine();

		TopicMap tm = jtme.createTopicMap(tmLocator1);

		assertEquals(tm, tm.getTopicMap());
		assertNull(tm.getParent());

		// test various cases when handling topics
		Topic top1 = tm.createTopic();
		Topic top2 = tm.createTopicByItemIdentifier(tm.createLocator(iiLocator1));
		Topic top3 = tm.createTopicBySubjectIdentifier(tm.createLocator(siLocator1));
		Topic top4 = tm.createTopicBySubjectLocator(tm.createLocator(slLocator1));
		assertNotNull(top1);
		assertNotNull(top2);
		assertNotNull(top3);
		assertNotNull(top4);
		assertEquals(4, tm.getTopics().length());
		assertEquals(1, top2.getItemIdentifiers().length());
		assertEquals(iiLocator1, top2.getItemIdentifiers().get(0).getReference());
		assertEquals(1, top3.getSubjectIdentifiers().length());
		assertEquals(siLocator1, top3.getSubjectIdentifiers().get(0).getReference());
		assertEquals(1, top4.getSubjectLocators().length());
		assertEquals(slLocator1, top4.getSubjectLocators().get(0).getReference());
		assertEquals(top2, (Topic)tm.getConstructByItemIdentifier(tm.createLocator(iiLocator1)));
		assertEquals(top3, tm.getTopicBySubjectIdentifier(tm.createLocator(siLocator1)));
		assertEquals(top4, tm.getTopicBySubjectLocator(tm.createLocator(slLocator1)));
		assertEquals(tm, top3.getTopicMap());
		assertEquals(tm, top3.getParent());

		// test various cases of the TopicMap construct itself
		tm.addItemIdentifier(tm.createLocator(tmLocator2));
		assertEquals(1, tm.getItemIdentifiers().length());
		assertEquals(tmLocator2, tm.getItemIdentifiers().get(0).getReference());
		tm.setReifier(top2);
		assertEquals(top2, tm.getReifier());

		// test various cases when handling associations
		Association assoc1 = tm.createAssociation(top2, appendToJsArray(null, null));
		ArrayList<Topic> scope = new ArrayList<Topic>();
		scope.add(top2);
		scope.add(top4);
		Association assoc2 = tm.createAssociation(top3, createJsArray(scope));
		assertNotNull(assoc1);
		assertNotNull(assoc2);
		assertEquals(2, tm.getAssociations().length());
		assoc1.addItemIdentifier(tm.createLocator(iiLocator2));
		assertEquals(1, assoc1.getItemIdentifiers().length());
		assertEquals(iiLocator2, assoc1.getItemIdentifiers().get(0).getReference());
		assertEquals(top2, assoc1.getType());
		assertEquals(top3, assoc2.getType());
		assertEquals(tm, assoc1.getTopicMap());
		assertEquals(tm, assoc1.getParent());
		assertEquals(0, assoc1.getScope().length());
		assertEquals(2, assoc2.getScope().length());
	}
	
	
	public void testName(){
		jtme = new JtmsTmEngine();

		TopicMap tm = jtme.createTopicMap(tmLocator1);
		
		Topic top1 = tm.createTopicBySubjectLocator(tm.createLocator(slLocator1));
		Topic theme1 = tm.createTopic();
		Topic theme2 = tm.createTopic();
		Topic theme3 = tm.createTopic(); 
		Topic type = tm.createTopic();
		ArrayList<Topic> scope = new ArrayList<Topic>();
		scope.add(theme1);
		scope.add(theme2);
		
		Topic defaultNameType = jtme.createDefaultNameType(tm);
		assertNotNull(defaultNameType);
		Name name1 = top1.createName("name 1", appendToJsArray(null, null));
		Name name2 = top1.createName("name 2", createJsArray(scope));
		Name name3 = top1.createName("name 3", type, appendToJsArray(null, null));
		Name name4 = top1.createName("name 4", type, createJsArray(scope));
		name4.addItemIdentifier(tm.createLocator(iiLocator1));
		name4.addTheme(theme3);
		
		assertEquals(4, top1.getNames().length());
		assertEquals(2, top1.getNames(type).length());
		assertEquals(top1, name1.getParent());
		assertEquals(top1, name4.getParent());
		assertEquals(tm, name2.getTopicMap());
		assertEquals(tm, name3.getTopicMap());
		assertEquals("name 1", name1.getValue());
		assertEquals(1, name4.getItemIdentifiers().length());
		assertEquals(iiLocator1, name4.getItemIdentifiers().get(0).getReference());
		assertEquals(3, name4.getScope().length());
	}

	
	public void testVariant(){
		jtme = new JtmsTmEngine();

		TopicMap tm = jtme.createTopicMap(tmLocator1);
		
		Topic top1 = tm.createTopicBySubjectLocator(tm.createLocator(slLocator1));
		Name name1 = top1.createName("name 1", appendToJsArray(null, null));
		
		Topic theme1 = tm.createTopic();
		Topic theme2 = tm.createTopic();
		Topic theme3 = tm.createTopic();
		ArrayList<Topic> scope = new ArrayList<Topic>();
		scope.add(theme1);
		scope.add(theme2);
		
		Variant variant1 = name1.createVariant("variant 1", appendToJsArray(null, null));
		Variant variant2 = name1.createVariant("variant 2", createJsArray(scope));
		Variant variant3 = name1.createVariant("variant 3", tm.createLocator(xsdUri), appendToJsArray(null, null));
		Variant variant4 = name1.createVariant("variant 4", tm.createLocator(xsdUri), createJsArray(scope));
		Variant variant5 = name1.createVariant(tm.createLocator("http://some.where/variant-5"), appendToJsArray(null, null));
		Variant variant6 = name1.createVariant(tm.createLocator("http://some.where/variant-6"), createJsArray(scope));
		variant4.addItemIdentifier(tm.createLocator(iiLocator1));
		variant4.addTheme(theme3);
		
		assertEquals(6, name1.getVariants().length());
		assertEquals(name1, variant1.getParent());
		assertEquals(name1, variant4.getParent());
		assertEquals(name1, variant5.getParent());
		assertEquals(tm, variant2.getTopicMap());
		assertEquals(tm, variant3.getTopicMap());
		assertEquals(tm, variant6.getTopicMap());
		assertEquals("variant 1", variant1.getValue());
		assertEquals(1, variant4.getItemIdentifiers().length());
		assertEquals(iiLocator1, variant4.getItemIdentifiers().get(0).getReference());
		assertEquals("http://some.where/variant-6", variant6.getValue());
		assertEquals(3, variant4.getScope().length());
		assertEquals(2, variant6.getScope().length());
		assertEquals(xsdString, variant1.getDatatype().getReference());
		assertEquals(xsdUri, variant3.getDatatype().getReference());
		assertEquals(xsdUri, variant5.getDatatype().getReference());
	}

	
	public void testOccurrence(){
		jtme = new JtmsTmEngine();

		TopicMap tm = jtme.createTopicMap(tmLocator1);
		
		Topic top1 = tm.createTopicBySubjectLocator(tm.createLocator(slLocator1));
		Topic theme1 = tm.createTopic();
		Topic theme2 = tm.createTopic();
		Topic theme3 = tm.createTopic(); 
		Topic type1 = tm.createTopic();
		Topic type2 = tm.createTopic();
		ArrayList<Topic> scope = new ArrayList<Topic>();
		scope.add(theme1);
		scope.add(theme2);
		
		Topic defaultNameType = jtme.createDefaultNameType(tm);
		assertNotNull(defaultNameType);
		Occurrence occ1 = top1.createOccurrence(type1, tm.createLocator("http://some.where/occurrence-1"), appendToJsArray(null, null));
		Occurrence occ2 = top1.createOccurrence(type1, tm.createLocator("http://some.where/occurrence-2"), createJsArray(scope));
		Occurrence occ3 = top1.createOccurrence(type2, "occurrence 3", appendToJsArray(null, null));
		Occurrence occ4 = top1.createOccurrence(type2, "occurrence 4", createJsArray(scope));
		Occurrence occ5 = top1.createOccurrence(type1, "occurrence-5", tm.createLocator(xsdUri), appendToJsArray(null, null));
		Occurrence occ6 = top1.createOccurrence(type1, "occurrence-6", tm.createLocator(xsdUri), createJsArray(scope));
		occ4.addItemIdentifier(tm.createLocator(iiLocator1));
		occ4.addTheme(theme3);
		
		assertEquals(6, top1.getOccurrences().length());
		assertEquals(4, top1.getOccurrences(type1).length());
		assertEquals(top1, occ1.getParent());
		assertEquals(top1, occ4.getParent());
		assertEquals(top1, occ5.getParent());
		assertEquals(tm, occ2.getTopicMap());
		assertEquals(tm, occ3.getTopicMap());
		assertEquals(tm, occ6.getTopicMap());
		assertEquals("http://some.where/occurrence-1", occ1.getValue());
		assertEquals(1, occ4.getItemIdentifiers().length());
		assertEquals(iiLocator1, occ4.getItemIdentifiers().get(0).getReference());
		assertEquals(3, occ4.getScope().length());
		assertEquals(xsdUri, occ1.getDatatype().getReference());
		assertEquals(xsdString, occ3.getDatatype().getReference());
		assertEquals(xsdUri, occ5.getDatatype().getReference());
	}
	
	
	public void testAssociation(){
		jtme = new JtmsTmEngine();

		TopicMap tm = jtme.createTopicMap(tmLocator1);
		Topic type1 = tm.createTopic();
		Topic type2 = tm.createTopic();
		Topic theme1 = tm.createTopicBySubjectIdentifier(tm.createLocator(siLocator1));
		Topic theme2 = tm.createTopicBySubjectIdentifier(tm.createLocator(siLocator2));
		Topic theme3 = tm.createTopicBySubjectIdentifier(tm.createLocator(siLocator3));
		Topic reifier = tm.createTopic();
		ArrayList<Topic> scope = new ArrayList<Topic>();
		scope.add(theme1);
		scope.add(theme2);
		Association assoc1 = tm.createAssociation(type1, createJsArray(scope));
		Association assoc2 = tm.createAssociation(type2, appendToJsArray(null, null));
		assoc1.addItemIdentifier(tm.createLocator(iiLocator1));
		assoc1.addItemIdentifier(tm.createLocator(iiLocator2));
		assoc1.setReifier(reifier);
		assoc1.addTheme(theme3);
		
		assertEquals(2, tm.getAssociations().length());
		assertTrue(assoc1.equals(tm.getAssociations().get(0)) || assoc1.equals(tm.getAssociations().get(1)));
		assertTrue(assoc2.equals(tm.getAssociations().get(0)) || assoc2.equals(tm.getAssociations().get(1)));
		assertEquals(2, assoc1.getItemIdentifiers().length());
		assertTrue(iiLocator1.equals(assoc1.getItemIdentifiers().get(0).getReference()) || iiLocator1.equals(assoc1.getItemIdentifiers().get(1).getReference()));
		assertTrue(iiLocator2.equals(assoc1.getItemIdentifiers().get(0).getReference()) || iiLocator2.equals(assoc1.getItemIdentifiers().get(1).getReference()));
		assertEquals(reifier, assoc1.getReifier());
		assertEquals(3, assoc1.getScope().length());
		assertTrue(theme1.equals(assoc1.getScope().get(0)) || theme1.equals(assoc1.getScope().get(1)) || theme1.equals(assoc1.getScope().get(2)));
		assertTrue(theme2.equals(assoc1.getScope().get(0)) || theme2.equals(assoc1.getScope().get(1)) || theme2.equals(assoc1.getScope().get(2)));  
		assertTrue(theme3.equals(assoc1.getScope().get(0)) || theme3.equals(assoc1.getScope().get(1)) || theme3.equals(assoc1.getScope().get(2)));
		assoc1.removeTheme(theme3);
		assoc1.removeTheme(theme2);
		assoc1.removeItemIdentifier(tm.createLocator(iiLocator1));
		assertEquals(1, assoc1.getItemIdentifiers().length());
		assertEquals(iiLocator2, assoc1.getItemIdentifiers().get(0).getReference());
		assertEquals(1, assoc1.getScope().length());
		assertEquals(theme1, assoc1.getScope().get(0));
		assoc1.remove();
		assertEquals(1, tm.getAssociations().length());
		assertEquals(assoc2, tm.getAssociations().get(0));
		assertEquals(tm, assoc1.getParent());
		assertEquals(tm, assoc1.getTopicMap());
	}
	
	
	public void testRole(){
		jtme = new JtmsTmEngine();

		TopicMap tm = jtme.createTopicMap(tmLocator1);
		Topic aType = tm.createTopic();
		Topic rType = tm.createTopic();
		Topic rPlayer1 = tm.createTopic();
		Topic rPlayer2 = tm.createTopic();
		Topic reifier = tm.createTopic();
		Association assoc = tm.createAssociation(aType, appendToJsArray(null, null));
		Role role1 = assoc.createRole(rType, rPlayer1);
		Role role2 = assoc.createRole(rType, rPlayer2);
		role1.addItemIdentifier(tm.createLocator(iiLocator1));
		role1.setReifier(reifier);
		assertEquals(assoc, role1.getParent());
		assertEquals(tm, role1.getTopicMap());
		assertEquals(rPlayer1, role1.getPlayer());
		assertEquals(rType, role1.getType());
		assertEquals(1, role1.getItemIdentifiers().length());
		assertEquals(iiLocator1, role1.getItemIdentifiers().get(0).getReference());
		assertEquals(reifier, role1.getReifier());
		role1.removeItemIdentifier(tm.createLocator(iiLocator1));
		role1.setReifier(null);
		assertEquals(0, role1.getItemIdentifiers().length());
		assertNull(role1.getReifier());
		assertEquals(2, assoc.getRoles(rType).length());
		assertEquals(1, assoc.getRoleTypes().length());
		assertEquals(rType, assoc.getRoleTypes().get(0));
		role2.remove();
		assertEquals(1, assoc.getRoles().length());
		assertEquals(role1, assoc.getRoles().get(0));
	}
	
	
	public void testJtm11import(){
		try{
			jtme = new JtmsTmEngine();
			TopicMap tm1 = jtme.createTopicMap(tmLocator1);
			tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/person"));
			Topic defaultNameType = jtme.createDefaultNameType(tm1);
			Topic thomasVinterberg = jtme.importTopic(jtmTop, tm1);
			Topic dateOfBirth = tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/date-of-birth"));
			jtme.importOccurrence(jtmOccurrence, tm1);
			Topic movie = tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/movie"));
			Topic director = tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/director"));
			Topic author = tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/author"));
			Topic work = tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/work"));
			Topic title = tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/title"));
			TopicMap tm2 = jtme.importTopicMap(jtmTm, tm1);
			
			assertEquals(tm1, tm2);
			assertEquals(15, tm1.getTopics().length());
			assertNotNull((Topic)tm1.getConstructByItemIdentifier(tm1.createLocator("http://psi.topincs.com/movies/reifier-1")));
			assertEquals(defaultNameType, tm1.getTopicBySubjectIdentifier(tm1.createLocator(Name.defaultNameTypePsi)));
			assertNotNull(tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://www.topicmaps.org/xtm/1.0/country.xtm#DE")));
			assertNotNull(tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://www.topicmaps.org/xtm/1.0/country.xtm#PL")));
			assertNotNull(tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://www.topicmaps.org/xtm/1.0/country.xtm#US")));
			assertNotNull(tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/person")));
			assertEquals(dateOfBirth, tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/date-of-birth")));
			assertEquals(movie, tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/movie")));
			assertEquals(title, tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/title")));
			assertNotNull(tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/publication-year")));
			assertEquals(director, tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/director")));
			assertEquals(work, tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/work")));
			assertEquals(author, tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/author")));
			assertEquals(thomasVinterberg, tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/people/thomas-vinterberg")));
			assertNotNull(tm1.getTopicBySubjectLocator(tm1.createLocator("http://psi.topincs.com/movies/slDearWendy")));
			
			assertEquals(0, thomasVinterberg.getItemIdentifiers().length());
			assertEquals(0, thomasVinterberg.getSubjectLocators().length());
			assertEquals(1, thomasVinterberg.getSubjectIdentifiers().length());
			assertEquals(1, thomasVinterberg.getNames().length());
			assertEquals("Thomas Vinterberg", thomasVinterberg.getNames().get(0).getValue());
			assertEquals(defaultNameType, thomasVinterberg.getNames().get(0).getType());
			assertEquals(0, thomasVinterberg.getNames().get(0).getVariants().length());
			assertEquals(0, thomasVinterberg.getNames().get(0).getItemIdentifiers().length());
			assertEquals(null, thomasVinterberg.getNames().get(0).getReifier());
			assertEquals(1, thomasVinterberg.getOccurrences().length());
			assertEquals("1969-05-19", thomasVinterberg.getOccurrences().get(0).getValue());
			assertEquals(xsdDate, thomasVinterberg.getOccurrences().get(0).getDatatype().getReference());
			assertEquals(dateOfBirth, thomasVinterberg.getOccurrences().get(0).getType());
			assertEquals(0, thomasVinterberg.getOccurrences().get(0).getItemIdentifiers().length());
			assertEquals(0, thomasVinterberg.getOccurrences().get(0).getScope().length());
			assertEquals(1, thomasVinterberg.getRolesPlayed().length());
			assertEquals(author, thomasVinterberg.getRolesPlayed().get(0).getType());
			assertEquals(director, thomasVinterberg.getRolesPlayed().get(0).getParent().getType());
			assertEquals(1, thomasVinterberg.getTypes().length());
			assertEquals(tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/person")), thomasVinterberg.getTypes().get(0));
			
			Topic dearWendy = tm1.getTopicBySubjectLocator(tm1.createLocator("http://psi.topincs.com/movies/slDearWendy"));
			assertEquals(0, dearWendy.getItemIdentifiers().length());
			assertEquals(2, dearWendy.getSubjectLocators().length());
			assertEquals(1, dearWendy.getSubjectIdentifiers().length());
			assertEquals("http://psi.topincs.com/movies/dear-wendy", dearWendy.getSubjectIdentifiers().get(0).getReference());
			assertTrue("http://psi.topincs.com/movies/sl-dear-wendy".equals(dearWendy.getSubjectLocators().get(0).getReference()) || "http://psi.topincs.com/movies/sl-dear-wendy".equals(dearWendy.getSubjectLocators().get(1).getReference()));
			assertTrue("http://psi.topincs.com/movies/slDearWwendy".equals(dearWendy.getSubjectLocators().get(0).getReference()) || "http://psi.topincs.com/movies/slDearWendy".equals(dearWendy.getSubjectLocators().get(1).getReference()));
			assertEquals(1, thomasVinterberg.getNames().length());
			Name dwName = dearWendy.getNames().get(0);
			assertEquals(2, dwName.getScope().length());
			Topic us = tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://www.topicmaps.org/xtm/1.0/country.xtm#US"));
			Topic de = tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://www.topicmaps.org/xtm/1.0/country.xtm#DE"));
			Topic pl = tm1.getTopicBySubjectIdentifier(tm1.createLocator("http://www.topicmaps.org/xtm/1.0/country.xtm#PL"));
			assertTrue(de.equals(dwName.getScope().get(0)) || de.equals(dwName.getScope().get(1)));
			assertTrue(us.equals(dwName.getScope().get(0)) || us.equals(dwName.getScope().get(1)));
			assertEquals(2, dwName.getItemIdentifiers().length());
			assertTrue("http://psi.topincs.com/movies/ii-1".equals(dwName.getItemIdentifiers().get(0).getReference()) || "http://psi.topincs.com/movies/ii-1".equals(dwName.getItemIdentifiers().get(1).getReference()));
			assertTrue("http://psi.topincs.com/movies/ii-2".equals(dwName.getItemIdentifiers().get(0).getReference()) || "http://psi.topincs.com/movies/ii-2".equals(dwName.getItemIdentifiers().get(1).getReference()));
			assertEquals(1, dwName.getVariants().length());
			Variant dwVariant = dwName.getVariants().get(0);
			assertNull(dwVariant.getReifier());
			assertEquals(1, dwVariant.getItemIdentifiers().length());
			assertEquals("http://psi.topincs.com/movies/variant-ii", dwVariant.getItemIdentifiers().get(0).getReference());
			assertEquals("Dear Wendy (PL)", dwVariant.getValue());
			assertEquals(xsdString, dwVariant.getDatatype().getReference());
			assertEquals(3, dwVariant.getScope().length());
			assertTrue(de.equals(dwVariant.getScope().get(0)) || de.equals(dwVariant.getScope().get(1)) || de.equals(dwVariant.getScope().get(2)));
			assertTrue(us.equals(dwVariant.getScope().get(0)) || us.equals(dwVariant.getScope().get(1)) || us.equals(dwVariant.getScope().get(2)));
			assertTrue(pl.equals(dwVariant.getScope().get(0)) || pl.equals(dwVariant.getScope().get(1)) || pl.equals(dwVariant.getScope().get(2)));
			assertEquals(1, thomasVinterberg.getOccurrences().length());
			assertEquals((Topic)tm1.getConstructByItemIdentifier(tm1.createLocator("http://psi.topincs.com/movies/reifier-1")), dearWendy.getOccurrences().get(0).getReifier());
			
			assertEquals(1, tm1.getAssociations().length());
			Association assoc = tm1.getAssociations().get(0);
			assertEquals(2, assoc.getRoles().length());
			assertEquals(director, assoc.getType());
			assertEquals(0, assoc.getScope().length());
			assertEquals(0, assoc.getItemIdentifiers().length());
			assertNull(assoc.getReifier());
		}catch(Exception e){
			System.err.println("caught error in testJtm11import(): " + e.getLocalizedMessage());
			assertNull(e.getLocalizedMessage());
		}
	}
	
	
	public void testJTM11export(){
		try {
			jtme = new JtmsTmEngine();
			TopicMap tm1 = jtme.createTopicMap(tmLocator1);
			tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/person"));
			jtme.createDefaultNameType(tm1);
			Topic thomasVinterberg = jtme.importTopic(jtmTop, tm1);
			tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/date-of-birth"));
			Occurrence occ = jtme.importOccurrence(jtmOccurrence, tm1);
			Topic movie = tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/movie"));
			tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/director"));
			tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/author"));
			tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/work"));
			tm1.createTopicBySubjectIdentifier(tm1.createLocator("http://psi.topincs.com/title"));
			jtme.importTopicMap(jtmTm, tm1);
			tm1.getTopicBySubjectLocator(tm1.createLocator("http://psi.topincs.com/movies/slDearWendy"));
			Association assoc = tm1.getAssociations().get(0);
			assoc.addItemIdentifier(tm1.createLocator("http://psi.topincs.com/association-ii"));
			
			String jtmMovie = jtme.exportTm(movie);
			String jtmThomasVinterberg = jtme.exportTm(thomasVinterberg);
			String jtmVinterbergOccurrence = jtme.exportTm(occ);
			String jtmVinterbergRole = jtme.exportTm(thomasVinterberg.getRolesPlayed().get(0));
			String jtmTm = jtme.exportTm(tm1);
			
			String jtmMovieExpected = "{\"version\":\"1.1\"," +
									   "\"prefixes\":{" +
									       "\"xsd\":\"http://www.w3.org/2001/XMLSchema#\"," +
									       "\"pref_1\":\"http://us.isidor.gdl.anaToMia/\"," +
									       "\"pref_2\":\"http://psi.topincs.com/\"" +
									   "}," +
									   "\"item_type\":\"topic\"," +
									   "\"parent\":[" +
									       "\"ii:[pref_1:tm-1]\"" +
									   "]," +
									   "\"subject_identifiers\":[" +
									       "\"[pref_2:movie]\"" +
									   "]," +
									   "\"subject_locators\":null," +
									   "\"item_identifiers\":null," +
									   "\"names\":null," +
									   "\"occurrences\":null," +
									   "\"instance_of\":null" +
									"}";
			assertEquals(jtmMovieExpected, jtmMovie);
			
			String jtmVinterbergOccurrenceExpected = "{\"version\":\"1.1\"," +
													  "\"prefixes\":{" +
													      "\"xsd\":\"http://www.w3.org/2001/XMLSchema#\"," +
													      "\"pref_1\":\"http://psi.topincs.com/people/\"," +
													      "\"pref_2\":\"http://psi.topincs.com/\"" +
													  "}," +
													  "\"item_type\":\"occurrence\"," +
													  "\"parent\":[" +
													      "\"si:[pref_1:thomas-vinterberg]\"" +
													  "]," +
													  "\"item_identifiers\":null," +
													  "\"reifier\":null," +
													  "\"type\":\"si:[pref_2:date-of-birth]\"," +
													  "\"scope\":null," +
													  "\"datatype\":\"[xsd:date]\"," +
													  "\"value\":\"1969-05-19\"" +
													 "}";
			assertEquals(jtmVinterbergOccurrenceExpected, jtmVinterbergOccurrence);
			
			String jtmThomasVinterbergExpected = "{\"version\":\"1.1\"," +
												  "\"prefixes\":{" +
												      "\"xsd\":\"http://www.w3.org/2001/XMLSchema#\"," +
												      "\"pref_1\":\"http://us.isidor.gdl.anaToMia/\"," +
												      "\"pref_2\":\"http://psi.topincs.com/people/\"," +
												      "\"pref_3\":\"http://psi.topicmaps.org/iso13250/model/\"," +
												      "\"pref_4\":\"http://psi.topincs.com/\"" +
												  "}," +
												  "\"item_type\":\"topic\"," +
												  "\"parent\":[" +
												      "\"ii:[pref_1:tm-1]\"" +
												  "]," +
												  "\"subject_identifiers\":[" +
												      "\"[pref_2:thomas-vinterberg]\"" +
												  "]," +
												  "\"subject_locators\":null," +
												  "\"item_identifiers\":null," +
												  "\"names\":[" +
												      "{" +
												          "\"item_identifiers\":null," +
												          "\"reifier\":null," +
												          "\"scope\":null," +
												          "\"variants\":null," +
												          "\"value\":\"Thomas Vinterberg\"," +
												          "\"type\":\"si:[pref_3:topic-name]\"" +
												      "}" +
												  "]," +
												  "\"occurrences\":[" +
												      "{" +
												          "\"item_identifiers\":null," +
												          "\"reifier\":null," +
												          "\"type\":\"si:[pref_4:date-of-birth]\"," +
												          "\"scope\":null," +
												          "\"datatype\":\"[xsd:date]\"," +
												          "\"value\":\"1969-05-19\"" +
												      "}" +
												   "]," +
												  "\"instance_of\":[" +
												      "\"si:[pref_4:person]\"" +
												  "]" +
												 "}";
			assertEquals(jtmThomasVinterbergExpected, jtmThomasVinterberg);		
			
			String jtmVinterbergRoleExpected = "{\"version\":\"1.1\"," +
												"\"prefixes\":{" +
												    "\"xsd\":\"http://www.w3.org/2001/XMLSchema#\"," +
												    "\"pref_1\":\"http://psi.topincs.com/people/\"," +
												    "\"pref_2\":\"http://psi.topincs.com/\"" +
												"}," +
												"\"item_type\":\"role\"," +
												"\"parent\":[" +
												    "\"ii:[pref_2:association-ii]\"" +
												"]," +
												"\"item_identifiers\":null," +
												"\"reifier\":null," +
												"\"player\":\"si:[pref_1:thomas-vinterberg]\"," +
												"\"type\":\"si:[pref_2:author]\"" +
											  "}";
			
			assertEquals(jtmVinterbergRoleExpected, jtmVinterbergRole);		
			
			String jtmTmExpected = "{\"version\":\"1.1\",\"prefixes\":{\"xsd\":\"http://www.w3.org/2001/XMLSchema#\",\"pref_1\":\"http://us.isidor.gdl.anaToMia/\",\"pref_2\":\"http://psi.topincs.com/\",\"pref_3\":\"http://psi.topicmaps.org/iso13250/model/\",\"pref_4\":\"http://psi.topincs.com/people/\",\"pref_5\":\"http://psi.topincs.com/movies/\",\"pref_6\":\"http://www.topicmaps.org/xtm/1.0/country.xtm#\"},\"item_type\":\"topicmap\",\"item_identifiers\":null,\"reifier\":null,\"topics\":[{\"subject_identifiers\":[\"[pref_2:person]\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null,\"instance_of\":null},{\"subject_identifiers\":[\"[pref_3:topic-name]\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null,\"instance_of\":null},{\"subject_identifiers\":[\"[pref_4:thomas-vinterberg]\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":[{\"item_identifiers\":null,\"reifier\":null,\"scope\":null,\"variants\":null,\"value\":\"Thomas Vinterberg\",\"type\":\"si:[pref_3:topic-name]\"}],\"occurrences\":[{\"item_identifiers\":null,\"reifier\":null,\"type\":\"si:[pref_2:date-of-birth]\",\"scope\":null,\"datatype\":\"[xsd:date]\",\"value\":\"1969-05-19\"}],\"instance_of\":[\"si:[pref_2:person]\"]},{\"subject_identifiers\":[\"[pref_2:date-of-birth]\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null,\"instance_of\":null},{\"subject_identifiers\":[\"[pref_2:movie]\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null,\"instance_of\":null},{\"subject_identifiers\":[\"[pref_2:director]\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null,\"instance_of\":null},{\"subject_identifiers\":[\"[pref_2:author]\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null,\"instance_of\":null},{\"subject_identifiers\":[\"[pref_2:work]\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null,\"instance_of\":null},{\"subject_identifiers\":[\"[pref_2:title]\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null,\"instance_of\":null},{\"subject_identifiers\":null,\"subject_locators\":null,\"item_identifiers\":[\"[pref_5:reifier-1]\"],\"names\":null,\"occurrences\":null,\"instance_of\":null},{\"subject_identifiers\":[\"[pref_5:dear-wendy]\"],\"subject_locators\":[\"[pref_5:sl-dear-wendy]\",\"[pref_5:slDearWendy]\"],\"item_identifiers\":null,\"names\":[{\"item_identifiers\":[\"[pref_5:ii-1]\",\"[pref_5:ii-2]\"],\"reifier\":null,\"scope\":[\"si:[pref_6:US]\",\"si:[pref_6:DE]\"],\"variants\":[{\"item_identifiers\":[\"[pref_5:variant-ii]\"],\"reifier\":null,\"scope\":[\"si:[pref_6:PL]\"],\"datatype\":\"[xsd:string]\",\"value\":\"Dear Wendy (PL)\"}],\"value\":\"Dear Wendy\",\"type\":\"si:[pref_2:title]\"}],\"occurrences\":[{\"item_identifiers\":null,\"reifier\":\"ii: [pref_5:reifier-1]\",\"type\":\"si:[pref_2:publication-year]\",\"scope\":null,\"datatype\":\"[xsd:gYear]\",\"value\":\"2005\"}],\"instance_of\":[\"si:[pref_2:movie]\"]},{\"subject_identifiers\":[\"[pref_6:US]\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null,\"instance_of\":null},{\"subject_identifiers\":[\"[pref_6:PL]\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null,\"instance_of\":null},{\"subject_identifiers\":[\"[pref_2:publication-year]\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null,\"instance_of\":null},{\"subject_identifiers\":[\"[pref_6:DE]\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null,\"instance_of\":null}],\"associations\":[{\"item_identifiers\":[\"[pref_2:association-ii]\"],\"reifier\":null,\"scope\":null,\"roles\":[{\"item_identifiers\":null,\"reifier\":null,\"player\":\"si:[pref_5:dear-wendy]\",\"type\":\"si:[pref_2:work]\"},{\"item_identifiers\":null,\"reifier\":null,\"player\":\"si:[pref_4:thomas-vinterberg]\",\"type\":\"si:[pref_2:author]\"}],\"type\":\"si:[pref_2:director]\"}]}";
			assertEquals(jtmTmExpected, jtmTm);
		}catch(Exception e){
			System.err.println("caught error in testJtm11export(): " + e.getLocalizedMessage());
			assertNull(e.getLocalizedMessage());
		}
	}


	private JsArray<Topic> createJsArray(ArrayList<Topic> topics){
		JsArray<Topic> tops = null;

		for (Topic topic : topics) {
			tops = appendToJsArray(tops, topic);
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
}
