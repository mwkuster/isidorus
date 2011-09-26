package us.isidor.gdl.anaToMia.Widgets.isidorus;


import com.google.gwt.user.client.Window;


public class IsidorusConstants {
	public final static String XSD_STRING = "http://www.w3.org/2001/XMLSchema#string";
	public final static String TOPIC_MAP_IDENTIFIER = "http://textgrid.org/serviceregistry/gdl-frontend/gdl-tm";
	public final static String HASH_OBJECT_PSI = "http://textgrid.org/serviceregistry/model/types/Hash-Object";
	public final static String ENVIRONMENT_PSI = "http://textgrid.org/serviceregistry/model/types/Environment";
	public final static String HASH_VALUE_OCCURRENCE_TYPE = "http://textgrid.org/serviceregistry/model/value";
	public final static String ENVIRONMENT_PSI_PREFIX = "http://textgrid.org/serviceregistry/environment/";
	public final static String HASH_OBJECT_PSI_PREFIX = "http://textgrid.org/serviceregistry/hash-object/";
	public final static String HAS_ENVIRONMENT_PSI = "http://textgrid.org/serviceregistry/model/has-environment";
	public final static String ENVIRONMENT_ROLE_TYPE_PSI = "http://textgrid.org/serviceregistry/model/environment";
	public final static String KEY_OCCURRENCE_TYPE_PSI = "http://textgrid.org/serviceregistry/model/key";
	public final static String TM_SPARQL_ENDPOINT = Window.Location.getProtocol() + "//" + Window.Location.getHost() + "/gdl/tm-sparql";
	public final static String DELETE_REQUEST_URL = Window.Location.getProtocol() + "//" + Window.Location.getHost() + "/gdl/delete";
	public final static String COMMIT_REQUEST_URL = Window.Location.getProtocol() + "//" + Window.Location.getHost() + "/gdl/commit";
	public final static String GET_FRAGMENT_REQUEST_URL = Window.Location.getProtocol() + "//" + Window.Location.getHost() + "/gdl/fragment/";
	public final static String GET_SCHEMA_REQUEST_URL = Window.Location.getProtocol() + "//" + Window.Location.getHost() + "/gdl/schema";
	//public final static String GET_SCHEMA_REQUEST_URL = GWT.getModuleBaseURL() + "TextGrid_ServiceRegistry_full_TMCL_and_GDL_Schema.jtm";
	public final static String GET_HASH_OBJECT_PSIS_URL = IsidorusConstants.TM_SPARQL_ENDPOINT;
	//public final static String GET_HASH_OBJECT_PSIS_URL = GWT.getModuleBaseURL() + "HashObjectPsis.json";
	public final static String GET_ENVIRONMENT_PSIS_URL = IsidorusConstants.TM_SPARQL_ENDPOINT;
	//public final static String GET_ENVIRONMENT_PSIS_URL = GWT.getModuleBaseURL() + "EnvironmentPsis.json";
}
