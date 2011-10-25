package us.isidor.gdl.anaToMia.Widgets.base;

import java.util.ArrayList;
import us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel.TmEngine;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMap;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMapsTypes;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.GdlErrorTypes;
import us.isidor.gdl.anaToMia.Widgets.environment.GdlInstantiator;
import us.isidor.gdl.anaToMia.Widgets.environment.ICommitCallback;
import us.isidor.gdl.anaToMia.Widgets.environment.IDeleteCallback;
import us.isidor.gdl.anaToMia.Widgets.environment.IOnErrorCallback;
import us.isidor.gdl.anaToMia.Widgets.environment.ILoadSchemaCallback;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;
import us.isidor.gdl.anaToMia.Widgets.environment.TopicIdentifierTypes;
import us.isidor.gdl.anaToMia.Widgets.isidorus.IsidorusConstants;
import us.isidor.gdl.anaToMia.Widgets.view.GdlDefaultTopicView;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.AbsolutePanel;
import com.google.gwt.user.client.ui.Composite;


public class GdlPanel extends Composite{
	// tmEngine is used as Topic Maps engine for this GdlPanel instance
	private TmEngine tmEngine = null;
	
	// onErrorContainer references handlers that are executed if an error occurred, i.e. if one of the processes
	// GDL loading, GDL commit, GDL validate, GDL delete failed, but any other execution error also executes an
	// error handler
	private ArrayList<IOnErrorCallback> localOnErrorContainer = new ArrayList<IOnErrorCallback>();

	// if any value is set in this array list, the requested view is a Default-Creator-Topic-View
	private ArrayList<Pair<String, TopicIdentifierTypes>> requestedTopicsToCreate = new ArrayList<Pair<String,TopicIdentifierTypes>>();
	// if any value is set in this array list, the requested view is a Default-Editor-Topic-View
	private Pair<String, TopicIdentifierTypes> requestedTopicToEdit = null;
	
	// the GWT base for this panel 
	private AbsolutePanel mainPanel = new AbsolutePanel();
	
	// the actual view with the corresponding content
	GdlDefaultTopicView view = null;
	// the topic map that is the base of the created GDL view
	private TopicMap requestedSchemaTm = null;
	// the locator of the topic map "requestedSchemaTm"
	private final String REQUESTED_SCHEMA_TOPIC_MAP_LOCATOR = IsidorusConstants.TOPIC_MAP_IDENTIFIER;
	
	// callback instance for getting/loading the GDL schema
	private ILoadSchemaCallback loadSchemaCallback = null;
	// callback instance for committing the frontend data to the server
	private ICommitCallback commitCallback = null;
	// callback instance for deleting the frontend data on the server
	private IDeleteCallback deleteCallback = null;
	
	// contains all callbacks that are used as click handlers and the corresponding
	// html element id of the button element
	private static ArrayList<Pair<String, ArrayList<ClickHandler>>> buttonCallbacks = new ArrayList<Pair<String,ArrayList<ClickHandler>>>();

	
	// some constructors
	private GdlPanel() {
		initWidget(mainPanel);
	}
	
	
	protected GdlPanel(Pair<String, TopicIdentifierTypes> topicToEdit, ArrayList<Pair<String, TopicIdentifierTypes>> topicToCreate) throws ExecutionException{
		this();
		if(topicToEdit != null)this.requestedTopicToEdit = topicToEdit;
		if(topicToCreate != null && topicToCreate.size() != 0)this.requestedTopicsToCreate = topicToCreate;
		
		if(this.requestedTopicsToCreate.size() != 0 && this.requestedTopicToEdit != null)
			throw new ExecutionException("only one container can be set, i.e. either topics to create or a topic to edit");
	}
		
	
	public GdlPanel(Pair<String, TopicIdentifierTypes> topicToEdit, ArrayList<Pair<String, TopicIdentifierTypes>> topicToCreate, int width, int height) throws ExecutionException{
		this(topicToEdit, topicToCreate);
		this.setPixelSize(width, height);
	}
	
	
	public GdlPanel(Pair<String, TopicIdentifierTypes> topicToEdit, ArrayList<Pair<String, TopicIdentifierTypes>> topicToCreate, String width, String height) throws ExecutionException{
		this(topicToEdit, topicToCreate);
		this.setSize(width, height);
	}

	
	public static ArrayList<ClickHandler> getClickHandlers(String id){
		for (Pair<String, ArrayList<ClickHandler>> item : GdlPanel.buttonCallbacks)
			if(item.getFirst().equals(id)) return item.getSecond();
			
		return new ArrayList<ClickHandler>();
	}
	
	
	public static void addClickHandler(String id, ClickHandler handler) throws ExecutionException {
		if(id == null && handler == null) return;
		if(id == null || handler == null) throw new ExecutionException("id and handler must be set, but are: " + id + ", " + handler);
		
		ArrayList<ClickHandler> item = GdlPanel.getClickHandlers(id);
		if(item.size() == 0){
			ArrayList<ClickHandler> handlers = new ArrayList<ClickHandler>();
			handlers.add(handler);
			GdlPanel.buttonCallbacks.add(new Pair<String, ArrayList<ClickHandler>>(id, handlers));
		} else{
			item.add(handler);
		}
	}

	
	public void registerOnErrorHandler(String descriptorId, IOnErrorCallback handler){
		if(!localOnErrorContainer.contains(handler)){
			localOnErrorContainer.add(handler);
		}
	}
	
	
	public void deregisterOnErrorHandler(String descriptorId, IOnErrorCallback handler){
		localOnErrorContainer.remove(handler);
	}
	
	
	// getter for the Topic Maps engine instance
	public TmEngine getTmEngine(){
		return this.tmEngine;
	}
	
	
	// setter for the Topic Maps engine instance
	public void setTmEngine(TmEngine tmEngine) {
		// the Topic Maps engine can be only set if no Topic Maps engine is set
		// or if the topic map "tm" is null, or contains no topics and associations
		if(tmEngine != null && (requestedSchemaTm == null || (requestedSchemaTm.getTopics().length() == 0 && requestedSchemaTm.getAssociations().length() == 0))){
			this.tmEngine = tmEngine;
			this.requestedSchemaTm = this.tmEngine.createTopicMap(this.REQUESTED_SCHEMA_TOPIC_MAP_LOCATOR);
		}
	}
	
	
	// some setters for callback instances
	public void setLoadSchemaCallback(ILoadSchemaCallback callback) throws ExecutionException{
		if(callback == null) throw new ExecutionException("callback must not be null");
		this.loadSchemaCallback = callback;
	}
	
	
	public void setCommitCallback(ICommitCallback callback) throws ExecutionException{
		if(callback == null) throw new ExecutionException("callback must not be null");
		this.commitCallback = callback;
	}
	
	
	public void setDeleteCallback(IDeleteCallback callback) throws ExecutionException{
		if(callback == null) throw new ExecutionException("callback must not be null");
		this.deleteCallback = callback;
	}

	
	// some getters for callback instances
	public ILoadSchemaCallback getLoadSchemaCallback(){
		return this.loadSchemaCallback;
	}
	
	
	public ICommitCallback getCommitCallback(){
		return this.commitCallback;
	}
	
	
	public IDeleteCallback getDeleteCallback(){
		return this.deleteCallback;
	}
	

	// a getter for the internal topic map
	public TopicMap getSchemaTm(){
		return this.requestedSchemaTm;
	}
	
	
	// this method is responsible for loading the Topic Map by using the
	// loadCallback. After a successfully load operation the
	// onLoadHandlers are executed
	public void loadSchema(){
		try{
			if(tmEngine == null || requestedSchemaTm == null){
				throw new ExecutionException("No Topic Maps engine was set yet");
			}
			if(this.loadSchemaCallback == null){
				throw new ExecutionException("No LoadSchemaCallback was set yet");
			}
			this.loadSchemaCallback.loadSchema(this, this.requestedTopicToEdit, this.requestedTopicsToCreate);
		}catch(Exception e){
			for (IOnErrorCallback handler : localOnErrorContainer) {
				handler.onError(GdlErrorTypes.LoadError, e);
			}
		}
	}
	
	
	// this method is responsible for committing the Topic Map by using the
	// commitCallback. After a successfully commit operation the 
	// onCommitHandlers are executed
	public void doCommit(){
		try{
			if(tmEngine == null || requestedSchemaTm == null){
				throw new ExecutionException("No Topic Maps engine was set yet");
			}
			if(this.commitCallback == null){
				throw new ExecutionException("No CommitCallback was set yet");
			}
			ArrayList<Pair<Object, TopicMapsTypes>> data = new ArrayList<Pair<Object,TopicMapsTypes>>();
			data.add(new Pair<Object, TopicMapsTypes>(this.requestedSchemaTm, TopicMapsTypes.TopicMap));
			this.commitCallback.commitTmConstruct(data, null, this.tmEngine);
		}catch(Exception e){
			for (IOnErrorCallback handler : localOnErrorContainer) {
				handler.onError(GdlErrorTypes.CommitError, e);
			}
		}
	}
	
	
	// this method is responsible for validating the Topic Map by calling the
	// view's validate method. After a successfully validate operation the 
	// onValidateHandlers are executed
	public void doValidate() {
		try{
			if(tmEngine == null || requestedSchemaTm == null)
				throw new ExecutionException("No Topic Maps engine was set yet");
			
			// TODO: validate
			//	throw new InvalidContentException("The topic map content is not valid:\n" + tmEngine.exportTm(view.getContent()));

		//}catch(InvalidContentException e){
		//	for (IOnErrorCallback handler : localOnErrorContainer)
		//		handler.onError(GdlErrorTypes.ValidateError, e);
		} catch(Exception e){
			for (IOnErrorCallback handler : localOnErrorContainer)
				handler.onError(GdlErrorTypes.ExecutionError, e);
		}
	}
	
	
	// this method is responsible for deleting the Topic Map by using the
	// deleteCallback. After a successfully delete operation the 
	// onDeleteHandlers are executed and the panel's content is removed
	public void doDelete() {
		try{
			if(tmEngine == null || requestedSchemaTm == null)
				throw new ExecutionException("No Topic Maps engine was set yet");

			if(this.deleteCallback == null)
				throw new ExecutionException("No DeleteCallback was set yet");
	
			ArrayList<Pair<Object, TopicMapsTypes>> data = new ArrayList<Pair<Object,TopicMapsTypes>>();
			data.add(new Pair<Object, TopicMapsTypes>(this.requestedSchemaTm, TopicMapsTypes.TopicMap));
			this.deleteCallback.deleteTmConstruct(data, this.getTmEngine(), null);
		}catch(Exception e){
			for (IOnErrorCallback handler : localOnErrorContainer)
				handler.onError(GdlErrorTypes.DeleteError, e);
		}
	}
	
	
	// this method is responsible for generating a Topic Map fo the user's
	// data by using the view's getContent method.
	public ArrayList<Pair<Object, TopicMapsTypes>> getContent(boolean validate) throws Exception {
		try{
			return this.view.getContent(null, validate);
		}catch(Exception e){
			for (IOnErrorCallback handler : localOnErrorContainer) {
				handler.onError(GdlErrorTypes.TopicMapsGenerationError, e);
			}
			throw e;
		}
	}

	
	// Creates the actual view from the requested topic map
	public void createView(){
		try{
			if(this.requestedTopicsToCreate != null && this.requestedTopicsToCreate.size() != 0)
				this.view = GdlInstantiator.instantiateDefaultCreatorTopicView(this);
			else if(this.requestedTopicToEdit != null)
				this.view = GdlInstantiator.instantiateDefaultEditorTopicView(this);
			mainPanel.add(view);
		}catch(Exception e){
			Window.alert("could not create a view (" + e.getClass() + "): " + e.getMessage());
			e.printStackTrace();
			for (IOnErrorCallback handler : localOnErrorContainer) {
				handler.onError(GdlErrorTypes.ViewCreationError, e);
			}
		}
	}
	

	// remove all content from the main panel
	public void clear(){
		this.mainPanel.clear();
	}
	
	
	public ArrayList<Pair<String, TopicIdentifierTypes>> getReqeustedTopicsToCreate(){
		return this.requestedTopicsToCreate;
	}
	
	
	public Pair<String, TopicIdentifierTypes> getRequestedTopicToEdit(){
		return this.requestedTopicToEdit;
	}
}
