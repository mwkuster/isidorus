package us.isidor.gdl.anaToMia.TopicMaps.TmEngineModel;

public class MissingReference extends Exception {
	private static final long serialVersionUID = 1L;
	
	protected MissingReference() {}
	
	
	public MissingReference(String message){
		super(message);
	}
}
