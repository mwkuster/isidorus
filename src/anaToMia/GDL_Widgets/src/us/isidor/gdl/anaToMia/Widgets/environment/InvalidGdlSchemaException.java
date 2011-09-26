package us.isidor.gdl.anaToMia.Widgets.environment;

public class InvalidGdlSchemaException extends Exception {
	private static final long serialVersionUID = 9130908618285633391L;


	protected InvalidGdlSchemaException() {}
	
	
	public InvalidGdlSchemaException(String message){
		super(message);
	}
}
