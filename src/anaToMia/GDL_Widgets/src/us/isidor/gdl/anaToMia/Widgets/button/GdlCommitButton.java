package us.isidor.gdl.anaToMia.Widgets.button;


import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.ICommitCallback;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Button;


public class GdlCommitButton extends GdlActionButton {
	// TODO: implement
	
	protected GdlCommitButton(){
		super();
	}
	
	
	public GdlCommitButton(Topic tmRepresentative, Construct receivedData, GdlVisibleObject parent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, parent);
		((Button)this.subElements.get(0)).setText("commit");
		this.getButton().addClickHandler(new CommitButtonClickHandler());
	}
	
	
	protected class CommitButtonClickHandler implements ClickHandler {
		public CommitButtonClickHandler() { }
		
		
		@Override
		public void onClick(ClickEvent event) {
			try{
				ICommitCallback callback = GdlCommitButton.this.getRoot().getCommitCallback();
				
				callback.commitTmConstruct(GdlCommitButton.this.getGdlParent().getContent(null, true), GdlCommitButton.this.getId(), GdlCommitButton.this.getRoot().getTmEngine());
			}catch(Exception e){
				e.printStackTrace();
				Window.alert("caught: " + e.getMessage());
			}
		}		
	}
}
