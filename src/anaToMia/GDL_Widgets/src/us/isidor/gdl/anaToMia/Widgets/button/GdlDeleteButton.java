package us.isidor.gdl.anaToMia.Widgets.button;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Button;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.IDeleteCallback;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;

public class GdlDeleteButton extends GdlActionButton {
	protected GdlDeleteButton(){
		super();
	}
	
	
	public GdlDeleteButton(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		((Button)this.subElements.get(0)).setText("delete");
		this.getButton().addClickHandler(new DeleteButtonClickHandler(this));
	}
	
	
	protected class DeleteButtonClickHandler implements ClickHandler {
		private GdlDeleteButton owner = null; 
		
		public DeleteButtonClickHandler(GdlDeleteButton owner){
			this.owner = owner;
		}
		
		
		@Override
		public void onClick(ClickEvent event) {
			try{
				IDeleteCallback callback = this.owner.getRoot().getDeleteCallback();
				callback.deleteTmConstruct(this.owner.getGdlParent().getContent(null, false), this.owner.getRoot().getTmEngine(), this.owner.getId());
			}catch(Exception e){
				e.printStackTrace();
				Window.alert("caught: " + e.getMessage());
			}
		}		
	}
}
