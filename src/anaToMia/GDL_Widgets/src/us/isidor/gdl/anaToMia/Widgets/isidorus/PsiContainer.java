package us.isidor.gdl.anaToMia.Widgets.isidorus;

import java.util.ArrayList;

import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;

import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;


public class PsiContainer {
	private ArrayList<Label> psiLabels = new ArrayList<Label>();
	private VerticalPanel containerPanel = new VerticalPanel();
	private ClickHandler lblClickHandler = null;
	private final String PSI_LABEL_STYLE_CLASS = "psi_label";
	
	
	public void removeFromParent(){
		this.containerPanel.removeFromParent();
	}
	
	
	public PsiContainer(ClickHandler labelClikcHandler) throws ExecutionException{
		if(labelClikcHandler == null) throw new ExecutionException("labelClickHandler must not be null");
		this.lblClickHandler = labelClikcHandler;
	}
	
	
	public void addPsi(String psi){
		if(psi == null) return;
		
		Label lbl = new Label(psi);
		lbl.addStyleName(this.PSI_LABEL_STYLE_CLASS);
		this.psiLabels.add(lbl);
		
		int widgetIdx = 0;
		for( ; widgetIdx != this.containerPanel.getWidgetCount(); ++widgetIdx){
			Widget wdgt = this.containerPanel.getWidget(widgetIdx);
			if((wdgt instanceof Label) && ((Label)wdgt).getText().compareTo(psi) >= 0){
				this.containerPanel.insert(lbl, widgetIdx);
				break;
			}
		}
		
		if(widgetIdx == this.containerPanel.getWidgetCount()) this.containerPanel.add(lbl);
		if(this.lblClickHandler != null)lbl.addClickHandler(this.lblClickHandler);
	}
	
	
	public void addPsi(JSONString psi){
		if(psi != null)	this.addPsi(psi.stringValue());
	}
	
	
	public ArrayList<String> getPsis(){
		ArrayList<String> result = new ArrayList<String>();
		for (Label lbl : this.psiLabels) result.add(lbl.getText());
		
		return result;
	}
	
	
	public void insertIn(Panel panel){
		if(panel == null) return;
		panel.add(this.containerPanel);
	}
}
