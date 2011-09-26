package us.isidor.gdl.anaToMia.Widgets.base;

import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.DialogBox;
import com.google.gwt.user.client.ui.HasHorizontalAlignment;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.VerticalPanel;

public class ButtonDialog extends DialogBox {
	private Button btn1 = new Button();
	private Button btn2 = new Button();
	private HandlerRegistration hrBtn1 = null;
	private HandlerRegistration hrBtn2 = null;
	private Label message = new Label();
	private VerticalPanel mainPanel = new VerticalPanel();
	private HorizontalPanel buttonPanel = new HorizontalPanel();
	
	
	public ButtonDialog(){
		this.add(mainPanel);
		mainPanel.add(message);
		mainPanel.add(buttonPanel);
		mainPanel.setCellHorizontalAlignment(buttonPanel, HasHorizontalAlignment.ALIGN_RIGHT);
		buttonPanel.add(btn1);
		buttonPanel.add(btn2);
		buttonPanel.setSpacing(5);
	}
	
	
	public ButtonDialog(String dialogText, String dialogMessage, String leftButtonText, String rightButtonText, ClickHandler leftButtonHandler, ClickHandler rightButtonHandler){
		this();
		btn1.setText(leftButtonText);
		btn2.setText(rightButtonText);
		this.setText(dialogText);
		message.setText(dialogMessage);
		if(leftButtonHandler != null) hrBtn1 = btn1.addClickHandler(leftButtonHandler);
		if(rightButtonHandler != null) hrBtn2 = btn2.addClickHandler(rightButtonHandler);
	}


	public void setLeftButtonText(String text){
		btn1.setText(text);
	}
	
	
	public void setRightButtonText(String text){
		btn2.setText(text);
	}
	
	
	public void setDialogMessage(String text){
		message.setText(text);
	}
	
	
	public void setLeftButtonClickHandler(ClickHandler handler){
		if(handler != null){
			if(hrBtn1 != null) hrBtn1.removeHandler();
			hrBtn1 = btn1.addClickHandler(handler);
		}
	}
	
	
	public void setRightButtonClickHandler(ClickHandler handler){
		if(handler != null){
			if(hrBtn2 != null) hrBtn2.removeHandler();
			hrBtn2 = btn2.addClickHandler(handler);
		}
	}
}
