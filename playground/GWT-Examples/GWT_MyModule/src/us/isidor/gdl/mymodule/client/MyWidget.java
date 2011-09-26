package us.isidor.gdl.mymodule.client;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.ui.AbsolutePanel;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.TextBox;


public class MyWidget extends Composite{
	private AbsolutePanel mainPanel;
	private CheckBox checkbox;
	private TextBox textbox;
	private Button button;
	private int width = 900;
	private int height = 450;
	
	public MyWidget(){
		checkbox = new CheckBox("read-only");
		checkbox.setValue(true);
		textbox = new TextBox();
		textbox.setText("default value");
		textbox.setReadOnly(true);
		button = new Button();
		button.setText("destroy");
		
		mainPanel = new AbsolutePanel();
		mainPanel.setPixelSize(width, height);
		DOM.setStyleAttribute(mainPanel.getElement(), "backgroundColor", "Yellow");
		
		
		button.addClickHandler(new ClickHandler(){
			@Override
			public void onClick(ClickEvent event) {
				MyWidget.this.removeFromParent();
				MyWidget.this.destroy();
			}
		});
		
		checkbox.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				textbox.setReadOnly(checkbox.getValue());
			}
		});
		
		initWidget(mainPanel);
	}
	
	
	@Override
	protected void onAttach() {
		super.onAttach();
		
		mainPanel.add(checkbox);
		mainPanel.add(textbox);
		mainPanel.add(button);
		
		int allSpace = this.width - (checkbox.getOffsetWidth() + textbox.getOffsetWidth() + button.getOffsetWidth());
		int elemSpace = allSpace / 4;
		mainPanel.setWidgetPosition(checkbox, elemSpace, checkbox.getAbsoluteTop() - this.getAbsoluteTop());
		mainPanel.setWidgetPosition(textbox, 2 * elemSpace + checkbox.getOffsetWidth(), textbox.getAbsoluteTop() - this.getAbsoluteTop());
		mainPanel.setWidgetPosition(button, 3* elemSpace + checkbox.getOffsetHeight() + textbox.getOffsetWidth(), button.getAbsoluteTop() - this.getAbsoluteTop());
	}
	
		
	private void destroy(){
		this.checkbox = null;
		this.mainPanel = null;
		this.textbox = null;
		this.button = null;
	}
}
