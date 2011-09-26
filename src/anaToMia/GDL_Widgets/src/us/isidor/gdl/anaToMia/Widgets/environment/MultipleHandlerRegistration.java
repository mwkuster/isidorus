package us.isidor.gdl.anaToMia.Widgets.environment;

import java.util.ArrayList;

import com.google.gwt.event.shared.HandlerRegistration;

public class MultipleHandlerRegistration implements HandlerRegistration {
	private ArrayList<HandlerRegistration> registrations = new ArrayList<HandlerRegistration>();
	
	
	@Override
	public void removeHandler() {
		for (HandlerRegistration reg : this.registrations) {
			reg.removeHandler();
		}
	}
	
	
	public void addHandlerRegistration(HandlerRegistration registration){
		this.registrations.add(registration);
	}

}
