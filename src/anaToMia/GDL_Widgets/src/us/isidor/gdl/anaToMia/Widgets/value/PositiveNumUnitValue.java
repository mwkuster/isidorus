package us.isidor.gdl.anaToMia.Widgets.value;

import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;

public class PositiveNumUnitValue extends NumUnitValue {
	public PositiveNumUnitValue(){
		super.value = 0f;
	}
	
	
	public PositiveNumUnitValue(String value) throws InvalidGdlSchemaException{
		super(value);
		if(super.value < 0){
			throw new InvalidGdlSchemaException("positive numeric values supported by the GDL containing a unit definition must be of the form <positive-numeric-value>(pt|px|%), but found: " + value);
		}
	}
}
