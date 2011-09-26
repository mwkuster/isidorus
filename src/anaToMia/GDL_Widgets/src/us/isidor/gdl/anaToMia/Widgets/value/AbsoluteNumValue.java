package us.isidor.gdl.anaToMia.Widgets.value;

import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;

public class AbsoluteNumValue extends NumUnitValue{
	public AbsoluteNumValue() throws InvalidGdlSchemaException{
		super("0px");
	}
	
	
	public AbsoluteNumValue(String value) throws InvalidGdlSchemaException {
		String upperValue = value.toUpperCase();
		
		if(upperValue.endsWith("PX")){
			super.value = super.makeFloat(upperValue, 2);
			super.unit = CssUnit.PIXEL;
		}else if (upperValue.endsWith("PT")){
			super.value = super.makeFloat(upperValue, 2);
			super.unit = CssUnit.POINT;
		}else {
			throw new InvalidGdlSchemaException("border width values supported by the GDL containing a unit definition must be of the form <numeric-value>(pt|px), but found: " + value);
		}
	}
}
