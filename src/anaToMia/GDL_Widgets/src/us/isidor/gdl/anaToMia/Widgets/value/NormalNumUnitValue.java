package us.isidor.gdl.anaToMia.Widgets.value;

import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;

public class NormalNumUnitValue extends NumUnitValue{
	public NormalNumUnitValue(){
		super.unit = null; // if unit is null, the default value is normal
		super.value = 0f;
	}
	
	
	public NormalNumUnitValue(String value) throws InvalidGdlSchemaException{
		String upperString = value.trim().toUpperCase();
		if(upperString.equals("NORMAL")){
			super.unit = null; // if unit is null, the default value is normal
			super.value = 0f;
		}else if(upperString.endsWith("PX")){
			this.value = makeFloat(upperString, 2);
			this.unit = CssUnit.PIXEL;
		}else if (upperString.endsWith("PT")){
			this.value = makeFloat(upperString, 2);
			this.unit = CssUnit.POINT;
		} else if(upperString.endsWith("%")){
			this.value = makeFloat(upperString, 1);
			this.unit = CssUnit.PERCENTAGE;
		} else {
			throw new InvalidGdlSchemaException("normal numeric values supported by the GDL containing a unit definition must be of the form <numeric-value>(pt|px|%)|normal, but found: " + value);
		}
	}
	
	
	@Override
	public String getCssValue() {
		if(super.unit == null){
			return "normal";
		} else {
			return super.getCssValue();
		}
	}
	
	
	// this method returns true, if the value must be treated as "auto"
	public boolean isNormal(){
		return super.unit == null;
	}
}