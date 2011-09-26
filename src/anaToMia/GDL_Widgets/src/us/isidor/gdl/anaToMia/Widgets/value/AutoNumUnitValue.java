package us.isidor.gdl.anaToMia.Widgets.value;

import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;

public class AutoNumUnitValue extends NumUnitValue{
	public AutoNumUnitValue(){
		super.unit = null; // if unit is null, the default value is auto
		super.value = 0f;
	}
	
	
	public AutoNumUnitValue(String value) throws InvalidGdlSchemaException{
		String upperString = value.trim().toUpperCase();
		if(upperString.equals("AUTO")){
			super.unit = null; // if unit is null, the default value is auto
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
			throw new InvalidGdlSchemaException("auto numeric values supported by the GDL containing a unit definition must be of the form <numeric-value>(pt|px|%)|auto, but found: " + value);
		}
	}
	
	
	@Override
	public String getCssValue() {
		if(super.unit == null){
			return "auto";
		} else {
			return super.getCssValue();
		}
	}
	
	
	// this method returns true, if the value must be treated as "auto"
	public boolean isAuto(){
		return super.unit == null;
	}
}
