package us.isidor.gdl.anaToMia.Widgets.value;

import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;

public class NumUnitValue implements CssValue{
	protected CssUnit unit = CssUnit.PIXEL;
	protected float value = 0f;
	
	
	// some constructors
	public NumUnitValue(){}
	
	
	public NumUnitValue(String numUnit) throws InvalidGdlSchemaException {
		String value = numUnit.toUpperCase();
		if(value.endsWith("PX")){
			this.value = makeFloat(value, 2);
			this.unit = CssUnit.PIXEL;
		}else if (value.endsWith("PT")){
			this.value = makeFloat(value, 2);
			this.unit = CssUnit.POINT;
		} else if(value.endsWith("%")){
			this.value = makeFloat(value, 1);
			this.unit = CssUnit.PERCENTAGE;
		} else {
			throw new InvalidGdlSchemaException("numeric values supported by the GDL containing a unit definition must be of the form <numeric-value>(pt|px|%), but found: " + numUnit);
		}
	}
	
	
	// a helper method that returns a float parsed of the passed stringToParse,
	// whereas the tailing endToIgnore characters are ignored
	protected float makeFloat(String stringToParse, int endToIgnore) throws InvalidGdlSchemaException {
		if(stringToParse == null || stringToParse.length() <= endToIgnore){
			throw new InvalidGdlSchemaException("numeric values supported by the GDL containing a unit definition must be of the form <numeric-value>(pt|px|%), but found: " + stringToParse);
		}
		
		String str = stringToParse.substring(0, stringToParse.length() - endToIgnore);
		
		try{
			return Float.valueOf(str);
		}catch(NumberFormatException e){
			throw new InvalidGdlSchemaException("numeric values supported by the GDL containing a unit definition must be of the form <numeric-value>(pt|px|%), but found: " + stringToParse);
		}
	}
	
	
	// returns the value represented by this instance as a css string
	public String getCssValue(){
		switch(this.unit){
		case PIXEL: return (int)this.value + "px";
		case POINT: return (int)this.value + "pt";
		default: return this.value + "%";
		}
	}
	
	
	// returns the numeric value as a float
	public int getNumValue(){
		return (int)this.value;
	}
	
	
	// returns the CssUnit that is represented by this instance
	public CssUnit getUnit(){
		return this.unit;
	}
	
	
	// a subset of CSS units that are supported by the GDL
	public enum CssUnit implements CssValue{
		POINT,
		PIXEL,
		PERCENTAGE;
		
		
		public String getCssValue(){
			return this.toString().toLowerCase();
		}
	}
}
