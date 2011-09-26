package us.isidor.gdl.anaToMia.Widgets.value;

import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;


public class AutoNumValue implements CssValue{
	private Integer intValue = null;
	
	
	public AutoNumValue()throws InvalidGdlSchemaException{
		this("auto");
	}
	
	
	public AutoNumValue(String value) throws InvalidGdlSchemaException{
		String val = value.toLowerCase();
		if(val.equals("auto")){
			// do nothing, a null reference of intValue is handled as the value "autho"
		} else {
			try{
				intValue = Integer.valueOf(value);
			}catch(NumberFormatException e){
				throw new InvalidGdlSchemaException("An HTML auto-numeric value must bei either \"auto\" or an integer");
			}
		}
	}
	
	
	// return a string that contains either an integer as a string value
	// or the string auto
	public String getCssValue(){
		if(intValue == null){
			return "auto";
		}else {
			return String.valueOf(intValue);
		}
	}
	
	
	// returns an integer value or null. If null is returned, the value must be treated
	// as auto
	public Integer getIntegerValue(){
		return intValue;
	}


	// this method returns true, if the value must be treated as "auto"
	public boolean isAuto(){
		return intValue == null;
	}
}
