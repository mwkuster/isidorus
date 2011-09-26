package us.isidor.gdl.anaToMia.Widgets.value;

public enum ListStyleTypeValue implements CssValue{
	DECIMAL,
	DECIMAL_LEADING_ZERO,
	LOWER_GREEK,
	LOWER_ROMAN,
	ARMENIAN,
	GEORGIAN,
	UPPER_ROMAN,
	LOWER_ALPHA,
	UPPER_ALPHA,
	LOWER_LATIN,
	UPPER_LATIN,
	DISC,
	CIRCLE,
	SQUARE,
	NONE;

	@Override
	public String getCssValue() {
		return this.toString().toLowerCase().replace("_", "-");
	}
	
	
	public static ListStyleTypeValue fromString(String str) throws IllegalArgumentException{
		if(null == str) return null;
		
		String upperStr = str.toUpperCase();
		if(upperStr.equals("DECIMAL")) return DECIMAL;
		else if(upperStr.equals("DECIMAL-LEADING-ZERO")) return DECIMAL_LEADING_ZERO;
		else if(upperStr.equals("LOWER-GREEK")) return LOWER_GREEK;
		else if(upperStr.equals("LOWER-ROMAN")) return LOWER_ROMAN;
		else if(upperStr.equals("ARMENIAN")) return ARMENIAN;
		else if(upperStr.equals("GEORGIAN")) return GEORGIAN;
		else if(upperStr.equals("UPPER-ROMAN")) return UPPER_ROMAN;
		else if(upperStr.equals("LOWER-ALPHA")) return LOWER_ALPHA;
		else if(upperStr.equals("UPPER-ALPHA")) return UPPER_ALPHA;
		else if(upperStr.equals("LOWER-LATIN")) return LOWER_LATIN;
		else if(upperStr.equals("UPPER-LATIN")) return UPPER_LATIN;
		else if(upperStr.equals("DISC")) return DISC;
		else if(upperStr.equals("CIRCLE")) return CIRCLE;
		else if(upperStr.equals("SQUARE")) return SQUARE;
		else if(upperStr.equals("NONE")) return NONE;
		else throw new IllegalArgumentException("the value " + str + "is not a FontWeightValue value");
	}
}
