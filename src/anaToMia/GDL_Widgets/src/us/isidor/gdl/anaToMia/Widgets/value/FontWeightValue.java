package us.isidor.gdl.anaToMia.Widgets.value;

public enum FontWeightValue implements CssValue {
	NORMAL,
	BOLD,
	BOLDER,
	LIGHTER,
	_100,
	_200,
	_300,
	_400,
	_500,
	_600,
	_700,
	_800,
	_900;

	@Override
	public String getCssValue() {
		String str = this.toString().toLowerCase();
		if(str.startsWith("_")) return str.substring(1);
		else return str;
	}
	
	
	public static FontWeightValue fromString(String str) throws IllegalArgumentException{
		if(null == str) return null;
		
		String upperStr = str.toUpperCase();
		if(upperStr.equals("NORMAL")) return NORMAL;
		else if(upperStr.equals("BOLD")) return BOLD;
		else if(upperStr.equals("BOLDER")) return BOLDER;
		else if(upperStr.equals("LIGHTER")) return LIGHTER;
		else if(upperStr.equals("100")) return _100;
		else if(upperStr.equals("200")) return _200;
		else if(upperStr.equals("300")) return _300;
		else if(upperStr.equals("400")) return _400;
		else if(upperStr.equals("500")) return _500;
		else if(upperStr.equals("600")) return _600;
		else if(upperStr.equals("700")) return _700;
		else if(upperStr.equals("800")) return _800;
		else if(upperStr.equals("900")) return _900;
		else throw new IllegalArgumentException("the value " + str + "is not a FontWeightValue value");
	}
}
