package us.isidor.gdl.anaToMia.Widgets.value;

import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;

public class ColorValue implements CssValue{
	private String stringValue = null;
	
	
	// some constructors
	public ColorValue(){
		this.stringValue = "#000000";
	}
	
	
	public ColorValue(String color) throws InvalidGdlSchemaException{
		String value = color.toUpperCase();
		
		if(value.matches("^(MAROON|RED|ORANGE|YELLOW|OLIVE|PURPLE|FUCHSIA|WHITE|LIME|GREEN|NAVY|BLUE|AQUA|TEAL|BLACK|SILVER|GRAY)$")){
			this.stringValue = cssColorToRRGGBB(CssColor.valueOf(value));
		}else if(value.matches("^#[0-9A-F]{6}$")) {
			this.stringValue = value;
		}else if(value.matches("^#[0-9A-F]{3}$")) {
			this.stringValue = "#" + value.charAt(1) + value.charAt(1) + value.charAt(2) + value.charAt(2) + value.charAt(3) + value.charAt(3);
		}else if(value.matches("^RGB\\( *\\+?[0-9]{1,3} *, *\\+?[0-9]{1,3} *, *\\+?[0-9]{1,3} *\\)$")){
			String[] rgb = value.substring(4, value.length() - 1).split(",");
			this.stringValue = "#" + decToHexIntegerString(rgb[0]) + decToHexIntegerString(rgb[1]) + decToHexIntegerString(rgb[2]); 
		}else if(value.matches("^RGB\\( *\\+?[0-9]{1,3}% *, *\\+?[0-9]{1,3}% *, *\\+?[0-9]{1,3}% *\\)$")){
			String[] rgb = value.substring(4, value.length() - 1).split(",");
			this.stringValue = "#" + percentToHexIntegerString(rgb[0]) + percentToHexIntegerString(rgb[1]) + percentToHexIntegerString(rgb[2]); 
		}else {
			throw new InvalidGdlSchemaException("a color value must be a value of the format #RRGGBB, #RGB, rgb(rrr,ggg,bbb), rgb(rrr%,ggg%,bbb%) or a CssColor, but is " + color);
		}
	}
	
	
	// converts an integer string of a decimal to a hex representation
	private String decToHexIntegerString(String decIntegerString){
		int intValue = Integer.valueOf(decIntegerString.replaceFirst("\\+", "").trim());
		intValue = intValue > 255 ? 255 : intValue;
		String result = Integer.toHexString(intValue);
		return result.length() == 1 ? "0" + result : result;
	}
	
	
	// converts an integer representing a percentage value string 
	// to a hex representation
	private String percentToHexIntegerString(String percentageString){
		String rawValue = percentageString.replaceFirst("%", "").replaceFirst("\\+", "").trim();
		int percentValue = Integer.valueOf(rawValue);
		percentValue = percentValue > 100 ? 100 : percentValue;
		String result = Integer.toHexString((int)(255 * ((float)percentValue / 100)));
		return result.length() == 1 ? "0" + result : result;
	}
	
	
	public ColorValue(int r, int g, int b) throws InvalidGdlSchemaException {
		this("rgb(" + r + "," + g + "," + b + ")");
	}
	
	
	public ColorValue(CssColor color){
		this.stringValue = cssColorToRRGGBB(color); 
	}
	
	
	// a helper method that parses CssColor instances
	// to a string of the format ##RRGGBB
	private String cssColorToRRGGBB(CssColor color){
		switch(color){
		case MAROON: return "#800000";
		case RED: return "#ff0000";
		case ORANGE: return "#ffa500";
		case YELLOW: return "#ffff00";
		case OLIVE: return "#808000";
		case PURPLE: return "#800080";
		case FUCHSIA: return "#ff00ff";
		case WHITE: return "#ffffff";
		case LIME: return "#00ff00";
		case GREEN: return "#008000";
		case NAVY: return "#000080";
		case BLUE: return "#0000ff";
		case AQUA: return "#00ffff";
		case TEAL: return "#008080";
		case BLACK: return "#000000";
		case SILVER: return "#c0c0c0";
		default: return "#808080";
		}
	}
	
	
	// returns a string of the format #RRGGBB
	public String getCssValue(){
		return this.stringValue;
	}
	
	
	// returns an int array of the form [r, g, b] of the stored
	// color value
	public int[] getRGBvalue(){
		String r = "" + this.stringValue.charAt(1) + this.stringValue.charAt(2);
		String g = "" + this.stringValue.charAt(3) + this.stringValue.charAt(4);
		String b = "" + this.stringValue.charAt(5) + this.stringValue.charAt(6);
		
		return new int[]{Integer.valueOf(r, 16), Integer.valueOf(g, 16), Integer.valueOf(b, 16)};
	}
	
	
	// returns a CSSColor instance of the stored value, if it is
	// a value that corresponds to the defined keywords, otherwise
	// the return value is null
	public CssColor getCssColor(){
		if(this.stringValue.equals("#800000")){
			return CssColor.MAROON;
		}else if(this.stringValue.equals("#ff0000")){
			return CssColor.RED;
		}else if(this.stringValue.equals("#ffa500")) {
			return CssColor.ORANGE;
		}else if(this.stringValue.equals("#ffff00")){
			return CssColor.YELLOW;
		}else if(this.stringValue.equals("#808000")) {
			return CssColor.OLIVE;
		}else if(this.stringValue.equals("#800080")){
			return CssColor.PURPLE;
		}else if(this.stringValue.equals("#ff00ff")) {
			return CssColor.FUCHSIA;
		}else if(this.stringValue.equals("#ffffff")){
			return CssColor.WHITE;
		}else if(this.stringValue.equals("#00ff00")) {
			return CssColor.LIME;
		}else if(this.stringValue.equals("#008000")) {
			return CssColor.GREEN;
		}else if(this.stringValue.equals("#000080")) {
			return CssColor.NAVY;
		}else if(this.stringValue.equals("#0000ff")) {
			return CssColor.BLUE;
		}else if(this.stringValue.equals("#00ffff")) {
			return CssColor.AQUA;
		}else if(this.stringValue.equals("#008080")) {
			return CssColor.TEAL;
		}else if(this.stringValue.equals("#000000")) {
			return CssColor.BLACK;
		}else if(this.stringValue.equals("#c0c0c0")) {
			return CssColor.SILVER;
		}else if(this.stringValue.equals("#808080")) {
			return CssColor.GRAY;
		}else {
			return null;
		}
	}
	
	
	// represents the color key words that are defined in CSS chapter 4.3.6
	// (http://www.w3.org/TR/CSS21/syndata.html#value-def-color)
	public enum CssColor implements CssValue{
		MAROON,
		RED,
		ORANGE,
		YELLOW,
		OLIVE,
		PURPLE,
		FUCHSIA,
		WHITE,
		LIME,
		GREEN,
		NAVY,
		BLUE,
		AQUA,
		TEAL,
		BLACK,
		SILVER,
		GRAY;
		
		public String getCssValue(){
			return this.toString().toLowerCase();
		}
	}
}
