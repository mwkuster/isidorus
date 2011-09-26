package us.isidor.gdl.anaToMia.Widgets.value;

public enum TextDecorationValue implements CssValue {
	UNDERLINE,
	OVERLINE,
	LINE_THROUGH,
	BLINK,
	NONE;

	
	@Override
	public String getCssValue() {
		return this.toString().toLowerCase().replace("_", "-");
	}	
}
