package us.isidor.gdl.anaToMia.Widgets.value;

public enum TextAlignValue implements CssValue{
	LEFT, 
	RIGHT,
	CENTER, 
	JUSTIFY;

	
	@Override
	public String getCssValue() {
		return this.toString().toLowerCase();
	}
}
