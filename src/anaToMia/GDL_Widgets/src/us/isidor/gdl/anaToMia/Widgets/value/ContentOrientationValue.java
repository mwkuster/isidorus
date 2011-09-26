package us.isidor.gdl.anaToMia.Widgets.value;

public enum ContentOrientationValue implements CssValue{
	HORIZONTAL,
	VERTICAL;

	@Override
	public String getCssValue() {
		return this.toString().toLowerCase();
	}
	
	
}
