package us.isidor.gdl.anaToMia.Widgets.value;

public enum ResizeValue implements CssValue {
	BOTH,
	VERTICAL,
	HORIZONTAL,
	NONE;

	
	@Override
	public String getCssValue() {
		return this.toString().toLowerCase();
	}

}
