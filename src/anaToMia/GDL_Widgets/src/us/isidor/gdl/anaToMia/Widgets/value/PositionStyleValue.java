package us.isidor.gdl.anaToMia.Widgets.value;

public enum PositionStyleValue implements CssValue{
	STATIC,
	RELATIVE,
	ABSOLUTE;
	

	@Override
	public String getCssValue() {
		return this.toString().toLowerCase();
	}

}
