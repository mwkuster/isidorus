package us.isidor.gdl.anaToMia.Widgets.value;

public enum ListStylePositionValue implements CssValue {
	INSIDE,
	OUTSIDE;
	

	@Override
	public String getCssValue() {
		return this.toString().toLowerCase();
	}

}
