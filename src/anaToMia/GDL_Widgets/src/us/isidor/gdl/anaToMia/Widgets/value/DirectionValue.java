package us.isidor.gdl.anaToMia.Widgets.value;

public enum DirectionValue implements CssValue {
	LTR,
	RTL;
	
	
	public String getCssValue(){
		return this.toString().toLowerCase();
	}
}
