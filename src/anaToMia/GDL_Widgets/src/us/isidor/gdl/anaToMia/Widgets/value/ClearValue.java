package us.isidor.gdl.anaToMia.Widgets.value;

public enum ClearValue implements CssValue{
	NONE,
	LEFT,
	RIGHT,
	BOTH;
	
	public String getCssValue(){
		return this.toString().toLowerCase();
	}
}
