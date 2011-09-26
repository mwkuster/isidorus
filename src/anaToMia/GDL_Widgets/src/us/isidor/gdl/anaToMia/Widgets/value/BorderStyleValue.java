package us.isidor.gdl.anaToMia.Widgets.value;

public enum BorderStyleValue implements CssValue{
	NONE,
	HIDDEN,
	DOTTED,
	DASHED,
	SOLID,
	DOUBLE,
	GROOVE,
	RIDGE,
	INSET,
	OUTSET;
	
	public String getCssValue(){
		return this.toString().toLowerCase();
	}
}
