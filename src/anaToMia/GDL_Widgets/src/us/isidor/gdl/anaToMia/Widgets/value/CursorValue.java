package us.isidor.gdl.anaToMia.Widgets.value;

public enum CursorValue implements CssValue{
	AUTO,
	DEFAULT,
	CROSSHAIR,
	POINTER,
	MOVE,
	N_RESIZE,
	NE_RESIZE,
	NW_RESIZE,
	E_RESIZE,
	SE_RESIZE,
	S_RESIZE,
	SW_RESIZE,
	W_RESIZE,
	TEXT,
	WAIT,
	HELP,
	PROGRESS;
	
	
	public String getCssValue(){
		return this.toString().toLowerCase();
	}
}
