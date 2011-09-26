package us.isidor.gdl.anaToMia.Widgets.value;

public enum TextTypeValue implements HtmlValue{
	Text,
	Password;

	
	@Override
	public String getHtmlValue() {
		return this.toString().toLowerCase();
	}
	
}
