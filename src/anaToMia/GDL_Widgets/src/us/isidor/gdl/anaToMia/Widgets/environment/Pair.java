package us.isidor.gdl.anaToMia.Widgets.environment;

public class Pair<T, U> {
	private T first = null;
	private U second = null;
	
	
	@SuppressWarnings("unused")
	private Pair(){}
	
	
	public Pair(T first, U second){
		this.first = first;
		this.second = second;
	}
	
	
	public T getFirst(){
		return this.first;
	}
	
	
	public U getSecond(){
		return this.second;
	}
}
