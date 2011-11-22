package us.isidor.gdl.anaToMia.Widgets.base;

import java.util.ArrayList;
import java.util.Arrays;

import com.google.gwt.core.client.JavaScriptObject;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Association;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Role;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.environment.Pattern;
import com.google.gwt.core.client.JsArray;
import com.google.gwt.dom.client.Element;

public class Utils {
	
	// sorts a passed ArrayList
	@SuppressWarnings("unchecked")
	public static <T> ArrayList<T> sort(ArrayList<T> arrayList){
		if(arrayList == null) return new ArrayList<T>();
		
		Object[] array = arrayList.toArray();
		Arrays.sort(array);
		
		ArrayList<T> result = new ArrayList<T>();
		for (Object item : array) result.add((T)item);
		
		return result;
	}
	
	
	// returns an array with two string objects.
	//  * the first element is the uri prefix until the last "/" or "#" character
	//  * the second element is the sufix
	public static String[] splitUriByLastFragment(String uri) {
		if(uri == null) return new String[]{null, null};
		
		int idxSlash = uri.lastIndexOf("/");
		int idxSharp = uri.lastIndexOf("#");
		int lastPos = idxSlash < idxSharp ? idxSharp : idxSlash;
		if(lastPos <= 0) return new String[]{uri, null}; 
		
		String prefix = uri.substring(0, lastPos + 1);
		String suffix = uri.substring(lastPos + 1);
		suffix = suffix == null ? "" : suffix;

		return new String[]{prefix, suffix};
	}
	
	
	// returns a string of the form:
	// {
	// association-type: psi
	// roles: [type: psi, player: psi]
	// [type: psi, player: psi]...
	// }
	// this method is only defined for debugging purpose
	public static String toString(Association association){
		if(association == null) return "{ }";
		
		String result = "{\ntype: ";
		result += TmHelper.getAnyIdOfTopic(association.getType());
		result += "\n\nroles:\n";
		
		JsArray<Role> roles = association.getRoles();
		for(int i = 0; i != roles.length(); ++i)
			result += "[type: " + TmHelper.getAnyIdOfTopic(roles.get(i).getType()) + "\nplayer: " + TmHelper.getAnyIdOfTopic(roles.get(i).getPlayer()) + "]\n\n";
			
		return result.substring(0, result.length() - 1) + "\n}";
	}
	
	
	// returns a string of the form [item1, item2, ... ] for all
	// items of an ArrayList
	public static <T> String arrayToString(ArrayList<T> data){
		if(data == null || data.size() == 0) return "[ ]";
		
		String result = "[";
		for (T t : data) {
			result += t + ", ";
		}
		
		return result.substring(0, result.length() - 2) + "]";
	}
	
	
	// returns a string of the form [item1, item2, ... ] for all
	// topics of an ArrayList, whereas each item is any identifier
	// of the topic
	public static String topicArrayToString(ArrayList<Topic> topicList){
		if(topicList == null || topicList.size() == 0) return "[ ]";
		
		String result = "[";
		for (Topic top : topicList) {
			result += TmHelper.getAnyIdOfTopic(top) + ", ";
		}
		
		return result.substring(0, result.length() - 2) + "]";
	}
	
	
	// returns true if both arrays have the same items
	public static <T> boolean compareLists(ArrayList<T> lft, ArrayList<T> rgt){
		if(lft == null && rgt == null) return true;
		if(lft == null || rgt == null) return false;
		if(lft.size() != rgt.size()) return false;
		
		for(Object obj : lft) if(!rgt.contains(obj))return false;
		
		// because of duplicate values the reverse comparison must also be done
		for(Object obj : rgt) if(!lft.contains(obj))return false;
			
		return true;
	}
	
	
	public static <T extends JavaScriptObject> boolean contains (JsArray<T> container, T item){
		if(container == null || item == null) return false;
		
		for(int i = 0; i != container.length(); ++i)
			if(container.get(i).equals(item)) return true;
		
		return false;
	}
	
	
	public static <T extends JavaScriptObject> ArrayList<T> jsArrayToArrayList(JsArray<T> input){
		ArrayList<T> result = new ArrayList<T>();
		
		if(input != null) for(int i = 0; i != input.length(); ++i) result.add(input.get(i));
		
		return result;
	}
	
	
	// returns a list that contains a union of both lists
	public static <T> ArrayList<T> intersection(ArrayList<T> fst, ArrayList<T> snd){
		ArrayList<T> result = new ArrayList<T>();
		if(fst == null || snd == null) return result;
		
		for (T t : fst)	if(snd.contains(t)) result.add(t);
		
		return result;
	}

	
	// returns an ArrayList that is a merged ArrayList of fst and snd
	public static <T> ArrayList<T> union(ArrayList<T> fst, ArrayList<T> snd){
		ArrayList<T> result = new ArrayList<T>();
		
		if(fst != null)	for (T t : fst) if(!result.contains(t)) result.add(t);
		if(snd != null) for (T t : snd) if(!result.contains(t)) result.add(t);
		
		return result;
	}
	
	
	// returns true if the string consists only of digits
	public static boolean isDecNumber(String str){
		if(str == null) return false;
		for(int i = 0; i != str.length(); ++i) if(!Character.isDigit(str.charAt(i))) return false;
		return true;
	}
	
	
	// replaces the given style property by the passed value, if no old value
	// for this property was found, the new valu is imply added to the style attribute
	public static void replaceStyleProperty(Element elem, String styleName, String styleValue){
		String oldStyle = elem.getAttribute("style");
		String newValue = styleName + ": " + styleValue + ";";
		
		Pattern pattern1 = new Pattern("(^| )" + styleName + " *:[^;]");
		Pattern pattern2 = new Pattern(";" + styleName + " *:[^;]");
		if(pattern1.matches(oldStyle)){
			elem.setAttribute("style", oldStyle.replaceFirst(pattern1.pattern(), newValue));
		}else if(pattern2.matches(oldStyle)){
			elem.setAttribute("style", oldStyle.replaceFirst(pattern1.pattern(), ";" + newValue));
		}else{
			String valueToAppend = styleName + ": " + styleValue + ";";
			if(oldStyle.length() == 0) valueToAppend = " " + valueToAppend;
			elem.setAttribute("style", oldStyle + valueToAppend);
		}			
	}
}
