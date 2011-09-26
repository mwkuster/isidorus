package gdl.isidor.us.TopicMaps;


public class TmEngine {
	public native TopicMap createTM(String itemIdentifier) /*-{
		try{
			var factory, sys, tmid, tm;
    	    factory = $wnd.TopicMapSystemFactory.newInstance();
    		factory.setProperty('com.semanticheadache.tmjs.backend', 'memory');
    		sys = factory.newTopicMapSystem();
    		tmid = sys.createLocator(itemIdentifier);
    		tm = sys.createTopicMap(tmid);
    		return tm;
		}catch(e){
			$wnd.alert(e);
		}
	}-*/;
}
