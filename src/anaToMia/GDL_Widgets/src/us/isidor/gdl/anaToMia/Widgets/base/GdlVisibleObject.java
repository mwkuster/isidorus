package us.isidor.gdl.anaToMia.Widgets.base;


import java.util.ArrayList;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Association;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Locator;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Name;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Reifiable;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.ReifiableStub;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Role;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMap;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.TopicMapsTypes;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Variant;
import us.isidor.gdl.anaToMia.Widgets.button.GdlActionButton;
import us.isidor.gdl.anaToMia.Widgets.environment.ActiveStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.FocusStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.GdlInstantiator;
import us.isidor.gdl.anaToMia.Widgets.environment.HoverStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidContentException;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.environment.MultipleHandlerRegistration;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;
import us.isidor.gdl.anaToMia.Widgets.environment.Pattern;
import us.isidor.gdl.anaToMia.Widgets.text.GdlInfo;
import us.isidor.gdl.anaToMia.Widgets.value.AbsoluteNumValue;
import us.isidor.gdl.anaToMia.Widgets.value.AutoNumUnitValue;
import us.isidor.gdl.anaToMia.Widgets.value.AutoNumValue;
import us.isidor.gdl.anaToMia.Widgets.value.BorderStyleValue;
import us.isidor.gdl.anaToMia.Widgets.value.ClearValue;
import us.isidor.gdl.anaToMia.Widgets.value.ColorValue;
import us.isidor.gdl.anaToMia.Widgets.value.ContentOrientationValue;
import us.isidor.gdl.anaToMia.Widgets.value.CursorValue;
import us.isidor.gdl.anaToMia.Widgets.value.NumUnitValue;
import com.google.gwt.core.client.JsArray;
import com.google.gwt.dom.client.Style.Display;
import com.google.gwt.dom.client.Style.Float;
import com.google.gwt.dom.client.Style.VerticalAlign;
import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.FocusEvent;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.dom.client.HasBlurHandlers;
import com.google.gwt.event.dom.client.HasClickHandlers;
import com.google.gwt.event.dom.client.HasFocusHandlers;
import com.google.gwt.event.dom.client.HasMouseDownHandlers;
import com.google.gwt.event.dom.client.HasMouseOutHandlers;
import com.google.gwt.event.dom.client.HasMouseOverHandlers;
import com.google.gwt.event.dom.client.HasMouseUpHandlers;
import com.google.gwt.event.dom.client.MouseDownEvent;
import com.google.gwt.event.dom.client.MouseDownHandler;
import com.google.gwt.event.dom.client.MouseOutEvent;
import com.google.gwt.event.dom.client.MouseOutHandler;
import com.google.gwt.event.dom.client.MouseOverEvent;
import com.google.gwt.event.dom.client.MouseOverHandler;
import com.google.gwt.event.dom.client.MouseUpEvent;
import com.google.gwt.event.dom.client.MouseUpHandler;
import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Element;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.AbsolutePanel;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;


public abstract class GdlVisibleObject extends Composite implements GdlDescriptor, HasClickHandlers, HasMouseOutHandlers, HasMouseOverHandlers, HasFocusHandlers, HasMouseDownHandlers, HasMouseUpHandlers, HasBlurHandlers{
	private GdlVisibleObject gdlParent = null;
	protected AbsolutePanel mainPanel = new AbsolutePanel();
	protected Panel containerPanel = null;
	protected Topic tmRepresentative = null;
	protected TopicMap tm = null;
	protected ArrayList<Pair<String, String>> activeCssNamesAndStyles = new ArrayList<Pair<String,String>>();
	protected ArrayList<Pair<String, String>> focusCssNamesAndStyles = new ArrayList<Pair<String,String>>();
	protected ArrayList<Pair<String, String>> hoverCssNamesAndStyles = new ArrayList<Pair<String,String>>();
	protected ArrayList<Widget> subElements = new ArrayList<Widget>();
	protected ArrayList<Pair<Widget, ArrayList<EventHandler>>> eventHandlers = new ArrayList<Pair<Widget, ArrayList<EventHandler>>>();
	protected ArrayList<Pair<Topic, Integer>> actionButtonsAndPositions = null;
	protected ArrayList<GdlInfo> infoElements = new ArrayList<GdlInfo>();
	protected Construct receivedData = null;
	protected Topic valueGroupTopic = null;
	protected boolean valueGroupTopicSet = false;
	protected Topic constraintTopic = null;
	protected boolean constraintTopicSet = false;
	protected Topic rootConstraintTopic = null;
	protected boolean rootConstraintTopicSet = false;
	protected Topic defaultTmValueTopic = null;
	protected boolean defaultTmValueTopicSet = false;
	protected Topic defaultLiteralValueTopic = null;
	protected boolean defaultLiteralValueTopicSet = false;
	protected ArrayList<Topic> tmValueTopics = new ArrayList<Topic>();
	protected boolean tmValueTopicsSet = false;
	protected ArrayList<Topic> literalValueTopics = new ArrayList<Topic>();
	protected boolean literalValueTopicsSet = false;
	protected ArrayList<Topic> preferredScopes = new ArrayList<Topic>();
	protected boolean prefferedScopesSet = false;
	protected Topic displayByConstraint = null;
	protected boolean displayByConstraintSet = false;
	protected ArrayList<String> literals = new ArrayList<String>();
	protected boolean literalsSet = false;
	protected int cardMin = 0;
	protected boolean cardMinSet = false;
	protected int cardMax = 0;
	protected boolean cardMaxSet = false;
	protected ArrayList<Topic> rawTmValues = new ArrayList<Topic>();
	protected boolean rawTmValuesSet = false;


	// some constructors
	protected GdlVisibleObject() {
		initWidget(this.mainPanel);
		DOM.setStyleAttribute(this.mainPanel.getElement(), "overflow", "visible");
	}


	public GdlVisibleObject(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		this();
		this.tmRepresentative = tmRepresentative;
		this.tm = this.tmRepresentative.getTopicMap();
		this.gdlParent = gdlParent;

		if(!(receivedData instanceof Topic) && !(receivedData instanceof Association) && !(receivedData instanceof Name) && !(receivedData instanceof Variant) && !(receivedData instanceof Occurrence) && !(receivedData instanceof Role) && receivedData != null) throw new ExecutionException("receivedData must be either a Topic, Association, Topic-Name, Name-Variant, Topic-Occurrence or Association-Role, but is: " + receivedData.getClass());
		this.receivedData = receivedData;

		this.setId(this.getId());
		this.setGdlStyle();
	}


	public GdlVisibleObject getGdlParent(){
		return this.gdlParent;
	}


	public int getSubElementsCount(){
		if(this.subElements == null) return 0;
		return this.subElements.size();
	}


	public GdlPanel getRoot(){
		return this.getGdlParent().getRoot();
	}


	// this method takes a string and creates a new sub-element within
	// an instance of GdlVisibleObject with the passed string value
	public abstract void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException;


	// returns the topic that represents this element
	public Topic getTmRepresentative(){
		return this.tmRepresentative;
	}


	// sets all info elements as defined in the GDL
	public void setInfoElements() throws InvalidGdlSchemaException, ExecutionException{
		ArrayList<Topic> infos = TmHelper.topicContainsInfo(this.tmRepresentative);

		for (Topic info : infos){
			GdlInfo elem = (GdlInfo)GdlInstantiator.instantiate(info, this.receivedData, this);
			this.infoElements.add(elem);
			elem.setPosition(this);			
		}
	}


	// this method should be invoked if a new sub-element is added to this instance
	protected ButtonableObject addToContainerPanel(Widget widget) throws ExecutionException, InvalidGdlSchemaException{
		this.setContentOrientation(this.getContentOrientation());		
		ButtonableObject btn = new ButtonableObject(widget);
		this.subElements.add(btn);
		this.containerPanel.add(btn);
		this.setNthButtons();
		return btn;
	}


	// removes the passed widget, it's parent, and returns the parent
	protected ButtonableObject removeFromContainer(Widget widget) throws InvalidGdlSchemaException, ExecutionException {
		for (Widget elem : this.subElements) {
			if(((ButtonableObject)elem).getMainObject().equals(widget)){
				elem.removeFromParent();
				this.subElements.remove(elem);
				return (ButtonableObject)elem;
			}
		}
		this.setNthButtons();

		return null;
	}


	// sets all buttons in the sub-elements that are specified by a
	// gdl:button-position association
	protected void setNthButtons() throws InvalidGdlSchemaException, ExecutionException {
		if(this.actionButtonsAndPositions == null) this.actionButtonsAndPositions = TmHelper.topicContainsNthButtons(this.tmRepresentative);

		for(int i = 0; i != this.subElements.size(); ++i){
			// search all buttons for the current sub element
			ArrayList<Topic> currentButtons = new ArrayList<Topic>();
			for (Pair<Topic, Integer> pair : this.actionButtonsAndPositions)
				if(pair.getSecond() == i || (i == this.subElements.size() -1 && pair.getSecond() == -1)) currentButtons.add(pair.getFirst());

			// remove buttons that do not belong to the current sub element anymore
			((ButtonableObject)this.subElements.get(i)).removeObsoleteButtons(currentButtons);

			// add buttons that are not bound to the current sub element, but belong to it
			for (Topic currentButton : currentButtons)
				if(!((ButtonableObject)this.subElements.get(i)).containsButton(currentButton)) ((ButtonableObject)this.subElements.get(i)).addButton((GdlActionButton)GdlInstantiator.instantiate(currentButton, null, this));
		}
	}


	// a helper method that returns all occurrences of the type bound to the passed PSI
	@SuppressWarnings("unchecked")
	protected JsArray<Occurrence> getOccurrences(String occurrenceType){
		Topic occType = tm.getTopicBySubjectIdentifier(tm.createLocator(occurrenceType));
		if(occType == null) return (JsArray<Occurrence>)JsArray.createArray();
		else return tmRepresentative.getOccurrences(occType);
	}


	// a helper method that returns one occurrence of the type bound to the passed PSI.
	// If more than one occurrence is available an InvalidGdlSchemaException is thrown.
	// If nor occurrence is available the return value is null
	protected Occurrence getNoneOrOneUnscopedOccurrence(String occurrenceType) throws InvalidGdlSchemaException{
		JsArray<Occurrence> occs = getOccurrences(occurrenceType);
		ArrayList<Occurrence> unscopedOccs = new ArrayList<Occurrence>();
		for(int i = 0; i != occs.length(); ++i){
			if(occs.get(i).getScope().length() == 0) unscopedOccs.add(occs.get(i));
		}

		if(unscopedOccs.size() > 1){
			throw new InvalidGdlSchemaException("The topic " + TmHelper.getAnyIdOfTopic(this.tmRepresentative) + " must be bound to none or one unscoped occurrence of the type " + occurrenceType + ", but is bound " + unscopedOccs.size() + " times to it");
		} else if(unscopedOccs.size() == 1){
			return unscopedOccs.get(0);
		} else {
			return null;
		}
	}


	// a helper method that returns one occurrence of the type bound to the passed PSI and scoped
	// by the theme bound to the passed PSI. If no such occurrence exist, the default value is null
	protected Occurrence getNoneOrOneScopedOccurrence(String occurrenceType, String theme) throws InvalidGdlSchemaException{
		Topic themeTopic = tm.getTopicBySubjectIdentifier(tm.createLocator(theme));
		if(themeTopic == null){
			return null;
		} else {
			JsArray<Occurrence> occurrences = getOccurrences(occurrenceType);
			ArrayList<Occurrence> matchedOccurrences = new ArrayList<Occurrence>();
			for(int i = 0; i != occurrences.length(); ++i){
				for(int j = 0; j != occurrences.get(i).getScope().length(); ++j){
					if(occurrences.get(i).getScope().get(j).equals(themeTopic)){
						matchedOccurrences.add(occurrences.get(i));
						break;
					}
				}
			}

			if(matchedOccurrences.size() > 1){
				throw new InvalidGdlSchemaException("The topic " + TmHelper.getAnyIdOfTopic(this.tmRepresentative) + "must be bound to none or one occurrence of the type " + occurrenceType + " and the scope " + theme + " but is bound " + matchedOccurrences.size() + " times to it");
			} else if(matchedOccurrences.size() == 1){
				return matchedOccurrences.get(0);
			} else {
				return null;
			}
		}
	}


	// returns the string value of a gdl:id occurrence
	public String getId() throws InvalidGdlSchemaException {
		JsArray<Occurrence> idOccs = getOccurrences(PSIs.GDL.OccurrenceType.gdlId);
		if(idOccs.length() != 1){
			throw new InvalidGdlSchemaException("The topic " + TmHelper.getAnyIdOfTopic(this.tmRepresentative) + " must be bound to exactly one occurrence of the type " + PSIs.GDL.OccurrenceType.gdlId + ", but is bound " + idOccs.length() + " times to it");
		} else {
			return idOccs.get(0).getValue();
		}
	}


	// returns a Display instance of a gdl:display occurrence.
	// If no gdl:display occurrence is set, the default value is returned
	public Display getDisplay() throws InvalidGdlSchemaException {
		Occurrence displayOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlDisplay);

		if(displayOcc != null){
			String value = displayOcc.getValue().toLowerCase();
			if(value.equals("none")){
				return Display.NONE;
			} else if (value.equals("inline")){
				return Display.INLINE;
			} else if (value.equals("inline-block")){
				return Display.INLINE_BLOCK;
			} else if(value.equals("block")){
				return Display.BLOCK;
			} else {
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlDisplay + " must be set to one of \"none\", \"inline\", \"inline-block\" or \"block\", but is \"" + displayOcc.getValue() + "\"");
			}
		} else {
			return Display.INLINE_BLOCK;
		}
	}


	// returns an AutoNumValue instance of a gdl:z-index occurrence.
	// If no gdl:z-index occurrence is set, the default value is returned
	public AutoNumValue getZindex() throws InvalidGdlSchemaException {
		Occurrence zOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlZindex);
		if(zOcc != null){
			return new AutoNumValue(zOcc.getValue());
		} else {
			return new AutoNumValue();
		}
	}


	// returns a Float instance of a gdl:float occurrence or the default value for
	// this property if no gdl:float occurrence is available
	public Float getFloat() throws InvalidGdlSchemaException {
		Occurrence floatOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlFloat);

		if(floatOcc != null){
			String value = floatOcc.getValue().toLowerCase();
			if(value.equals("none")){
				return Float.NONE;
			} else if (value.equals("left")){
				return Float.LEFT;
			} else if (value.equals("right")){
				return Float.RIGHT;
			} else {
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlFloat + " must be set to one of \"none\", \"left\" or \"right\", but is \"" + floatOcc.getValue() + "\"");
			}
		} else {
			return Float.NONE;
		}
	}


	// returns a ClearValue instance of a gdl:clear occurrence or the default value for
	// this property if no gdl:clear occurrence is available
	public ClearValue getClear() throws InvalidGdlSchemaException {
		Occurrence clearOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlClear);

		if(clearOcc != null){
			try{
				return ClearValue.valueOf(clearOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlClear + " must be set to one of \"none\", \"left\", \"right\" or \"both\", but is \"" + clearOcc.getValue() + "\"");
			}
		} else {
			return ClearValue.NONE;
		}
	}


	// returns a ContentOrientationValue instance of a gdl:content-orientation occurrence or the default value for
	// this property if no gdl:content-orientation occurrence is available
	public ContentOrientationValue getContentOrientation() throws InvalidGdlSchemaException {
		Occurrence orientationOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlContentOrientation);

		if(orientationOcc != null){
			try{
				return ContentOrientationValue.valueOf(orientationOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlContentOrientation + " must be set to one of \"horizontal\" or \"vertical\", but is \"" + orientationOcc.getValue() + "\"");
			}
		} else {
			return ContentOrientationValue.VERTICAL;
		}
	}


	// returns a VerticalAlign instance of a gdl:vertical-align occurrence
	// or the default value for this property if no gdl:vertical-align occurrence
	// is available. The styleClass attribute is used as scope for expressing
	// a css pseudo-class, if styleClass is null the occurrence must be unscoped
	public VerticalAlign getVerticalAlign(String styleClass) throws InvalidGdlSchemaException {
		Occurrence vaOcc = null;
		if(styleClass != null){
			vaOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlVerticalAlign, styleClass);
		} else {
			vaOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlVerticalAlign);
		}

		if(vaOcc == null && styleClass != null){
			return null;
		} else if(vaOcc == null) {
			return VerticalAlign.BASELINE;
		}else {
			String value = vaOcc.getValue().toLowerCase();
			if(value.equals("baseline")){
				return VerticalAlign.BASELINE;
			} else if(value.equals("sub")){
				return VerticalAlign.SUB;
			} else if(value.equals("super")) {
				return VerticalAlign.SUPER;
			} else if(value.equals("top")) {
				return VerticalAlign.TOP;
			}else if(value.equals("text-top")) {
				return VerticalAlign.TEXT_TOP;
			}else if(value.equals("middle")) {
				return VerticalAlign.MIDDLE;
			}else if(value.equals("bottom")) {
				return VerticalAlign.BOTTOM;
			}else if(value.equals("text-bottom")) {
				return VerticalAlign.TEXT_BOTTOM;
			} else {
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlVerticalAlign + " must be set to one of \"baseline\", \"sub\", \"super\", \"top\", \"text-top\", \"middle\", \"bottom\" or \"text-bottom\", but is \"" + vaOcc.getValue() + "\"");
			}
		}
	}


	// returns a NumUnitValue instance that represents the margin of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public NumUnitValue getMargin(String styleClass) throws InvalidGdlSchemaException {
		Occurrence marginOcc = null;
		if(styleClass != null){
			marginOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlMargin, styleClass);
		} else {
			marginOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlMargin);
		}

		if(marginOcc == null && styleClass != null){
			return null;
		} else if(marginOcc == null) {
			return new NumUnitValue();
		} else {
			return new NumUnitValue(marginOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the margin-top of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public NumUnitValue getMarginTop(String styleClass) throws InvalidGdlSchemaException {
		Occurrence marginOcc = null;
		if(styleClass != null){
			marginOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlMarginTop, styleClass);
		} else {
			marginOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlMarginTop);
		}

		if(marginOcc == null){
			return null;
		} else {
			return new NumUnitValue(marginOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the margin-right of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public NumUnitValue getMarginRight(String styleClass) throws InvalidGdlSchemaException {
		Occurrence marginOcc = null;
		if(styleClass != null){
			marginOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlMarginRight, styleClass);
		} else {
			marginOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlMarginRight);
		}

		if(marginOcc == null){
			return null;
		} else {
			return new NumUnitValue(marginOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the margin-bottom of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public NumUnitValue getMarginBottom(String styleClass) throws InvalidGdlSchemaException {
		Occurrence marginOcc = null;
		if(styleClass != null){
			marginOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlMarginBottom, styleClass);
		} else {
			marginOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlMarginBottom);
		}

		if(marginOcc == null){
			return null;
		} else {
			return new NumUnitValue(marginOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the margin-left of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null.
	public NumUnitValue getMarginLeft(String styleClass) throws InvalidGdlSchemaException {
		Occurrence marginOcc = null;
		if(styleClass != null){
			marginOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlMarginLeft, styleClass);
		} else {
			marginOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlMarginLeft);
		}

		if(marginOcc == null){
			return null;
		} else {
			return new NumUnitValue(marginOcc.getValue());
		}
	}


	// returns a ColorValue instance that represents the color of this element's border.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public ColorValue getBorderColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderColor, styleClass);
		} else {
			colorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderColor);
		}

		if(colorOcc == null && styleClass != null){
			return null;
		} else if(colorOcc == null) {
			return new ColorValue();
		} else {
			return new ColorValue(colorOcc.getValue());
		}
	}


	// returns a ColorValue instance that represents the color of this element's border-top.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public ColorValue getBorderTopColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopColor, styleClass);
		} else {
			colorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopColor);
		}

		if(colorOcc == null ){
			return null;
		} else {
			return new ColorValue(colorOcc.getValue());
		}
	}


	// returns a ColorValue instance that represents the color of this element's border-right.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public ColorValue getBorderRightColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRightColor, styleClass);
		} else {
			colorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRightColor);
		}

		if(colorOcc == null ){
			return null;
		} else {
			return new ColorValue(colorOcc.getValue());
		}
	}


	// returns a ColorValue instance that represents the color of this element's border-bottom.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public ColorValue getBorderBottomColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomColor, styleClass);
		} else {
			colorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomColor);
		}

		if(colorOcc == null ){
			return null;
		} else {
			return new ColorValue(colorOcc.getValue());
		}
	}


	// returns a ColorValue instance that represents the color of this element's border-left.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public ColorValue getBorderLeftColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderLeftColor, styleClass);
		} else {
			colorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderLeftColor);
		}

		if(colorOcc == null ){
			return null;
		} else {
			return new ColorValue(colorOcc.getValue());
		}
	}


	// returns a ColorValue instance that represents the style of this element's border.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public BorderStyleValue getBorderStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderStyle, styleClass);
		} else {
			styleOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderStyle);
		}

		if(styleOcc == null && styleClass != null){
			return null;
		} else if(styleOcc == null) {
			return BorderStyleValue.NONE;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}


	// returns a ColorValue instance that represents the style of this element's border-top.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public BorderStyleValue getBorderTopStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopStyle, styleClass);
		} else {
			styleOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopStyle);
		}

		if(styleOcc == null){
			return null;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-top-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}


	// returns a ColorValue instance that represents the style of this element's border-right.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public BorderStyleValue getBorderRightStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRightStyle, styleClass);
		} else {
			styleOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRightStyle);
		}

		if(styleOcc == null){
			return null;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-right-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}


	// returns a ColorValue instance that represents the style of this element's border-bottom.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public BorderStyleValue getBorderBottomStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomStyle, styleClass);
		} else {
			styleOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomStyle);
		}

		if(styleOcc == null){
			return null;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-bottom-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}


	// returns a ColorValue instance that represents the style of this element's border-left.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public BorderStyleValue getBorderLeftStyle(String styleClass) throws InvalidGdlSchemaException {
		Occurrence styleOcc = null;
		if(styleClass != null){
			styleOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderLeftStyle, styleClass);
		} else {
			styleOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderLeftStyle);
		}

		if(styleOcc == null){
			return null;
		} else {
			try{
				return BorderStyleValue.valueOf(styleOcc.getValue().toUpperCase());
			}catch(IllegalArgumentException e){
				String values = "none, hidden, dotted, dashed, solid, double, groove, ridge, inset, outset";
				throw new InvalidGdlSchemaException("border-left-style must be set to one of " + values + ", but is " + styleOcc.getValue());
			}
		}
	}


	// returns a AbsoluteNumValue instance that represents the width of this element's border.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AbsoluteNumValue getBorderWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderWidth);
		}

		if(widthOcc == null && styleClass != null){
			return null;
		} else if(widthOcc == null) {
			return new AbsoluteNumValue();
		} else {
			return new AbsoluteNumValue(widthOcc.getValue());
		}
	}


	// returns a AbsoluteNumValue instance that represents the width of this element's border-top.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public AbsoluteNumValue getBorderTopWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopWidth);
		}

		if(widthOcc == null){
			return null;
		} else {
			return new AbsoluteNumValue(widthOcc.getValue());
		}
	}


	// returns a AbsoluteNumValue instance that represents the width of this element's border-right.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public AbsoluteNumValue getBorderRightWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRightWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRightWidth);
		}

		if(widthOcc == null){
			return null;
		} else {
			return new AbsoluteNumValue(widthOcc.getValue());
		}
	}


	// returns a AbsoluteNumValue instance that represents the width of this element's border-bottom.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public AbsoluteNumValue getBorderBottomWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomWidth);
		}

		if(widthOcc == null){
			return null;
		} else {
			return new AbsoluteNumValue(widthOcc.getValue());
		}
	}


	// returns a AbsoluteNumValue instance that represents the width of this element's border-left.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public AbsoluteNumValue getBorderLeftWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderLeftWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderLeftWidth);
		}

		if(widthOcc == null){
			return null;
		} else {
			return new AbsoluteNumValue(widthOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the radius of this element's border.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public NumUnitValue getBorderRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRadius, styleClass);
		} else {
			radiusOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderRadius);
		}

		if(radiusOcc == null && styleClass != null){
			return null;
		} else if(radiusOcc == null) {
			return new NumUnitValue();
		} else {
			return new NumUnitValue(radiusOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the radius of this element's border-top-left.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getBorderTopLeftRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopLeftRadius, styleClass);
		} else {
			radiusOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopLeftRadius);
		}

		if(radiusOcc == null && styleClass != null){
			return null;
		} else if(radiusOcc == null){
			return null;
		} else {
			return new NumUnitValue(radiusOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the radius of this element's border-top-right.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getBorderTopRightRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopRightRadius, styleClass);
		} else {
			radiusOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderTopRightRadius);
		}

		if(radiusOcc == null && styleClass != null){
			return null;
		} else if(radiusOcc == null){
			return null;
		} else {
			return new NumUnitValue(radiusOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the radius of this element's border-bottom-left.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getBorderBottomLeftRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomLeftRadius, styleClass);
		} else {
			radiusOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomLeftRadius);
		}

		if(radiusOcc == null && styleClass != null){
			return null;
		} else if(radiusOcc == null){
			return null;
		} else {
			return new NumUnitValue(radiusOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the radius of this element's border-bottom-right.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getBorderBottomRightRadius(String styleClass) throws InvalidGdlSchemaException {
		Occurrence radiusOcc = null;
		if(styleClass != null){
			radiusOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomRightRadius, styleClass);
		} else {
			radiusOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBorderBottomRightRadius);
		}

		if(radiusOcc == null && styleClass != null){
			return null;
		} else if(radiusOcc == null){
			return null;
		} else {
			return new NumUnitValue(radiusOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the padding of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public NumUnitValue getPadding(String styleClass) throws InvalidGdlSchemaException {
		Occurrence paddingOcc = null;
		if(styleClass != null){
			paddingOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlPadding, styleClass);
		} else {
			paddingOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlPadding);
		}

		if(paddingOcc == null && styleClass != null){
			return null;
		} else if(paddingOcc == null) {
			return new NumUnitValue();
		} else {
			return new NumUnitValue(paddingOcc.getValue());
		}		
	}


	// returns a NumUnitValue instance that represents the padding of this element's top.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getPaddingTop(String styleClass) throws InvalidGdlSchemaException {
		Occurrence paddingOcc = null;
		if(styleClass != null){
			paddingOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlPaddingTop, styleClass);
		} else {
			paddingOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlPaddingTop);
		}

		if(paddingOcc == null){
			return null;
		} else {
			return new NumUnitValue(paddingOcc.getValue());
		}		
	}


	// returns a NumUnitValue instance that represents the padding of this element's right.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getPaddingRight(String styleClass) throws InvalidGdlSchemaException {
		Occurrence paddingOcc = null;
		if(styleClass != null){
			paddingOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlPaddingRight, styleClass);
		} else {
			paddingOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlPaddingRight);
		}

		if(paddingOcc == null){
			return null;
		} else {
			return new NumUnitValue(paddingOcc.getValue());
		}		
	}


	// returns a NumUnitValue instance that represents the padding of this element's bottom.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getPaddingBottom(String styleClass) throws InvalidGdlSchemaException {
		Occurrence paddingOcc = null;
		if(styleClass != null){
			paddingOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlPaddingBottom, styleClass);
		} else {
			paddingOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlPaddingBottom);
		}

		if(paddingOcc == null){
			return null;
		} else {
			return new NumUnitValue(paddingOcc.getValue());
		}
	}


	// returns a NumUnitValue instance that represents the padding of this element's left.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise.
	public NumUnitValue getPaddingLeft(String styleClass) throws InvalidGdlSchemaException {
		Occurrence paddingOcc = null;
		if(styleClass != null){
			paddingOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlPaddingLeft, styleClass);
		} else {
			paddingOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlPaddingLeft);
		}

		if(paddingOcc == null){
			return null;
		} else {
			return new NumUnitValue(paddingOcc.getValue());
		}		
	}


	// returns an AutoNumUnitValue instance that represents the width of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AutoNumUnitValue getWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlWidth);
		}

		if(widthOcc == null && styleClass != null){
			return null;
		} else if(widthOcc == null) {
			return new AutoNumUnitValue();
		} else {
			return new AutoNumUnitValue(widthOcc.getValue());
		}		
	}


	// returns an AutoNumUnitValue instance that represents the min-width of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AutoNumUnitValue getMinWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlMinWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlMinWidth);
		}

		if(widthOcc == null && styleClass != null){
			return null;
		} else if(widthOcc == null) {
			return new AutoNumUnitValue();
		} else {
			return new AutoNumUnitValue(widthOcc.getValue());
		}		
	}


	// returns an AutoNumUnitValue instance that represents the max-width of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AutoNumUnitValue getMaxWidth(String styleClass) throws InvalidGdlSchemaException {
		Occurrence widthOcc = null;
		if(styleClass != null){
			widthOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlMaxWidth, styleClass);
		} else {
			widthOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlMaxWidth);
		}

		if(widthOcc == null && styleClass != null){
			return null;
		} else if(widthOcc == null) {
			return new AutoNumUnitValue();
		} else {
			return new AutoNumUnitValue(widthOcc.getValue());
		}		
	}


	// returns an AutoNumUnitValue instance that represents the height of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AutoNumUnitValue getHeight(String styleClass) throws InvalidGdlSchemaException {
		Occurrence heightOcc = null;
		if(styleClass != null){
			heightOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlHeight, styleClass);
		} else {
			heightOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlHeight);
		}

		if(heightOcc == null && styleClass != null){
			return null;
		} else if(heightOcc == null) {
			return new AutoNumUnitValue();
		} else {
			return new AutoNumUnitValue(heightOcc.getValue());
		}		
	}


	// returns an AutoNumUnitValue instance that represents the min-height of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AutoNumUnitValue getMinHeight(String styleClass) throws InvalidGdlSchemaException {
		Occurrence heightOcc = null;
		if(styleClass != null){
			heightOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlMinHeight, styleClass);
		} else {
			heightOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlMinHeight);
		}

		if(heightOcc == null && styleClass != null){
			return null;
		} else if(heightOcc == null) {
			return new AutoNumUnitValue();
		} else {
			return new AutoNumUnitValue(heightOcc.getValue());
		}		
	}


	// returns an AutoNumUnitValue instance that represents the max-height of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public AutoNumUnitValue getMaxHeight(String styleClass) throws InvalidGdlSchemaException {
		Occurrence heightOcc = null;
		if(styleClass != null){
			heightOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlMaxHeight, styleClass);
		} else {
			heightOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlMaxHeight);
		}

		if(heightOcc == null && styleClass != null){
			return null;
		} else if(heightOcc == null) {
			return new AutoNumUnitValue();
		} else {
			return new AutoNumUnitValue(heightOcc.getValue());
		}		
	}


	// returns a CursorValue instance that represents the cursor of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public CursorValue getCursor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence cursorOcc = null;
		if(styleClass != null){
			cursorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlCursor, styleClass);
		} else {
			cursorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlCursor);
		}

		if(cursorOcc == null && styleClass != null){
			return null;
		} else if(cursorOcc == null) {
			return CursorValue.AUTO;
		} else {
			try{
				return CursorValue.valueOf(cursorOcc.getValue().toUpperCase().replace("-", "_"));
			}catch(IllegalArgumentException e){
				String values = "auto, default, crosshair, pointer, move, n-resize, ne-resize," +
				"nw-resize, e-resize, se-resize, s-resize, sw-resize, w-resize," +
				"text, wait, help, or progress";
				throw new InvalidGdlSchemaException("cursor must be set to one of " + values + ", but is " + cursorOcc.getValue());
			}
		}	
	}


	// returns a ColorValue instance that represents the background-color of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// null, null otherwise. If the styleClass is null and no occurrence was found, the default value for this
	// property is returned.
	public ColorValue getBackgroundColor(String styleClass) throws InvalidGdlSchemaException {
		Occurrence colorOcc = null;
		if(styleClass != null){
			colorOcc = getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlBackgroundColor, styleClass);
		} else {
			colorOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlBackgroundColor);
		}

		if(colorOcc == null && styleClass != null){
			return null;
		} else if(colorOcc == null) {
			return new ColorValue("#ffffff");
		} else {
			return new ColorValue(colorOcc.getValue());
		}	
	}


	// sets the id property of this element by using the GWT DOM class
	public void setId(String id){
		if(id != null){
			DOM.setElementProperty(this.mainPanel.getElement(), "id", id);
		}
	}


	// sets the display style property of this element by using the GWT DOM class
	public void setDisplay(Display display){
		if(display != null){
			DOM.setStyleAttribute(this.mainPanel.getElement(), "display", display.getCssName());
		}
	}


	// sets the z-index style property of this element by using the GWT DOM class
	public void setZindex(AutoNumValue value){
		if(value != null){
			DOM.setStyleAttribute(this.mainPanel.getElement(), "zIndex", value.getCssValue());
		}
	}


	// sets the float style property of this element by using the GWT DOM class
	public void setFloat(Float value){
		if(value != null){ // captures some inconsequent implementations over the browsers
			DOM.setStyleAttribute(this.mainPanel.getElement(), "float", value.getCssName());
			DOM.setStyleAttribute(this.mainPanel.getElement(), "cssFloat", value.getCssName());
			DOM.setStyleAttribute(this.mainPanel.getElement(), "styleFloat", value.getCssName());
		}
	}


	// sets the float style property of all sub elements by using the GWT DOM class
	public void setContentOrientation(ContentOrientationValue value) throws InvalidGdlSchemaException, ExecutionException {
		if(value == ContentOrientationValue.HORIZONTAL && this.containerPanel == null) this.containerPanel = new HorizontalPanel();
		else if(this.containerPanel == null) this.containerPanel = new VerticalPanel();

		this.mainPanel.add(this.containerPanel);
	}


	// sets the clear style property of this element by using the GWT DOM class
	public void setClear(ClearValue value){
		if(value != null){
			DOM.setStyleAttribute(this.mainPanel.getElement(), "clear", value.getCssValue());
		}
	}


	// sets the vertical-align style property of this element and all it's sub-elements by using the GWT DOM class
	public void setVerticalAlign(Widget widget, VerticalAlign value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null){
			this.setCssProperty(styleClass, "verticalAlign", value.getCssName());
		}
	}


	// sets the margin style property of this element by using the GWT DOM class
	public void setMargin(Widget widget, NumUnitValue value, String styleClass)	throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "margin", value.getCssValue());
	}


	// sets the margin-top style property of this element by using the GWT DOM class
	public void setMarginTop(Widget widget, NumUnitValue value,	String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "marginTop", value.getCssValue());
	}


	// sets the margin-right style property of this element by using the GWT DOM class
	public void setMarginRight(Widget widget, NumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "marginRight", value.getCssValue());
	}


	// sets the margin-bottom style property of this element by using the GWT DOM class
	public void setMarginBottom(Widget widget, NumUnitValue value, String styleClass) throws InvalidGdlSchemaException,	ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "marginBottom", value.getCssValue());
	}


	// sets the margin-left style property of this element by using the GWT DOM class
	public void setMarginLeft(Widget widget, NumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "marginLeft", value.getCssValue());
	}


	// sets the border-color style property of this element by using the GWT DOM class
	public void setBorderColor(Widget widget, ColorValue value,	String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderColor", value.getCssValue());
	}


	// sets the border-top-color style property of this element by using the GWT DOM class
	public void setBorderTopColor(Widget widget, ColorValue value, String styleClass) throws InvalidGdlSchemaException,	ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderTopColor", value.getCssValue());
	}


	// sets the border-right-color style property of this element by using the GWT DOM class
	public void setBorderRightColor(Widget widget, ColorValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderRightColor", value.getCssValue());
	}


	// sets the border-bottom-color style property of this element by using the GWT DOM class
	public void setBorderBottomColor(Widget widget, ColorValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderBottomColor", value.getCssValue());
	}


	// sets the border-left-color style property of this element by using the GWT DOM class
	public void setBorderLeftColor(Widget widget, ColorValue value,	String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderLeftColor", value.getCssValue());
	}


	// sets the border-style style property of this element by using the GWT DOM class
	public void setBorderStyle(Widget widget, BorderStyleValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderStyle", value.getCssValue());
	}


	// sets the border-top-style style property of this element by using the GWT DOM class
	public void setBorderTopStyle(Widget widget, BorderStyleValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderTopStyle", value.getCssValue());
	}


	// sets the border-right-style style property of this element by using the GWT DOM class
	public void setBorderRightStyle(Widget widget, BorderStyleValue value, String styleClass) throws InvalidGdlSchemaException,	ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderRightStyle", value.getCssValue());
	}


	// sets the border-bottom-style style property of this element by using the GWT DOM class
	public void setBorderBottomStyle(Widget widget, BorderStyleValue value,	String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderBottomStyle", value.getCssValue());
	}


	// sets the border-left-style style property of this element by using the GWT DOM class
	public void setBorderLeftStyle(Widget widget, BorderStyleValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderLeftStyle", value.getCssValue());
	}


	// sets the border-width style property of this element by using the GWT DOM class
	public void setBorderWidth(Widget widget, AbsoluteNumValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderWidth", value.getCssValue());
	}


	// sets the border-width style property of this element by using the GWT DOM class
	public void setBorderTopWidth(Widget widget, AbsoluteNumValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderTopWidth", value.getCssValue());
	}

	// sets the border-width style property of this element by using the GWT DOM class@Override
	public void setBorderRightWidth(Widget widget, AbsoluteNumValue value, String styleClass) throws InvalidGdlSchemaException,	ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderRightWidth", value.getCssValue());
	}


	// sets the border-width style property of this element by using the GWT DOM class
	public void setBorderBottomWidth(Widget widget, AbsoluteNumValue value,	String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderBottomWidth", value.getCssValue());
	}


	// sets the border-width style property of this element by using the GWT DOM class
	public void setBorderLeftWidth(Widget widget, AbsoluteNumValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderLeftWidth", value.getCssValue());
	}	


	// sets the border-radius style property of this element by using the GWT DOM class
	public void setBorderRadius(Widget widget, NumUnitValue value, String styleClass) throws InvalidGdlSchemaException,	ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderRadius", value.getCssValue());
	}


	// sets the border-top-right-radius style property of this element by using the GWT DOM class
	public void setBorderTopRightRadius(Widget widget, NumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderTopRightRadius", value.getCssValue());
	}


	// sets the border-bottom-right-radius style property of this element by using the GWT DOM class
	public void setBorderBottomRightRadius(Widget widget, NumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderBottomRightRadius", value.getCssValue());
	}


	// sets the border-bottom-left-radius style property of this element by using the GWT DOM class
	public void setBorderBottomLeftRadius(Widget widget, NumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderBottomLeftRadius", value.getCssValue());
	}


	// sets the border-top-left-radius style property of this element by using the GWT DOM class
	public void setBorderTopLeftRadius(Widget widget, NumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "borderTopLeftRadius", value.getCssValue());
	}


	// sets the padding style property of this element by using the GWT DOM class
	public void setPadding(Widget widget, NumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "padding", value.getCssValue());
	}


	// sets the padding-top style property of this element by using the GWT DOM class
	public void setPaddingTop(Widget widget, NumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "paddingTop", value.getCssValue());
	}


	// sets the padding-right style property of this element by using the GWT DOM class
	public void setPaddingRight(Widget widget, NumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "paddingRight", value.getCssValue());
	}


	// sets the padding-bottom style property of this element by using the GWT DOM class
	public void setPaddingBottom(Widget widget, NumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "paddingBottom", value.getCssValue());
	}


	// sets the padding-left style property of this element by using the GWT DOM class
	public void setPaddingLeft(Widget widget, NumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "paddingLeft", value.getCssValue());
	}


	// sets the width style property of this element by using the GWT DOM class
	public void setWidth(Widget widget, AutoNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "width", value.getCssValue());
	}


	// sets the min-width style property of this element by using the GWT DOM class
	public void setMinWidth(Widget widget, AutoNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "minWidth", value.getCssValue());
	}


	// sets the max-width style property of this element by using the GWT DOM class
	public void setMaxWidth(Widget widget, AutoNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "maxWidth", value.getCssValue());
	}


	// sets the height style property of this element by using the GWT DOM class
	public void setHeight(Widget widget, AutoNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "height", value.getCssValue());
	}


	// sets the min-height style property of this element by using the GWT DOM class
	public void setMinHeight(Widget widget, AutoNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "minHeight", value.getCssValue());
	}


	// sets the max-height style property of this element by using the GWT DOM class
	public void setMaxHeight(Widget widget, AutoNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "maxHeight", value.getCssValue());
	}


	// sets the cursor style property of this element by using the GWT DOM class
	public void setCursor(Widget widget, CursorValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null) this.setCssProperty(widget, styleClass, "cursor", value.getCssValue());
	}


	// sets the background-color style property of this element by using the GWT DOM class
	public void setBackgroundColor(Widget widget, ColorValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {		
		if(value != null) this.setCssProperty(widget, styleClass, "backgroundColor", value.getCssValue());
	}


	// sets the passed css style porperty to the passed css value.
	// If a styleClass is given, the style is applied to either active, hover or focus
	protected void setCssProperty(String styleClass, String cssProperty, String cssValue)throws InvalidGdlSchemaException, ExecutionException{
		if(cssValue == null || cssProperty == null) return;

		if(styleClass == null){
			DOM.setStyleAttribute(this.getElement(), cssProperty, cssValue);
		} else if(styleClass.equals(PSIs.GDL.Scope.gdlActive)){
			this.activeCssNamesAndStyles.add(new Pair<String, String>(cssProperty, cssValue));
		} else if(styleClass.equals(PSIs.GDL.Scope.gdlFocus)){
			this.focusCssNamesAndStyles.add(new Pair<String, String>(cssProperty, cssValue));
		} else if (styleClass.equals(PSIs.GDL.Scope.gdlHover)){
			this.hoverCssNamesAndStyles.add(new Pair<String, String>(cssProperty, cssValue));
		} else {
			String values = PSIs.GDL.Scope.gdlActive + ", " + PSIs.GDL.Scope.gdlFocus + ", " + PSIs.GDL.Scope.gdlHover; 
			throw new InvalidGdlSchemaException("GDL defines only the style classes " + values + ", but found " + styleClass);
		}
	}


	// sets a given css property and css value of this element's sub element
	protected void setCssProperty(Widget elem, String styleClass, String cssProperty, String cssValue)throws InvalidGdlSchemaException, ExecutionException{
		if(cssValue == null || cssProperty == null) return;

		if(styleClass == null){
			DOM.setStyleAttribute(elem.getElement(), cssProperty, cssValue);
		} else if(styleClass.equals(PSIs.GDL.Scope.gdlActive)){
			this.activeCssNamesAndStyles.add(new Pair<String, String>(cssProperty, cssValue));
		} else if(styleClass.equals(PSIs.GDL.Scope.gdlFocus)){
			this.focusCssNamesAndStyles.add(new Pair<String, String>(cssProperty, cssValue));
		} else if (styleClass.equals(PSIs.GDL.Scope.gdlHover)){
			this.hoverCssNamesAndStyles.add(new Pair<String, String>(cssProperty, cssValue));
		} else {
			String values = PSIs.GDL.Scope.gdlActive + ", " + PSIs.GDL.Scope.gdlFocus + ", " + PSIs.GDL.Scope.gdlHover; 
			throw new InvalidGdlSchemaException("GDL defines only the style classes " + values + ", but found " + styleClass);
		}
	}


	// sets a given css property and css value of this element's sub element
	protected void setCssProperty(Element elem, String styleClass, String cssProperty, String cssValue)throws InvalidGdlSchemaException, ExecutionException{
		if(cssValue == null || cssProperty == null) return;

		if(styleClass == null){
			DOM.setStyleAttribute(elem, cssProperty, cssValue);
		} else if(styleClass.equals(PSIs.GDL.Scope.gdlActive)){
			this.activeCssNamesAndStyles.add(new Pair<String, String>(cssProperty, cssValue));
		} else if(styleClass.equals(PSIs.GDL.Scope.gdlFocus)){
			this.focusCssNamesAndStyles.add(new Pair<String, String>(cssProperty, cssValue));
		} else if (styleClass.equals(PSIs.GDL.Scope.gdlHover)){
			this.hoverCssNamesAndStyles.add(new Pair<String, String>(cssProperty, cssValue));
		} else {
			String values = PSIs.GDL.Scope.gdlActive + ", " + PSIs.GDL.Scope.gdlFocus + ", " + PSIs.GDL.Scope.gdlHover; 
			throw new InvalidGdlSchemaException("GDL defines only the style classes " + values + ", but found " + styleClass);
		}
	}


	// sets all GDL styles that are defined by the topic map representative to tha passed widget
	public void setGdlStyle(Widget widget) throws InvalidGdlSchemaException, ExecutionException {
		String[] styleClasses = new String[]{null, PSIs.GDL.Scope.gdlActive, PSIs.GDL.Scope.gdlFocus, PSIs.GDL.Scope.gdlHover};
		for (String styleClass : styleClasses) {
			this.setVerticalAlign(widget, this.getVerticalAlign(styleClass), styleClass);

			this.setMargin(widget, this.getMargin(styleClass), styleClass);
			this.setMarginTop(widget, this.getMarginTop(styleClass), styleClass);
			this.setMarginRight(widget, this.getMarginRight(styleClass), styleClass);
			this.setMarginBottom(widget, this.getMarginBottom(styleClass), styleClass);
			this.setMarginLeft(widget, this.getMarginLeft(styleClass), styleClass);

			this.setPadding(widget, this.getPadding(styleClass), styleClass);
			this.setPaddingTop(widget, this.getPaddingTop(styleClass), styleClass);
			this.setPaddingRight(widget, this.getPaddingRight(styleClass), styleClass);
			this.setPaddingBottom(widget, this.getPaddingBottom(styleClass), styleClass);
			this.setPaddingLeft(widget, this.getPaddingLeft(styleClass), styleClass);

			this.setBorderColor(widget, this.getBorderColor(styleClass), styleClass);
			this.setBorderTopColor(widget, this.getBorderTopColor(styleClass), styleClass);
			this.setBorderRightColor(widget, this.getBorderRightColor(styleClass), styleClass);
			this.setBorderBottomColor(widget, this.getBorderBottomColor(styleClass), styleClass);
			this.setBorderLeftColor(widget, this.getBorderLeftColor(styleClass), styleClass);

			this.setBorderStyle(widget, this.getBorderStyle(styleClass), styleClass);
			this.setBorderTopStyle(widget, this.getBorderTopStyle(styleClass), styleClass);
			this.setBorderRightStyle(widget, this.getBorderRightStyle(styleClass), styleClass);
			this.setBorderBottomStyle(widget, this.getBorderBottomStyle(styleClass), styleClass);
			this.setBorderLeftStyle(widget, this.getBorderLeftStyle(styleClass), styleClass);

			this.setBorderWidth(widget, this.getBorderWidth(styleClass), styleClass);
			this.setBorderTopWidth(widget, this.getBorderTopWidth(styleClass), styleClass);
			this.setBorderRightWidth(widget, this.getBorderRightWidth(styleClass), styleClass);
			this.setBorderBottomWidth(widget, this.getBorderBottomWidth(styleClass), styleClass);
			this.setBorderLeftWidth(widget, this.getBorderLeftWidth(styleClass), styleClass);

			this.setBorderRadius(widget, this.getBorderRadius(styleClass), styleClass);
			this.setBorderTopRightRadius(widget, this.getBorderTopRightRadius(styleClass), styleClass);
			this.setBorderBottomRightRadius(widget, this.getBorderBottomRightRadius(styleClass), styleClass);
			this.setBorderBottomLeftRadius(widget, this.getBorderBottomLeftRadius(styleClass), styleClass);
			this.setBorderTopLeftRadius(widget, this.getBorderTopLeftRadius(styleClass), styleClass);

			this.setCursor(widget, this.getCursor(styleClass), styleClass);

			this.setWidth(widget, this.getWidth(styleClass), styleClass);
			this.setMaxWidth(widget, this.getMaxWidth(styleClass), styleClass);
			this.setMinWidth(widget, this.getMinWidth(styleClass), styleClass);

			this.setHeight(widget, this.getHeight(styleClass), styleClass);
			this.setMaxHeight(widget, this.getMaxHeight(styleClass), styleClass);
			this.setMinHeight(widget, this.getMinHeight(styleClass), styleClass);

			this.setBackgroundColor(widget, this.getBackgroundColor(styleClass), styleClass);
		}
	}


	// sets all GDL styles that are defined by the topic map representative
	protected void setGdlStyle() throws InvalidGdlSchemaException, ExecutionException {
		this.setDisplay(this.getDisplay());
		this.setZindex(this.getZindex());
		this.setFloat(this.getFloat());
		this.setClear(this.getClear());
		this.setContentOrientation(this.getContentOrientation());
	}


	// registers a click handler to all sub-elements of this element
	@Override
	public MultipleHandlerRegistration addClickHandler(ClickHandler handler) {
		MultipleHandlerRegistration regs = new MultipleHandlerRegistration();
		for (Widget item : this.subElements) {
			regs.addHandlerRegistration(((ButtonableObject)item).getMainObject().addHandler(handler, ClickEvent.getType()));
		}
		return regs;
	}


	// registers a mouse-out handler to all sub-elements of this element
	@Override
	public MultipleHandlerRegistration addMouseOutHandler(MouseOutHandler handler){
		MultipleHandlerRegistration regs = new MultipleHandlerRegistration();
		for (Widget item : this.subElements) {
			regs.addHandlerRegistration(((ButtonableObject)item).getMainObject().addHandler(handler, MouseOutEvent.getType()));
		}
		return regs;
	}


	// registers a mouse-over handler to all sub-elements of this element
	@Override
	public MultipleHandlerRegistration addMouseOverHandler(MouseOverHandler handler){
		MultipleHandlerRegistration regs = new MultipleHandlerRegistration();
		for (Widget item : this.subElements) {
			regs.addHandlerRegistration(((ButtonableObject)item).getMainObject().addHandler(handler, MouseOverEvent.getType()));
		}
		return regs;
	}


	// registers a focus handler to all sub-elements of this element
	@Override
	public MultipleHandlerRegistration addFocusHandler(FocusHandler handler){
		MultipleHandlerRegistration regs = new MultipleHandlerRegistration();
		for (Widget item : this.subElements) {
			regs.addHandlerRegistration(((ButtonableObject)item).getMainObject().addHandler(handler, FocusEvent.getType()));
		}
		return regs;
	}


	// registers a mouse-down handler to all sub-elements of this element
	@Override
	public MultipleHandlerRegistration addMouseDownHandler(MouseDownHandler handler){
		MultipleHandlerRegistration regs = new MultipleHandlerRegistration();
		for (Widget item : this.subElements) {
			regs.addHandlerRegistration(((ButtonableObject)item).getMainObject().addHandler(handler, MouseDownEvent.getType()));
		}
		return regs;
	}


	// registers a mouse-up handler to all sub-elements of this element
	@Override
	public MultipleHandlerRegistration addMouseUpHandler(MouseUpHandler handler){
		MultipleHandlerRegistration regs = new MultipleHandlerRegistration();
		for (Widget item : this.subElements) {
			regs.addHandlerRegistration(((ButtonableObject)item).getMainObject().addHandler(handler, MouseUpEvent.getType()));
		}
		return regs;
	}



	// registers a blur handler to all sub-elements of this element
	@Override
	public MultipleHandlerRegistration addBlurHandler(BlurHandler handler){
		MultipleHandlerRegistration regs = new MultipleHandlerRegistration();
		for (Widget item : this.subElements) {
			regs.addHandlerRegistration(((ButtonableObject)item).getMainObject().addHandler(handler, BlurEvent.getType()));
		}
		return regs;
	}


	// adds the passed handler to the list eventHandlers
	protected void addEventHandler(Widget elem, EventHandler handler){
		if(handler == null || elem == null) return;

		for (Pair<Widget, ArrayList<EventHandler>> item : this.eventHandlers) {
			if(item.getFirst().equals(elem)){
				if(!item.getSecond().contains(handler)){
					item.getSecond().add(handler);
					return;
				}
			}
		}
		ArrayList<EventHandler> newHandlerList = new ArrayList<EventHandler>();
		newHandlerList.add(handler);
		this.eventHandlers.add(new Pair<Widget, ArrayList<EventHandler>>(elem, newHandlerList));
	}


	// remove the passed handler of the list eventHandlers
	protected void removeEventHandler(Widget elem, EventHandler handler){
		for (Pair<Widget, ArrayList<EventHandler>> item : this.eventHandlers) {
			if(item.getFirst().equals(elem)){
				item.getSecond().remove(handler);
			}
		}
	}


	// returns the last handler bound to the passed element.
	protected EventHandler getLastHandler(Widget elem){
		for (Pair<Widget, ArrayList<EventHandler>> item : this.eventHandlers) {
			if(item.getFirst().equals(elem)){
				int idx = item.getSecond().size() - 1;
				if(idx >= 0){
					return item.getSecond().get(idx);
				}
			}
		}

		return null;
	}


	// applies the styles bound to hover and the passed element
	protected void onHoverStart(Widget widget){
		for (Pair<String, String> elem : this.hoverCssNamesAndStyles) {
			DOM.setStyleAttribute(widget.getElement(), elem.getFirst(), elem.getSecond());
		}
	}


	// applies the styles bound to acitve and the passed element
	protected void onActiveStart(Widget widget){
		for (Pair<String, String> elem : this.activeCssNamesAndStyles) {
			DOM.setStyleAttribute(widget.getElement(), elem.getFirst(), elem.getSecond());
		}
	}


	// applies the styles bound to focus and the passed element
	protected void onFocusStart(Widget widget){
		for (Pair<String, String> elem : this.focusCssNamesAndStyles) {
			DOM.setStyleAttribute(widget.getElement(), elem.getFirst(), elem.getSecond());
		}
	}


	// some handler for applying the css style bound to the pseudo classes hover, active and focus 
	public void onHoverStart(MouseOverEvent event, HoverStyleHandler handler) {
		Widget source = (Widget)event.getSource();
		this.addEventHandler(source, handler);
		for (Pair<String, String> elem : this.hoverCssNamesAndStyles) {
			DOM.setStyleAttribute(source.getElement(), elem.getFirst(), elem.getSecond());
		}
	}


	// shall be called when the focus event was fired 
	public void onHoverEnd(MouseOutEvent event, HoverStyleHandler handler) {
		try{
			Widget source = (Widget)event.getSource();
			this.removeEventHandler(source, handler);
			EventHandler lastHandler = this.getLastHandler(source);
			if(lastHandler == null){
				this.setGdlStyle(source);
			}else {
				if(lastHandler.getClass().equals(ActiveStyleHandler.class)) this.onActiveStart(source);
				else this.onFocusStart(source);
			}
		}catch(Exception e){
			Window.alert("could not apply the default CSS style >> " + e.getClass() + " >> " + e.getMessage());
			e.printStackTrace();
		}
	}


	// shall be called to apply the styles of the focus class
	public void onFocusStart(FocusEvent event, FocusStyleHandler handler) {
		Widget source = (Widget)event.getSource();
		this.addEventHandler(source, handler);
		for (Pair<String, String> elem : this.focusCssNamesAndStyles) {
			DOM.setStyleAttribute(source.getElement(), elem.getFirst(), elem.getSecond());
		}
	}


	// shall be called when the blur event was fired 
	public void onFocusEnd(BlurEvent event, FocusStyleHandler handler) {
		try{
			Widget source = (Widget)event.getSource();
			this.removeEventHandler(source, handler);
			EventHandler lastHandler = this.getLastHandler(source);
			if(lastHandler == null){
				this.setGdlStyle(source);
			}else {
				if(lastHandler.getClass().equals(ActiveStyleHandler.class)) this.onActiveStart(source);
				else this.onHoverStart(source);
			}
		}catch(Exception e){
			Window.alert("could not apply the default CSS style >> " + e.getClass() + " >> " + e.getMessage());
		}
	}


	// shall be called to apply the styles of the active class
	public void onActiveStart(MouseDownEvent event, ActiveStyleHandler handler) {
		Widget source = (Widget)event.getSource();
		this.addEventHandler(source, handler);
		for (Pair<String, String> elem : this.activeCssNamesAndStyles) {
			DOM.setStyleAttribute(source.getElement(), elem.getFirst(), elem.getSecond());
		}
	}


	// shall be called to apply the styles of the focus class
	public void onActiveEnd(MouseUpEvent event, ActiveStyleHandler handler) {
		try{
			Widget source = (Widget)event.getSource();
			this.removeEventHandler(source, handler);
			EventHandler lastHandler = this.getLastHandler(source);
			if(lastHandler == null){
				this.setGdlStyle(source);
			}else {
				if(lastHandler.getClass().equals(HoverStyleHandler.class)) this.onHoverStart(source);
				else this.onFocusStart(source);
			}
		}catch(Exception e){
			Window.alert("could not apply the default CSS style >> " + e.getClass() + " >> " + e.getMessage());
		}
	}


	// removes all GdlInfo elements. They must be remove explicitly, since they are
	// bound to the root panel/body
	public void clear(){
		for (GdlInfo elem : this.infoElements)
			elem.removeFromParent();
	}


	@Override
	public void onAttach(){
		super.onAttach();
		try{
			this.setInfoElements();
		}catch(Exception e){
			e.printStackTrace();
		}
	}


	// if this method is called the user cannot change the represented value
	// of this control
	public abstract void fixValue();

	
	// sets the item-identifier-fields for the received data
	private void setReceivedItemIdentifiers() throws InvalidGdlSchemaException, ExecutionException {
		if(!(this.receivedData instanceof Reifiable) || !(this.receivedData instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Reifiable, but is: " + receivedData.getClass());
		
		// get type
		Topic constrainedTopicType = TmHelper.getConstrainedTopicType(this.getConstraint());

		int typeIdx = -1;
		JsArray<Topic> types = null;
		if((this.receivedData instanceof Topic)){
			types = ((Topic)this.receivedData).getTypes();
			if(types.length() != 0){
				for(typeIdx = 0; typeIdx != types.length(); ++typeIdx) if(types.get(typeIdx).equals(constrainedTopicType)) break;
			}
		}

		if((this.receivedData instanceof Topic) && types != null && typeIdx != types.length()){
			Pattern rex = new Pattern(this.getLiteralValueForConstraint());
			Topic top = (Topic)this.receivedData;
			for(int i = 0; i != top.getItemIdentifiers().length(); ++i){
				String ii = top.getItemIdentifiers().get(i).getReference();
				if(rex.matches(ii)){
					this.addSubItem(ii);
				}
			}
		} else {			
			// search for the topic type
			Reifiable ref = null;

			// search for the characteristics type
			if(this.receivedData instanceof Topic){
				JsArray<Name> names = ((Topic)this.receivedData).getNames(constrainedTopicType);
				if(names.length() != 0){
					ref = names.get(0);
				} else {
					JsArray<Occurrence> occs = ((Topic)this.receivedData).getOccurrences(constrainedTopicType);
					if(occs.length() != 0) ref = occs.get(0);
				}
			} else if(this.receivedData instanceof Association){
				JsArray<Role> roles = ((Association)this.receivedData).getRoles(constrainedTopicType);
				if(roles.length() != 0) ref = roles.get(0);
			}

			// search for item-identifiers of the found topic type or characteristics
			Pattern rex = new Pattern(this.getLiteralValueForConstraint());
			for(int i = 0; i != ((ReifiableStub)ref).getItemIdentifiers().length(); ++i){
				String ii = ((ReifiableStub)ref).getItemIdentifiers().get(i).getReference();
				if(rex.matches(ii)){
					this.addSubItem(ii);
				}
			}
		}
	}
	
	
	// sets the reifier-fields for the received data
	private void setReceivedReifier() throws InvalidGdlSchemaException, ExecutionException {
		Topic type = TmHelper.getConstrainedStatement(this.getConstraint());

		Topic reifier = null;
		if(this.receivedData instanceof Topic){
			JsArray<Name> names = ((Topic)this.receivedData).getNames(type);
			if(names.length() != 0) reifier = names.get(0).getReifier();
			if(reifier == null){
				JsArray<Occurrence> occs = ((Topic)this.receivedData).getOccurrences(type);
				if(occs.length() != 0) reifier = occs.get(0).getReifier();
			}
		} else if(this.receivedData instanceof Association){
			if(((Association)this.receivedData).getType().equals(type)) type = ((Association)this.receivedData).getType();
		} else {
			throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a topic or association, but is: " + this.receivedData.getClass());
		}

		String str = this.getTopicRepresentation(reifier, this.getDisplayByOfValueGroup(), this.getPreferredScopeOfValueGroup());
		if(str == null) str = "";
		this.addSubItem(str);
	}
	

	// sets the scope-fields for the received data
	private void setReceivedScope() throws InvalidGdlSchemaException, ExecutionException {
		Topic type = TmHelper.getConstrainedStatement(this.getConstraint());

		JsArray<Topic> scope = null;
		if(this.receivedData instanceof Topic){
			JsArray<Name> names = ((Topic)this.receivedData).getNames(type);
			if(names.length() != 0) scope = names.get(0).getScope();
			if(scope == null){
				JsArray<Occurrence> occs = ((Topic)this.receivedData).getOccurrences(type);
				if(occs.length() != 0) scope = occs.get(0).getScope();
			}
		} else if(this.receivedData instanceof Association){
			if(((Association)this.receivedData).getType().equals(type)) type = ((Association)this.receivedData).getType();
		} else {
			throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a topic or association, but is: " + this.receivedData.getClass());
		}

		if(scope != null){
			for(int i = 0; i != scope.length(); ++i) this.addSubItem(this.getTopicRepresentation(scope.get(i), this.getDisplayByOfValueGroup(), this.getPreferredScopeOfValueGroup()));
		}
	}
	
	
	// sets the type-fields for the received data
	private void setReceivedType() throws InvalidGdlSchemaException, ExecutionException {
		Topic type = null;
		if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclTopicNameConstraint)){
			if(!(this.receivedData instanceof Topic)) throw new ExecutionException("The constraints " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " and " + TmHelper.getAnyIdOfTopic(this.getRootConstraint()) + " are only valid when a topic is processed, but is: " + this.receivedData.getClass());
			type = TmHelper.getConstrainedStatement(this.getRootConstraint());
		} else if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclTopicOccurrenceConstraint)){
			if(!(this.receivedData instanceof Topic)) throw new ExecutionException("The constraints " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " and " + TmHelper.getAnyIdOfTopic(this.getRootConstraint()) + " are only valid when a topic is processed, but is: " + this.receivedData.getClass());
			type = TmHelper.getConstrainedStatement(this.getRootConstraint());
		} else if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclTopicRoleConstraint)){
			if(!(this.receivedData instanceof Association)) throw new ExecutionException("The constraints " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " and " + TmHelper.getAnyIdOfTopic(this.getRootConstraint()) + " are only valid when an association is processed, but is: " + this.receivedData.getClass());
			type = TmHelper.getConstrainedStatement(this.getRootConstraint());
		} else if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclAssociationRoleConstraint)){
			if(!(this.receivedData instanceof Association)) throw new ExecutionException("The constraints " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " and " + TmHelper.getAnyIdOfTopic(this.getRootConstraint()) + " are only valid when an association is processed, but is: " + this.receivedData.getClass());
			type = TmHelper.getConstraintRoleOfConstraint(this.getRootConstraint());
		} else {
			String constraints = PSIs.TMCL.tmclTopicNameConstraint + ", " + PSIs.TMCL.tmclTopicOccurrenceConstraint + ", " + PSIs.TMCL.tmclTopicRoleConstraint + ", " + PSIs.TMCL.tmclAssociationRoleConstraint;
			throw new ExecutionException("The topic " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to the following root constraints: " + constraints);
		}

		String str = this.getTopicRepresentation(type, this.getDisplayByOfValueGroup(), this.getPreferredScopeOfValueGroup());
		if(str == null) str = "";
		this.addSubItem(str);
	}
	
	
	// sets the variant-name-scope-fields for the received data
	private void setReceivedVariantNameScope() throws InvalidGdlSchemaException, ExecutionException {
		if(!(this.receivedData instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + receivedData.getClass());
		if(!TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclVariantNameConstraint)) throw new InvalidGdlSchemaException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a constraint of the type " + PSIs.TMCL.tmclVariantNameConstraint + ",  but is bound to: " + TmHelper.getAnyIdOfTopic(this.getRootConstraint()));
		
		ArrayList<Variant> variants = TmHelper.getVariantsForConstraint((Topic)this.receivedData, this.getRootConstraint());
		if(variants.size() != 0){
			for(int i = 0; i != variants.get(0).getScope().length(); ++i) this.addSubItem(this.getTopicRepresentation(variants.get(0).getScope().get(i), this.getDisplayByOfValueGroup(), this.getPreferredScopeOfValueGroup()));
		}
	}
	
	
	// sets the variant-name-reifier-fields for the received data
	private void setReceivedVariantNameReifier() throws InvalidGdlSchemaException, ExecutionException {
		if(!(this.receivedData instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + receivedData.getClass());
		if(!TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclVariantNameConstraint)) throw new InvalidGdlSchemaException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a constraint of the type " + PSIs.TMCL.tmclVariantNameConstraint + ",  but is bound to: " + TmHelper.getAnyIdOfTopic(this.getRootConstraint()));
		
		ArrayList<Variant> variants = TmHelper.getVariantsForConstraint((Topic)this.receivedData, this.getRootConstraint());
		if(variants.size() != 0 && variants.get(0).getReifier() != null){
			this.addSubItem(this.getTopicRepresentation(variants.get(0).getReifier(), this.getDisplayByOfValueGroup(), this.getPreferredScopeOfValueGroup()));	
		}
	}
	
	
	// sets the subject-identifier and subjetc-locator-fields for the received data
	private void setReceivedTopicIdentifiers() throws InvalidGdlSchemaException, ExecutionException {
		if(!(receivedData instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + receivedData.getClass());

		Pattern rex = new Pattern(this.getLiteralValueForConstraint());
		
		JsArray<Locator> identifiers = null;
		if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclSubjectIdentifierConstraint)){
			identifiers = ((Topic)receivedData).getSubjectIdentifiers();
		}else {
			identifiers = ((Topic)receivedData).getSubjectLocators();
		}
		
		for(int i = 0; i != identifiers.length(); ++i){
			String id = identifiers.get(i).getReference();
			if(rex.matches(id)){
				this.addSubItem(id);
			}
		}
	}
	
	
	// sets the variant-name-fields for the received data
	private void setReceivedVariantName() throws InvalidGdlSchemaException, ExecutionException {
		if(!(this.receivedData instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + receivedData.getClass());

		ArrayList<Variant> variants = TmHelper.getVariantsForConstraint((Topic)this.receivedData, this.getConstraint());
		if(variants.size() != 0){
			for (Variant variant : variants) {
				 this.addSubItem(variant.getValue());
			}
		}
		else{
			this.addSubItem("");
		}	
	}
	
	
	// sets the role-player-fields for the received data
	private void setReceivedRolePlayer() throws InvalidGdlSchemaException, ExecutionException {
		if(!(receivedData instanceof Association)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to an Association, but is: " + receivedData.getClass());
		if(this.getRootConstraint() == null || !TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclTopicRoleConstraint)) throw new InvalidGdlSchemaException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a root constraint of the type " + PSIs.TMCL.tmclTopicRoleConstraint + ", but ist bound to the root topic: " + (this.getRootConstraint() == null ? "null" : TmHelper.getAnyIdOfTopic(this.getRootConstraint())));

		Pair<Topic, Topic> roleAndPlayerType = TmHelper.getConstrainedRoleAndPlayerTypeOfConstraint(this.getRootConstraint());	
		JsArray<Role> assocRoles = ((Association)this.receivedData).getRoles(roleAndPlayerType.getFirst());
		for(int i = 0; i != assocRoles.length(); ++i){
			if(TmHelper.isInstanceOf(assocRoles.get(i).getPlayer(), roleAndPlayerType.getSecond())){
				this.addSubItem(this.getTopicRepresentation(assocRoles.get(i).getPlayer(), this.getDisplayByOfValueGroup(), this.getPreferredScopeOfValueGroup()));
			}
		}
	}
	
	
	// sets the variant-name-identifiers-fields for the received data
	private void setReceivedVariantNameIdentifiers() throws InvalidGdlSchemaException, ExecutionException {
		if(!(this.receivedData instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + receivedData.getClass());
		if(!TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclVariantNameConstraint)) throw new InvalidGdlSchemaException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a constraint of the type " + PSIs.TMCL.tmclVariantNameConstraint + ",  but is bound to: " + TmHelper.getAnyIdOfTopic(this.getRootConstraint()));
		
		ArrayList<Variant> variants = TmHelper.getVariantsForConstraint((Topic)this.receivedData, this.getRootConstraint());
		if(variants.size() != 0){
			for(int i = 0; i != variants.get(0).getItemIdentifiers().length(); ++i) this.addSubItem(variants.get(0).getItemIdentifiers().get(i).getReference());
		}
	}
	
	
	// sets the datatype-fields for the received data
	private void setReceivedDatatype() throws InvalidGdlSchemaException, ExecutionException {
		if(!(this.receivedData instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + receivedData.getClass());

		if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclVariantNameConstraint)){
			ArrayList<Variant> variants = TmHelper.getVariantsForConstraint((Topic)this.receivedData, this.getRootConstraint());
			if(variants.size() != 0){
				this.addSubItem(variants.get(0).getDatatype().getReference());
			}
		} else if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclOccurrenceConstraint)){
			Topic occType = TmHelper.getConstrainedStatement(this.getRootConstraint());
			JsArray<Occurrence> occs = ((Topic)this.receivedData).getOccurrences(occType);
			if(occs.length() != 0) this.addSubItem(occs.get(0).getDatatype().getReference());
		} else {
			throw new InvalidGdlSchemaException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a constraint of the type " + PSIs.TMCL.tmclVariantNameConstraint + " or " + PSIs.TMCL.tmclOccurrenceConstraint + ",  but is bound to: " + TmHelper.getAnyIdOfTopic(this.getRootConstraint()));
		}
	}
	
	
	// sets the name- and occurrence-fields for the received data
	private void setReceivedTopicCharacteristics() throws InvalidGdlSchemaException, ExecutionException {
		if(!(receivedData instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + receivedData.getClass());

		Topic characteristicType = TmHelper.getConstrainedStatement(this.getConstraint());
		
		if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclTopicNameConstraint)){
			for(int i = 0; i != ((Topic)this.receivedData).getNames(characteristicType).length(); ++i)
				this.addSubItem(((Topic)this.receivedData).getNames(characteristicType).get(i).getValue());
		} else {
			for(int i = 0; i != ((Topic)this.receivedData).getOccurrences(characteristicType).length(); ++i)
				this.addSubItem(((Topic)this.receivedData).getOccurrences(characteristicType).get(i).getValue());
		}
	}
	
	
	// sets the fields for the received data
	protected void setReceivedData() throws InvalidGdlSchemaException, ExecutionException {
		if(receivedData == null) return;

		if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclSubjectIdentifierConstraint)){
			this.setReceivedTopicIdentifiers();
		} else if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclSubjectLocatorConstraint)){
			this.setReceivedTopicIdentifiers();
		} else if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclItemIdentifierConstraint)){
			this.setReceivedItemIdentifiers();
		} else if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclTopicNameConstraint)){
			this.setReceivedTopicCharacteristics();
		} else if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclTopicOccurrenceConstraint)){
			this.setReceivedTopicCharacteristics();
		} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclReifierConstraint)){
			this.setReceivedReifier();
		} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclVariantNameConstraint)){
			this.setReceivedVariantName();
		} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclScopeConstraint)){
			this.setReceivedScope();
		} else if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.GDL.TopicType.gdlRolePlayer)){
			this.setReceivedRolePlayer();
		} else if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.GDL.TopicType.gdlType)){
			this.setReceivedType();
		} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.GDL.TopicType.gdlDatatype)){
			this.setReceivedDatatype();
		} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.GDL.TopicType.gdlVariantNameIdentifiers)){
			this.setReceivedVariantNameIdentifiers();
		} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.GDL.TopicType.gdlVariantNameReifier)){
			this.setReceivedVariantNameReifier();
		} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.GDL.TopicType.gdlVariantNameScope)){
			this.setReceivedVariantNameScope();
		} else {
			throw new InvalidGdlSchemaException("The constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " is not suported to be bound to the value group instance " + TmHelper.getAnyIdOfTopic(this.getValueGroup()));
		}
	}


	// sets the field for the set default value or sets at least one empty field (card-min)
	protected void setDefaultValue() throws InvalidGdlSchemaException, ExecutionException {
		if(this.getDefaultLiteralValue() != null && this.getDefaultTmValue() != null) throw new InvalidGdlSchemaException("the topic " + TmHelper.getAnyIdOfTopic(this.getValueGroup()) + " must be bound to maximal one " + PSIs.GDL.TopicType.gdlDefaultValue + ", but is: 2");

		if(this.getDefaultLiteralValue() != null){
			TopicMap tm = this.getDefaultLiteralValue().getTopicMap();
			Topic occType = TmHelper.getTopicByPsi(PSIs.GDL.OccurrenceType.gdlLiteralValue, tm);
			JsArray<Occurrence> vals = this.getDefaultLiteralValue().getOccurrences(occType);

			if(vals.length() != 1) throw new InvalidGdlSchemaException("the topic " + TmHelper.getAnyIdOfTopic(this.getDefaultLiteralValue()) + " must be bound exactly once to a " + PSIs.GDL.OccurrenceType.gdlLiteralValue + " occurrence, but is: " + vals.length());

			int minValues = this.getCardMin() == 0 ? 1 : this.getCardMin();
			for(int i = 0; i < minValues; ++i)
				this.addSubItem(vals.get(0).getValue());
			if(this.fixedDefaultValue()) this.fixValue();
		} else if(this.getDefaultTmValue() != null){
			ArrayList<Topic> values = TmHelper.getValuesForTmValue(this.getDefaultTmValue());

			if(values.size() != 1) throw new InvalidGdlSchemaException("the default value " + TmHelper.getAnyIdOfTopic(this.getDefaultValue()) + " represetns more than one value");

			int minValues = this.getCardMin() == 0 ? 1 : this.getCardMin();
			for(int i = 0; i < minValues; ++i)
				this.addSubItem(this.getTopicRepresentation(this.getDefaultTmValue(), this.getDisplayByOfTmValue(this.getDefaultTmValue()), this.getPreferredScopeOfTmValue(this.getDefaultTmValue())));
			if(this.fixedDefaultValue()) this.fixValue();
		} else {
			this.addSubItem("");
		}
	}


	public int getCardMin() throws InvalidGdlSchemaException {
		if(this.cardMinSet){
			return this.cardMin;
		} else {
			this.cardMaxSet = true;
			return TmHelper.getCardMin(this.getRootConstraint());
		}
	}


	public int getCardMax() throws InvalidGdlSchemaException {
		if(this.cardMaxSet){
			return this.cardMax;
		} else {
			this.cardMaxSet = true;
			return TmHelper.getCardMax(this.getRootConstraint());
		}
	}


	// returns the topic instance of gdlt:Value-Group that is bound to this
	// visible element, or null if it is unbound
	public Topic getValueGroup() throws InvalidGdlSchemaException {
		if(this.valueGroupTopicSet){
			return this.valueGroupTopic;
		} else {
			this.valueGroupTopic = TmHelper.getValueGroupOf(this.tmRepresentative);
			this.valueGroupTopicSet = true;
			return this.valueGroupTopic;
		}
	}


	// returns the direct (first) constraint that is bound to the value-group
	// of this element - or null if it is unbound
	public Topic getConstraint() throws InvalidGdlSchemaException {
		if(this.constraintTopicSet){
			return this.constraintTopic;
		} else {
			this.constraintTopic = TmHelper.getConstraintOfValueGroup(this.getValueGroup());
			this.constraintTopicSet = true;
			return this.constraintTopic;
		}
	}


	// returns the root (last) constraint that is bound to the value-group
	// of this element - or null if it is unbound
	public Topic getRootConstraint() throws InvalidGdlSchemaException {
		if(this.rootConstraintTopicSet){
			return this.rootConstraintTopic;
		} else {
			this.rootConstraintTopic = TmHelper.getRootConstraintOfValueGroup(this.getValueGroup(), this.getConstraint());
			this.rootConstraintTopicSet = true;
			return this.rootConstraintTopic;
		}
	}


	// returns the topic that represents the default topic maps value of
	// the value-group that is bound to this element - null if it is unbound
	public Topic getDefaultTmValue() throws InvalidGdlSchemaException {
		if(this.defaultTmValueTopicSet){
			return this.defaultTmValueTopic;
		} else {
			this.defaultTmValueTopic = TmHelper.getDefaultTmValue(this.getValueGroup());
			this.defaultTmValueTopicSet = true;
			return this.defaultTmValueTopic;
		}
	}


	// returns the topic that represents the default literal value of the
	// value-group that is bound to this element - or null if it is unbound
	public Topic getDefaultLiteralValue() throws InvalidGdlSchemaException {
		if(this.defaultLiteralValueTopicSet){
			return this.defaultLiteralValueTopic;
		} else {
			this.defaultLiteralValueTopic = TmHelper.getDefaultLiteralValue(this.getValueGroup());
			this.defaultLiteralValueTopicSet = true;
			return this.defaultLiteralValueTopic;
		}
	}


	// returns the topic that represents the default value of
	// the value-group that is bound to this element - null if it is unbound
	public Topic getDefaultValue() throws InvalidGdlSchemaException {
		if(this.getDefaultLiteralValue() != null && this.getDefaultTmValue() != null) throw new InvalidGdlSchemaException("the topic " + TmHelper.getAnyIdOfTopic(this.getValueGroup()) + " must be bound to maximal one " + PSIs.GDL.TopicType.gdlDefaultValue + ", but is: 2");
		else if(this.getDefaultLiteralValue() != null) return this.getDefaultLiteralValue();
		else return this.getDefaultTmValue();
	}


	// returns true if the default value is fixed
	// otherwise the return value is false
	public boolean fixedDefaultValue() throws InvalidGdlSchemaException{
		Topic defVal = this.getDefaultValue();

		if(defVal == null) return false;

		TopicMap tm = defVal.getTopicMap();
		Occurrence fixedOcc = TmHelper.getSingleOccurrence(defVal, TmHelper.getTopicByPsi(PSIs.GDL.OccurrenceType.gdlFixed, tm));

		if(fixedOcc != null){
			try{
				return Boolean.valueOf(fixedOcc.getValue().toLowerCase());
			}catch(Exception e){
				throw new InvalidGdlSchemaException("the occurrence of type " + PSIs.GDL.OccurrenceType.gdlFixed + " bound to the topic " + TmHelper.getAnyIdOfTopic(defVal) + " must be set to either true or false, but is: " + fixedOcc.getValue());
			}
		} else {
			return false;
		}
	}


	// returns all topic maps values represented by topics of the type gdlt:Tm-Value
	// that are valid and declared for the value-group of this element - or
	// an empty ArrayList
	public ArrayList<Topic> getTmValues() throws InvalidGdlSchemaException {
		if(this.tmValueTopicsSet){
			return this.tmValueTopics;
		} else {
			this.tmValueTopics = TmHelper.getTmValues(this.getValueGroup());
			this.tmValueTopicsSet = true;
			return this.tmValueTopics;
		}
	}


	// returns all topics that represents literal values for this value-group - or
	// an empty ArrayList
	public ArrayList<Topic> getLiteralValues() throws InvalidGdlSchemaException {
		if(this.literalValueTopicsSet){
			return this.literalValueTopics;
		} else {
			this.literalValueTopics = TmHelper.getLiteralValues(this.getValueGroup());
			this.literalValueTopicsSet = true;
			return this.literalValueTopics;
		}
	}


	// returns an ArrayList of strings that are set to a value group as literal values
	public ArrayList<String> getLiterals() throws InvalidGdlSchemaException {
		if(this.literalsSet){
			return this.literals;
		} else {
			this.literalsSet = true;
			this.literals = TmHelper.getLiterals(this.getValueGroup());
			return this.literals;
		}
	}


	// returns the valid topic maps value for the constraint bound
	// to the value-group that is bound to this element - or an empty ArrayList
	public ArrayList<Topic> getTmValuesForConstraint() throws InvalidGdlSchemaException, ExecutionException {
		return TmHelper.getTmValuesForConstraint(this.getConstraint(), this.getValueGroup());
	}


	// returns the regular expression that is set for the constraint bound to the
	// value-group of this element
	public String getLiteralValueForConstraint() throws InvalidGdlSchemaException {
		return TmHelper.getLiteralValueForConstraint(this.getConstraint());
	}


	// returns the display-by schema that is defined for the value-group that
	// is bound to this element
	public Topic getDisplayByOfValueGroup() throws InvalidGdlSchemaException {
		if(this.displayByConstraintSet){
			return this.displayByConstraint;
		} else {
			this.displayByConstraintSet = true;
			this.displayByConstraint = TmHelper.getDisplayByTopicOf(this.getValueGroup());
			return this.displayByConstraint;
		}
	}


	// returns the display-by schema that is defined for the passed
	// TM-Value that is bound to this element
	public Topic getDisplayByOfTmValue(Topic tmValue) throws InvalidGdlSchemaException{
		return TmHelper.getDisplayByTopicOf(tmValue);
	}


	// returns the preferred scope that is bound to the value-group of
	// this element - or an empty ArrayList
	public ArrayList<Topic> getPreferredScopeOfValueGroup() throws InvalidGdlSchemaException {
		if(this.prefferedScopesSet){
			return this.preferredScopes;
		} else {
			this.prefferedScopesSet = true;
			this.preferredScopes = TmHelper.getPrefferedScopesForTopicOf(this.getValueGroup());
			return this.preferredScopes;
		}
	}


	// returns the preferred scope that is bound to the passed TM-Value topic
	// this element - or an empty ArrayList
	public ArrayList<Topic> getPreferredScopeOfTmValue(Topic tmValue) throws InvalidGdlSchemaException{
		return TmHelper.getPrefferedScopesForTopicOf(tmValue);
	}


	// returns the string that represents the topic topicToRepresent corresponding
	// to the passed displayBy and prefferedScopes arguments
	public String getTopicRepresentation(Topic topicToRepresent, Topic displayBy, ArrayList<Topic> preferredScopes) throws InvalidGdlSchemaException {
		return TmHelper.getTopicRepresentation(topicToRepresent, displayBy, preferredScopes);
	}


	// returns the strings of the control that are entered/selected 
	// returns the strings of the control that are entered/selected 
	public abstract ArrayList<String> getSelectedValues();


	// validates names, occurrences and identifiers for the passed value
	private void validateLiteralValue(String selectedValue) throws InvalidContentException, InvalidGdlSchemaException{
		if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclSubjectIdentifierConstraint) || TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclSubjectLocatorConstraint) || TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclItemIdentifierConstraint)){
			Pattern pattern = new Pattern(TmHelper.getRegExp(this.getConstraint()));
			if(!pattern.matches(selectedValue)) throw new InvalidContentException("The value \"" + selectedValue + "\" does not satisfy the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " with the regular expression: " + TmHelper.getRegExp(this.getConstraint()));
		} else {
			ArrayList<Pattern> validLiteralValues = new ArrayList<Pattern>();
			for (String literal : this.getLiterals()) validLiteralValues.add(new Pattern(literal));
			if(validLiteralValues.size() == 0) validLiteralValues.add(new Pattern(this.getLiteralValueForConstraint()));	
			int i = 0;
			for( ; i != validLiteralValues.size(); ++i) if(validLiteralValues.get(i).matches(selectedValue)) break;

			if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclTopicNameConstraint)){
				if(i == validLiteralValues.size()) throw new InvalidContentException("the user data " + selectedValue + " for the topic-name " + TmHelper.getAnyIdOfTopic(TmHelper.getConstrainedStatement(this.getConstraint())) + " does not satisfy any of the constraints: " + Utils.arrayToString(validLiteralValues));
			} else if(TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclTopicOccurrenceConstraint)){
				if(i == validLiteralValues.size()) throw new InvalidContentException("the user data " + selectedValue + " for the topic-occurrence " + TmHelper.getAnyIdOfTopic(TmHelper.getConstrainedStatement(this.getConstraint())) + " does not satisfy any of the constraints: " + Utils.arrayToString(validLiteralValues));
			}
		}
	}


	// validates the the tm values of a constraint
	private void validateTmValue(Topic selectedPlayer) throws InvalidContentException, InvalidGdlSchemaException, ExecutionException {
		if(this.getRawTmValues().size() != 0 && !this.getRawTmValues().contains(selectedPlayer)){
			throw new InvalidContentException("the topic " + TmHelper.getAnyIdOfTopic(selectedPlayer) + " does not satisfy the contraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " and the topic values: " + Utils.topicArrayToString(this.getRawTmValues()));
		} else {
			if(!this.getTmValuesForConstraint().contains(selectedPlayer)){
				throw new InvalidContentException("the topic " + TmHelper.getAnyIdOfTopic(selectedPlayer) + " does not satisfy the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " and topic values: " + Utils.topicArrayToString(this.getTmValuesForConstraint()));
			}
		}
	}


	// returns the actual topics that are set as possible tm-values
	private ArrayList<Topic> getRawTmValues() throws InvalidGdlSchemaException{
		if(this.rawTmValuesSet){
			return this.rawTmValues;
		} else {
			this.rawTmValuesSet = true;
			ArrayList<Topic> tmValues = this.getTmValues();
			for (Topic tmValue : tmValues) this.rawTmValues = Utils.union(this.rawTmValues, TmHelper.getValuesForTmValue(tmValue));
			return this.rawTmValues;
		}
	}

	
	// returns all locators contained in the passed locators array, which match the
	// passed regular expression value
	private ArrayList<Locator> filterLocators(String pattern, JsArray<Locator> locators){
		ArrayList<Locator> result = new ArrayList<Locator>();
		if(locators == null || locators.length() == 0) return result;
		Pattern patternObject = new Pattern(pattern == null ? ".*" : pattern);
		
		for(int i = 0; i != locators.length(); ++i){
			if(patternObject.matches(locators.get(i).getReference())) result.add(locators.get(i));
		}
		
		return result;
	}
	
	
	// handles the getContent call for subject identifiers and subject locators
	private void getTopicIdentifierContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Topic carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(!(carrier instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + carrier.getClass());
		JsArray<Locator> identifiers = null;
			
		ArrayList<Locator> filteredIdentifiers = null;
		boolean isPsiConstraint = false;
		if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclSubjectIdentifierConstraint)){
			isPsiConstraint = true;
			identifiers = carrier.getSubjectIdentifiers();
			filteredIdentifiers = this.filterLocators(TmHelper.getRegExp(this.getConstraint()), identifiers);
		} else if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclSubjectLocatorConstraint)){
			identifiers = carrier.getSubjectLocators();
			filteredIdentifiers = this.filterLocators(TmHelper.getRegExp(this.getConstraint()), identifiers);
		} else {
			throw new ExecutionException("Only the constraints " + PSIs.TMCL.tmclSubjectIdentifierConstraint + " and " + PSIs.TMCL.tmclSubjectLocatorConstraint + " are supported by the function getTopicIdentifierContent");
		}
		
		Locator changedIdentifier = null;
		if(validate) this.validateLiteralValue(this.getSelectedValues().get(selectedValueIndex));
		
		if(filteredIdentifiers.size() > selectedValueIndex){
			changedIdentifier = filteredIdentifiers.get(selectedValueIndex);
			if(isPsiConstraint) carrier.removeSubjectIdentifier(changedIdentifier);
			else carrier.removeSubjectLocator(changedIdentifier);
		}
		
		changedIdentifier = carrier.getTopicMap().createLocator(this.getSelectedValues().get(selectedValueIndex));
		if(isPsiConstraint) carrier.addSubjectIdentifier(changedIdentifier);
		else carrier.addSubjectLocator(changedIdentifier);
		contents.add(new Pair<Object, TopicMapsTypes>(changedIdentifier, TopicMapsTypes.Locator));
	}
	
	
	// handles the getContent call for item identifiers
	private void getItemIdentifierContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Construct carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(!(this.receivedData instanceof Reifiable) || !(this.receivedData instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Reifiable, but is: " + receivedData.getClass());

		// get type
		Topic constrainedTopicType = TmHelper.getConstrainedTopicType(this.getConstraint());

		int typeIdx = -1;
		JsArray<Topic> types = null;
		if((carrier instanceof Topic)){
			types = ((Topic)carrier).getTypes();
			if(types.length() != 0){
				for(typeIdx = 0; typeIdx != types.length(); ++typeIdx) if(types.get(typeIdx).equals(constrainedTopicType)) break;
			}
		}

		JsArray<Locator> identifiers = null;
		ArrayList<Locator> filteredIdentifiers = null;
		if((carrier instanceof Topic) && types != null && typeIdx != types.length()){
			identifiers = ((Topic)carrier).getItemIdentifiers();
			filteredIdentifiers = this.filterLocators(TmHelper.getRegExp(this.getConstraint()), identifiers);
			
			Locator changedIdentifier = null;
			if(validate) this.validateLiteralValue(this.getSelectedValues().get(selectedValueIndex));
			
			if(filteredIdentifiers.size() > selectedValueIndex){
				changedIdentifier = filteredIdentifiers.get(selectedValueIndex);
				((Topic)carrier).removeItemIdentifier(changedIdentifier);
			}
			
			changedIdentifier = ((Topic)carrier).getTopicMap().createLocator(this.getSelectedValues().get(selectedValueIndex));
			((Topic)carrier).addItemIdentifier(changedIdentifier);
			contents.add(new Pair<Object, TopicMapsTypes>(changedIdentifier, TopicMapsTypes.Locator));
		} else {			
			// search for the topic type
			Reifiable ref = null;

			// search for the characteristics type
			if(carrier instanceof Topic){
				JsArray<Name> names = ((Topic)carrier).getNames(constrainedTopicType);
				if(names.length() != 0){
					ref = names.get(0);
				} else {
					JsArray<Occurrence> occs = ((Topic)carrier).getOccurrences(constrainedTopicType);
					if(occs.length() != 0) ref = occs.get(0);
				}
			} else if(carrier instanceof Association){
				JsArray<Role> roles = ((Association)carrier).getRoles(constrainedTopicType);
				if(roles.length() != 0) ref = roles.get(0);
			}
			if(ref == null) return;
			
			// search for item-identifiers of the found topic type or characteristics
			identifiers = ((ReifiableStub)ref).getItemIdentifiers();
			filteredIdentifiers = this.filterLocators(TmHelper.getRegExp(this.getConstraint()), identifiers);
			
			Locator changedIdentifier = null;
			if(validate) this.validateLiteralValue(this.getSelectedValues().get(selectedValueIndex));
			
			if(filteredIdentifiers.size() > selectedValueIndex){
				changedIdentifier = filteredIdentifiers.get(selectedValueIndex);
				((ReifiableStub)carrier).removeItemIdentifier(changedIdentifier);
			}
			
			changedIdentifier = ((ReifiableStub)carrier).getTopicMap().createLocator(this.getSelectedValues().get(selectedValueIndex));
			((ReifiableStub)carrier).addItemIdentifier(changedIdentifier);
			contents.add(new Pair<Object, TopicMapsTypes>(changedIdentifier, TopicMapsTypes.Locator));
		}
	}
	

	// handles the getContent call for item identifiers of variant-names
	private void getVariantIdentifierContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Topic carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(!(carrier instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + carrier.getClass());
		ArrayList<Variant> possibleVariants = TmHelper.getVariantsForConstraint(carrier, this.getRootConstraint());		
		if(possibleVariants.size() != 0){
			Variant variant = possibleVariants.get(0);
			JsArray<Locator> identifiers = null;

			ArrayList<Locator> filteredIdentifiers = null;

			identifiers = variant.getItemIdentifiers();
			filteredIdentifiers = this.filterLocators(TmHelper.getRegExp(this.getConstraint()), identifiers);

			Locator changedIdentifier = null;
			if(validate) this.validateLiteralValue(this.getSelectedValues().get(selectedValueIndex));

			if(filteredIdentifiers.size() > selectedValueIndex){
				changedIdentifier = filteredIdentifiers.get(selectedValueIndex);
				variant.removeItemIdentifier(changedIdentifier);
			}

			changedIdentifier = variant.getTopicMap().createLocator(this.getSelectedValues().get(selectedValueIndex));
			variant.addItemIdentifier(changedIdentifier);
			contents.add(new Pair<Object, TopicMapsTypes>(changedIdentifier, TopicMapsTypes.Locator));
		}
	}
	
	
	// handles the getContent call for item identifiers of variant-names
	private void getVariantReifierContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Topic carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(!(carrier instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + carrier.getClass());
		ArrayList<Variant> possibleVariants = TmHelper.getVariantsForConstraint(carrier, this.getRootConstraint());		
		if(possibleVariants.size() != 0){
			Variant variant = possibleVariants.get(0);
			Topic reifier = TmHelper.getTopicFromStringRepresentation(this.getSelectedValues().get(selectedValueIndex), this.getValueGroup());
			
			if(reifier != null && !variant.getReifier().equals(reifier)) variant.setReifier(reifier);
			contents.add(new Pair<Object, TopicMapsTypes>(variant, TopicMapsTypes.Variant));
		}
	}
	
	
	// handles the getContent call for occurrence and name values
	private void getTopicCharacteristicContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Topic carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(!(carrier instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + carrier.getClass());
		Topic characteristicType = TmHelper.getConstrainedStatement(this.getConstraint());
		
		boolean isOccConstraint = true;
		if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclTopicOccurrenceConstraint)){
			isOccConstraint = true;
		} else if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclTopicNameConstraint)){
			isOccConstraint = false;
		} else {
			throw new ExecutionException("the function getTopicCharacteristicContent can operate only on constraints of the type " + PSIs.TMCL.tmclTopicOccurrenceConstraint + " or " + PSIs.TMCL.tmclTopicNameConstraint + ", but is called with " + TmHelper.getAnyIdOfTopic(this.getConstraint()));
		}
		
		JsArray<Name> names = null;
		JsArray<Occurrence> occurrences = null;
		if(isOccConstraint) occurrences = carrier.getOccurrences(characteristicType);
		else names = carrier.getNames(characteristicType);
		

		Construct changedConstruct = null;
		if(validate) this.validateLiteralValue(this.getSelectedValues().get(selectedValueIndex)); 

		if(isOccConstraint){
			if(occurrences.length() > selectedValueIndex){
				changedConstruct = occurrences.get(selectedValueIndex);
				((Occurrence)changedConstruct).setValue(this.getSelectedValues().get(selectedValueIndex));
			}else {
				changedConstruct = carrier.createOccurrence(characteristicType, this.getSelectedValues().get(selectedValueIndex), null);
			}
		} else {
			if(names.length() > selectedValueIndex){
				changedConstruct = names.get(selectedValueIndex);
				((Name)changedConstruct).setValue(this.getSelectedValues().get(selectedValueIndex));
			}else {
				changedConstruct = carrier.createName(this.getSelectedValues().get(selectedValueIndex), characteristicType, null);
			}
		}
		contents.add(new Pair<Object, TopicMapsTypes>(changedConstruct, isOccConstraint ? TopicMapsTypes.Occurrence : TopicMapsTypes.Name));
	}
	
	
	// handles the getContent call for subject identifiers and subject locators
	private void getVariantNameContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Topic carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(!(this.receivedData instanceof Topic)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a Topic, but is: " + receivedData.getClass());

		ArrayList<Variant> variants = TmHelper.getVariantsForConstraint((Topic)this.receivedData, this.getConstraint());

		Variant changedVariant = null;
		if(validate) this.validateLiteralValue(this.getSelectedValues().get(selectedValueIndex)); 

		if(variants.size() > selectedValueIndex){
			changedVariant = variants.get(selectedValueIndex);
			changedVariant.setValue(this.getSelectedValues().get(selectedValueIndex));
		}else {
			Topic nameType = TmHelper.getConstrainedStatement(this.getConstraint());
			JsArray<Name> names = carrier.getNames(nameType);
			Name owner = names.get(0);
			Topic scope = TmHelper.getConstrainedScopeTopic(this.getConstraint());
			@SuppressWarnings("unchecked")
			JsArray<Topic> scopes = (JsArray<Topic>) JsArray.createArray();
			scopes.push(scope);
			changedVariant = owner.createVariant(this.getSelectedValues().get(selectedValueIndex), scopes);
		}
		contents.add(new Pair<Object, TopicMapsTypes>(changedVariant, TopicMapsTypes.Variant));
	}
	
	
	// handles the getContent call for role players
	private void getRolePlayerContent(ArrayList<Pair<Object, TopicMapsTypes>> contents, boolean validate, Association carrier, int selectedValueIndex) throws InvalidGdlSchemaException, InvalidContentException, ExecutionException{
		if(!(carrier instanceof Association)) throw new ExecutionException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to an Association, but is: " + carrier.getClass());
		
		if(!TmHelper.isInstanceOf(this.getRootConstraint(), PSIs.TMCL.tmclTopicRoleConstraint))throw new InvalidGdlSchemaException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " must be bound to a root constraint of the type " + PSIs.TMCL.tmclTopicRoleConstraint + ", but is: " + TmHelper.getAnyIdOfTopic(this.getRootConstraint()));
		Pair<Topic, Topic> roleAndPlayerType = TmHelper.getConstrainedRoleAndPlayerTypeOfConstraint(this.getRootConstraint());
		Topic roleType = roleAndPlayerType.getFirst();
		Topic playerType = roleAndPlayerType.getSecond();

		JsArray<Role> typedRoles = carrier.getRoles(roleType);
		ArrayList<Role> roles = new ArrayList<Role>();
		for(int i = 0; i != typedRoles.length(); ++i)
			if(TmHelper.isInstanceOf(typedRoles.get(i).getPlayer(), playerType)) roles.add(typedRoles.get(i));

		Role changedRole = null;

		Topic player = TmHelper.getTopicFromStringRepresentation(this.getSelectedValues().get(selectedValueIndex), this.getValueGroup());
		if(validate) this.validateTmValue(player);
		if(roles.size() > selectedValueIndex){
			changedRole = roles.get(selectedValueIndex);
			changedRole.setPlayer(player);
		} else {
			changedRole = carrier.createRole(roleType, player);
		}
		contents.add(new Pair<Object, TopicMapsTypes>(changedRole, TopicMapsTypes.Role));
	}
	
		
	// returns the actual data that is hold by this instance
	public ArrayList<Pair<Object, TopicMapsTypes>> getContent(Construct carrier, boolean validate) throws InvalidGdlSchemaException, ExecutionException, InvalidContentException {
		ArrayList<Pair<Object, TopicMapsTypes>> result = new ArrayList<Pair<Object,TopicMapsTypes>>();
		if(this.getRootConstraint() == null) return result;
		Construct localCarrier = carrier;
		if(carrier == null) localCarrier = TmHelper.getNearestTopicOrAssociation(this);

		for (int idx = 0; idx != this.getSelectedValues().size(); ++idx){
			if(TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclItemIdentifierConstraint)){
				this.getItemIdentifierContent(result, validate, (Topic)localCarrier, idx);
			} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclSubjectIdentifierConstraint)){
				this.getTopicIdentifierContent(result, validate, (Topic)localCarrier, idx);
			} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclSubjectLocatorConstraint)){
				this.getTopicIdentifierContent(result, validate, (Topic)localCarrier, idx);
			} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclTopicNameConstraint)){
				this.getTopicCharacteristicContent(result, validate, (Topic)localCarrier, idx);
			} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclVariantNameConstraint)){
				this.getVariantNameContent(result, validate, (Topic)localCarrier, idx);
			} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclScopeConstraint)){
				// TODO: implement
			} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclReifierConstraint)){
				// TODO: implement
			} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.GDL.TopicType.gdlDatatype)){
				// TODO: implement
			} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.GDL.TopicType.gdlType)){
				// TODO: implement
			} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.GDL.TopicType.gdlVariantNameScope)){
				// TODO: implement
			} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.GDL.TopicType.gdlVariantNameReifier)){
				this.getVariantReifierContent(result, validate, (Topic)localCarrier, idx);
			} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.GDL.TopicType.gdlVariantNameIdentifiers)){
				this.getVariantIdentifierContent(result, validate, (Topic)localCarrier, idx);
			} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.TMCL.tmclTopicOccurrenceConstraint)){
				this.getTopicCharacteristicContent(result, validate, (Topic)localCarrier, idx);
			} else if (TmHelper.isInstanceOf(this.getConstraint(), PSIs.GDL.TopicType.gdlRolePlayer)){
				this.getRolePlayerContent(result, validate, (Association)localCarrier, idx);
			} else {
				throw new InvalidGdlSchemaException("the constraint " + TmHelper.getAnyIdOfTopic(this.getConstraint()) + " is not supported");
			}
		}

		return result;
	}
}
