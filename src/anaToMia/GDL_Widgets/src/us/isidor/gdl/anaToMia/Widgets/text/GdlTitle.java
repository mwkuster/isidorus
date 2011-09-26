package us.isidor.gdl.anaToMia.Widgets.text;

import java.util.ArrayList;

import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Construct;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Occurrence;
import us.isidor.gdl.anaToMia.TopicMaps.TopicMapsModel.Topic;
import us.isidor.gdl.anaToMia.Widgets.base.GdlVisibleObject;
import us.isidor.gdl.anaToMia.Widgets.base.PSIs;
import us.isidor.gdl.anaToMia.Widgets.environment.ActiveStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.ExecutionException;
import us.isidor.gdl.anaToMia.Widgets.environment.FocusStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.HoverStyleHandler;
import us.isidor.gdl.anaToMia.Widgets.environment.InvalidGdlSchemaException;
import us.isidor.gdl.anaToMia.Widgets.environment.Pair;
import us.isidor.gdl.anaToMia.Widgets.value.AutoNumUnitValue;
import us.isidor.gdl.anaToMia.Widgets.value.ColorValue;
import us.isidor.gdl.anaToMia.Widgets.value.DirectionValue;
import us.isidor.gdl.anaToMia.Widgets.value.FontWeightValue;
import us.isidor.gdl.anaToMia.Widgets.value.NormalNumUnitValue;
import us.isidor.gdl.anaToMia.Widgets.value.PositiveNumUnitValue;
import us.isidor.gdl.anaToMia.Widgets.value.TextAlignValue;
import us.isidor.gdl.anaToMia.Widgets.value.TextDecorationValue;
import com.google.gwt.dom.client.Style.FontStyle;
import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.FocusEvent;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.dom.client.HasBlurHandlers;
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
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Element;
import com.google.gwt.user.client.ui.AbsolutePanel;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;


public class GdlTitle extends GdlTextObject {
	protected ArrayList<Pair<String, String>> titleActiveCssNamesAndStyles = new ArrayList<Pair<String,String>>();
	protected ArrayList<Pair<String, String>> titleFocusCssNamesAndStyles = new ArrayList<Pair<String,String>>();
	protected ArrayList<Pair<String, String>> titleHoverCssNamesAndStyles = new ArrayList<Pair<String,String>>();
	
	protected GdlTitle() throws InvalidGdlSchemaException, ExecutionException {
		super();
	}
	
	
	public GdlTitle(Topic tmRepresentative, Construct receivedData, GdlVisibleObject gdlParent) throws InvalidGdlSchemaException, ExecutionException{
		super(tmRepresentative, receivedData, gdlParent);
		// TODO: create a Title element for each TM-elem
		this.createNewTitle().setText("Title");
		this.setNthButtons();
	}
	
	
	@Override
	public void addSubItem(String value) throws InvalidGdlSchemaException, ExecutionException {
		this.createNewTitle().setText(value);
	}
	
	
	// sets the list Element to either ol or ul depending on the gdl:ordered property
	public TitleWidget createNewTitle() throws InvalidGdlSchemaException, ExecutionException {
		// this object is able to own only one sub-element
		TitleWidget title = new TitleWidget(this.getTitleOrder());
		ActiveStyleHandler asHandler = new ActiveStyleHandler(this);
		FocusStyleHandler fsHandler = new FocusStyleHandler(this);
		HoverStyleHandler hsHandler = new HoverStyleHandler(this);
		int idSuffix = 0;
		if(this.subElements != null) idSuffix = this.subElements.size(); 
		title.setId(this.getId() + "__GDL_" + idSuffix);
		title.addMouseDownHandler(asHandler);
		title.addMouseUpHandler(asHandler);
		title.addMouseOverHandler(hsHandler);
		title.addMouseOutHandler(hsHandler);
		title.addFocusHandler(fsHandler);
		title.addBlurHandler(fsHandler);
		super.addToContainerPanel(title);	
		this.setGdlStyle(title);
		return title;
	}
	
	
	// removes the passed element and all its handlers from the outer element
	public void removeTitle(TitleWidget elem) throws InvalidGdlSchemaException, ExecutionException {
		this.removeFromContainer(elem);
	}
	
	
	// returns a PositiveNumUnitValue instance that represents the text font-size of this element.
	// If a styleClass is set, only the corresponding value of the scoped occurrence is returned
	// or null. The default values of gdl:Title differ to gdl:Text-Object
	@Override
	public PositiveNumUnitValue getFontSize(String styleClass) throws InvalidGdlSchemaException {
		Occurrence sizeOcc = null;
		if(styleClass != null){
			sizeOcc = super.getNoneOrOneScopedOccurrence(PSIs.GDL.OccurrenceType.gdlFontSize, styleClass);
		} else {
			sizeOcc = super.getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlFontSize);
		}

		if(sizeOcc == null && styleClass != null){
			return null;
		} else if(sizeOcc == null) {
			switch(this.getTitleOrder()){
			case 1: return new PositiveNumUnitValue("24pt");
			case 2: return new PositiveNumUnitValue("19pt");
			case 3: return new PositiveNumUnitValue("16pt");
			default: return new PositiveNumUnitValue("14pt");
			}
		} else {
			return new PositiveNumUnitValue(sizeOcc.getValue());
		}
	}
	
	
	// returns the property of a gdl:title-order occurrence, otherwise the default value if no occurrence is set
	public int getTitleOrder() throws InvalidGdlSchemaException{
		Occurrence orderOcc = getNoneOrOneUnscopedOccurrence(PSIs.GDL.OccurrenceType.gdlTitleOrder);

		if(orderOcc != null){
			try{
				int titleOrder = Integer.valueOf(orderOcc.getValue());
				return titleOrder;
			}catch(NumberFormatException e){
				throw new InvalidGdlSchemaException("The occurrence " + PSIs.GDL.OccurrenceType.gdlTitleOrder + " must be set to one of \"1\", \"2\", \"3\" or \"4\", but is \"" + orderOcc.getValue() + "\"");
			}
		} else {
			return 1;
		}
	}
		
	
	@Deprecated
	public void setTitleOrder(Widget widget, int value){
		// this method is not implemented, since the title-order is set in initTitle
		// It exists only for consistency reasons
	}

	
	// adds a stly property and value as a pair to the corresponding array list
	private void addStyleToStore(String styleClass, String property, String value){
		if(styleClass == null) return;
		else if(styleClass.equals(PSIs.GDL.Scope.gdlActive)) this.titleActiveCssNamesAndStyles.add(new Pair<String, String>(property, value));
		else if(styleClass.equals(PSIs.GDL.Scope.gdlFocus)) this.titleFocusCssNamesAndStyles.add(new Pair<String, String>(property, value));
		else if(styleClass.equals(PSIs.GDL.Scope.gdlHover)) this.titleHoverCssNamesAndStyles.add(new Pair<String, String>(property, value));
	}
	

	// sets the direction style property of this element by using the GWT DOM class
	@Override
	public void setDirection(Widget widget, DirectionValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "direction", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "direction", value.getCssValue());
		}
	}
	
	
	// sets the text-align style property of this element by using the GWT DOM class
	@Override
	public void setTextAlign(Widget widget, TextAlignValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "textAlign", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "textAlign", value.getCssValue());
		}
	}
	
	
	// sets the line-height style property of this element by using the GWT DOM class
	@Override
	public void setLineHeight(Widget widget, NormalNumUnitValue value, String styleClass) throws InvalidGdlSchemaException,	ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "lineHeight", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "lineHeight", value.getCssValue());
		}
	}
	
	
	// sets the text-decoration style property of this element by using the GWT DOM class
	@Override
	public void setTextDecoration(Widget widget, TextDecorationValue value,	String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "textDecoration", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "textDecoration", value.getCssValue());
		}
	}
	
	
	// sets the color style property of this element by using the GWT DOM class
	@Override
	public void setColor(Widget widget, ColorValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "color", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "color", value.getCssValue());
		}
	}
	
	
	// sets the font-family style property of this element by using the GWT DOM class
	@Override
	public void setFontFamily(Widget widget, String value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "fontFamily", value);
			}
			this.addStyleToStore(styleClass, "fontFamily", value);
		}
	}
	
	
	// sets the font-style style property of this element by using the GWT DOM class
	@Override
	public void setFontStyle(Widget widget, FontStyle value, String styleClass)	throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "fontStyle", value.getCssName());
			}
			this.addStyleToStore(styleClass, "fontStyle", value.getCssName());
		}
	}
	
	
	// sets the font-size style property of this element by using the GWT DOM class
	@Override
	public void setFontSize(Widget widget, PositiveNumUnitValue value, String styleClass) throws InvalidGdlSchemaException,	ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "fontSize", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "fontSize", value.getCssValue());
		}
	}
	
	
	// sets the font-weight style property of this element by using the GWT DOM class
	@Override
	public void setFontWeight(Widget widget, FontWeightValue value,	String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "fontWeight", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "fontWeight", value.getCssValue());
		}
	}
	
	
	// sets the letter-spacing style property of this element by using the GWT DOM class
	@Override
	public void setLetterSpacing(Widget widget, NormalNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "letterSpacing", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "letterSpacing", value.getCssValue());
		}
	}
	
	// sets the word-spacing style property of this element by using the GWT DOM class
	@Override
	public void setWordSpacing(Widget widget, NormalNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "wordSpacing", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "wordSpacing", value.getCssValue());
		}
	}
		
	
	// sets the width style property of this element by using the GWT DOM class
	public void setWidth(Widget widget, AutoNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "width", value.getCssValue()); // TODO: fix
			}
			this.addStyleToStore(styleClass, "width", value.getCssValue());
			super.setCssProperty(widget, styleClass, "width", value.getCssValue());
		}
	}


	// sets the min-width style property of this element by using the GWT DOM class
	public void setMinWidth(Widget widget, AutoNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "minWidth", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "minWidth", value.getCssValue());
			super.setCssProperty(widget, styleClass, "minWidth", value.getCssValue());
		}
	}


	// sets the max-width style property of this element by using the GWT DOM class
	public void setMaxWidth(Widget widget, AutoNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "maxWidth", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "maxWidth", value.getCssValue());
			super.setCssProperty(widget, styleClass, "maxWidth", value.getCssValue());
		}
	}


	// sets the height style property of this element by using the GWT DOM class
	public void setHeight(Widget widget, AutoNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "height", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "height", value.getCssValue());
			super.setCssProperty(widget, styleClass, "height", value.getCssValue());
		}
	}


	// sets the min-height style property of this element by using the GWT DOM class
	public void setMinHeight(Widget widget, AutoNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "minHeight", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "minHeight", value.getCssValue());
			super.setCssProperty(widget, styleClass, "minHeight", value.getCssValue());
		}
	}


	// sets the max-height style property of this element by using the GWT DOM class
	public void setMaxHeight(Widget widget, AutoNumUnitValue value, String styleClass) throws InvalidGdlSchemaException, ExecutionException {
		if(value != null && widget.getClass().equals(TitleWidget.class)) {
			if(styleClass == null){
				DOM.setStyleAttribute(((TitleWidget)widget).getTitleElement(), "maxHeight", value.getCssValue());
			}
			this.addStyleToStore(styleClass, "maxHeight", value.getCssValue());
			super.setCssProperty(widget, styleClass, "maxHeight", value.getCssValue());
		}
	}
	
	
	// some handler for applying the css style bound to the pseudo classes hover, active and focus
	@Override
	public void onHoverStart(MouseOverEvent event, HoverStyleHandler handler) {
		Widget source = (Widget)event.getSource();
		GdlTitle.super.onHoverStart(event, handler);
		if(source.getClass().equals(TitleWidget.class)){
			for (Pair<String, String> pair : this.titleHoverCssNamesAndStyles) {
				DOM.setStyleAttribute(((TitleWidget)source).titleElement, pair.getFirst(), pair.getSecond());
			}
		}
	}
	
	
	// applies the styles bound to hover and the passed element
	@Override
	protected void onHoverStart(Widget widget){
		GdlTitle.super.onHoverStart(widget);
		if(widget.getClass().equals(TitleWidget.class)){
			for (Pair<String, String> pair : this.titleHoverCssNamesAndStyles) {
				DOM.setStyleAttribute(((TitleWidget)widget).titleElement, pair.getFirst(), pair.getSecond());
			}
		}
	}

	
	// applies the styles bound to focus and the passed element
	protected void onFocusStart(Widget widget){
		GdlTitle.super.onFocusStart(widget);
		if(widget.getClass().equals(TitleWidget.class)){
			for (Pair<String, String> pair : this.titleFocusCssNamesAndStyles) {
				DOM.setStyleAttribute(((TitleWidget)widget).titleElement, pair.getFirst(), pair.getSecond());
			}
		}
	}
	
	
	// shall be called to apply the styles of the focus class
	public void onFocusStart(FocusEvent event, FocusStyleHandler handler) {
		Widget source = (Widget)event.getSource();
		GdlTitle.super.onFocusStart(event, handler);
		if(source.getClass().equals(TitleWidget.class)){
			for (Pair<String, String> pair : this.titleFocusCssNamesAndStyles) {
				DOM.setStyleAttribute(((TitleWidget)source).titleElement, pair.getFirst(), pair.getSecond());
			}
		}
	}
	
	
	// applies the styles bound to acitve and the passed element
	protected void onActiveStart(Widget widget){
		GdlTitle.super.onActiveStart(widget);
		if(widget.getClass().equals(TitleWidget.class)){
			for (Pair<String, String> pair : this.titleActiveCssNamesAndStyles) {
				DOM.setStyleAttribute(((TitleWidget)widget).titleElement, pair.getFirst(), pair.getSecond());
			}
		}
	}
	
	
	// shall be called to apply the styles of the active class
	public void onActiveStart(MouseDownEvent event, ActiveStyleHandler handler) {
		Widget source = (Widget)event.getSource();
		GdlTitle.super.onActiveStart(event, handler);
		if(source.getClass().equals(TitleWidget.class)){
			for (Pair<String, String> pair : this.titleActiveCssNamesAndStyles) {
				DOM.setStyleAttribute(((TitleWidget)source).titleElement, pair.getFirst(), pair.getSecond());
			}
		}
	}
	
	
	@Override
	@Deprecated
	public ArrayList<String> getSelectedValues(){
		// TODO: implement
		return new ArrayList<String>();
	}
	
	
	@Override
	@Deprecated
	public void fixValue(){
		// has no effect on this element
	}
	
	
	// this class represents the acutal content of this widget, i.e.
	// it wraps a h1, h2, h3 or h4 element
	protected class TitleWidget extends Composite implements HasMouseDownHandlers, HasMouseUpHandlers, HasMouseOverHandlers, HasMouseOutHandlers, HasBlurHandlers, HasFocusHandlers{
		private AbsolutePanel basePanel = new AbsolutePanel();
		private Element titleElement = null;
		
		
		// some constructurs
		public TitleWidget()throws ExecutionException{
			initWidget(this.basePanel);
			this.initTitleElement(1);
		}
		
		
		
		public Element getTitleElement(){
			return this.titleElement;
		}
		
		
		public TitleWidget(int titleOrder) throws ExecutionException{
			if(titleOrder <= 0 || titleOrder > 4) throw new ExecutionException("The title order of a GDL Title element must be 1, 2, 3 or 4");
			initWidget(this.basePanel);
			this.initTitleElement(titleOrder);
		}
		
		
		// creates and returns the actual title element
		private void initTitleElement(int titleOrder) throws ExecutionException {
			switch(titleOrder){
			case 1: this.titleElement = DOM.createElement("h1");
				break;
			case 2: this.titleElement = DOM.createElement("h2");
				break;
			case 3: this.titleElement = DOM.createElement("h3");
				break;
			case 4: this.titleElement = DOM.createElement("h4");
				break;
			default: throw new ExecutionException("The title order of a GDL Title element must be 1, 2, 3 or 4");
			}
			this.basePanel.getElement().insertFirst(this.titleElement);
			DOM.setStyleAttribute(this.titleElement, "margin", "0px");
		}
		
		
		// inserts the passed string as content to the heading
		public void setText(String text){
			if(text == null) return;
			this.titleElement.setInnerText(text);
		}


		public void setId(String id){
			DOM.setElementAttribute(this.basePanel.getElement(), "id", id);
		}


		@Override
		public HandlerRegistration addMouseOverHandler(MouseOverHandler handler) {
			return this.addDomHandler(handler, MouseOverEvent.getType());
		}


		@Override
		public HandlerRegistration addMouseOutHandler(MouseOutHandler handler) {
			return this.addDomHandler(handler, MouseOutEvent.getType());
		}


		@Override
		public HandlerRegistration addBlurHandler(BlurHandler handler) {
			return this.addDomHandler(handler, BlurEvent.getType());
		}


		@Override
		public HandlerRegistration addFocusHandler(FocusHandler handler) {
			return this.addDomHandler(handler, FocusEvent.getType());
		}


		@Override
		public HandlerRegistration addMouseUpHandler(MouseUpHandler handler) {
			return this.addDomHandler(handler, MouseUpEvent.getType());
		}


		@Override
		public HandlerRegistration addMouseDownHandler(MouseDownHandler handler) {
			return this.addDomHandler(handler, MouseDownEvent.getType());
		}
	}
}
