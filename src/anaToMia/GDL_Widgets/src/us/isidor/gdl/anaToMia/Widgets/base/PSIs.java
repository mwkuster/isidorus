package us.isidor.gdl.anaToMia.Widgets.base;



public class PSIs {	
	// some psis of the TMDM
	public class TMDM{
		public final static String tmdm = "http://psi.topicmaps.org/iso13250/model/";
		public final static String supertype = tmdm + "supertype";
		public final static String subtype = tmdm + "subtype";
		public final static String supertypeSubtype = tmdm + "supertype-subtype";
	}

	
	// some psis of the TMCL
	public class TMCL {
		public final static String tmcl = "http://psi.topicmaps.org/tmcl/";
		public final static String tmclTopictype = tmcl + "topic-type";
		public final static String tmclNameType = tmcl + "name-type";
		public final static String tmclOccurrenceType = tmcl + "occurrence-type";
		public final static String tmclAssociationType = tmcl + "association-type";
		public final static String tmclRoleType = tmcl + "role-type";
		public final static String tmclConstraint = tmcl + "constraint";
		public final static String tmclConstrained = tmcl + "constrained";
		public final static String tmclConstrainedStatement = tmcl + "constrained-statement";
		public final static String tmclRegularExpressionConstraint = tmcl + "regular-expression-constraint";
		public final static String tmclRegexp = tmcl + "regexp";
		public final static String tmclCardMin = tmcl + "card-min";
		public final static String tmclCardMax = tmcl + "card-max";
		public final static String tmclReifierConstraint = tmcl + "reifier-constraint";
		public final static String tmclScopeConstraint = tmcl + "scope-constraint";
		public final static String tmclTopicNameConstraint = tmcl + "topic-name-constraint";
		public final static String tmclVariantNameConstraint = tmcl + "variant-name-constraint";
		public final static String tmclTopicOccurrenceConstraint = tmcl + "topic-occurrence-constraint";
		public final static String tmclTopicRoleConstraint = tmcl + "topic-role-constraint";
		public final static String tmclAssociationRoleConstraint = tmcl + "association-role-constraint";
		public final static String tmclAllows = tmcl + "allows";
		public final static String tmclAllowed = tmcl + "allowed";
		public final static String tmclAllowedReifier = tmcl + "allowed-reifier";
		public final static String tmclConstrainedScope = tmcl + "constrained-scope";
		public final static String tmclSubjectIdentifierConstraint = tmcl + "subject-identifier-constraint";
		public final static String tmclSubjectLocatorConstraint = tmcl + "subject-locator-constraint";
		public final static String tmclItemIdentifierConstraint = tmcl + "item-identifier-constraint";
		public final static String tmclConstrainedTopicType = tmcl + "constrained-topic-type";
		public final static String tmclConstrainedRole = tmcl + "constrained-role";
		public final static String tmclRoleCombinationConstraint = tmcl + "role-combination-constraint";
		public final static String tmclOtherConstrainedTopicType = tmcl + "other-constrained-topic-type";
		public final static String tmclOtherConstrainedRole = tmcl + "other-constrained-role"; 
	}
	
	
	// all psis of the GDL
	public class GDL {
		public final static String gdl = "http://psi.isidor.us/gdl/";
		
		// topic types
		public class TopicType{
			public final static String gdlt = gdl + "types/";
			public final static String gdlSchema = gdlt + "Schema";
			public final static String gdlView = gdlt + "View";
			public final static String gdlAssociationView = gdlt + "Association-View";
			public final static String gdlCreatorAssociationview = gdlt + "Creator-Association-View";
			public final static String gdlEditorAssociationView = gdlt + "Editor-Association-View";
			public final static String gdlTopicView = gdlt + "Topic-View";
			public final static String gdlDefaultTopicView = gdlt + "Default-Topic-View";
			public final static String gdlDefaultCreatorTopicView = gdlt + "Default-Creator-Topic-View";
			public final static String gdlDefaultEditorTopicView = gdlt + "Default-Editor-Topic-View";
			public final static String gdlSpecialTopicView = gdlt + "Special-Topic-View";
			public final static String gdlSpecialCreatorTopicView = gdlt + "Special-Creator-TopicView";
			public final static String gdlSpecialEditorTopicView = gdlt + "SpecialEditorTopicView";
			public final static String gdlVisibleObject = gdlt + "Visible-Object";
			public final static String gdlStyleClass = gdlt + "Style-Class";
			public final static String gdlDescriptor = gdlt + "Descriptor";
			public final static String gdlSpace = gdlt + "Space";
			public final static String gdlList = gdlt + "List";
			public final static String gdlPosition = gdlt + "Position";
			public final static String gdlNthElement = gdlt + "Nth-Element";
			public final static String gdlValue = gdlt + "Value";
			public final static String gdlDefaultValue = gdlt + "Default-Value";
			public final static String gdlLiteralValue = gdlt + "Literal-Value";
			public final static String gdlDefaultLiteralValue = gdlt + "Default-Literal-Value";
			public final static String gdlTmValue = gdlt + "TM-Value";
			public final static String gdlTmTypeValue = gdlt + "TM-Type-Value";
			public final static String gdlTmSingleTypeValue = gdlt + "TM-Single-Type-Value";
			public final static String gdlTmMultipleTypeValue = gdlt + "TM-Multiple-Type-Value";
			public final static String gdlTmInstanceValue = gdlt + "TM-Instance-Value";
			public final static String gdlDefaultTmValue = gdlt + "Default-TM-Value";
			public final static String gdlDefaultTmTypeValue = gdlt + "Default-TM-Type-Value";
			public final static String gdlDefaultTmSingleTypeValue = gdlt + "Default-TM-Single-Type-Value";
			public final static String gdlDefaultTmMultipleTypeValue = gdlt + "Default-TM-Multiple-Type-Value";
			public final static String gdlDefaultTmInstanceValue = gdlt + "Default-TM-Instance-Value";
			public final static String gdlDatatype = gdlt + "Datatype";
			public final static String gdlType = gdlt + "Type";
			public final static String gdlInfo = gdlt + "Info";
			public final static String gdlVariantNameScope = gdlt + "Variant-Name-Scope";
			public final static String gdlVariantNameReifier = gdlt + "Variant-Name-Reifier";
			public final static String gdlVariantNameIdentifiers = gdlt + "Variant-Name-Identifiers";
			public final static String gdlRolePlayer = gdlt + "Role-Player";
			public final static String gdlAssociationRole = gdlt + "Association-Role";
			public final static String gdlHiddenValue = gdlt + "Hidden-Value";
			public final static String gdlValueGroup = gdlt + "Value-Group";
			public final static String gdlTextObject = gdlt + "Text-Object";
			public final static String gdlTitle = gdlt + "Title";
			public final static String gdlReference = gdlt + "Reference";
			public final static String gdlListBox = gdlt + "List-Box";
			public final static String gdlCompleData = gdlt + "Complex-Data";
			public final static String gdlImage = gdlt + "Image";
			public final static String gdlAudio = gdlt + "Audio";
			public final static String gdlVideo = gdlt + "Video";
			public final static String gdlTimePicker = gdlt + "Time-Picker";
			public final static String gdlDatePicker = gdlt + "Date-Picker";
			public final static String gdlDateTimePicker = gdlt + "Date-Time-Picker";
			public final static String gdlText = gdlt + "Text";
			public final static String gdlUnit = gdlt + "Unit";
			public final static String gdlButton = gdlt + "Button";
			public final static String gdlInputbutton = gdlt + "Input-Button";
			public final static String gdlRadioButton = gdlt + "Radio-Button";
			public final static String gdlCheckBox = gdlt + "Check-Box";
			public final static String gdlActionButton = gdlt + "Action-Button";
			public final static String gdlValidateButton = gdlt + "Validate-Button";
			public final static String gdlCreateButton = gdlt + "Create-Button";
			public final static String gdlDeleteButton = gdlt + "Delete-Button";
			public final static String gdlCommitButton = gdlt + "Commit-Button";
		}
		
		
		// name types
		public class NameType{
			public final static String gdlViewName = gdl + "view-name";
			public final static String gdlSchemaName = gdl + "schema-name";
		}
		
		
		// occurrence types
		public class OccurrenceType {
			public final static String gdlVerticalAlign = gdl + "vertical-align";
			public final static String gdlDisplay = gdl + "display";
			public final static String gdlId = gdl + "id";
			public final static String gdlMargin = gdl + "margin";
			public final static String gdlMarginTop = gdl + "margin-top";
			public final static String gdlMarginRight = gdl + "margin-right";
			public final static String gdlMarginBottom = gdl + "margin-bottom";
			public final static String gdlMarginLeft = gdl + "margin-left"; 
			public final static String gdlBorderColor = gdl + "border-color";
			public final static String gdlBorderTopColor = gdl + "border-top-color";
			public final static String gdlBorderRightColor = gdl + "border-right-color";
			public final static String gdlBorderBottomColor = gdl + "border-bottom-color";
			public final static String gdlBorderLeftColor = gdl + "border-left-color"; 
			public final static String gdlBorderStyle = gdl + "border-style";
			public final static String gdlBorderTopStyle = gdl + "border-top-style";
			public final static String gdlBorderRightStyle = gdl + "border-right-style";
			public final static String gdlBorderBottomStyle = gdl + "border-bottom-style";
			public final static String gdlBorderLeftStyle = gdl + "border-left-style";
			public final static String gdlBorderWidth = gdl + "border-width";
			public final static String gdlBorderTopWidth = gdl + "border-top-width";
			public final static String gdlBorderRightWidth = gdl + "border-right-width";
			public final static String gdlBorderBottomWidth = gdl + "border-bottom-width";
			public final static String gdlBorderLeftWidth = gdl + "border-left-width"; 
			public final static String gdlBorderRadius = gdl + "border-radius"; 
			public final static String gdlBorderTopRightRadius = gdl + "border-top-right-radius";
			public final static String gdlBorderBottomRightRadius = gdl + "border-bottom-right-radius";
			public final static String gdlBorderBottomLeftRadius = gdl + "border-bottom-left-radius";
			public final static String gdlBorderTopLeftRadius = gdl + "border-top-left-radius";
			public final static String gdlCursor = gdl + "cursor";
			public final static String gdlZindex = gdl + "z-index";
			public final static String gdlWidth = gdl + "width";
			public final static String gdlMinWidth = gdl + "min-width";
			public final static String gdlMaxWidth = gdl + "max-width";
			public final static String gdlHeight = gdl + "height";
			public final static String gdlMinHeight = gdl + "min-height";
			public final static String gdlMaxHeight = gdl + "max-height";
			public final static String gdlPadding = gdl + "padding";
			public final static String gdlPaddingTop = gdl + "padding-top";
			public final static String gdlPaddingRight = gdl + "padding-right";
			public final static String gdlPaddingBottom = gdl + "padding-bottom";
			public final static String gdlPaddingLeft = gdl + "padding-left";
			public final static String gdlClear = gdl + "clear";
			public final static String gdlFloat = gdl + "float";
			public final static String gdlBackgroundColor = gdl + "background-color";
			public final static String gdlOrdered = gdl + "ordered";
			public final static String gdlListStyleType = gdl + "list-style-type";
			public final static String gdlListStylePosition = gdl + "list-style-position";
			public final static String gdlPositionStyle = gdl + "position-style";
			public final static String gdlNthValue = gdl + "nth-value";
			public final static String gdlTop = gdl + "top";
			public final static String gdlRight = gdl + "right";
			public final static String gdlBottom = gdl + "bottom";
			public final static String gdlLeft = gdl + "left";
			public final static String gdlFixed = gdl + "fixed";
			public final static String gdlLiteralValue = gdl + "literal-value";
			public final static String gdlDirection = gdl + "direction";
			public final static String gdlTextAlign = gdl + "text-align";
			public final static String gdlLineHeight = gdl + "line-height";
			public final static String gdlTextDecoration = gdl + "text-decoration";
			public final static String gdlColor = gdl + "color";
			public final static String gdlFontfamily = gdl + "font-family";
			public final static String gdlFontStyle = gdl + "font-style";
			public final static String gdlFontSize = gdl + "font-size";
			public final static String gdlFontWeight = gdl + "font-weight";
			public final static String gdlLetterSpacing = gdl + "letter-spacing";
			public final static String gdlWordSpacing = gdl + "word-spacing";
			public final static String gdlMultiple = gdl + "multiple";
			public final static String gdlSize = gdl + "size";
			public final static String gdlOnePerGroup = gdl + "one-per-group";
			public final static String gdlTextType = gdl + "text-type";
			public final static String gdlReadonly = gdl + "readonly";
			public final static String gdlRows = gdl + "rows";
			public final static String gdlCols = gdl + "cols";
			public final static String gdlResize = gdl + "resize";
			public final static String gdlTitleOrder = gdl + "title-order";
			public final static String gdlUnitName = gdl + "unit-name";
			public final static String gdlContentOrientation = gdl + "content-orientation";
		}
		
		
		// association types
		public class AssociationType {
			public final static String gdlContains = gdl + "contains";
			public final static String gdlPosition = gdl + "position";
			public final static String gdlNthPosition = gdl + "nth-position";
			public final static String gdlButtonPosition = gdl + "button-position";
			public final static String gdlTmBinding = gdl + "tm-binding";
			public final static String gdlViewBinding = gdl + "view-binding";
			public final static String gdlTopicViewBinding = gdl + "topic-view-binding";
			public final static String gdlAssociationViewBinding = gdl + "association-view-binding";
			public final static String gdlValueBinding = gdl + "value-binding";
			public final static String gdlDisplayBy = gdl + "display-by";
			public final static String gdlPrefferedScope = gdl + "preferred-scope";
		}
		
		
		// role types
		public class RoleType {
			public final static String gdlContainer = gdl + "container";
			public final static String gdlContainee = gdl + "containee";
			public final static String gdlTmConstruct = gdl + "tm-construct";
			public final static String gdlDescriptor = gdl + "descriptor";
			public final static String gdlDescendant = gdl + "descendant";
			public final static String gdlAncestor = gdl + "ancestor";
			public final static String gdlNthElement = gdl + "nth-element";
			public final static String gdlHiddenValue = gdl + "hidden-value";
			public final static String gdlValueGroup = gdl + "value-group";
			public final static String gdlTmValue = gdl + "tm-value";
			public final static String gdlActionButton = gdl + "action-button";
			public final static String gdlValue = gdl + "value";
		}
		
		
		// themes (topic instances)
		public class Scope {
			public final static String gdlHover = gdl + "hover";
			public final static String gdlFocus = gdl + "focus";
			public final static String gdlActive = gdl + "active";
		}
	}
}
