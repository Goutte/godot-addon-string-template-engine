@tool
extends EditorSyntaxHighlighter
## A syntax highlighter for string templates.
class_name StringTemplateSyntaxHighlighter

## We need this dependency to grab colors from the editor theme.
## If instead we instantiate a new EditorSettings, we get the Godot2 theme.
var editor_settings: EditorSettings

## Local cache of the highlights, which is fully invalidated on each keystroke.
## We invalidate this cache too much; there's room for improvements.
var lines_highlights := []


func _create() -> EditorSyntaxHighlighter:
	var highlighter := StringTemplateSyntaxHighlighter.new()
	highlighter.editor_settings = editor_settings
	return highlighter

func _get_name() -> String:
	return tr("String Template")

## This method is badly named ; it returns the supported **extensions**.
func _get_supported_languages() -> PackedStringArray:
	return ['tpl']

## The Code Editor is going to call this method a lot; for each line.
func _get_line_syntax_highlighting(line: int) -> Dictionary:
	return lines_highlights[line]

## This is called multiple times (7) when the file is opened. (!?)
## This is not called when the text changes, but it is called on save.
func _update_cache() -> void:
	# NOTE: `text_changed` is emitted AFTER _get_line_syntax_highlighting()
	# Hence we use the `lines_edited_from` signal, which is emitted before.
	if not get_text_edit().lines_edited_from.is_connected(on_lines_edited):
		get_text_edit().lines_edited_from.connect(on_lines_edited)
	collect_colors()
	recompute_highlighting()

## This is called BEFORE _get_line_syntax_highlighting, yay!
func on_lines_edited(_from_line: int, _to_line: int) -> void:
	recompute_highlighting()


## Names of the colors in the Editor Settings that we're using.
## We're looking at the setting "text_editor/theme/highlighting/…"
var editor_colors_to_collect := [
	&'text_color',
	&'comment_color',
	&'symbol_color',
	&'string_color',
	&'number_color',
	&'function_color',
	&'keyword_color',
	&'control_flow_keyword_color',
	&'base_type_color',
	&'engine_type_color',
	&'member_variable_color',
]
# The default values defined below are never used in practice, since we load
# the editor color values in these variables ; see collect_colors()
var text_color := Color(0.803, 0.81, 0.822, 1.0)
var comment_color := Color(0.804, 0.812, 0.824, 0.502)
var symbol_color := Color(0.67, 0.79, 1.0, 1.0)
var string_color := Color(1.0, 0.93, 0.63, 1.0)
var number_color := Color(0.631, 1.0, 0.878, 1.0)
var function_color := Color(0.341, 0.702, 1.0, 1.0)
var keyword_color := Color(1.0, 0.44, 0.52, 1.0)
var control_flow_keyword_color := Color(1.0, 0.549, 0.8, 1.0)
var base_type_color := Color(0.259, 1.0, 0.761, 1.0)
var engine_type_color := Color(0.56, 1.0, 0.86, 1.0)
var member_variable_color := Color(0.736, 0.88, 1.0, 1.0)


func collect_colors() -> void:
	if not editor_settings:
		# NOTE: a new instance will load the Godot2 theme by default
		editor_settings = EditorSettings.new()
	for color_name in editor_colors_to_collect:
		set(
			color_name,
			editor_settings.get_setting(
				"text_editor/theme/highlighting/%s" % color_name
			)
		)

func recompute_highlighting() -> void:
	var text := get_text_edit().get_text()
	
	lines_highlights.clear()
	for line in text.count("\n") + 1:
		lines_highlights.append({})
	
	var tokenizer := StringEngine.Tokenizer.new()
	tokenizer.break_on_error = false
	var tokens := tokenizer.tokenize(text)
	
	for token in tokens:
		var starts_at_line := token.starts_in_source_at_line - 1
		var line_starts_at := text.rfind("\n", token.starts_in_source_at-1) + 1
		var starts_in_line_at := token.starts_in_source_at - line_starts_at
		var ends_at_line := token.ends_in_source_at_line - 1
		#var end_line_starts_at := text.rfind("\n", token.ends_in_source_at-1) + 1
		#var ends_in_line_at := token.ends_in_source_at - end_line_starts_at
		
		var color := text_color
		match token.type:
			StringEngine.Token.Types.RAW_DATA:
				color = text_color
			StringEngine.Token.Types.PRINT_OPENER:
				color = base_type_color
			StringEngine.Token.Types.PRINT_CLOSER:
				color = base_type_color
			StringEngine.Token.Types.STATEMENT_OPENER:
				color = engine_type_color
			StringEngine.Token.Types.STATEMENT_CLOSER:
				color = engine_type_color
			StringEngine.Token.Types.STATEMENT_IDENTIFIER:
				color = control_flow_keyword_color
			StringEngine.Token.Types.STATEMENT_ASSIGN:
				color = control_flow_keyword_color
			StringEngine.Token.Types.COMMENT_OPENER:
				color = comment_color
			StringEngine.Token.Types.COMMENT_CONTENT:
				color = comment_color
			StringEngine.Token.Types.COMMENT_CLOSER:
				color = comment_color
			StringEngine.Token.Types.OPERATOR_ADDITION:
				color = symbol_color
			StringEngine.Token.Types.OPERATOR_SUBTRACTION:
				color = symbol_color
			StringEngine.Token.Types.OPERATOR_MULTIPLICATION:
				color = symbol_color
			StringEngine.Token.Types.OPERATOR_DIVISION:
				color = symbol_color
			StringEngine.Token.Types.OPERATOR_CONCATENATION:
				color = symbol_color
			StringEngine.Token.Types.OPERATOR_MODULO:
				color = symbol_color
			StringEngine.Token.Types.COMPARATOR_EQUAL:
				color = symbol_color
			StringEngine.Token.Types.COMPARATOR_INEQUAL:
				color = symbol_color
			StringEngine.Token.Types.COMPARATOR_LESS:
				color = symbol_color
			StringEngine.Token.Types.COMPARATOR_LESS_EQUAL:
				color = symbol_color
			StringEngine.Token.Types.COMPARATOR_GREATER:
				color = symbol_color
			StringEngine.Token.Types.COMPARATOR_GREATER_EQUAL:
				color = symbol_color
			StringEngine.Token.Types.COMBINATOR_AND:
				color = keyword_color
			StringEngine.Token.Types.COMBINATOR_NAND:
				color = keyword_color
			StringEngine.Token.Types.COMBINATOR_OR:
				color = keyword_color
			StringEngine.Token.Types.COMBINATOR_XOR:
				color = keyword_color
			StringEngine.Token.Types.INFIX_IN:
				color = keyword_color
			StringEngine.Token.Types.EXPRESSION_GROUP_OPENER:
				color = symbol_color
			StringEngine.Token.Types.EXPRESSION_GROUP_CLOSER:
				color = symbol_color
			StringEngine.Token.Types.EXPRESSIONS_SEPARATOR:
				color = symbol_color
			StringEngine.Token.Types.ACCESSOR_PROPERTY:
				color = symbol_color
			StringEngine.Token.Types.FILTER:
				color = symbol_color
			StringEngine.Token.Types.FILTER_IDENTIFIER:
				color = function_color
			StringEngine.Token.Types.LITERAL_IDENTIFIER:
				color = member_variable_color
			StringEngine.Token.Types.LITERAL_STRING:
				color = string_color
			StringEngine.Token.Types.LITERAL_INTEGER:
				color = number_color
			StringEngine.Token.Types.LITERAL_FLOAT:
				color = number_color
			StringEngine.Token.Types.LITERAL_BOOLEAN_TRUE:
				color = keyword_color
			StringEngine.Token.Types.LITERAL_BOOLEAN_FALSE:
				color = keyword_color
		
		lines_highlights[starts_at_line][starts_in_line_at] = {
			'color': color,
		}
		
		# Handle colorization of multiline tokens
		if token.type != StringEngine.Token.Types.RAW_DATA:
			for i in range(ends_at_line - starts_at_line):
				lines_highlights[starts_at_line + i][0] = {
					'color': color,
				}
