@tool
class_name StringTemplateSyntaxHighlighter
extends EditorSyntaxHighlighter

func _get_name() -> String:
	return "String Template"

func _get_supported_languages() -> PackedStringArray:
	return ["tpl"]

func _get_line_syntax_highlighting(_line: int) -> Dictionary:
	# FIXME
	return {
		0: {
			"color": Color.CHARTREUSE,
		},
		3: {
			"color": Color.DARK_ORANGE,
		},
		6: {
			"color": Color.MAGENTA,
		},
	}
