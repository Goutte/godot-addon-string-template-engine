@tool
extends EditorPlugin


var stsh: StringTemplateSyntaxHighlighter


func _enable_plugin() -> void:
	# Add autoloads here.
	pass


func _disable_plugin() -> void:
	# Remove autoloads here.
	pass


func _enter_tree() -> void:
	stsh = StringTemplateSyntaxHighlighter.new()
	EditorInterface.get_script_editor().register_syntax_highlighter(stsh)


func _exit_tree() -> void:
	if is_instance_valid(stsh):
		EditorInterface.get_script_editor().unregister_syntax_highlighter(stsh)
	stsh = null
