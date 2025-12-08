@tool
extends EditorPlugin

# This editor plugin : 
# - Adds 'tpl' as a TextFile extension so that template files are recognized.
# - Registers our string template syntax highlighter.
# 
# The StringEngine itself is a global class you can readily use.


const string_list_separator := ','
const editor_override_prefix := "editor_overrides"
const textfiles_editor_setting := "docks/filesystem/textfile_extensions"


var stsh: StringTemplateSyntaxHighlighter


func _enable_plugin() -> void:
	add_textfile_extension('tpl')


func _disable_plugin() -> void:
	remove_textfile_extension('tpl')


func _enter_tree() -> void:
	stsh = StringTemplateSyntaxHighlighter.new()
	EditorInterface.get_script_editor().register_syntax_highlighter(stsh)


func _exit_tree() -> void:
	if is_instance_valid(stsh):
		EditorInterface.get_script_editor().unregister_syntax_highlighter(stsh)
	stsh = null


## Adds the provided extension to the list of textfile extensions supported by
## the editor, for this project only, using an editor setting override.
func add_textfile_extension(extension: String) -> Error:
	if extension.contains(string_list_separator):
		return ERR_INVALID_PARAMETER
	
	var editor_settings := EditorInterface.get_editor_settings()
	var textfiles_project_setting := "%s/%s" % [
		editor_override_prefix,
		textfiles_editor_setting,
	]
	
	var value := ""
	if ProjectSettings.has_setting(textfiles_project_setting):
		value = ProjectSettings.get_setting(textfiles_project_setting)
	if not value:
		if editor_settings.has_setting(textfiles_editor_setting):
			value = editor_settings.get_setting(textfiles_editor_setting)
	
	var extensions := value.split(string_list_separator)
	if not extensions.has(extension):
		extensions.append(extension)
	
	ProjectSettings.set_setting(
		textfiles_project_setting,
		string_list_separator.join(extensions),
	)
	
	return OK


## Removes the provided extension to the list of textfile extensions supported
## by the editor, for this project only, in the editor setting override.
## NOTE: this does not remove the override itself (setting it to null),
## because I have found no way to get the original editor setting value.
func remove_textfile_extension(extension: String) -> Error:
	if extension.contains(string_list_separator):
		return ERR_INVALID_PARAMETER
	
	var textfiles_project_setting := "%s/%s" % [
		editor_override_prefix,
		textfiles_editor_setting,
	]
	
	if not ProjectSettings.has_setting(textfiles_project_setting):
		return OK
	
	var value: String = ProjectSettings.get_setting(textfiles_project_setting)
	
	var extensions := value.split(string_list_separator)
	if extensions.has(extension):
		extensions.erase(extension)
	
	ProjectSettings.set_setting(
		textfiles_project_setting,
		string_list_separator.join(extensions),
	)
	
	return OK
