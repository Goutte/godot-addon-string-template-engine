## This tells Godot that the .tpl extension is legit and recognized.
## Apparently we don't need to override a lot for this to work.
## Godot will open *.tpl files in the script editor, and then our highlighter
## does the rest of the heavy lifting.
## NOTE: this needs to be a global class (Godot auto-detects loaders)
@tool
class_name StringTemplateFormatLoader
#extends ResourceFormatLoader

func _get_recognized_extensions():
	return ['tpl']

func _handles_type(type: StringName) -> bool:
	return type == &'StringTemplateResource'

func _get_resource_type(path: String) -> String:
	if path.get_extension() == 'tpl':
		return 'StringTemplateResource'
	return ''

func _load(path: String, original_path: String, use_sub_threads: bool, cache_mode: int) -> Variant:
	print("Custom TPL loading of " + path)
	var content := FileAccess.get_file_as_string(path)
	
	# ERROR: Script is abstract
	#var s := Script.new()
	#s.source_code = content
	#return content
	
	# ERROR: Condition "res.is_null()" is true. Returning: ERR_CANT_OPEN
	#return content
	
	#var se := StringTemplateScript.new()
	#se.source_code = content
	#return se
	
	var r := StringTemplateResource.new()
	r.text = content
	return r
