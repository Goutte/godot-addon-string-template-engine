## This tells Godot that the .tpl extension is legit and recognized.
## Apparently we don't need to override a lot for this to work.
## Godot will open *.tpl files in the script editor, and then our highlighter
## does the rest of the heavy lifting.
## NOTE: this needs to be a global class (Godot auto-detects loaders)
@tool
class_name StringTemplateFormatLoader
extends ResourceFormatLoader

func _get_recognized_extensions():
	return ['tpl']

func _load(path: String, original_path: String, use_sub_threads: bool, cache_mode: int) -> Variant:
	var content := FileAccess.get_file_as_string(path)
	#var s := Script.new()
	#s.source_code = content
	print("WHAT?! "+path)
	#return content
	
	var r := StringTemplateResource.new()
	r.source = content
	return r
