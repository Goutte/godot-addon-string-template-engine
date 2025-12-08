@tool
class_name StringTemplateFormatSaver
#extends ResourceFormatSaver


## Returns the list of extensions available for saving the resource object,
## provided it is recognized (see _recognize()).
func _get_recognized_extensions(_resource: Resource) -> PackedStringArray:
	return ['tpl']
	#if resource is StringTemplateResource:
		#return ['tpl']
	#return []


## Returns whether the given resource object can be saved by this saver.
func _recognize(resource: Resource) -> bool:
	return resource is StringTemplateResource


## Returns true if this saver handles a given save path and false otherwise.
## If this method is not implemented, the default behavior returns whether the
## path's extension is within the ones provided by _get_recognized_extensions().
#func _recognize_path(_resource: Resource, path: String) -> bool:
#	return path.get_extension() == 'tpl'


## Saves the given resource object to a file at the target path.
## flags is a bitmask composed with ResourceSaver.SaverFlags constants.
## Returns OK on success, or an Error constant in case of failure.
func _save(resource: Resource, path: String, _flags: int) -> Error:
	var tpl := resource as StringTemplateResource
	if not tpl:
		return ERR_CANT_ACQUIRE_RESOURCE
	
	var file = FileAccess.open(path, FileAccess.WRITE)
	file.store_string(tpl.text)
	
	return OK


#func Error _set_uid(path: String, uid: int) virtual
#Sets a new UID for the resource at the given path. Returns OK on success, or an Error constant in case of failure.
