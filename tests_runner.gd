extends Node
class_name TestsRunner

@export var print_to_console := true


signal test_started(test_name: String, test_filepath: String)
signal test_ended(test_name: String, test_filepath: String)
signal logged(message: String)


func _ready() -> void:
	run_tests()


func run_tests() -> void:
	info("Running tests…")
	run_tests_directory("res://tests")
	info("Done")


func run_tests_directory(dirpath: String) -> void:
	for filepath: String in DirAccess.get_files_at(dirpath):
		if not filepath.ends_with(".gd"):
			continue
		run_test_file(dirpath + '/' + filepath)


func run_test_file(filepath: String) -> void:
	var script: GDScript = ResourceLoader.load(filepath) as GDScript
	assert(script, "Cannot load test file at %s" % filepath)
	var method_list := script.get_script_method_list()
	for method: Dictionary in method_list:
		if not is_method_a_test(method):
			continue
		run_test_method(method, script)


# method structure example:
# { "name": "test_variables", "args": [], "default_args": [], "flags": 1, "id": 0, "return": { "name": "", "class_name": &"", "type": 0, "hint": 0, "hint_string": "", "usage": 6 } }
func run_test_method(method: Dictionary, script: GDScript) -> void:
	var filepath := script.resource_path
	var script_instance: Object = script.new() as Object
	
	info("Running %s…" % [method['name']])
	test_started.emit(method['name'], filepath)
	var started_at_usec := Time.get_ticks_usec()
	script_instance.call(method['name'])  # may throw yet we can't catch
	var ended_at_usec := Time.get_ticks_usec()
	test_ended.emit(method['name'], filepath)
	info("\tTook %.3f ms" % [(ended_at_usec-started_at_usec)*0.001])
	
	script_instance.free()


func is_method_a_test(method: Dictionary) -> bool:
	return (
		(method['name'] as String).begins_with('test_')
	)


func info(message: String) -> void:
	if print_to_console:
		print_rich(message)
	logged.emit(message)
