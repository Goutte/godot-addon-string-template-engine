extends Control

@export var runner: TestsRunner
@export var logger: RichTextLabel


func _enter_tree() -> void:
	assert(runner)
	assert(logger)
	runner.logged.connect(on_logged)


func on_logged(message: String) -> void:
	logger.append_text(message)
	logger.newline()
	logger.pop_all()
