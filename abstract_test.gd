extends Object
class_name AbstractTest


func assert_equals(expected: Variant, actual: Variant, message := "") -> void:
	assert(expected == actual, message)
