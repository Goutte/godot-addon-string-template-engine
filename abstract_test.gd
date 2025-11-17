extends Object
class_name AbstractTest


func assert_equals(expected: Variant, actual: Variant, message := "") -> void:
	if expected == actual:
		return
	printerr("Assertion of equality failed !")
	printerr("Expected:\n\n%s\n\nbut got:\n\n%s\n\n" % [expected, actual])
	assert(false, message)
