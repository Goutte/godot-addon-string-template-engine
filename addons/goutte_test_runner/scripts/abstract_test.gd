extends Object
class_name AbstractTest


func assert_equals(expected: Variant, actual: Variant, message := "") -> void:
	if expected == actual:
		return
	message = """
Assertion of equality failed !
%s
Expected:
------------------------------
%s
------------------------------
but got:
------------------------------
%s
------------------------------

""" % [message, expected, actual]
	printerr(message)
	assert(false, message)
