extends AbstractTest

func test_regex() -> void:
	var r1 := RegEx.create_from_string(
		"^c"
	)
	var s := "abcdef"
	var m1 := r1.search(s)
	assert(null == m1, "Start anchor works")
	
	# Hopefully this test will fail someday.
	var m2 := r1.search(s, 2)
	assert(null == m2, "Start anchor fails when using offset, as per doc")


func test_regex_negative_lookahead() -> void:
	var rgx := RegEx.create_from_string(
		"false(?![a-zA-Z0-9_])"
	)
	var m1 := rgx.search("false")
	assert(null != m1)
	var m2 := rgx.search("falsey")
	assert(null == m2)  # it works!
	var m3 := rgx.search("false ")
	assert(null != m3)
	var m4 := rgx.search("{{false}}")
	assert(null != m4)
