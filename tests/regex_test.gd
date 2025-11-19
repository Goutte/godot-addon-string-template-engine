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
