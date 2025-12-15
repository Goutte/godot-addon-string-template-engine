extends AbstractTest

func test_readme_example_01() -> void:
	# Instantiate the string (template) engine
	var se := StringEngine.new()

	# Configure the string engine however you desire (like Twig, in this example)
	se.clear_newline_after_statement = true
	se.clear_newline_after_comment = true

	# Render a template with some variables
	var rendered := se.render(
		"Hello {% if name %}{{ name|uppercase }}{% else %}World{% endif %}!",
		{ &'name': "Godette" },
	)
	assert(rendered.output == "Hello GODETTE!")
	assert(rendered.errors.is_empty())
