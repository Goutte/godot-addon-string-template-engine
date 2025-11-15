extends AbstractTest

func test_a_bunch_of_use_cases() -> void:
	var data := [
		{
			&'test': "Empty string",
			&'template': "",
			&'variables': {},
			&'expected': "",
		},
		{
			&'test': "Static string",
			&'template': "Bonjour !",
			&'variables': {},
			&'expected': "Bonjour !",
		},
		{
			&'test': "Ignore unused variables",
			&'template': "Bonjour !",
			&'variables': {
				&'name': 'Goutte'
			},
			&'expected': "Bonjour !",
		},
		{
			&'test': "Echo a variable",
			&'template': "Hello {{ name }}!",
			#&'template': "Hello {{ name | uppercase | reverse }}!",
			&'variables': {
				&'name': 'Godette'
			},
			&'expected': "Hello Godette!",
		},
		{
			&'test': "Simple Variable (allow no spaces)",
			&'template': "Hello {{name}}!",
			&'variables': {
				&'name': 'Goutte'
			},
			&'expected': "Hello Goutte!",
		},
		{
			&'test': "Simple Variable (ignore spaces)",
			&'template': "Hello {{   name     }}!",
			&'variables': {
				&'name': 'Goutte'
			},
			&'expected': "Hello Goutte!",
		},
	]
	var engine: StringEngine = StringEngine.new()
	for datum: Dictionary in data:
		var expected: String = datum[&'expected']
		var actual: String = engine.render(datum[&'template'], datum[&'variables'])
		assert_equals(expected, actual, datum['test'])
