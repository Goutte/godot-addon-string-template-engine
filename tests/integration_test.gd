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
				&'name': 'Goutte',
			},
			&'expected': "Bonjour !",
		},
		{
			&'test': "Echo a variable",
			&'template': "Hello {{ name }}!",
			#&'template': "Hello {{ name | uppercase | reverse }}!",
			&'variables': {
				&'name': 'Godette',
			},
			&'expected': "Hello Godette!",
		},
		{
			&'test': "Echo a variable (allow no spaces)",
			&'template': "Hello {{name}}!",
			&'variables': {
				&'name': 'Goutte',
			},
			&'expected': "Hello Goutte!",
		},
		{
			&'test': "Echo a variable (ignore spaces)",
			&'template': "Hello {{   name     }}!",
			&'variables': {
				&'name': 'there',
			},
			&'expected': "Hello there!",
		},
		{
			&'test': "Echo two variables",
			&'template': "Hello {{ surname }} {{ name }}!",
			&'variables': {
				&'surname': "Godot's",
				&'name': "Community",
			},
			&'expected': "Hello Godot's Community!",
		},
		{
			&'test': "Accept multiline templates",
			&'template': """
			Hello {{ name }},
			I hope this email finds you well.
			
			{{ greetings }}, yours truly.
			""",
			&'variables': {
				&'name': "Pierrot",
				&'greetings': "Cordially",
			},
			&'expected': """
			Hello Pierrot,
			I hope this email finds you well.
			
			Cordially, yours truly.
			""",
		},
		{
			&'test': "Allow unicode runes (unsafely)",
			&'template': """
			🥳 with 🦊 like {{ 🦋 }}
			""",
			&'variables': {
				&'🦋': "🦺",
			},
			&'expected': """
			🥳 with 🦊 like 🦺
			""",
		},
		# Nope: was my first instinct, but {% verbatim %} is less annoying
		#{
			#&'test': "Escape echo statement shorthand delimiters",
			#&'template': """
			#Use an echo statement like so: `Hello \\{{ name \\}}`
			#With the key `'name'` set to `"world"` in the `variables` Dictionary.
			#""",
			#&'variables': {
				#&'name': "Escaper",
			#},
			#&'expected': """
			#Use an echo statement like so: `Hello {{ name }}`
			#With the key `'name'` set to `"world"` in the `variables` Dictionary.
			#""",
		#},
		{
			&'test': "Verbatim statement",
			&'template': """
			{% verbatim %}
			Use an echo statement like so: `Hello {{ name }}`
			With the key `'name'` set to `"world"` in the `variables` Dictionary.
			{% endverbatim %}
			""",
			&'variables': {
				&'name': "Escaper",
			},
			&'expected': """
			
			Use an echo statement like so: `Hello {{ name }}`
			With the key `'name'` set to `"world"` in the `variables` Dictionary.
			
			""",
		},
		
		
	]
	var engine: StringEngine = StringEngine.new()
	for datum: Dictionary in data:
		var expected: String = datum[&'expected']
		var actual: String = engine.render(datum[&'template'], datum[&'variables'])
		print("\t* %s" % [datum[&'test']])
		assert_equals(expected, actual, datum['test'])
