extends AbstractTest

func test_a_bunch_of_use_cases() -> void:
	var data := [
		{
			&'rule': "Empty template yields empty string",
			&'template': "",
			&'variables': {},
			&'expected': "",
		},
		{
			&'rule': "Template without statements yields itself",
			&'template': "Bonjour !",
			&'variables': {},
			&'expected': "Bonjour !",
		},
		{
			&'rule': "Ignore unused variables",
			&'template': "Bonjour !",
			&'variables': {
				&'i_am_not_used': '.oOwOo.',
			},
			&'expected': "Bonjour !",
		},
		{
			&'rule': "Echo a variable",
			&'template': "Hello {{ name }}!",
			&'variables': {
				&'name': 'Godette',
			},
			&'expected': "Hello Godette!",
		},
		{
			&'rule': "Echo a variable (whitespaces are optional)",
			&'template': "Hello {{name}}!",
			&'variables': {
				&'name': 'Goutte',
			},
			&'expected': "Hello Goutte!",
		},
		{
			&'rule': "Echo a variable (whitespaces are conflatable)",
			&'template': "Hello {{   name  		  }}!",
			&'variables': {
				&'name': 'there',
			},
			&'expected': "Hello there!",
		},
		{
			&'rule': "Echo two variables",
			&'template': "Hello {{ surname }} {{ name }}!",
			&'variables': {
				&'surname': "Godot's",
				&'name': "Community",
			},
			&'expected': "Hello Godot's Community!",
		},
		{
			&'rule': "Accept multiline templates",
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
		#{
			#&'rule': "Allow unicode runes (unsafe)",
			#&'template': """
			#🥳 with 🦊 like {{ 🦋 }}
			#""",
			#&'variables': {
				#&'🦋': "🦺",
			#},
			#&'expected': """
			#🥳 with 🦊 like 🦺
			#""",
		#},
		{
			# NOTE: Use {% verbatim %} … {% endverbatim %} instead of escape sequances
			&'rule': "Backslashes do NOT escape instructions",
			&'template': """
			C:\\{{ folder }}\\{{ filename }}
			""",
			&'variables': {
				&'folder': "system32",
				&'filename': "ms32.exe",
			},
			&'expected': """
			C:\\system32\\ms32.exe
			""",
		},
		{
			&'rule': "Addition of integers",
			&'template': """
			Current: {{ current }}
			Next: {{ current + 1 }}
			""",
			&'variables': {
				&'current': 41,
			},
			&'expected': """
			Current: 41
			Next: 42
			""",
		},
		{
			&'rule': "Chained addition of integers",
			&'template': """
			{{ current + 1 + current + 3 }}
			""",
			&'variables': {
				&'current': 41,
			},
			&'expected': """
			86
			""",
		},
		{
			&'rule': "Floats",
			&'template': """
			{{ 1.618 }}
			{{ tau }}
			{{ tau * tau }}
			""",
			&'variables': {
				&'tau': TAU,
			},
			&'expected': """
			1.618
			6.28318530717959
			39.4784176043574
			""",
		},
		{
			&'rule': "Arithmetic with Floats",
			&'template': """
			{{ tau + tau }}
			{{ tau - tau }}
			{{ tau * tau }}
			{{ tau / tau }}
			""",
			&'variables': {
				&'tau': TAU,
			},
			&'expected': """
			12.5663706143592
			0.0
			39.4784176043574
			1.0
			""",
		},
		{
			&'rule': "Multiplication of integers",
			&'template': """
			{{ current * 3 }}
			""",
			&'variables': {
				&'current': 7,
			},
			&'expected': """
			21
			""",
		},
		{
			&'rule': "Basic +-*/ arithmetic with precedence",
			&'template': """
			{{ 1 + a * 3 + 6 / 3 }}
			""",
			&'variables': {
				&'a': 4,
			},
			&'expected': """
			15
			""",
		},
		{
			&'rule': "Negative numbers",
			&'template': """
			{{ -2 * -5 * -11 }}
			""",
			&'variables': {},
			&'expected': """
			-110
			""",
		},
		{
			&'rule': "Positive unary operator does nothing special",
			&'template': """
			{{ +2 }}
			{{ +2 * -5 * +1 }}
			""",
			&'variables': {},
			&'expected': """
			2
			-10
			""",
		},
		#{
			#&'rule': "Modulo of integers",
			#&'template': """
			#{{ 20 % 3 }}
			#{{ -20 % 3 }}
			#{{ 20 % -3 }}
			#""",
			#&'variables': {},
			#&'expected': """
			#2
			#-2
			#2
			#""",
		#},
		#{
			#&'rule': "Modulo of floats",
			#&'template': """
			#{{ 10.1 % 3.3 }}
			#{{ 20.125 % 3 }}
			#{{ 10 % 3.3 }}
			#""",
			#&'variables': {},
			#&'expected': """
			#0.2
			#2.125
			#0.1
			#""",
		#},
		{
			&'rule': "Not operator works like Godot's",
			&'template': """
			{{ !0 }}
			{{ !1 }}
			{{ ! 41 }}
			""",
			&'variables': {},
			&'expected': """
			true
			false
			false
			""",
		},
		{
			&'rule': "Equality",
			&'template': """
			{{ 1 == 1 }}
			{{ 1 == 2 }}
			{{ a == b }}
			{{ a != b }}
			{{ 2.0 == 2 }}
			{{ 2.0 != 2.2 }}
			""",
			&'variables': {
				&'a': 7,
				&'b': 0,
			},
			&'expected': """
			true
			false
			false
			true
			true
			true
			""",
		},
		{
			&'rule': "Comparison",
			&'template': """
			{{ 1 <= 1 }}
			{{ 1 < 1 }}
			{{ 1 <= 2 }}
			{{ 1 >= 2 }}
			{{ 1 >= 1 }}
			""",
			&'variables': {},
			&'expected': """
			true
			false
			true
			false
			true
			""",
		},
		{
			&'rule': "If statement",
			&'template': """
			{% if age < 18 %}< 18{% endif %}
			{% if age >= 60 %}Retiree{% endif %}
			""",
			&'variables': {
				&'age': 70,
			},
			&'expected': """
			
			Retiree
			""",
		},
		{
			&'rule': "Verbatim statement",
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
		{
			&'rule': "Verbatim statement (preserves conflatable whitespaces)",
			&'template': """
			{% verbatim %}Use an echo statement like so: `Hello {{  name  }}`.{% endverbatim %}
			""",
			&'variables': {
				&'name': "Escaper",
			},
			&'expected': """
			Use an echo statement like so: `Hello {{  name  }}`.
			""",
		},
		# I don't know what to do with this one for now…
		#{
			#&'rule': "Verbatim statement (recursion)",
			#&'template': """
			#{% verbatim %}
			#{% verbatim %}
			#Hello {{  name  }}
			#{% endverbatim %}
			#{% endverbatim %}
			#""",
			#&'variables': {
				#&'name': "verboten",
			#},
			#&'expected': """
			#
			#{% verbatim %}
			#Hello {{  name  }}
			#{% endverbatim %}
			#
			#""",
		#},
	]
	var engine := StringEngine.new()
	for datum: Dictionary in data:
		var expected: String = datum[&'expected']
		var actual: String = engine.render(datum[&'template'], datum[&'variables'])
		print("\t* %s" % [datum[&'rule']])
		assert_equals(expected, actual, "Error in %s\nwith template:\n%s\nand variables:\n%s" % [
			datum[&'rule'],
			datum[&'template'],
			datum[&'variables'],
		])
