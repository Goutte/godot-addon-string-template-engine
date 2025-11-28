extends AbstractTest

## A test object.
class Person:
	extends RefCounted
	var rank := 0
	var name := ""
	var surname := ""
	func _init(
		some_rank: int,
		some_name: String,
		some_surname: String,
	) -> void:
		self.rank = some_rank
		self.name = some_name
		self.surname = some_surname
		

func test_a_bunch_of_rules() -> void:
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
			&'rule': "Print a variable with {{ … }}",
			&'template': "Hello {{ name }}!",
			&'variables': {
				&'name': 'Godette',
			},
			&'expected': "Hello Godette!",
		},
		{
			&'rule': "Whitespaces inside are optional",
			&'template': "Hello {{name}}!",
			&'variables': {
				&'name': 'Goutte',
			},
			&'expected': "Hello Goutte!",
		},
		{
			&'rule': "Whitespaces inside are conflatable",
			&'template': "Hello {{   name  	\n	  }}!",
			&'variables': {
				&'name': 'there',
			},
			&'expected': "Hello there!",
		},
		{
			&'rule': "Accept unicode runes as values",
			&'template': "Hello {{ surname }} {{ name }}{{ emote }}!",
			&'variables': {
				&'surname': "🤖 Godot's",
				&'name': "Community ♥",
				&'emote': "♥",
			},
			&'expected': "Hello 🤖 Godot's Community ♥♥!",
		},
		{
			&'rule': "Access objects' properties with .",
			&'template': "Hello {{ person.name }} {{ person.surname }}!",
			&'variables': {
				&'person': Person.new(42, "Jean", "Valjean"),
			},
			&'expected': "Hello Jean Valjean!",
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
			use \\{{namespace}}\\{{class}};
			""",
			&'variables': {
				&'namespace': "Addons",
				&'class': "StringEngine",
			},
			&'expected': """
			use \\Addons\\StringEngine;
			""",
		},
		{
			&'rule': "Integers",
			&'template': """
			0{{ 11 }}0
			0{{ 011 }}0
			0{{ 0 }}0
			""",
			&'variables': {},
			&'expected': """
			0110
			0110
			000
			""",
		},
		{
			&'rule': "Floats",
			&'template': """
			{{ 1.618 }}
			{{ 1.618e3 }}
			1.0 / phi == {{ 1.0 / phi }}
			      phi == {{ phi }}
			phi * phi == {{ phi * phi }}
			phi * phi == phi + 1   is   {{ phi * phi == phi + 1 }}
			""",
			&'variables': {
				&'phi': (1.0 + sqrt(5.0)) * 0.5,
			},
			&'expected': """
			1.618
			1618.0
			1.0 / phi == 0.61803398874989
			      phi == 1.61803398874989
			phi * phi == 2.61803398874989
			phi * phi == phi + 1   is   true
			""",
		},
		{
			&'rule': "Strings",
			&'template': """
			{{ "" }}{{ "." }}{{ "" }}
			{{ "{{" }} "🦋" {{ "}}" }} yields {{ "🦋" }}
			{{ "Sphinx of Black Quartz, Judge my Vow" }}
			{{ "The ship" }} "Espoir"
			{{ "The ship \\"Espoir\\"" }}
			""",
			&'variables': {},
			&'expected': """
			.
			{{ "🦋" }} yields 🦋
			Sphinx of Black Quartz, Judge my Vow
			The ship "Espoir"
			The ship "Espoir"
			""",
		},
		{
			&'rule': "String escape sequence is \\",
			&'template': """
			{{ "\\"" }}
			{{ "\\\\" }}
			{{ "\\\\" }}{{ "\\"" }}
			{{ "\\\\\\"" }}
			{{ "\\"\\\\" }}
			""",
			&'variables': {},
			&'expected': """
			"
			\\
			\\"
			\\"
			"\\
			""",
		},
		{
			&'rule': "Naughty Strings",
			&'template': """
			TODO
			""",
			&'variables': {},
			&'expected': """
			TODO
			""",
		},
		{
			&'rule': "Addition of integers",
			&'template': """
			Current: {{ 0 + current }}
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
			&'rule': "Arithmetic with floats",
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
			&'rule': "Grouping using parentheses",
			&'template': """
			{{ ((1 + a) * (1 + a)) * 3 + 6 / 3 }}
			""",
			&'variables': {
				&'a': 4,
			},
			&'expected': """
			77
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
		# NOTE: Nah, can't keep LL(1) with the default set of delimiters
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
			&'rule': "Multiline arithmetic",
			&'template': """
			{{
				(
					(1 + a)
					*
					(1 + a)
				)
				*
				3
				+ 6 / 3
			}}
			""",
			&'variables': {
				&'a': 19,
			},
			&'expected': """
			1202
			""",
		},
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
			&'rule': "Composition",
			&'template': """
			{{ true }} {{ false }}
			{{ true and true }}
			{{ true and false }}
			""",
			&'variables': {},
			&'expected': """
			true false
			true
			false
			""",
		},
		{
			&'rule': "Filters",
			&'template': """
			{{ "yoLo" | uppercase }}
			{{ "YoLo" | upper }}
			{{ "YoLo" | lowercase }}
			{{ "YoLo" | lower }}
			{{ "YoLo" | lower | upper }}
			{{"YoLo"|upper|lower}}
			{{ "ÆÛŒÉÈÇÀ" | lower }}
			""",
			&'variables': {},
			&'expected': """
			YOLO
			YOLO
			yolo
			yolo
			YOLO
			yolo
			æûœéèçà
			""",
		},
		{
			&'rule': "Filter abs",
			&'template': """
			{{ 0 | abs }}
			{{ 1 | abs }}
			{{ 2.0 | abs }}
			{{ -0 | abs }}
			{{ -1 | abs }}
			{{ -2.0 | abs }}
			""",
			&'variables': {},
			&'expected': """
			0
			1
			2.0
			0
			1
			2.0
			""",
		},
		{
			&'rule': "Filter capitalize",
			&'template': """
			{{ "" | capitalize }}
			{{ "paris" | capitalize }}
			{{ "BERLIN" | capitalize }}
			{{ "ElSalvador" | capitalize }}
			{{ "Saint-Louis du Senegal" | capitalize }}
			""",
			&'variables': {},
			&'expected': """
			
			Paris
			Berlin
			El Salvador
			Saint Louis Du Senegal
			""",
		},
		{
			&'rule': "Comments",
			&'template': """
			{#################################}
			A{# nother one #}B{#ites the dust#}!
			{#################################}
			""",
			&'variables': {},
			&'expected': """
			
			AB!
			
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
			&'rule': "If statement (recursive)",
			&'template': """
			{% if age > 18 %}
				You can vote.
				{% if age > 40 %}
				You can retire.
				{% endif %}
			{% endif %}
			""",
			&'variables': {
				&'age': 70,
			},
			&'expected': """
			
				You can vote.
				
				You can retire.
				
			
			""",
		},
		{
			&'rule': "If Else statement",
			&'template': """
			{% if red_to_alpha %}
				ALPHA = {{ value }};
			{% else %}
				ALBEDO.r = {{ value }};
			{% endif %}
			""",
			&'variables': {
				&'red_to_alpha': true,
				&'value': 0.62,
			},
			&'expected': """
			
				ALPHA = 0.62;
			
			""",
		},
		{
			&'rule': "Set statement",
			&'template': """
			{% set name = "poupou" %}
			Hello {{ name }}!
			{% set name = "pouloupi" %}
			Hello {{ name }}!
			""",
			&'variables': {
				&'name': "Escaper",
			},
			&'expected': """
			
			Hello poupou!
			
			Hello pouloupi!
			""",
		},
		{
			&'rule': "While statement",
			&'template': """
			{% set i = 5 %}
			{% while i > 0 %}{{ i }}{% set i = i - 1 %}{% endwhile %}
			""",
			&'variables': {
				&'name': "Escaper",
			},
			&'expected': """
			
			54321
			""",
		},
		{
			&'rule': "For statement",
			&'template': """
			{% for i in numbers %}{{ i }} {% endfor %}
			""",
			&'variables': {
				&'numbers': [2, 3, 5, 7, 11],
			},
			&'expected': """
			2 3 5 7 11 
			""",
		},
		{
			&'rule': "Clear spaces and tabs before statements with ~",
			&'template': """
			{%~ for i in numbers %}
			{{ i }}
			{%~ endfor %}
			""",
			&'variables': {
				&'numbers': [2, 3, 5],
			},
			&'expected': """

			2

			3

			5

			""",
		},
		{
			&'rule': "Clear all whitespaces before statements with -",
			&'template': """
			Primes:
			
			{%- for i in numbers %}
			{{ i }}
			

			{%- endfor %}
			""",
			&'variables': {
				&'numbers': [2, 3, 5],
			},
			&'expected': """
			Primes:
			2
			3
			5
			""",
		},
		{
			&'rule': "Clear spaces and tabs after statements with ~",
			&'template': """
			Primes:
			{% for i in numbers ~%}     	 	{{ i }} {% endfor %}
			""",
			&'variables': {
				&'numbers': [2, 3, 5],
			},
			&'expected': """
			Primes:
			2 3 5 
			""",
		},
		{
			&'rule': "Clear all whitespaces after statements with -",
			&'template': """
			Primes:
			{% for i in numbers -%}
			
			{{ i }}
			{% endfor -%}
			""",
			&'variables': {
				&'numbers': [2, 3, 5],
			},
			&'expected': """
			Primes:
			2
			3
			5
			""",
		},
		{
			&'rule': "Option to clear whitespaces like in Twig",
			&'template': """
			Primes:
			
			{%~ for i in numbers %}
			{{ i }}
			{%~ endfor %}
			""",
			&'variables': {
				&'numbers': [2, 3, 5],
			},
			&'configure': func(se: StringEngine):
				se.clear_newline_after_comment = true
				se.clear_newline_after_statement = true,
			&'expected': """
			Primes:
			
			2
			3
			5
			""",
		},
		{
			&'rule': "Option to clear a newline after comments and statements",
			&'template': """
			Lots:
			{#~ I am a comment #}
			{%~ set n = 999 %}
			{#~ I am another comment,
			and I am multiline !!! #}
			{%~ if n == 999 %}
			{{ n }}
			{%~ endif %}
			""",
			&'variables': {},
			&'configure': func(se: StringEngine):
				se.clear_newline_after_comment = true
				se.clear_newline_after_statement = true,
			&'expected': """
			Lots:
			999
			""",
		},
		{
			&'rule': "Option to clear a newline after echo",
			&'template': """
			{{ 7 }}

			{{ 7 }}

			{{ 7 }}

			""",
			&'variables': {},
			&'configure': func(se: StringEngine):
				se.clear_newline_after_echo = true
				,
			&'expected': """
			7
			7
			7
			""",
		},
		#{
			#&'rule': "Optionally, clear lines with only one silent statement",
			#&'template': """
			#{% for i in numbers %}
			#{{ i }}
			#{% endfor %}
			#""",
			#&'variables': {
				#&'numbers': [2, 3, 5],
			#},
			#&'configure': func(se: StringEngine): se.clear_statement_lines = true,
			#&'expected': """
			#2
			#3
			#5
			#""",
		#},
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
			# ?
			#""",
		#},
	]
	for datum: Dictionary in data:
		var engine := StringEngine.new()
		var configure: Callable = datum.get(&'configure', func(_se: StringEngine): return)
		configure.call(engine)
		var expected: String = datum[&'expected']
		var actual: String = engine.render(datum[&'template'], datum[&'variables'])
		print("\t* %s" % [datum[&'rule']])
		assert_equals(expected, actual, "Error in %s\nwith template:\n%s\nand variables:\n%s" % [
			datum[&'rule'],
			datum[&'template'],
			datum[&'variables'],
		])
	print("\tRan %d subtests" % data.size())
