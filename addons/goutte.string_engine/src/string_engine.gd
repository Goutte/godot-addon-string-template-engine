## A String Template Engine, slow but flexible.
## Vanilla GDScript "port wannabe" of Inja, Twig, Jinja…
class_name StringEngine
extends RefCounted

# WHY?  Generate shaders, dynamic dialogues, maybe even HTML…
# TOO SLOW?  try the fast Ginja addon using a gdextension to wrap Inja.

class Options:
	extends Resource
	
	## Clear a single newline if it immediately follows a statement.
	@export var clear_newline_after_statement := false
	## Clear a single newline if it immediately follows a comment.
	@export var clear_newline_after_comment := false
	## Clear a single newline if it immediately follows a print.
	@export var clear_newline_after_print := false
	## Enter debug mode immediately when there's an error. (if applicable)
	@export var break_on_error := true
	## Because we unit-test some errors; you should probably ignore this option.
	@export var silence_errors := false

	## Only these runes are allowed in literal identifiers, as well as statement
	## and filter identifiers.  The dash (-) is a shorthand syntax. (see RegEx)
	## This is meant to be part of a Regular Expression, in a char class [].
	## Make sure to escape accordingly; also, you can't put anything in here.
	## If you put symbols in here that have another meaning in our grammar,
	## the engine will likely break and there's no way around it I guess.
	@export var allowed_runes_in_identifiers := 'a-zA-Z0-9_'
	
	@export var symbol_clear_whitespace := '-'  # as in {{- for example
	@export var symbol_clear_line_whitespace := '~'  # as in {{~ for example
	@export var symbol_print_opener := '{{'
	@export var symbol_print_closer := '}}'
	@export var symbol_comment_opener := '{#'
	@export var symbol_comment_closer := '#}'
	@export var symbol_statement_opener := '{%'
	@export var symbol_statement_closer := '%}'
	@export var symbol_statement_assign := '='
	@export var symbol_filter := '|'
	@export var symbol_expression_group_opener := '('
	@export var symbol_expression_group_closer := ')'
	@export var symbol_boolean_true := 'true'
	@export var symbol_boolean_false := 'false'
	@export var symbol_combinator_and := 'and'
	@export var symbol_combinator_nand := 'nand'
	@export var symbol_combinator_or := 'or'
	@export var symbol_combinator_xor := 'xor'
	@export var symbol_containor_in := 'in'
	@export var symbol_operator_not := 'not'

	# The order we tokenize expressions (if cascade) is hardcoded, right now.
	# Therefore, the following values are read-only;
	# perhaps we could make their precedence configurable (at a cost).
	const symbol_operator_addition := '+'
	const symbol_operator_subtraction := '-'
	const symbol_operator_multiplication := '*'
	const symbol_operator_division := '/'
	const symbol_operator_modulo := '%'
	const symbol_operator_concatenation := '~'
	const symbol_comparator_equal := '=='
	const symbol_comparator_inequal := '!='
	const symbol_comparator_less_than := '<'
	const symbol_comparator_less_or_equal_than := '<='
	const symbol_comparator_greater_than := '>'
	const symbol_comparator_greater_or_equal_than := '>='
	const symbol_string_delimiter := '"'  # single rune, MUST NOT be backslash
	const symbol_group_separator := ','
	
	const symbol_accessor_property := '.'
	const symbol_accessor_array_opener := '['
	const symbol_accessor_array_closer := ']'


## Statement extensions used by the engine.
## You can append your own in here before calling render(…).
## Your best bet is to look at and copy existing extensions.
## TODO: move these to Options above.
var statement_extensions: Array[StatementExtension] = [
	ForStatementExtension.new(),
	IfElseStatementExtension.new(),
	SetStatementExtension.new(),
	VerbatimStatementExtension.new(),
	WhileStatementExtension.new(),
]

## Filter extensions used by the engine.
## You can append your own in here before calling render(…).
## Your best bet is to look at and copy existing extensions.
## TODO: move these to Options above.
var filter_extensions: Array[FilterExtension] = [
	AbsFilterExtension.new(),
	CapitalizeFilterExtension.new(),
	LowerFilterExtension.new(),
	LowercaseFilterExtension.new(),
	RoundFilterExtension.new(),
	UpperFilterExtension.new(),
	UppercaseFilterExtension.new(),
]

#region Miscellaneous Data Structures

## A basic (and quite inefficient) view on a portion of a string.
## The goal was/is to copy less strings, especially as templates can be big.
## But regex search using offset ignores anchors like `^`, so we copy anyway.
## Not when shrinking though, so it's still worth having this I think.
class StringView:
	extends Resource
	# Private variables and reserved words; hence the dunders.
	@export_storage var __string: String  # immutable whole string
	@export_storage var __start: int
	@export_storage var __length: int

	func _init(
		target_string := "",    ## Provide this; optional cause it MUST be IIRC
		optional_start := -1,   ## Defaults to 0
		optional_length := -1,  ## Defaults to the string's length
	) -> void:
		self.__string = target_string
		if optional_start < 0:
			optional_start = 0
		self.__start = optional_start
		if optional_length < 0:
			optional_length = __string.length()
		self.__length = optional_length
		sanitize_delimiters()

	func _to_string() -> String:
		return get_as_string()

	func get_as_string() -> String:
		return __string.substr(__start, __length)

	func start() -> int:
		return __start

	func length() -> int:
		return __length

	func begins_with(prefix: String) -> bool:
		return prefix == __string.substr(__start, prefix.length())

	func sanitize_delimiters() -> void:
		self.__start = clampi(__start, 0, __string.length())
		self.__length = clampi(__length, 0, __string.length() - __start)

	func line() -> int:
		return 1 + __string.count("\n", 0, __start)

	## Faster but breaks line anchor metacharacters like ^ and $
	func rsearch(regex: RegEx) -> RegExMatch:
		return regex.search(__string, __start, __start + __length)

	## Use this if the regex uses ^ or $
	func rsearch_start(regex: RegEx) -> RegExMatch:
		return regex.search(get_as_string())  # slower, but it "Just Works".

	func shrink_from_start_by(amount: int) -> void:
		self.__start += amount
		self.__length -= amount
		sanitize_delimiters()

	func read_character_at(relative_position: int) -> String:
		return __string[
			clampi(__start + relative_position, 0, __string.length() - 1)
		]


# Maybe we'll end up with this, it feels cleaner.  Is it faster, though ?
#class TokenStream:
	#extends Resource
	#@export var tokens: Array[Token]
	#@export var cursor: int
	#func _init(tokens: Array[Token] = []) -> void:
		#self.tokens = tokens
		#self.cursor = 0
	#func next() -> Token:
		#self.cursor += 1
		#return self.tokens[self.cursor - 1]
	#func get_current() -> Token:
		#return self.tokens[self.cursor]


class TemplateError:
	extends Resource
	@export var message: String

	func _to_string() -> String:
		return self.message


## This resource is the main output of the StringEngine.  See render().
class RenderedTemplate:
	extends Resource
	@export var output: String
	@export var errors: Array[TemplateError]
	
	func _to_string() -> String:
		return self.output

#endregion


#region Tokenization

## A syntax token of the domain specific language of the string template engine.
## The template source string is tokenized into a stream of those tokens,
## and from them the parser builds the syntax tree.
class Token:
	extends Resource
	enum Types {
		UNKNOWN,                  ## Means there was a failure somewhere.
		EOF,                      ## Token we get at the End Of File.
		RAW_DATA,                 ## Most of the stuff; all that is not our DSL.
		PRINT_OPENER,             ## {{
		PRINT_CLOSER,             ## }}
		COMMENT_OPENER,           ## {#
		COMMENT_CONTENT,          ## Anything except a comment closer.
		COMMENT_CLOSER,           ## #}
		STATEMENT_OPENER,         ## {%
		STATEMENT_IDENTIFIER,     ## for, if, else, endif, verbatim, etc.
		STATEMENT_CLOSER,         ## %}
		STATEMENT_ASSIGN,         ## =
		EXPRESSION_GROUP_OPENER,  ## (
		EXPRESSION_GROUP_CLOSER,  ## )
		EXPRESSIONS_SEPARATOR,    ## ,
		ACCESSOR_PROPERTY,        ## .
		ACCESSOR_ARRAY_OPENER,    ## [
		ACCESSOR_ARRAY_CLOSER,    ## ]
		FILTER,                   ## |
		FILTER_IDENTIFIER,        ## uppercase, round, abs…
		LITERAL_IDENTIFIER,       ## ^[a-zA-Z0-9_]+$  (roughly)
		LITERAL_BOOLEAN_TRUE,     ## true
		LITERAL_BOOLEAN_FALSE,    ## false
		LITERAL_INTEGER,          ## 42
		LITERAL_FLOAT,            ## 1.618
		LITERAL_STRING,           ## "Hello world !"
		OPERATOR_ADDITION,        ## +
		OPERATOR_SUBTRACTION,     ## -
		OPERATOR_MULTIPLICATION,  ## *
		OPERATOR_DIVISION,        ## /
		OPERATOR_MODULO,          ## %
		# TODO: use 'not' instead of '!'
		OPERATOR_NOT,             ## !
		OPERATOR_CONCATENATION,   ## ~
		COMPARATOR_EQUAL,         ## ==
		COMPARATOR_INEQUAL,       ## !=
		COMPARATOR_LESS,          ## <
		COMPARATOR_LESS_EQUAL,    ## <=
		COMPARATOR_GREATER,       ## >
		COMPARATOR_GREATER_EQUAL, ## >=
		COMBINATOR_AND,           ## and
		COMBINATOR_OR,            ## or
		COMBINATOR_NAND,          ## nand
		COMBINATOR_XOR,           ## xor
		CONTAINOR_IN,             ## in
	}
	
	## The type of this token ; its most important property.
	@export var type := Types.UNKNOWN
	
	## The raw, unadultered value found in the source template.
	@export var literal := ""
	
	## The actual value we're going to use in the parser.
	## Almost the same as the literal, but with some whitespace treatment.
	@export var lexeme := ""
	
	@export var starts_in_source_at := 0  # 0-indexed
	@export var starts_in_source_at_line := 0  # 1-indexed
	@export var ends_in_source_at := 0  # 0-indexed
	@export var ends_in_source_at_line := 0  # 1-indexed
	@export var whitespaces_before := ""
	@export var whitespaces_after := ""
	
	# TODO: wouldn't it be faster/better to host these toggles in the tokenizer?
	# It boils down to: Do we need to remember these once they've been applied?
	var clear_newline_after := false
	var clear_whitespaces_after := false
	var clear_tabspaces_after := false

	func with_type(value: Types) -> Token:
		self.type = value
		return self

	func with_literal(value: String) -> Token:
		self.literal = value
		return self

	func with_lexeme(value: String) -> Token:
		self.lexeme = value
		return self

	func starting_at(index0: int) -> Token:
		self.starts_in_source_at = index0
		return self

	func starting_at_line(index1: int) -> Token:
		self.starts_in_source_at_line = index1
		return self

	func ending_at(index0: int) -> Token:
		self.ends_in_source_at = index0
		return self

	func ending_at_line(index1: int) -> Token:
		self.ends_in_source_at_line = index1
		return self

	func _to_string() -> String:
		return self.literal


## The tokenizer reads the template string and produces a list of tokens.
## Probably closer to a Lexer now, as its states are kinda tied to our grammar.
class Tokenizer:
	extends RefCounted
	## I've found it handy to make the Tokenizer a Finite State Machine.
	## Especially as we do not know anything in advance about the raw data.
	enum States {
		RAW_DATA,  ## Reading raw data, the starting/default state
		PRINT,     ## Reading the expression inside of {{ … }}
		STATEMENT, ## Reading the statement inside of {% … %}
		COMMENT,   ## Reading the comment inside of {# … #}
	}

	# Public
	var options: Options
	
	# Private
	var state: States
	var tokens: Array[Token]  # Perhaps use a TokenStream class? (halfway there)
	var source: String  # Immutable whole source, we work on a view of it
	var source_view: StringView  # Our view on the source template.

	func _init(some_options := Options.new()) -> void:
		self.options = some_options

	# @protected
	func reset():
		state = States.RAW_DATA
		tokens = []  # do NOT clear() ; copy is needed ← output of tokenize() !
		source = ""
		source_view = null
		reset_regexes()

	var identifier_literal_regex: RegEx
	var integer_literal_regex: RegEx
	var float_literal_regex: RegEx
	var string_literal_regex: RegEx
	var whitespaces_regex: RegEx
	var openers_regex: RegEx
	var print_closer_regex: RegEx
	var comment_closer_regex: RegEx
	var statement_closer_regex: RegEx

	# Line start anchor breaks with regex's search offset, yet we use a StringView.
	# We might want to get rid of it and switch to solution B ; depends on the benchmarks.
	# A: keep it, keep searching on a substr (copy!) of source on each token (we're doing it)
	# B: remove it, use the search offset, hope regex is fast (feels like it'll scale worse)
	# C: find a satisfying tertium quid
	const LINE_START_ANCHOR := "^"

	func reset_regexes():
		var has_compiled: int

		self.identifier_literal_regex = RegEx.new()
		has_compiled = self.identifier_literal_regex.compile(
			LINE_START_ANCHOR
			+ "[a-zA-Z_][a-zA-Z0-9_]*"  # Safe, but ASCII only
			#+ "[\\w_][\\w0-9_]*"  # Nope: \w includes numbers and \L is absent
		)
		assert(has_compiled == OK, "Detection of variable identifiers broke.")

		self.integer_literal_regex = RegEx.new()
		has_compiled = self.integer_literal_regex.compile(
			LINE_START_ANCHOR
			+ "(?:"
			+ "0[xX][a-f0-9]+"
			+ "|0[bB][01]+"
			+ "|[0-9]+"
			+ ")"
		)
		assert(has_compiled == OK, "Numbers must be ßr0k3n")

		self.float_literal_regex = RegEx.new()
		has_compiled = self.float_literal_regex.compile(
			LINE_START_ANCHOR
			+ "(?:"
			+ "\\d+\\.\\d*e\\d+"        # 1.618e3
			+ "|\\d*\\.\\d+e[+-]?\\d+"  # .333e10
			+ "|\\d+e[+-]?\\d+"         # 3e9
			+ "|\\d+\\.\\d*"            # 2.7 or 2.
			+ "|\\.\\d+"                # .2
			+ ")"
		)
		assert(has_compiled == OK, "Never trust an IEEE754")

		self.string_literal_regex = RegEx.new()
		var quote := escape_for_regex(options.symbol_string_delimiter)
		has_compiled = self.string_literal_regex.compile(
			LINE_START_ANCHOR +
			quote +
			("(?:[^\\\\%s]|\\\\[\\s\\S])*" % quote) +
			quote
		)
		assert(has_compiled == OK, "Never trust an IEEE754")

		self.whitespaces_regex = RegEx.new()
		has_compiled = self.whitespaces_regex.compile(
			LINE_START_ANCHOR
			+ "[\\s]+"
		)
		assert(has_compiled == OK, "")

		self.openers_regex = RegEx.new()
		has_compiled = self.openers_regex.compile(
			"(?<symbol>" +
			escape_for_regex(options.symbol_print_opener) +
			"|" +
			escape_for_regex(options.symbol_statement_opener) +
			"|" +
			escape_for_regex(options.symbol_comment_opener) +
			")" +
			"(?<clear_whitespace>" +
			escape_for_regex(options.symbol_clear_whitespace) +
			"|" +
			escape_for_regex(options.symbol_clear_line_whitespace) +
			"|" +
			")"
		)
		assert(has_compiled == OK, "Did you change some symbols, perhaps?")

		var compile_closer_regex: Callable = \
		func(
			regex: RegEx,
			symbol: String,
			use_start_anchor := true,
		) -> Error:
			return regex.compile(
				(LINE_START_ANCHOR if use_start_anchor else "") +
				"(?<clear_whitespace>" +
				escape_for_regex(options.symbol_clear_whitespace) +
				"|" +
				escape_for_regex(options.symbol_clear_line_whitespace) +
				"|" +
				")" +
				"(?<symbol>" + escape_for_regex(symbol) + ")"
			)

		self.print_closer_regex = RegEx.new()
		has_compiled = compile_closer_regex.call(
			self.print_closer_regex,
			options.symbol_print_closer,
		)
		assert(has_compiled == OK, "Regex of print closer is broken.")

		self.statement_closer_regex = RegEx.new()
		has_compiled = compile_closer_regex.call(
			self.statement_closer_regex,
			options.symbol_statement_closer,
		)
		assert(has_compiled == OK, "Regex of statement closer is broken.")

		self.comment_closer_regex = RegEx.new()
		has_compiled = compile_closer_regex.call(
			self.comment_closer_regex,
			options.symbol_comment_closer,
			false,
		)
		assert(has_compiled == OK, "Regex of comment closer is broken.")

	## The job of a Tokenizer is to create a stream of tokens from a source.
	func tokenize(template: String) -> Array[Token]:
		reset()
		self.source = template
		self.source_view = StringView.new(template, 0, template.length())

		while self.source_view.length() > 0:
			match self.state:
				States.RAW_DATA:
					# Let's consume as raw data until we find any opener.
					tokenize_raw_data()
				States.PRINT:
					# At this point we have consumed the {{ opener already.
					consume_whitespaces_into_previous_token()
					tokenize_expression()
					consume_whitespaces_into_previous_token()
					tokenize_print_closer()
					set_state(States.RAW_DATA)
				States.STATEMENT:
					# At this point we have consumed the {% opener already.
					consume_whitespaces_into_previous_token()
					tokenize_statement_identifier()
					consume_whitespaces_into_previous_token()

					# FIXME: ask the statement extension on how to tokenize here
					tokenize_expression()

					consume_whitespaces_into_previous_token()
					tokenize_statement_closer()
					set_state(States.RAW_DATA)
				States.COMMENT:
					# At this point we have consumed the {# opener already.
					consume_whitespaces_into_previous_token()
					tokenize_comment_content()
					tokenize_comment_closer()
					set_state(States.RAW_DATA)
				_:
					breakpoint  # unknown state (implement it!)

		return self.tokens

	func tokenize_raw_data() -> void:
		# There is no way to discriminate raw data by itself.
		# We're going to advance to the first found OPENER of our syntax.
		# With the default configuration, OPENERs are `{{`, `{%` and `{#`.

		var openers_match := self.source_view.rsearch_start(self.openers_regex)
		var source_remaining := self.source_view.get_as_string()

		if null == openers_match:
			# No opener found, we can consume as raw data 'til the end
			add_raw_data_token(source_remaining)
			consume_source(source_remaining.length())
		
		else:
			# Let's consume any potential raw data before the opener
			var match_start := openers_match.get_start()
			if match_start > 0:
				var raw_data_contents := source_remaining.substr(0, match_start)
				add_raw_data_token(raw_data_contents)
				consume_source(match_start)

			# Finally we can consume the opener
			var whole_match := openers_match.get_string()
			var opener_symbol := openers_match.get_string(&'symbol')
			var clear_whitespace_symbol := openers_match.get_string(&'clear_whitespace')
			
			# And perhaps clear previous whitespaces
			if clear_whitespace_symbol and not self.tokens.is_empty():
				var previous_token := self.tokens[-1]
				if previous_token.type == Token.Types.RAW_DATA:
					previous_token.lexeme = previous_token.lexeme.rstrip(
						" \t" +
						("\r\n" if clear_whitespace_symbol == '-' else '')
					)

			match opener_symbol:
				options.symbol_print_opener:
					add_token(Token.Types.PRINT_OPENER, whole_match)
					set_state(States.PRINT)
				options.symbol_statement_opener:
					add_token(Token.Types.STATEMENT_OPENER, whole_match)
					set_state(States.STATEMENT)
				options.symbol_comment_opener:
					add_token(Token.Types.COMMENT_OPENER, whole_match)
					set_state(States.COMMENT)
				_:
					breakpoint  # implementation is missing; git to werk!

			consume_source(whole_match.length())

	## Consumes until it finds a content closer symbol.
	func tokenize_comment_content() -> void:
		var closer_match := source_view.rsearch_start(comment_closer_regex)
		if null == closer_match:
			raise_error("Expected a %s symbol to close the comment, but found none." % [
				options.symbol_comment_closer,
			])
		else:
			var match_start := closer_match.get_start()
			var source_remaining := source_view.get_as_string()
			var comment_contents := source_remaining.substr(0, match_start)
			add_token(Token.Types.COMMENT_CONTENT, comment_contents)
			consume_source(match_start)

	func tokenize_print_closer() -> Error:
		var status := tokenize_closer(
			options.symbol_print_closer,
			Token.Types.PRINT_CLOSER,
			self.print_closer_regex,
		)
		if status == OK and options.clear_newline_after_print:
			mark_next_newline_for_clearing()
		return status

	func tokenize_statement_closer() -> Error:
		var status := tokenize_closer(
			options.symbol_statement_closer,
			Token.Types.STATEMENT_CLOSER,
			self.statement_closer_regex,
		)
		if status == OK and options.clear_newline_after_statement:
			mark_next_newline_for_clearing()
		return status

	func tokenize_comment_closer() -> Error:
		var status := tokenize_closer(
			options.symbol_comment_closer,
			Token.Types.COMMENT_CLOSER,
			self.comment_closer_regex,
		)
		if status == OK and options.clear_newline_after_comment:
			mark_next_newline_for_clearing()
		return status

	func scan_regex(closer_regex: RegEx) -> bool:
		return null != self.source_view.rsearch_start(closer_regex)

	func tokenize_closer(
		symbol: String,           ## closer symbol to look for
		token_type: Token.Types,  ## token type to create
		closer_regex: RegEx,      ## compiled regex to match
	) -> Error:
		var closer_match := self.source_view.rsearch_start(closer_regex)
		if closer_match == null:
			raise_error("Expected closer token `%s`, got `%s` instead." % [
				symbol,
				self.source_view.read_character_at(0) +
				self.source_view.read_character_at(1)
			])
			consume_source(self.source_view.length())  # l∞p prevention
			return ERR_INVALID_DATA
		else:
			var whole_match := closer_match.get_string()
			var closer_symbol := closer_match.get_string(&'symbol')
			assert(closer_symbol == symbol)
			var clear_whitespace_symbol := closer_match.get_string(&'clear_whitespace')
			
			add_token(token_type, whole_match)
			consume_source(whole_match.length())
			
			# TBD Is this faster or slower than match ?
			if clear_whitespace_symbol.is_empty():
				pass  # nothing is cool
			elif clear_whitespace_symbol == options.symbol_clear_line_whitespace:
				mark_next_tabspaces_for_clearing()
			elif clear_whitespace_symbol == options.symbol_clear_whitespace:
				mark_next_whitespaces_for_clearing()
			
			return OK

	func mark_next_newline_for_clearing() -> void:
		if not self.tokens.is_empty():
			self.tokens[-1].clear_newline_after = true

	func mark_next_whitespaces_for_clearing() -> void:
		if not self.tokens.is_empty():
			self.tokens[-1].clear_whitespaces_after = true

	func mark_next_tabspaces_for_clearing() -> void:
		if not self.tokens.is_empty():
			self.tokens[-1].clear_tabspaces_after = true

	func tokenize_statement_identifier() -> void:
		var regex_match := self.source_view.rsearch_start(identifier_literal_regex)
		if regex_match == null:
			assert(false, "Not a statement identifier")
		else:
			var whole_match := regex_match.get_string()
			add_token(Token.Types.STATEMENT_IDENTIFIER, whole_match)
			consume_source(whole_match.length())

	func tokenize_expression() -> Error:
		var tokenized_at_least_once := ERR_UNAVAILABLE
		var tokenized := OK
		while tokenized == OK:
			tokenized = tokenize_expression_once()
			if tokenized_at_least_once != OK and tokenized == OK:
				tokenized_at_least_once = OK
			consume_whitespaces_into_previous_token()
		return tokenized_at_least_once

	func tokenize_expression_once() -> Error:
		# The closer symbols may start with characters that are valid expression
		# characters such as %, ~ or -.  Or some other, if they're customized.
		# Therefore, we need this expensive forward scan for closer symbols.
		if (
			scan_regex(self.statement_closer_regex) or
			scan_regex(self.print_closer_regex)
		):
			return ERR_OUT_OF_MEMORY
		
		# Goal: ordered by decreasing usage, yet respecting special precedences?
		if tokenize_literal_boolean() == OK: return OK  # before identifier
		if tokenize_literal_float() == OK: return OK  # before integer
		if tokenize_literal_integer() == OK: return OK
		if tokenize_literal_string() == OK: return OK
		if tokenize_accessor_property() == OK: return OK
		if tokenize_accessor_array() == OK: return OK
		if tokenize_operator_addition() == OK: return OK
		if tokenize_operator_subtraction() == OK: return OK
		if tokenize_operator_multiplication() == OK: return OK
		if tokenize_operator_division() == OK: return OK
		if tokenize_operator_modulo() == OK: return OK
		if tokenize_operator_concatenation() == OK: return OK
		if tokenize_comparator_equality() == OK: return OK
		if tokenize_comparator_inequality() == OK: return OK  # before not
		if tokenize_comparator_comparison() == OK: return OK
		if tokenize_combinator() == OK: return OK  # before identifier
		if tokenize_operator_not() == OK: return OK
		if tokenize_statement_assign() == OK: return OK  # after equality
		if tokenize_group_delimiter() == OK: return OK
		if tokenize_expressions_separator() == OK: return OK
		if tokenize_filter() == OK: return OK
		if tokenize_containor_in() == OK: return OK
		#if tokenize_function_call() == OK: return OK
		if tokenize_literal_identifier() == OK: return OK  # lastly
		return ERR_DOES_NOT_EXIST

	#func tokenize_function_call() -> Error:
		#return ERR_INVALID_DATA

	func tokenize_literal_identifier() -> Error:
		var regex_match := self.source_view.rsearch_start(identifier_literal_regex)
		if regex_match == null:
			return ERR_INVALID_DATA

		var whole_match := regex_match.get_string()
		add_token(Token.Types.LITERAL_IDENTIFIER, whole_match)
		consume_source(whole_match.length())
		return OK

	func tokenize_symbol(symbol: String, token_type: Token.Types) -> Error:
		if not self.source_view.begins_with(symbol):
			return ERR_INVALID_DATA
		add_token(token_type, symbol)
		consume_source(symbol.length())
		return OK

	func tokenize_alphanumerical_symbol(symbol: String, token_type: Token.Types) -> Error:
		# TODO: only compile this once
		var allowed_character_regex := RegEx.create_from_string(
			"[%s]" % options.allowed_runes_in_identifiers,
		)
		
		if not self.source_view.begins_with(symbol):
			return ERR_INVALID_DATA
		
		var has_more_after := self.source_view.length() > symbol.length()
		var after := self.source_view.read_character_at(symbol.length())
		if has_more_after:
			var found_allowed := allowed_character_regex.search(after)
			if found_allowed != null:
				return ERR_LINK_FAILED
		
		add_token(token_type, symbol)
		consume_source(symbol.length())
		return OK

	func tokenize_expressions_separator() -> Error:
		return tokenize_symbol(
			options.symbol_group_separator,
			Token.Types.EXPRESSIONS_SEPARATOR,
		)

	func tokenize_filter() -> Error:
		if tokenize_symbol(
			options.symbol_filter,
			Token.Types.FILTER,
		) == OK:
			consume_whitespaces_into_previous_token()
			if tokenize_filter_identifier() == OK:
				return OK
			else:
				raise_error("Expected a filter identifier after %s" % options.symbol_filter)
				return OK
		return ERR_INVALID_DATA

	func tokenize_filter_identifier() -> Error:
		var regex_match := self.source_view.rsearch_start(identifier_literal_regex)
		if regex_match != null:
			var whole_match := regex_match.get_string()
			add_token(Token.Types.FILTER_IDENTIFIER, whole_match)
			consume_source(whole_match.length())
			return OK
		return ERR_INVALID_DATA

	func tokenize_containor_in() -> Error:
		return tokenize_alphanumerical_symbol(
			options.symbol_containor_in,
			Token.Types.CONTAINOR_IN,
		)

	func tokenize_accessor_property() -> Error:
		return tokenize_symbol(
			options.symbol_accessor_property,
			Token.Types.ACCESSOR_PROPERTY,
		)

	func tokenize_accessor_array() -> Error:
		if OK == tokenize_symbol(
			options.symbol_accessor_array_opener,
			Token.Types.ACCESSOR_ARRAY_OPENER,
		): return OK
		if OK == tokenize_symbol(
			options.symbol_accessor_array_closer,
			Token.Types.ACCESSOR_ARRAY_CLOSER,
		): return OK
		return ERR_INVALID_DATA

	func tokenize_statement_assign() -> Error:
		return tokenize_symbol(
			options.symbol_statement_assign,
			Token.Types.STATEMENT_ASSIGN,
		)

	func tokenize_group_delimiter() -> Error:
		if OK == tokenize_symbol(
			options.symbol_expression_group_opener,
			Token.Types.EXPRESSION_GROUP_OPENER,
		): return OK
		if OK == tokenize_symbol(
			options.symbol_expression_group_closer,
			Token.Types.EXPRESSION_GROUP_CLOSER,
		): return OK
		return ERR_INVALID_DATA

	func tokenize_operator_addition() -> Error:
		return tokenize_symbol(
			options.symbol_operator_addition,
			Token.Types.OPERATOR_ADDITION,
		)

	func tokenize_operator_subtraction() -> Error:
		return tokenize_symbol(
			options.symbol_operator_subtraction,
			Token.Types.OPERATOR_SUBTRACTION,
		)

	func tokenize_operator_multiplication() -> Error:
		return tokenize_symbol(
			options.symbol_operator_multiplication,
			Token.Types.OPERATOR_MULTIPLICATION,
		)

	func tokenize_operator_division() -> Error:
		return tokenize_symbol(
			options.symbol_operator_division,
			Token.Types.OPERATOR_DIVISION,
		)

	func tokenize_operator_modulo() -> Error:
		return tokenize_symbol(
			options.symbol_operator_modulo,
			Token.Types.OPERATOR_MODULO,
		)

	func tokenize_operator_concatenation() -> Error:
		return tokenize_symbol(
			options.symbol_operator_concatenation,
			Token.Types.OPERATOR_CONCATENATION,
		)

	func tokenize_comparator_equality() -> Error:
		return tokenize_symbol(
			options.symbol_comparator_equal,
			Token.Types.COMPARATOR_EQUAL,
		)

	func tokenize_comparator_inequality() -> Error:
		return tokenize_symbol(
			options.symbol_comparator_inequal,
			Token.Types.COMPARATOR_INEQUAL,
		)

	func tokenize_comparator_comparison() -> Error:
		if OK == tokenize_symbol(
			options.symbol_comparator_less_or_equal_than,
			Token.Types.COMPARATOR_LESS_EQUAL,
		): return OK
		if OK == tokenize_symbol(
			options.symbol_comparator_less_than,
			Token.Types.COMPARATOR_LESS,
		): return OK
		if OK == tokenize_symbol(
			options.symbol_comparator_greater_or_equal_than,
			Token.Types.COMPARATOR_GREATER_EQUAL,
		): return OK
		if OK == tokenize_symbol(
			options.symbol_comparator_greater_than,
			Token.Types.COMPARATOR_GREATER,
		): return OK
		return ERR_INVALID_DATA

	func tokenize_combinator() -> Error:
		if tokenize_alphanumerical_symbol(
			options.symbol_combinator_and,
			Token.Types.COMBINATOR_AND,
		) == OK: return OK
		if tokenize_alphanumerical_symbol(
			options.symbol_combinator_or,
			Token.Types.COMBINATOR_OR,
		) == OK: return OK
		if tokenize_alphanumerical_symbol(
			options.symbol_combinator_nand,
			Token.Types.COMBINATOR_NAND,
		) == OK: return OK
		if tokenize_alphanumerical_symbol(
			options.symbol_combinator_xor,
			Token.Types.COMBINATOR_XOR,
		) == OK: return OK
		return ERR_INVALID_DATA

	func tokenize_operator_not() -> Error:
		return tokenize_alphanumerical_symbol(
			options.symbol_operator_not,
			Token.Types.OPERATOR_NOT,
		)

	func tokenize_literal_boolean() -> Error:
		if OK == tokenize_alphanumerical_symbol(
			options.symbol_boolean_true,
			Token.Types.LITERAL_BOOLEAN_TRUE,
		): return OK
		if OK == tokenize_alphanumerical_symbol(
			options.symbol_boolean_false,
			Token.Types.LITERAL_BOOLEAN_FALSE,
		): return OK
		return ERR_INVALID_DATA

	func tokenize_literal_integer() -> Error:
		var regex_match := self.source_view.rsearch_start(integer_literal_regex)
		if regex_match == null:
			return ERR_INVALID_DATA

		var whole_match := regex_match.get_string()
		add_token(Token.Types.LITERAL_INTEGER, whole_match)
		consume_source(whole_match.length())
		return OK

	func tokenize_literal_float() -> Error:
		var regex_match := self.source_view.rsearch_start(float_literal_regex)
		if regex_match == null:
			return ERR_INVALID_DATA

		var whole_match := regex_match.get_string()
		add_token(Token.Types.LITERAL_FLOAT, whole_match)
		consume_source(whole_match.length())
		return OK

	func tokenize_literal_string() -> Error:
		var regex_match := self.source_view.rsearch_start(string_literal_regex)
		if regex_match == null:
			return ERR_INVALID_DATA

		var whole_match := regex_match.get_string()
		add_token(Token.Types.LITERAL_STRING, whole_match)
		consume_source(whole_match.length())
		return OK

	func set_state(value: States) -> void:
		self.state = value

	## WARNING: Always add a token BEFORE consuming the source.
	func add_token(type: Token.Types, literal: String) -> void:
		self.tokens.append(
			Token.new()
			.with_type(type)
			.with_literal(literal)
			.with_lexeme(literal)
			.starting_at(self.source_view.start())
			.starting_at_line(self.source_view.line())
			.ending_at(self.source_view.start() + literal.length())
			.ending_at_line(self.source_view.line() + literal.count('\n'))
		)

	func add_raw_data_token(literal: String) -> void:
		if not self.tokens.is_empty():
			var previous_token := self.tokens[-1]
			if previous_token.clear_newline_after:
				if literal.begins_with("\n"):
					literal = literal.substr(1)
				elif literal.begins_with("\r\n"):
					literal = literal.substr(2)
			if previous_token.clear_tabspaces_after:
				literal = literal.rstrip("\t ")
			if previous_token.clear_whitespaces_after:
				literal = literal.rstrip("\r\n\t ")
		add_token(Token.Types.RAW_DATA, literal)

	func consume_source(amount: int) -> void:
		assert(amount >= 0, "Cannot consume a source negatively. … For now.")
		if amount < 0:
			return
		if amount == 0:  # just to see if/when that happens, remove me at will
			breakpoint
			return
		self.source_view.shrink_from_start_by(amount)

	func consume_whitespaces() -> String:
		var whitespaces_match := self.source_view.rsearch_start(whitespaces_regex)
		if null != whitespaces_match:
			consume_source(whitespaces_match.get_string().length())
			return whitespaces_match.get_string()
		return ""

	func consume_whitespaces_into_previous_token() -> void:
		assert(not self.tokens.is_empty())
		var consumed_whitespaces := consume_whitespaces()
		self.tokens[-1].whitespaces_after += consumed_whitespaces

	var errors: Array[TemplateError] = []
	func raise_error(message: String) -> void:
		if self.tokens:
			var token: Token = self.tokens[-1]
			message += "\n" + "At line %d" % [
				token.starts_in_source_at_line
			]
		var error := TemplateError.new()
		error.message = message
		errors.append(error)
		
		if not options.silence_errors:
			printerr(message)
		if options.break_on_error:
			assert(false, message)

	# No RegEx.escape() support yet, see https://github.com/godotengine/godot-proposals/issues/7995
	func escape_for_regex(input: String) -> String:
		return (
			input
			.replace("\\", "\\\\")
			.replace(".", "\\.")
			.replace("^", "\\^")
			.replace("$", "\\$")
			.replace("*", "\\*")
			.replace("+", "\\+")
			.replace("?", "\\?")
			.replace("(", "\\(")
			.replace(")", "\\)")
			.replace("[", "\\[")
			.replace("]", "\\]")
			.replace("{", "\\{")
			.replace("}", "\\}")
			.replace("<", "\\<")
			.replace(">", "\\>")
			.replace(":", "\\:")
			.replace("|", "\\|")
			.replace("#", "\\#")
		)

#endregion


#region Syntax Nodes

## Ubiquitous element of our (Abstract) Syntax Tree.
class SyntaxNode:
	extends Resource
	@export var children: Array[SyntaxNode] = []
	@export var tokens: Array[Token] = []

	func with_token(token: Token) -> SyntaxNode:
		self.tokens.append(token)
		return self

	func with_tokens(value: Array[Token]) -> SyntaxNode:
		self.tokens.append_array(value)
		return self

	func with_child(value: SyntaxNode) -> SyntaxNode:
		self.children.append(value)
		return self

	func with_children(value: Array[SyntaxNode]) -> SyntaxNode:
		self.children.append_array(value)
		return self

	## You probably want to override this in all expression nodes
	@warning_ignore("unused_parameter")
	func evaluate(context: VisitorContext) -> Variant:
		return ""

	func serialize(context: VisitorContext) -> String:
		return serialize_self(context) + serialize_children(context)

	func serialize_self(context: VisitorContext) -> String:
		return str(evaluate(context))

	func serialize_children(context: VisitorContext) -> String:
		var output := ""
		for child: SyntaxNode in self.children:
			output += child.serialize(context)
		return output


## Our Syntax Tree Root Node.
## There's room for other nodes than the body ; it'll make sense later.
class SyntaxTree:
	extends SyntaxNode
	@export var body: BodyNode

	func serialize(context: VisitorContext) -> String:
		return self.body.serialize(context)

	func with_body(value: BodyNode) -> SyntaxTree:
		self.body = value
		self.children.append(value)
		return self


class BodyNode:
	extends SyntaxNode


class RawDataNode:
	extends SyntaxNode
	@export var data := ""

	func with_data(value: String) -> RawDataNode:
		self.data += value
		return self

	func evaluate(_context: VisitorContext) -> Variant:
		return self.data


class CommentNode:
	extends SyntaxNode


class ExpressionNode:
	extends SyntaxNode


class ExpressionsNode:
	extends SyntaxNode
	func with_expressions(value: Array[SyntaxNode]) -> ExpressionsNode:
		return with_children(value) as ExpressionsNode


class IdentifierLiteralNode:
	extends ExpressionNode
	@export var identifier: String

	static func from_token(token: Token) -> IdentifierLiteralNode:
		assert(
			token.type == Token.Types.LITERAL_IDENTIFIER or
			token.type == Token.Types.FILTER_IDENTIFIER
		)
		var node := IdentifierLiteralNode.new()
		node.identifier = token.lexeme
		node.tokens.append(token)
		return node

	func evaluate(context: VisitorContext) -> Variant:
		if not context.has_variable(self.identifier):
			context.raise_error(
				self, "The variable `%s` is not defined." % identifier,
			)
		return context.get_variable(self.identifier, '')


class BooleanLiteralNode:
	extends ExpressionNode
	@export var value: bool

	static func from_token(token: Token, truth: bool) -> BooleanLiteralNode:
		assert(
			token.type == Token.Types.LITERAL_BOOLEAN_TRUE or
			token.type == Token.Types.LITERAL_BOOLEAN_FALSE
		)
		var node := BooleanLiteralNode.new()
		node.value = truth
		node.tokens.append(token)
		assert(str(node.value) == token.lexeme, "%s != %s" % [
			str(node.value), token.lexeme,
		])
		return node

	func evaluate(_context: VisitorContext) -> Variant:
		return self.value


class IntegerLiteralNode:
	extends ExpressionNode
	@export var value: int

	static func from_token(token: Token) -> IntegerLiteralNode:
		assert(token.type == Token.Types.LITERAL_INTEGER)
		assert(token.lexeme.length() >= 1)
		var node := IntegerLiteralNode.new()
		if token.lexeme.to_lower().begins_with('0x'):
			node.value = token.lexeme.hex_to_int()
		elif token.lexeme.to_lower().begins_with('0b'):
			node.value = token.lexeme.bin_to_int()
		else:
			node.value = int(token.lexeme)
		node.tokens.append(token)
		return node

	func evaluate(_context: VisitorContext) -> Variant:
		return self.value


class FloatLiteralNode:
	extends ExpressionNode
	@export var value: float

	static func from_token(token: Token) -> FloatLiteralNode:
		assert(token.type == Token.Types.LITERAL_FLOAT)
		assert(token.lexeme.length() >= 2)
		var node := FloatLiteralNode.new()
		node.value = float(token.lexeme)
		node.tokens.append(token)
		return node

	func with_value(some_value: float) -> FloatLiteralNode:
		self.value = some_value
		return self

	func evaluate(_context: VisitorContext) -> Variant:
		return self.value


class StringLiteralNode:
	extends ExpressionNode
	@export var value: String

	static func from_token(token: Token) -> StringLiteralNode:
		assert(token.type == Token.Types.LITERAL_STRING)
		assert(token.lexeme.length() >= 2)
		var node := StringLiteralNode.new()
		node.value = token.lexeme.substr(1, token.lexeme.length() - 2)
		node.value = node.value.replace("\\\"", "\"")
		node.value = node.value.replace("\\\\", "\\")
		node.tokens.append(token)
		return node

	func evaluate(_context: VisitorContext) -> Variant:
		return self.value


class OperatorNode:
	extends ExpressionNode
	func serialize(context: VisitorContext) -> String:
		return serialize_self(context)


@abstract
class UnaryOperatorNode:
	extends OperatorNode
	func evaluate_unary(_operand: Variant) -> Variant:
		breakpoint  # you MUST override me !
		return ""
	func evaluate(context: VisitorContext) -> Variant:
		assert(self.children.size() == 1)
		return evaluate_unary(self.children[0].evaluate(context))


class PositiveUnaryOperatorNode:
	extends UnaryOperatorNode
	func evaluate_unary(operand: Variant) -> Variant:
		return +operand


class NegativeUnaryOperatorNode:
	extends UnaryOperatorNode
	func evaluate_unary(operand: Variant) -> Variant:
		return -operand


class NotUnaryOperatorNode:
	extends UnaryOperatorNode
	func evaluate_unary(operand: Variant) -> Variant:
		return !operand


@abstract
class BinaryOperatorNode:
	extends OperatorNode

	@abstract
	#@warning_ignore("unused_parameter")
	func evaluate_binary(left: Variant, right: Variant) -> Variant

	func evaluate(context: VisitorContext) -> Variant:
		assert(self.children.size() >= 2)
		return evaluate_binary(
			self.children[0].evaluate(context),
			self.children[1].evaluate(context),
		)


class AdditionOperatorNode:
	extends BinaryOperatorNode
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		return left + right


class SubtractionOperatorNode:
	extends BinaryOperatorNode
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		return left - right


class MultiplicationOperatorNode:
	extends BinaryOperatorNode
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		return left * right


class DivisionOperatorNode:
	extends BinaryOperatorNode
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		return left / right


class ModuloOperatorNode:
	extends BinaryOperatorNode
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		if (left is int) and (right is int):
			return left % right
		return fmod(float(left), float(right))


class ConcatenationOperatorNode:
	extends BinaryOperatorNode
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		return str(left) + str(right)


@abstract
class BinaryComparatorNode:
	extends BinaryOperatorNode


class EqualityComparatorNode:
	extends BinaryComparatorNode
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		return left == right


class InequalityComparatorNode:
	extends BinaryComparatorNode
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		return left != right


class LessOrEqualComparatorNode:
	extends BinaryComparatorNode
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		return left <= right


class LessComparatorNode:
	extends BinaryComparatorNode
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		return left < right


class GreaterOrEqualComparatorNode:
	extends BinaryComparatorNode
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		return left >= right


class GreaterComparatorNode:
	extends BinaryComparatorNode
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		return left > right


class CombinatorNode:
	extends BinaryOperatorNode
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		assert(self.tokens.size() == 1)
		match self.tokens[0].type:
			Token.Types.COMBINATOR_AND:
				return left and right
			Token.Types.COMBINATOR_OR:
				return left or right
			Token.Types.COMBINATOR_NAND:
				return not (left and right)
			Token.Types.COMBINATOR_XOR:
				return bool(left) == bool(right)
		breakpoint  # did ya start implementing a new combinator?
		return false


class PropertyAccessorNode:
	extends ExpressionNode
	
	@export var object: ExpressionNode
	@export var property: IdentifierLiteralNode
	func with_object(value: ExpressionNode) -> PropertyAccessorNode:
		self.object = value
		self.children.push_front(value)
		return self
	func with_property(value: ExpressionNode) -> PropertyAccessorNode:
		self.property = value
		self.children.push_back(value)
		return self

	func evaluate(context: VisitorContext) -> Variant:
		var obj: Variant = self.object.evaluate(context)
		var prop: String = self.property.identifier
		return obj.get(prop)

	func serialize(context: VisitorContext) -> String:
		return serialize_self(context)


class ElementAccessorNode:
	extends ExpressionNode
	
	@export var subject: ExpressionNode
	@export var key: ExpressionNode
	func with_subject(value: ExpressionNode) -> ElementAccessorNode:
		self.subject = value
		self.children.push_front(value)
		return self
	func with_key(value: ExpressionNode) -> ElementAccessorNode:
		self.key = value
		self.children.push_back(value)
		return self

	func evaluate(context: VisitorContext) -> Variant:
		var subject_value: Variant = self.subject.evaluate(context)
		var key_value: Variant = self.key.evaluate(context)
		return subject_value[key_value]

	func serialize(context: VisitorContext) -> String:
		return serialize_self(context)


class PrintNode:
	extends SyntaxNode
	@export var expression: ExpressionNode

	func with_expression(value: ExpressionNode) -> PrintNode:
		self.expression = value
		self.children.append(value)
		return self

	func evaluate(context: VisitorContext) -> Variant:
		return self.expression.evaluate(context)

	func serialize(context: VisitorContext) -> String:
		return serialize_self(context)


class FilterNode:
	extends ExpressionNode
	@export var extension: FilterExtension
	@export var subject: ExpressionNode
	@export var parameters: ExpressionsNode

	func with_extension(value: FilterExtension) -> FilterNode:
		self.extension = value
		return self

	func with_subject(value: ExpressionNode) -> FilterNode:
		self.subject = value
		self.children.push_front(value)
		return self

	func with_parameters(value: ExpressionsNode) -> FilterNode:
		self.parameters = value
		self.children.push_back(value)
		return self

	func evaluate(context: VisitorContext) -> Variant:
		return self.extension.evaluate(context, self)

	func serialize(context: VisitorContext) -> String:
		return serialize_self(context)


class StatementNode:
	extends SyntaxNode
	@export var extension: StatementExtension

	func with_extension(value: StatementExtension) -> StatementNode:
		self.extension = value
		return self

	func evaluate(context: VisitorContext) -> Variant:
		return self.extension.evaluate(context, self)

	func serialize(context: VisitorContext) -> String:
		return serialize_self(context)

#endregion


#region Filter Extensions

## Write your own filters by extending and implementing this abstract class.
@abstract
class FilterExtension:
	extends Resource

	## Used mostly for documentation purposes.
	func get_identifier() -> String:
		breakpoint  # override me !
		return ''

	## Apply the logic of the filter to the node's evaluated subject and return.
	@warning_ignore("unused_parameter")
	func evaluate(context: VisitorContext, node: FilterNode) -> Variant:
		breakpoint  # override me !
		return ""

	## Used to find the appropriate filter extension from the filter identifier.
	func matches_identifier(id_to_match: String) -> bool:
		return id_to_match == get_identifier()


## Converts to absolute value, like Godot's abs().
class AbsFilterExtension:
	extends FilterExtension
	func get_identifier() -> String:	return 'abs'

	func evaluate(context: VisitorContext, node: FilterNode) -> Variant:
		return abs(node.subject.evaluate(context))


## Capitalizes the subject.
class CapitalizeFilterExtension:
	extends FilterExtension
	func get_identifier() -> String:	return 'capitalize'

	func evaluate(context: VisitorContext, node: FilterNode) -> Variant:
		return str(node.subject.evaluate(context)).capitalize()


## Converts the subject to lowercase.
class LowercaseFilterExtension:
	extends FilterExtension
	func get_identifier() -> String:	return 'lowercase'

	func evaluate(context: VisitorContext, node: FilterNode) -> Variant:
		return str(node.subject.evaluate(context)).to_lower()


## Short alias for "lowercase"
class LowerFilterExtension:
	extends LowercaseFilterExtension
	func get_identifier() -> String:	return 'lower'


## Rounds the subject.
class RoundFilterExtension:
	extends FilterExtension
	func get_identifier() -> String:	return 'round'

	func evaluate(context: VisitorContext, node: FilterNode) -> Variant:
		var value: float = node.subject.evaluate(context)
		var precision := 0

		var method := 'common'
		if node.parameters:
			if node.parameters.children.size() > 0:
				precision = int(node.parameters.children[0].evaluate(context))
			if node.parameters.children.size() > 1:
				method = str(node.parameters.children[1].evaluate(context))

		var shift := pow(10.0, precision);
		match method:
			'floor':
				value = floorf(value * shift) / shift
			'ceil':
				value = ceilf(value * shift) / shift
			'common':
				value = roundf(value * shift) / shift
			_:
				breakpoint  # unknown rounding method
				value = roundf(value * shift) / shift

		if precision == 0:
			return int(value)

		return value


## Converts the subject to uppercase.
class UppercaseFilterExtension:
	extends FilterExtension
	func get_identifier() -> String:	return 'uppercase'

	func evaluate(context: VisitorContext, node: FilterNode) -> Variant:
		return str(node.subject.evaluate(context)).to_upper()


## Short alias for "uppercase"
class UpperFilterExtension:
	extends UppercaseFilterExtension
	func get_identifier() -> String:	return 'upper'

#endregion


#region Statement Extensions

## Extend this to implement a new statement.
## Then append an instance of it to StringEngine.statement_extensions
class StatementExtension:
	extends Resource

	func get_identifier() -> String:
		breakpoint  # override me !
		return ''

	## When this method is called by the parser, the opener and identifier
	## tokens have been consumed already: {% <identifier> <we are here> … %} … …
	@warning_ignore("unused_parameter")
	func parse(
		parser: Parser,
		context: ParserContext,
	) -> StatementNode:
		breakpoint  # override me !
		#var opener_token := get_opener_token(context)
		#var identifier_token := get_identifier_token(context)
		var _closer_token := context.consume_type(Token.Types.STATEMENT_CLOSER)
		return (StatementNode.new().with_extension(self))

	@warning_ignore("unused_parameter")
	func evaluate(context: VisitorContext, node: StatementNode) -> Variant:
		return ""  # you probably want to override me
	
	func serialize(context: VisitorContext, node: StatementNode) -> String:
		return str(evaluate(context, node))

	func get_opener_token(context: ParserContext) -> Token:
		return context.get_current_token(-2)
	func get_identifier_token(context: ParserContext) -> Token:
		return context.get_current_token(-1)

	func matches_identifier(id_to_match: String) -> bool:
		return id_to_match == get_identifier()


class IfElseStatementExtension:
	extends StatementExtension

	func get_identifier() -> String:	return 'if'

	func parse(
		parser: Parser,
		context: ParserContext,
	) -> StatementNode:
		# The engine has already parsed the statement opener and identifier.
		# The rest is up to us, now.
		# {% if <some_condition> %} … {% else %} … {% end %}
		var if_identifier_token := context.get_previous_token()
		
		var condition := parser.parse_expression(context)
		context.consume_type(Token.Types.STATEMENT_CLOSER)

		var detect_else := context.detect_statement('else')
		var detect_end := context.detect_statement('end' + get_identifier())
		var detect_else_or_end := func(pc: ParserContext) -> bool:
			return detect_else.call(pc) or detect_end.call(pc)

		var then_node := SyntaxNode.new()
		parser.parse_tokens_until(
			context,
			then_node,
			detect_else_or_end,
		)
		
		var else_node: SyntaxNode
		var found_else := detect_else.call(context) as bool
		if found_else:
			context.consume_some_tokens(3)  # {% else %}
			else_node = SyntaxNode.new()
			parser.parse_tokens_until(context, else_node, detect_end)
		
		var found_end := detect_end.call(context) as bool
		if found_end:
			context.consume_some_tokens(3)  # {% endif %}
		else:
			parser.raise_error(
				context,
				"Expected an {%% %s %%} at some point to close the {%% %s … %%} found at line %d, but did not find it." % [
					'end' + get_identifier(),
					get_identifier(),
					if_identifier_token.starts_in_source_at_line,
				]
			)

		var if_node := (
			StatementNode
			.new()
			.with_extension(self)
			.with_child(condition)
			.with_child(then_node)
		) as StatementNode
		if else_node:
			if_node.with_child(else_node)

		return if_node

	func evaluate(context: VisitorContext, node: StatementNode) -> Variant:
		var condition := node.children[0] as ExpressionNode
		var then_node := node.children[1] as SyntaxNode
		var condition_evaluated: Variant = condition.evaluate(context)
		if condition_evaluated:
			return then_node.serialize(context)
		elif node.children.size() == 3:
			var else_node := node.children[2] as SyntaxNode
			return else_node.serialize(context)
		return ""


class SetStatementExtension:
	extends StatementExtension
	func get_identifier() -> String:	return 'set'

	func parse(
		parser: Parser,
		context: ParserContext,
	) -> StatementNode:
		var variable_identifier := parser.parse_literal_identifier(context)
		context.consume_type(Token.Types.STATEMENT_ASSIGN)
		var value := parser.parse_expression(context)
		context.consume_type(Token.Types.STATEMENT_CLOSER)
		
		return (
			StatementNode.new()
			.with_extension(self)
			.with_child(variable_identifier)
			.with_child(value)
		) as StatementNode
	
	func evaluate(context: VisitorContext, node: StatementNode) -> Variant:
		var identifier = (node.children[0] as IdentifierLiteralNode).identifier
		var value = (node.children[1] as ExpressionNode).evaluate(context)
		# TBD: I'm not sure about the behavior we want here ; local set for now
		context.set_variable(identifier, value)
		return ""


class LoopVariable:
	extends Resource
	var index := 1
	var index0 := 0


class ForStatementExtension:
	extends StatementExtension
	func get_identifier() -> String:	return 'for'

	func parse(parser: Parser, context: ParserContext) -> StatementNode:
		var for_identifier_token := context.get_previous_token()
		var element := parser.parse_literal_identifier(context)
		context.consume_type(Token.Types.CONTAINOR_IN)
		var iterable := parser.parse_expression(context)
		context.consume_type(Token.Types.STATEMENT_CLOSER)
		
		var detect_else := context.detect_statement('else')
		var detect_end := context.detect_statement('end' + get_identifier())
		var detect_else_or_end := func(pc: ParserContext) -> bool:
			return detect_else.call(pc) or detect_end.call(pc)

		var for_body := SyntaxNode.new()
		parser.parse_tokens_until(context, for_body, detect_else_or_end)
		
		var else_body := SyntaxNode.new()
		var found_else := detect_else.call(context) as bool
		if found_else:
			context.consume_some_tokens(3)  # {% else %}
			parser.parse_tokens_until(context, else_body, detect_end)
		
		var found_end := detect_end.call(context) as bool
		if found_end:
			context.consume_some_tokens(3)  # {% endfor %}
		else:
			parser.raise_error(
				context,
				"Expected a %s %s %s at some point to close the %s %s … %s found at line %d, but did not find it." % [
					parser.options.symbol_statement_opener,
					'end' + get_identifier(),
					parser.options.symbol_statement_closer,
					parser.options.symbol_statement_opener,
					get_identifier(),
					parser.options.symbol_statement_closer,
					for_identifier_token.starts_in_source_at_line,
				]
			)
		
		return (
			StatementNode.new()
			.with_extension(self)
			.with_child(element)
			.with_child(iterable)
			.with_child(for_body)
			.with_child(else_body)
		) as StatementNode

	func evaluate(context: VisitorContext, node: StatementNode) -> Variant:
		var loop_variable := LoopVariable.new()
		context.push_variables_stack({&'loop': loop_variable})
		var output := ""
		var loop_index0 := 0
		var loops_left := 1_000_000  # safeguard against l∞∞ps
		var element_node := node.children[0] as IdentifierLiteralNode
		var iterable_node := node.children[1] as ExpressionNode
		var for_body := node.children[2] as SyntaxNode
		var else_body := node.children[3] as SyntaxNode
		var iterable_value = iterable_node.evaluate(context)  # as Iterable
		if iterable_value:
			for thing in iterable_value:
				context.set_variable(element_node.identifier, thing)
				output += for_body.serialize(context)
				loops_left -= 1
				if loops_left < 0:
					break
				loop_index0 = loop_index0 + 1
				loop_variable.index += 1
				loop_variable.index0 += 1
			if loops_left < 0:
				push_error("A possible infinite loop (%d+ iterations) in a for statement was aborted." % [loop_index0+1])
				breakpoint
		context.pop_variables_stack()
		if loop_variable.index0 == 0 and else_body:
			output += else_body.serialize(context)
		return output


class VerbatimStatementExtension:
	extends StatementExtension

	func get_identifier() -> String:	return 'verbatim'

	func parse(
		_parser: Parser,
		context: ParserContext,
	) -> StatementNode:
		context.consume_type(Token.Types.STATEMENT_CLOSER)
		# TODO: less awkwardness please
		var content_tokens := context.consume_until(
			func(token_index: int):
				return (
					context.tokens[token_index].type == Token.Types.STATEMENT_OPENER
					and
					context.tokens[token_index+1].type == Token.Types.STATEMENT_IDENTIFIER
					and
					context.tokens[token_index+1].lexeme == 'end' + get_identifier()
					and
					context.tokens[token_index+2].type == Token.Types.STATEMENT_CLOSER
				)
		)
		context.consume_some_tokens(3)  # {% end<statement_identifier> %}

		# TBD: how to type this ?
		var content: String = content_tokens.reduce(
			func(acc: String, tk: Token) -> String:
				return (acc + tk.whitespaces_before + tk.literal + tk.whitespaces_after) as String
				,
			""
		) as String

		return (
			StatementNode.new()
			.with_extension(self)
			.with_child(RawDataNode.new().with_data(content))
		) as StatementNode

	func evaluate(context: VisitorContext, node: StatementNode) -> String:
		if node.children.is_empty():
			return ""
		assert(node.children.size() == 1, "Verbatim has gliiitcHHe…d")
		return node.children[0].serialize(context)


class WhileStatementExtension:
	extends StatementExtension
	func get_identifier() -> String:	return 'while'

	func parse(
		parser: Parser,
		context: ParserContext,
	) -> StatementNode:
		var condition := parser.parse_expression(context)
		context.consume_type(Token.Types.STATEMENT_CLOSER)
		
		var detect_end := context.detect_statement('end' + get_identifier())
		var while_body := SyntaxNode.new()
		parser.parse_tokens_until(
			context,
			while_body,
			detect_end,
		)
		context.consume_some_tokens(3)
		
		return (
			StatementNode.new()
			.with_extension(self)
			.with_child(condition)
			.with_child(while_body)
		) as StatementNode

	func evaluate(context: VisitorContext, node: StatementNode) -> Variant:
		var loops_left := 1_000_000
		var condition := node.children[0] as ExpressionNode
		var body := node.children[1] as SyntaxNode
		var output := ""
		#while condition.evaluate(context) and loops_left > 0:  # untyped (?)
		while condition.evaluate(context):
			output += body.serialize(context)
			loops_left -= 1
			if loops_left <= 0:
				break
		if loops_left == 0:
			push_error("Possible infinite l∞∞p!")
			breakpoint
		return output


#endregion


#region Parsing

## Holds the context of a parser's run.
## I thought we'd have to backtrack and therefore copy a parser context around,
## but we don't backtrack, and therefore this is somewhat redundant with Parser.
## Not sure if we'll ever end up backtracking…
## This makes the API in statement extensions horrible.  Let's work on it.
class ParserContext:
	extends Resource
	## All statements are implemented via Statement Extensions.
	var statement_extensions: Array[StatementExtension] = []
	## All filters are implemented via Filter Extensions.
	var filter_extensions: Array[FilterExtension] = []
	## The parser instance this context is pertaining to.
	var parser: Parser
	## Our stream of tokens we're parsing.
	var tokens: Array[Token]
	## Our current cursor index in the stream of tokens above.
	## We would not need this if we used a TokenStream.
	var current_token_index := 0

	func with_parser(value: Parser) -> ParserContext:
		self.parser = value
		return self

	func with_tokens(value: Array[Token]) -> ParserContext:
		self.tokens = value
		return self

	func with_statement_extensions(value: Array[StatementExtension]) -> ParserContext:
		self.statement_extensions.append_array(value)
		return self

	func with_filter_extensions(value: Array[FilterExtension]) -> ParserContext:
		self.filter_extensions.append_array(value)
		return self

	func get_statement_extension(identifier: String) -> StatementExtension:
		var i := self.statement_extensions.find_custom(
			func(se: StatementExtension):
				return se.matches_identifier(identifier)
		)
		assert(i >= 0, "No statement extension found for `%s`." % identifier)
		return self.statement_extensions[i]

	func get_filter_extension(identifier: String) -> FilterExtension:
		var i := self.filter_extensions.find_custom(
			func(fe: FilterExtension):
				return fe.matches_identifier(identifier)
		)
		if i >= 0:
			return self.filter_extensions[i]
		raise_error("No filter extension found for `%s`." % [
			identifier,
		], get_current_token())
		return null

	func has_tokens_remaining(at_least := 1) -> bool:
		return (
			self.current_token_index + at_least
			<=
			self.tokens.size()
		)

	## Scans the current token and returns whether it matches the type.
	func scan_type(type: Token.Types, offset := 0) -> bool:
		return get_current_token(offset).type == type

	## Scans the current token and consumes it if it matches the type.
	func match_type(type: Token.Types) -> bool:
		if not has_tokens_remaining():
			return false
		var matched := get_current_token().type == type
		if matched:
			consume_token()
		return matched

	func get_current_token(offset := 0) -> Token:
		var requested_index := self.current_token_index + offset
		if requested_index >= self.tokens.size():
			return Token.new().with_type(Token.Types.EOF)
		if requested_index < 0:
			breakpoint
			return Token.new()
		return self.tokens[self.current_token_index + offset]

	func get_previous_token() -> Token:
		return get_current_token(-1)

	func consume_current_token() -> Token:
		return consume_token()

	func consume_token() -> Token:
		self.current_token_index += 1
		return get_previous_token()

	func consume_some_tokens(amount: int) -> void:
		assert(amount >= 0)
		self.current_token_index += amount

	## Consumes and returns a token of the provided type, or yells.
	func consume_type(type: Token.Types) -> Token:
		if get_current_token().type == type:
			return consume_token()
		else:
			assert(false, "Expected a token of type %s but got %s" % [
				type, get_current_token().type
			])
			return null  # ErrorToken ?

	#func consume_until_type(type: Token.Types) -> Array[Token]:
		#var consumed: Array[Token] = []
		#while has_tokens_remaining():
			#var token := consume_current_token()
			#if token.type == type:
				#break
			#consumed.append(token)
		#return consumed

	func consume_until(condition: Callable) -> Array[Token]:
		var consumed: Array[Token] = []
		var found := false
		var cursor := -1
		while not found:
			cursor += 1
			if self.current_token_index + cursor >= self.tokens.size():
				raise_error("Did not find the end condition.")
				break
			found = condition.call(self.current_token_index + cursor)
		consumed.append_array(self.tokens.slice(self.current_token_index, self.current_token_index + cursor))
		consume_some_tokens(cursor)
		return consumed

	func sequence_of_types(types: Array[Token.Types]) -> Callable:
		return (
			func(context: ParserContext) -> bool:
				var matches_all := true
				for i in types.size():
					if context.get_current_token(i).type != types[i]:
						matches_all = false
						break
				return matches_all
		)

	func detect_statement(identifier: String) -> Callable:
		return (
			func(context: ParserContext) -> bool:
				return (
					context.get_current_token(0).type == Token.Types.STATEMENT_OPENER and
					context.get_current_token(1).type == Token.Types.STATEMENT_IDENTIFIER and
					context.get_current_token(1).lexeme == identifier
				)
		)

	func raise_error(
		message: String,
		token: Token = null,
	):
		parser.raise_error(self, message, token)


class Parser:
	extends RefCounted

	var options: Options
	var errors: Array[TemplateError] = []

	func _init(some_options := Options.new()) -> void:
		self.options = some_options

	func parse(
		tokens: Array[Token],
		statement_extensions: Array[StatementExtension],
		filter_extensions: Array[FilterExtension],
	) -> SyntaxTree:
		var body := BodyNode.new()
		var tree := SyntaxTree.new().with_body(body)
		var context := (
			ParserContext.new()
			.with_parser(self)
			.with_tokens(tokens)
			.with_filter_extensions(filter_extensions)
			.with_statement_extensions(statement_extensions)
		)

		parse_tokens_until(
			context, body,
			func(ctx: ParserContext): return not ctx.has_tokens_remaining(),
		)

		return tree

	## Adds the parsed nodes as child of into_parent.
	## We do this instead of returning the nodes to shave an array creation.
	func parse_tokens_until(
		context: ParserContext,
		into_parent: SyntaxNode,
		until: Callable,
	) -> void:
		while context.has_tokens_remaining() and not until.call(context):
			var node: SyntaxNode = parse_token(context)
			into_parent.children.append(node)

	func parse_token(context: ParserContext) -> SyntaxNode:
		var token_index = context.current_token_index
		var token: Token = context.consume_current_token()
		match token.type:
			Token.Types.RAW_DATA:
				return RawDataNode.new().with_data(token.lexeme).with_token(token)
			Token.Types.PRINT_OPENER:
				var expression: ExpressionNode = parse_expression(context)
				if not context.match_type(Token.Types.PRINT_CLOSER):
					var wrong_source := ""
					for i in range(token_index, context.current_token_index):
						wrong_source += str(context.tokens[i].literal)
					if context.has_tokens_remaining():
						raise_error(context, "Expected a print closer `%s` after `%s`, but found `%s`" % [
							options.symbol_print_closer,
							wrong_source,
							context.get_current_token(),
						])
					else:
						raise_error(context, "Expected a print closer `%s` after `%s`, but found the end of the document." % [
							options.symbol_print_closer,
							wrong_source,
						])

				var print_tokens: Array[Token] = []  # FIXME
				#print_tokens.append(token)
				#print_tokens.append_array(tokens_subset)
				#print_tokens.append(context.get_current_token(-1))
				return PrintNode.new().with_expression(expression).with_tokens(print_tokens)
			Token.Types.STATEMENT_OPENER:
				var identifier_token := context.consume_current_token()
				assert(identifier_token.type == Token.Types.STATEMENT_IDENTIFIER, "Expected statement identifier")
				var statement_extension := context.get_statement_extension(identifier_token.lexeme)
				return statement_extension.parse(self, context)
			Token.Types.COMMENT_OPENER:
				context.consume_some_tokens(2)
				return CommentNode.new()
			#_:
				#breakpoint  # implement your new "main" token type !

		# Somewhat safe fallback ; should not happen, unless you hack around.
		return RawDataNode.new().with_data("")

	# Stratified Grammar for Expressions
	# ----------------------------------
	# EXPRESSIONS = EXPRESSION ( "," EXPRESSION )*
	# EXPRESSION = COMBINATION
	# COMBINATION = EQUALITY ( ( "and" | "or" ) COMBINATION )?
	# EQUALITY = COMPARISON ( ( "!=" | "==" ) COMPARISON )?
	# COMPARISON = ADDITION ( ( "<=" | "<" | ">=" | ">" ) ADDITION )*
	# ADDITION = MULTIPLICATION ( ( "+" | "-" ) ADDITION )*
	# MULTIPLICATION = MODULO ( ( "*" | "/" ) MULTIPLICATION )*
	# MODULO = FILTER ( "%" FILTER )*
	# FILTER = UNARY ( "|" FILTER_IDENTIFIER ( "(" EXPRESSIONS ")" )? )*
	# UNARY = ( "+" | "-" | "!" )? ACCESS_PROPERTY
	# ACCESS_PROPERTY = ACCESS_ELEMENT ( "." LITERAL_IDENTIFIER )*
	# ACCESS_ELEMENT = PRIMARY ( "[" EXPRESSION "]" )*
	# PRIMARY = LITERAL
	#         | LITERAL_IDENTIFIER
	#         | "(" EXPRESSION ")"
	func parse_expressions(context: ParserContext) -> ExpressionsNode:
		var expressions_nodes: Array[SyntaxNode] = [parse_expression(context)]
		while context.match_type(Token.Types.EXPRESSIONS_SEPARATOR):
			expressions_nodes.append(parse_expression(context))
		return ExpressionsNode.new().with_expressions(expressions_nodes)

	func parse_expression(context: ParserContext) -> ExpressionNode:
		assert(context.has_tokens_remaining(), "Expected an expression, but got nothing.")
		return parse_combination(context)

	func parse_combination(context: ParserContext) -> ExpressionNode:
		var node := parse_equality(context)
		if (
			context.match_type(Token.Types.COMBINATOR_AND) or
			context.match_type(Token.Types.COMBINATOR_OR) or
			context.match_type(Token.Types.COMBINATOR_NAND) or
			context.match_type(Token.Types.COMBINATOR_XOR)
		):
			node = (
				CombinatorNode.new()
				.with_child(node)
				.with_child(parse_combination(context))
				.with_token(context.get_previous_token())  # /!. LAST! /!.
			) as CombinatorNode
		return node

	func parse_equality(context: ParserContext) -> ExpressionNode:
		var node := parse_comparison(context)
		if (
			context.match_type(Token.Types.COMPARATOR_EQUAL) or
			context.match_type(Token.Types.COMPARATOR_INEQUAL)
		):
			match context.get_previous_token().type:
				Token.Types.COMPARATOR_EQUAL:
					node = (
						EqualityComparatorNode.new()
						.with_child(node)
						.with_child(parse_comparison(context))
					) as EqualityComparatorNode
				Token.Types.COMPARATOR_INEQUAL:
					node = (
						InequalityComparatorNode.new()
						.with_child(node)
						.with_child(parse_comparison(context))
					) as InequalityComparatorNode
		return node

	func parse_comparison(context: ParserContext) -> ExpressionNode:
		var node := parse_concatenation(context)
		if (
			context.match_type(Token.Types.COMPARATOR_LESS) or
			context.match_type(Token.Types.COMPARATOR_LESS_EQUAL) or
			context.match_type(Token.Types.COMPARATOR_GREATER) or
			context.match_type(Token.Types.COMPARATOR_GREATER_EQUAL)
		):
			match context.get_previous_token().type:
				Token.Types.COMPARATOR_LESS:
					node = (
						LessComparatorNode.new()
						.with_child(node)
						.with_child(parse_comparison(context))
					) as LessComparatorNode
				Token.Types.COMPARATOR_LESS_EQUAL:
					node = (
						LessOrEqualComparatorNode.new()
						.with_child(node)
						.with_child(parse_comparison(context))
					) as LessOrEqualComparatorNode
				Token.Types.COMPARATOR_GREATER:
					node = (
						GreaterComparatorNode.new()
						.with_child(node)
						.with_child(parse_comparison(context))
					) as GreaterComparatorNode
				Token.Types.COMPARATOR_GREATER_EQUAL:
					node = (
						GreaterOrEqualComparatorNode.new()
						.with_child(node)
						.with_child(parse_comparison(context))
					) as GreaterOrEqualComparatorNode
		return node

	func parse_concatenation(context: ParserContext) -> ExpressionNode:
		var node := parse_addition(context)
		if context.match_type(Token.Types.OPERATOR_CONCATENATION):
			node = (
				ConcatenationOperatorNode.new()
				.with_child(node)
				.with_child(parse_concatenation(context))
			) as ConcatenationOperatorNode
		return node

	func parse_addition(context: ParserContext) -> ExpressionNode:
		var node := parse_multiplication(context)
		if (
			context.match_type(Token.Types.OPERATOR_ADDITION) or
			context.match_type(Token.Types.OPERATOR_SUBTRACTION)
		):
			match context.get_previous_token().type:
				Token.Types.OPERATOR_ADDITION:
					node = (
						AdditionOperatorNode.new()
						.with_child(node)
						.with_child(parse_addition(context))
					) as AdditionOperatorNode
				Token.Types.OPERATOR_SUBTRACTION:
					node = (
						SubtractionOperatorNode.new()
						.with_child(node)
						.with_child(parse_addition(context))
					) as SubtractionOperatorNode
		return node

	func parse_multiplication(context: ParserContext) -> ExpressionNode:
		var node := parse_modulo(context)
		if (
			context.match_type(Token.Types.OPERATOR_MULTIPLICATION) or
			context.match_type(Token.Types.OPERATOR_DIVISION)
		):
			match context.get_previous_token().type:
				Token.Types.OPERATOR_MULTIPLICATION:
					node = (
						MultiplicationOperatorNode.new()
						.with_child(node)
						.with_child(parse_multiplication(context))
					) as MultiplicationOperatorNode
				Token.Types.OPERATOR_DIVISION:
					node = (
						DivisionOperatorNode.new()
						.with_child(node)
						.with_child(parse_multiplication(context))
					) as DivisionOperatorNode
		return node

	func parse_modulo(context: ParserContext) -> ExpressionNode:
		var node := parse_filter(context)
		if context.match_type(Token.Types.OPERATOR_MODULO):
			node = (
				ModuloOperatorNode.new()
				.with_child(node)
				.with_child(parse_modulo(context))
			) as ModuloOperatorNode
		return node

	func parse_filter(context: ParserContext) -> ExpressionNode:
		var node := parse_unary(context)
		while context.match_type(Token.Types.FILTER):
			var filter_id_node := parse_filter_identifier(context)
			var filter_id := filter_id_node.tokens[0].lexeme
			var filter_extension := context.get_filter_extension(filter_id)
			assert(filter_extension)

			var filter_node := FilterNode.new()
			filter_node.with_subject(node)
			filter_node.with_extension(filter_extension)
			if context.match_type(Token.Types.EXPRESSION_GROUP_OPENER):
				filter_node.with_parameters(parse_expressions(context))
				context.consume_type(Token.Types.EXPRESSION_GROUP_CLOSER)
			node = filter_node
		return node

	func parse_unary(context: ParserContext) -> ExpressionNode:
		var node: ExpressionNode
		if (
			context.match_type(Token.Types.OPERATOR_NOT) or
			context.match_type(Token.Types.OPERATOR_ADDITION) or
			context.match_type(Token.Types.OPERATOR_SUBTRACTION)
		):
			match context.get_previous_token().type:
				Token.Types.OPERATOR_NOT:
					node = NotUnaryOperatorNode.new()
				Token.Types.OPERATOR_ADDITION:
					node = PositiveUnaryOperatorNode.new()
				Token.Types.OPERATOR_SUBTRACTION:
					node = NegativeUnaryOperatorNode.new()

		if node:
			node.with_child(parse_accessor_property(context))
			node.with_token(context.get_previous_token())
		else:
			node = parse_accessor_property(context)

		return node

	func parse_accessor_property(context: ParserContext) -> ExpressionNode:
		var node := parse_accessor_element(context)
		while context.match_type(Token.Types.ACCESSOR_PROPERTY):
			node = (
				PropertyAccessorNode.new()
				.with_object(node)
				.with_property(parse_literal_identifier(context))
			)
		return node
	
	func parse_accessor_element(context: ParserContext) -> ExpressionNode:
		var node := parse_primary(context)
		while context.match_type(Token.Types.ACCESSOR_ARRAY_OPENER):
			node = (
				ElementAccessorNode.new()
				.with_subject(node)
				.with_key(parse_expression(context))
			)
			context.consume_type(Token.Types.ACCESSOR_ARRAY_CLOSER)
		return node
	
	func parse_primary(context: ParserContext) -> ExpressionNode:
		if context.get_current_token().type == Token.Types.EXPRESSION_GROUP_OPENER:
			context.consume_token()
			var inside := parse_expression(context)
			if not context.match_type(Token.Types.EXPRESSION_GROUP_CLOSER):
				raise_error(context, "Expected `)` but got `%s`." % [
					context.get_current_token(),
				])
			return inside
		return parse_literal(context)

	func parse_literal(context: ParserContext) -> ExpressionNode:
		var token := context.consume_current_token()
		match token.type:
			Token.Types.LITERAL_IDENTIFIER:
				return IdentifierLiteralNode.from_token(token)
			Token.Types.LITERAL_BOOLEAN_TRUE:
				return BooleanLiteralNode.from_token(token, true)
			Token.Types.LITERAL_BOOLEAN_FALSE:
				return BooleanLiteralNode.from_token(token, false)
			Token.Types.LITERAL_INTEGER:
				return IntegerLiteralNode.from_token(token)
			Token.Types.LITERAL_FLOAT:
				return FloatLiteralNode.from_token(token)
			Token.Types.LITERAL_STRING:
				return StringLiteralNode.from_token(token)
			_:
				raise_error(context, "Parser expected a literal, but got `%s`." % token)
				return ExpressionNode.new()

	func parse_filter_identifier(context: ParserContext) -> ExpressionNode:
		var token := context.consume_current_token()
		if token.type != Token.Types.FILTER_IDENTIFIER:
			raise_error(context, "Expected a filter identifier, but got `%s`." % token, token)
		return IdentifierLiteralNode.from_token(token)

	func parse_literal_identifier(context: ParserContext) -> ExpressionNode:
		var token := context.consume_current_token()
		assert(token.type == Token.Types.LITERAL_IDENTIFIER)
		return IdentifierLiteralNode.from_token(token)
	
	func raise_error(
		context: ParserContext,
		message: String,
		token: Token = null,
	):
		if not token:
			if context.has_tokens_remaining():
				token = context.get_current_token()
			elif not context.tokens.is_empty():
				token = context.get_previous_token()
		if token:
			message += "\n" + "At line %d" % [
				token.starts_in_source_at_line
			]
		if not options.silence_errors:
			printerr(message)
		var error := TemplateError.new()
		error.message = message
		self.errors.append(error)
		if options.break_on_error:
			assert(false, message)

#endregion


#region Visitors

class VisitorContext:
	extends Resource
	
	var options := Options.new()
	
	# TBD: refacto idea: make a VariablesStack (reusable) class
	## Stack of Dictionaries of identifier:String => value:Variant
	var variables_stack: Array[Dictionary] = [{}]
	
	## Optional dependency, to get better error messages.
	var error_reporter: ErrorReporter
	
	## Outputted errors (if any).  Read this once the visit completes.
	var errors: Array[TemplateError] = []
	
	func push_variables_stack(more_variables: Dictionary = {}) -> void:
		self.variables_stack.push_back(more_variables)
	
	func pop_variables_stack() -> Dictionary:
		return self.variables_stack.pop_back()
	
	func has_variable(identifier: String) -> bool:
		for i in range(variables_stack.size()-1, -1, -1):
			if variables_stack[i].has(identifier):
				return true
		return false
	
	func set_variable(identifier: String, value: Variant) -> void:
		self.variables_stack[-1][identifier] = value
	
	func get_variable(identifier: String, default: Variant = null) -> Variant:
		for i in range(variables_stack.size()-1, -1, -1):
			var stack: Dictionary = variables_stack[i]
			if stack.has(identifier):
				return stack.get(identifier, default)
		return default
	
	func raise_error(
		syntax_node: SyntaxNode,
		message: String,
	) -> void:
		var token: Token
		if syntax_node:
			if not syntax_node.tokens.is_empty():
				token = syntax_node.tokens[0]
		if token and error_reporter:
			message += "\n" + error_reporter.get_information_about_token(token)
		if not options.silence_errors:
			printerr(message)
		var error := TemplateError.new()
		error.message = message
		self.errors.append(error)
		if options.break_on_error:
			assert(false, message)


# Cleans up the syntax tree ?  (unused)
#class JanitorVisitor:
	#extends RefCounted
	#func visit(_tree: SyntaxTree) -> void:
		#pass


## Outputs the evaluated template with variables replaced and logic applied.
class EvaluatorVisitor:
	extends RefCounted

	func visit(tree: SyntaxTree, context: VisitorContext) -> String:
		return tree.serialize(context)


#class CompilerVisitor:  # cache the template as pure GdScript?
#class StaticAnalyser:   # static analysis for Godot's code editor?

#endregion


class ErrorReporter:
	extends RefCounted
	
	var source: String
	var tokens: Array[Token]
	
	func get_information_about_token(token: Token) -> String:
		var s := ""
		if token:
			s += "At line %d" % token.starts_in_source_at_line
			s += " near `%s`" % get_excerpt(token)
			s += "."
		return s
	
	func get_excerpt(token: Token) -> String:
		var s := ""
		var i := tokens.find(token)
		assert(i >= 0, "Something WRONG happened.  Token not found.")
		if i >= 0:
			var start = maxi(i - 2, 0)
			var end = mini(i + 4, tokens.size())
			for j in range(start, end):
				s += "%s" % tokens[j].literal
		return s


## Options of the String Engine, also used by the Tokenizer and Parser.
var options: Options

func _init(some_options := Options.new()) -> void:
	self.options = some_options

## Renders a source template using the variables.
## This is the main method of the string template engine.
func render(source: String, variables: Dictionary) -> RenderedTemplate:
	var errors: Array[TemplateError] = []
	var error_reporter := ErrorReporter.new()
	error_reporter.source = source
	
	var tokenizer := Tokenizer.new(options)
	var tokens := tokenizer.tokenize(source)
	errors.append_array(tokenizer.errors)
	error_reporter.tokens = tokens

	var parser := Parser.new(options)
	var syntax_tree := parser.parse(
		tokens, self.statement_extensions, self.filter_extensions,
	)
	errors.append_array(parser.errors)
	
	var visitor_context := VisitorContext.new()
	visitor_context.options = options
	#visitor_context.tokens = tokens
	visitor_context.error_reporter = error_reporter
	#visitor_context.variables = variables.duplicate()
	visitor_context.push_variables_stack(variables.duplicate())

	#var cleaner_visitor := JanitorVisitor.new()
	#cleaner_visitor.visit(syntax_tree)

	var visitor := EvaluatorVisitor.new()
	var output := visitor.visit(syntax_tree, visitor_context)
	errors.append_array(visitor_context.errors)
	
	var rendered := RenderedTemplate.new()
	rendered.output = output
	rendered.errors = errors

	return rendered
