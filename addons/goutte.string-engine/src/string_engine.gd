## A String Template Engine, slow but flexible.
## Vanilla GDScript "port wannabe" of Inja, Twig, Jinja…
class_name StringEngine
extends RefCounted

# WHY: Useful to generate shaders, dynamic dialogues, maybe HTML pages…
# SLOW: try the fast Ginja addon using a gdextension to wrap Inja.

## Statement extensions used by the engine.
## You can append your own in here before calling render(…).
## Your best bet is to look at and copy existing extensions.
var statement_extensions: Array[StatementExtension] = [
	IfElseStatementExtension.new(),
	VerbatimStatementExtension.new(),
]

## A syntax token of the domain specific language of the string template engine.
## The template source string is tokenized into a stream of those tokens,
## and from them the parser builds the syntax tree.
class Token:
	extends Resource
	enum Types {
		UNKNOWN,                  ## Usually means there was a failure somewhere.
		RAW_DATA,                 ## Most of the stuff in the source; all that is not our DSL.
		# FIXME: ECHO → PRINT
		ECHO_OPENER,              ## {{
		ECHO_CLOSER,              ## }}
		COMMENT_OPENER,           ## {#
		COMMENT_CONTENT,          ## Anything except a comment closer.
		COMMENT_CLOSER,           ## #}
		STATEMENT_OPENER,         ## {%
		STATEMENT_IDENTIFIER,     ## for, if, else, endif, verbatim…
		STATEMENT_CLOSER,         ## %}
		EXPRESSION_GROUP_OPENER,  ## (
		EXPRESSION_GROUP_CLOSER,  ## )
		VARIABLE_IDENTIFIER,      ## The Token only knows the name of the variable, not its value.
		LITERAL_BOOLEAN_TRUE,     ## true
		LITERAL_BOOLEAN_FALSE,    ## false
		LITERAL_INTEGER,          ## 42
		LITERAL_FLOAT,            ## 1.618
		LITERAL_STRING,           ## "Hello world !"
		OPERATOR_ADDITION,        ## +
		OPERATOR_SUBTRACTION,     ## -
		OPERATOR_MULTIPLICATION,  ## *
		OPERATOR_DIVISION,        ## /
		#OPERATOR_MODULO,          ## %  Removed: conflicts with STATEMENT_CLOSER ; we use filters
		OPERATOR_NOT,             ## !
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
	}
	
	@export var type := Types.UNKNOWN
	@export var literal := ""
	@export var whitespaces_before := ""
	@export var whitespaces_after := ""
	
	func with_type(value: Types) -> Token:
		self.type = value
		return self
	
	func with_literal(value: String) -> Token:
		self.literal = value
		return self
	
	func _to_string() -> String:
		return self.literal


## A basic (and quite inefficient) view on a portion of a string.
## The goal was/is to copy less strings.
## But regex search using offset ignores line anchors like ^, so we copy anyway.
## Not when shrinking though, so it's still worth having this I think.
class StringView:
	extends Resource
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
			optional_length = self.__string.length()
		self.__length = optional_length
		sanitize_delimiters()
	
	func _to_string() -> String:
		return get_as_string()
	
	func get_as_string() -> String:
		return self.__string.substr(self.__start, self.__length)
	
	func length() -> int:
		return self.__length
	
	func begins_with(prefix: String) -> bool:
		return prefix == self.__string.substr(self.__start, prefix.length())
	
	func sanitize_delimiters() -> void:
		self.__start = clampi(self.__start, 0, self.__string.length())
		self.__length = clampi(self.__length, 0, self.__string.length() - self.__start)
	
	## Faster but breaks line anchor metacharacters like ^ and $
	func rsearch(regex: RegEx) -> RegExMatch:
		return regex.search(self.__string, self.__start, self.__start + self.__length)
	
	## Use this if the regex uses ^ or $
	func rsearch_start(regex: RegEx) -> RegExMatch:
		return regex.search(get_as_string())  # slower, but it "Just Works".
	
	func shrink_from_start_by(amount: int) -> void:
		self.__start += amount
		self.__length -= amount
		sanitize_delimiters()
	
	func read_character_at(relative_position: int) -> String:
		return self.__string[
			clampi(self.__start+relative_position, 0, self.__string.length()-1)
		]


## Probably closer to a Lexer now, as its states are kinda tied to our grammar.
class Tokenizer:
	extends RefCounted
	## I've found it handy to make the Tokenizer a Finite State Machine.
	## Especially as we do not know anything in advance about the raw data.
	enum States {
		RAW_DATA,  ## Reading raw data, the starting/default state
		ECHO,      ## Reading the expression inside of {{ … }}
		STATEMENT, ## Reading the statement inside of {% … %}
		COMMENT,   ## Reading the comment inside of {# … #}
	}
	
	# Public configuration
	var symbol_clear_whitespace := '-'  # as in {{- for example
	var symbol_clear_line_whitespace := '~'  # as in {{~ for example
	var symbol_echo_opener := '{{'
	var symbol_echo_closer := '}}'
	var symbol_statement_opener := '{%'
	var symbol_statement_closer := '%}'
	var symbol_comment_opener := '{#'
	var symbol_comment_closer := '#}'
	
	var symbol_group_opener := '('
	var symbol_group_closer := ')'
	var symbol_combinator_and := 'and'
	var symbol_combinator_or := 'or'
	var symbol_combinator_nand := 'nand'
	var symbol_combinator_xor := 'xor'
	var symbol_boolean_true := 'true'
	var symbol_boolean_false := 'false'
	
	# The order we tokenize expressions (if cascade) is hardcoded, right now.
	# Therefore, the following values are read-only;
	# perhaps we could make their precedence configurable (at a cost).
	const symbol_operator_addition := '+'
	const symbol_operator_subtraction := '-'
	const symbol_operator_multiplication := '*'
	const symbol_operator_division := '/'
	const symbol_operator_not := '!'
	#const symbol_operator_modulo := '%'  # this symbol is disabled ; LL(1)
	const symbol_comparator_equal := '=='
	const symbol_comparator_inequal := '!='
	const symbol_comparator_less_than := '<'
	const symbol_comparator_less_or_equal_than := '<='
	const symbol_comparator_greater_than := '>'
	const symbol_comparator_greater_or_equal_than := '>='
	const symbol_string_delimiter := '"'  # single rune, CANNOT be backslash
	
	# Privates
	var state: States
	var tokens: Array[Token]  # Perhaps use a TokenStream class? (halfway there)
	var source: String  # Immutable whole source, we work on a view of it
	var source_view: StringView  # Our view on the source template.
	
	# @protected
	func reset():
		state = States.RAW_DATA
		tokens = []  # do NOT clear() ; copy is needed ← output of tokenize() !
		source = ""
		source_view = null
		reset_regexes()
	
	var variable_identifier_regex: RegEx
	var statement_identifier_regex: RegEx
	var integer_literal_regex: RegEx
	var float_literal_regex: RegEx
	var string_literal_regex: RegEx
	var openers_regex: RegEx
	var echo_closer_regex: RegEx
	var statement_closer_regex: RegEx
	var comment_closer_regex: RegEx
	
	# Line start anchor breaks with regex's search offset, yet we use a StringView.
	# We might want to get rid of it and switch to solution B ; depends on the benchmarks.
	# A: keep it, keep searching on a substr (copy!) of source on each token (we're doing it)
	# B: remove it, use the search offset, hope regex is fast (feels like it'll scale worse)
	const LINE_START_ANCHOR := "^"
	
	func reset_regexes():
		var has_compiled: int
		
		self.variable_identifier_regex = RegEx.new()
		has_compiled = self.variable_identifier_regex.compile(
			LINE_START_ANCHOR +
			"[a-zA-Z_][a-zA-Z0-9_]*"  # Safe, but ASCII only
			#"[\\w_][\\w0-9_]*"  # Nope: \w includes numbers and \L is absent
		)
		assert(has_compiled == OK, "Detection of variable identifiers broke.")
		
		self.integer_literal_regex = RegEx.new()
		has_compiled = self.integer_literal_regex.compile(
			LINE_START_ANCHOR +
			"[0-9]+"
		)
		assert(has_compiled == OK, "Numbers must be ßr0k3n")
		
		self.float_literal_regex = RegEx.new()
		has_compiled = self.float_literal_regex.compile(
			LINE_START_ANCHOR +
			"(?:" +
			"\\d+\\.\\d*" +        # 2.7 or 2.
			"|\\.\\d+" +           # .2
			"|\\d+\\.\\d*e\\d+" +  # 1.618e3
			"|\\d*\\.\\d+e\\d+" +  # .333e10
			"|\\d+e\\d+" +         # 3e9
			# TODO: support 1.618e3 and 0xff and 0b101010
			")"
		)
		assert(has_compiled == OK, "Never trust an IEEE754")
		
		self.string_literal_regex = RegEx.new()
		has_compiled = self.string_literal_regex.compile(
			LINE_START_ANCHOR +
			("%s" % escape_for_regex(self.symbol_string_delimiter)) +
			#"(?:" +
			("(?:[^\\\\%s]|\\\\.)*" % escape_for_regex(self.symbol_string_delimiter)) +
			#("(?:[^%s\\\\]|\\\\[\\s\\S])*" % self.symbol_string_delimiter) +
			#")" +
			("%s" % escape_for_regex(self.symbol_string_delimiter))
		)
		assert(has_compiled == OK, "Never trust an IEEE754")
		
		self.openers_regex = RegEx.new()
		has_compiled = self.openers_regex.compile(
			"(?<symbol>" +
			escape_for_regex(self.symbol_echo_opener) +
			"|" +
			escape_for_regex(self.symbol_statement_opener) +
			"|" +
			escape_for_regex(self.symbol_comment_opener) +
			")" +
			"(?<clear_whitespace>" +
			escape_for_regex(self.symbol_clear_whitespace) +
			"|" +
			escape_for_regex(self.symbol_clear_line_whitespace) +
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
				escape_for_regex(self.symbol_clear_whitespace) +
				"|" +
				escape_for_regex(self.symbol_clear_line_whitespace) +
				"|" +
				")" +
				"(?<symbol>" + escape_for_regex(symbol) + ")"
			)
		
		self.echo_closer_regex = RegEx.new()
		has_compiled = compile_closer_regex.call(
			self.echo_closer_regex,
			self.symbol_echo_closer,
		)
		assert(has_compiled == OK, "Detection regex of }} is broken.")
	
		self.statement_closer_regex = RegEx.new()
		has_compiled = compile_closer_regex.call(
			self.statement_closer_regex,
			self.symbol_statement_closer,
		)
		assert(has_compiled == OK, "Detection regex of %} is broken.")
	
		self.comment_closer_regex = RegEx.new()
		has_compiled = compile_closer_regex.call(
			self.comment_closer_regex,
			self.symbol_comment_closer,
			false,
		)
		assert(has_compiled == OK, "Detection regex of #} is broken.")
	
	## The main job of a Tokenizer is to create a stream of tokens from a source.
	func tokenize(template: String) -> Array[Token]:
		reset()
		self.source = template
		self.source_view = StringView.new(template, 0, template.length())
		
		while self.source_view.length() > 0:
			match self.state:
				States.RAW_DATA:
					tokenize_raw_data()
				States.ECHO:
					# At this point we have consumed the {{ opener already.
					consume_whitespaces_into_previous_token()
					tokenize_expression()
					consume_whitespaces_into_previous_token()
					tokenize_echo_closer()
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
			add_token(Token.Types.RAW_DATA, source_remaining)
			consume_source(source_remaining.length())
		else:
			var match_start := openers_match.get_start()
			
			if match_start > 0:
				# Let's consume any raw data
				var raw_data_contents := source_remaining.substr(0, match_start)
				add_token(Token.Types.RAW_DATA, raw_data_contents)
				consume_source(match_start)
			
			var whole_match := openers_match.get_string()
			var opener_symbol := openers_match.get_string(&'symbol')
			var clear_whitespace_symbol := openers_match.get_string(&'clear_whitespace')  # TODO
			
			match opener_symbol:
				symbol_echo_opener:
					add_token(Token.Types.ECHO_OPENER, whole_match)
					set_state(States.ECHO)
				symbol_statement_opener:
					add_token(Token.Types.STATEMENT_OPENER, whole_match)
					set_state(States.STATEMENT)
				symbol_comment_opener:
					add_token(Token.Types.COMMENT_OPENER, whole_match)
					set_state(States.COMMENT)
				_:
					breakpoint  # implementation is missing; git to werk!
			
			consume_source(whole_match.length())

	## Advances until it finds a content closer symbol.
	func tokenize_comment_content() -> void:
		var closer_match := source_view.rsearch_start(comment_closer_regex)
		if null == closer_match:
			raise_error("Expected a %s symbol to close the comment, but found none." % [
				self.symbol_comment_closer,
			])
		else:
			var match_start := closer_match.get_start()
			#var closer_symbol := closer_match.get_string(&'symbol')
			#var clear_whitespace_symbol := closer_match.get_string(&'clear_whitespace')
			var source_remaining := source_view.get_as_string()
			var comment_contents := source_remaining.substr(0, match_start)
			add_token(Token.Types.COMMENT_CONTENT, comment_contents)
			consume_source(match_start)

	func tokenize_echo_closer() -> void:
		tokenize_closer(
			self.symbol_echo_closer,
			Token.Types.ECHO_CLOSER,
			self.echo_closer_regex,
		)

	func tokenize_statement_closer() -> void:
		tokenize_closer(
			self.symbol_statement_closer,
			Token.Types.STATEMENT_CLOSER,
			self.statement_closer_regex,
		)

	func tokenize_comment_closer() -> void:
		tokenize_closer(
			self.symbol_comment_closer,
			Token.Types.COMMENT_CLOSER,
			self.comment_closer_regex,
		)

	func tokenize_closer(
		symbol: String,           ## closer symbol to look for
		token_type: Token.Types,  ## token type to create
		closer_regex: RegEx,      ## compiled regex to match
	) -> void:
		var close_match := self.source_view.rsearch_start(closer_regex)
		if close_match == null:
			raise_error("Expected closer token `%s`, got `%s` instead." % [
				symbol,
				self.source_view.read_character_at(0) + 
				self.source_view.read_character_at(1)
			])
			consume_source(self.source_view.length())  # l∞p prevention
		else:
			var whole_match := close_match.get_string()
			var closer_symbol := close_match.get_string(&'symbol')
			assert(closer_symbol == symbol)
			var clear_whitespace_symbol := close_match.get_string(&'clear_whitespace')
			add_token(token_type, whole_match)
			consume_source(whole_match.length())
	
	func tokenize_statement_identifier() -> void:
		var regex_compiled: int
		var statement_identifier_regex := RegEx.new()
		regex_compiled = statement_identifier_regex.compile(
			LINE_START_ANCHOR +
			"[a-zA-Z_][a-zA-Z0-9_]*"  # Safe but ascii-only  (t.t)
		)
		if regex_compiled != OK:
			breakpoint
		
		var regex_match := self.source_view.rsearch_start(statement_identifier_regex)
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
		# Goal: ordered by decreasing usage, yet respecting special precedences
		# This goal is not yet achieved and will require serious benchmarking
		if tokenize_literal_boolean() == OK: return OK  # before identifier
		if tokenize_literal_float() == OK: return OK  # before integer
		if tokenize_literal_integer() == OK: return OK
		if tokenize_literal_string() == OK: return OK
		if tokenize_operator_addition() == OK: return OK
		if tokenize_operator_subtraction() == OK: return OK
		if tokenize_operator_multiplication() == OK: return OK
		if tokenize_operator_division() == OK: return OK
		if tokenize_comparator_equality() == OK: return OK
		if tokenize_comparator_inequality() == OK: return OK  # before not
		if tokenize_comparator_comparison() == OK: return OK
		if tokenize_combinator() == OK: return OK  # before identifier
		if tokenize_operator_not() == OK: return OK
		if tokenize_group_delimiter() == OK: return OK
		if tokenize_variable_identifier() == OK: return OK
		return ERR_DOES_NOT_EXIST
	
	func tokenize_variable_identifier() -> Error:
		var regex_match := self.source_view.rsearch_start(variable_identifier_regex)
		if regex_match == null:
			return ERR_INVALID_DATA
		
		var whole_match := regex_match.get_string()
		add_token(Token.Types.VARIABLE_IDENTIFIER, whole_match)
		consume_source(whole_match.length())
		return OK
	
	func tokenize_symbol(symbol: String, token_type: Token.Types) -> Error:
		if not self.source_view.begins_with(symbol):
			return ERR_INVALID_DATA
		add_token(token_type, symbol)
		consume_source(symbol.length())
		return OK
	
	func tokenize_group_delimiter() -> Error:
		if OK == tokenize_symbol(
			self.symbol_group_opener,
			Token.Types.EXPRESSION_GROUP_OPENER,
		): return OK
		if OK == tokenize_symbol(
			self.symbol_group_closer,
			Token.Types.EXPRESSION_GROUP_CLOSER,
		): return OK
		return ERR_INVALID_DATA
	
	func tokenize_operator_addition() -> Error:
		return tokenize_symbol(
			self.symbol_operator_addition,
			Token.Types.OPERATOR_ADDITION,
		)
	
	func tokenize_operator_subtraction() -> Error:
		return tokenize_symbol(
			self.symbol_operator_subtraction,
			Token.Types.OPERATOR_SUBTRACTION,
		)
	
	func tokenize_operator_multiplication() -> Error:
		return tokenize_symbol(
			self.symbol_operator_multiplication,
			Token.Types.OPERATOR_MULTIPLICATION,
		)
	
	func tokenize_operator_division() -> Error:
		return tokenize_symbol(
			self.symbol_operator_division,
			Token.Types.OPERATOR_DIVISION,
		)
	
	func tokenize_comparator_equality() -> Error:
		return tokenize_symbol(
			self.symbol_comparator_equal,
			Token.Types.COMPARATOR_EQUAL,
		)
	
	func tokenize_comparator_inequality() -> Error:
		return tokenize_symbol(
			self.symbol_comparator_inequal,
			Token.Types.COMPARATOR_INEQUAL,
		)
	
	func tokenize_comparator_comparison() -> Error:
		if OK == tokenize_symbol(
			self.symbol_comparator_less_or_equal_than,
			Token.Types.COMPARATOR_LESS_EQUAL,
		): return OK
		if OK == tokenize_symbol(
			self.symbol_comparator_less_than,
			Token.Types.COMPARATOR_LESS,
		): return OK
		if OK == tokenize_symbol(
			self.symbol_comparator_greater_or_equal_than,
			Token.Types.COMPARATOR_GREATER_EQUAL,
		): return OK
		if OK == tokenize_symbol(
			self.symbol_comparator_greater_than,
			Token.Types.COMPARATOR_GREATER,
		): return OK
		return ERR_INVALID_DATA
	
	func tokenize_combinator() -> Error:
		if tokenize_symbol(
			self.symbol_combinator_and,
			Token.Types.COMBINATOR_AND,
		) == OK: return OK
		if tokenize_symbol(
			self.symbol_combinator_or,
			Token.Types.COMBINATOR_OR,
		) == OK: return OK
		if tokenize_symbol(
			self.symbol_combinator_nand,
			Token.Types.COMBINATOR_NAND,
		) == OK: return OK
		if tokenize_symbol(
			self.symbol_combinator_xor,
			Token.Types.COMBINATOR_XOR,
		) == OK: return OK
		return ERR_INVALID_DATA
	
	func tokenize_operator_not() -> Error:
		return tokenize_symbol(
			self.symbol_operator_not,
			Token.Types.OPERATOR_NOT,
		)
	
	func tokenize_literal_boolean() -> Error:
		if OK == tokenize_symbol(
			self.symbol_boolean_true,
			Token.Types.LITERAL_BOOLEAN_TRUE,
		): return OK
		if OK == tokenize_symbol(
			self.symbol_boolean_false,
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
	
	func add_token(type: Token.Types, literal: String) -> void:
		self.tokens.append(Token.new().with_type(type).with_literal(literal))
	
	func consume_whitespaces_into_previous_token() -> void:
		assert(not self.tokens.is_empty())
		var consumed_whitespaces := consume_whitespaces()
		self.tokens[-1].whitespaces_after += consumed_whitespaces
		
	func consume_whitespaces() -> String:
		var regex_compiled: int
		var whitespaces_regex := RegEx.new()
		regex_compiled = whitespaces_regex.compile(
			"^[\\s]+"
		)
		if regex_compiled != OK:
			breakpoint  # regex broke ; dev intrigued
			return ""
		
		var whitespaces_match := self.source_view.rsearch_start(whitespaces_regex)
		if whitespaces_match != null:
			consume_source(whitespaces_match.get_string().length())
			return whitespaces_match.get_string()
		
		return ""
	
	func consume_source(amount: int) -> void:
		assert(amount >= 0, "Cannot consume a source negatively. … For now.")
		if amount < 0:
			return
		if amount == 0:  # just to see if/when that happens, remove me at will
			breakpoint
			return
		
		self.source_view.shrink_from_start_by(amount)
	
	func raise_error(message: String) -> void:
		printerr(message)
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
	func evaluate(_context: VisitorContext) -> Variant:
		return ""
	
	func serialize(context: VisitorContext) -> String:
		return serialize_self(context) + serialize_children(context)
	
	func serialize_self(context: VisitorContext) -> String:
		return str(evaluate(context))
	
	func serialize_children(context: VisitorContext) -> String:
		#return self.children.reduce(
			#func(accu: String, child: SyntaxNode): return accu + child.serialize(context),
			#"",
		#)
		# -^- which is faster ? (the bottom one is easier to read) -v-
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
	
	func serialize_self(_context: VisitorContext) -> String:
		return self.data


class CommentNode:
	extends SyntaxNode


class ExpressionNode:
	extends SyntaxNode


class VariableIdentifierNode:
	extends ExpressionNode
	@export var identifier: String
	
	func with_identifier(value: String) -> VariableIdentifierNode:
		self.identifier = value
		return self

	func evaluate(context: VisitorContext) -> Variant:
		return context.variables.get(self.identifier, '')


class BooleanLiteralNode:
	extends ExpressionNode
	@export var value: bool

	func with_value(some_value: bool) -> BooleanLiteralNode:
		self.value = some_value
		return self

	func evaluate(_context: VisitorContext) -> Variant:
		return self.value


class IntegerLiteralNode:
	extends ExpressionNode
	@export var value: int

	func with_value(some_value: int) -> IntegerLiteralNode:
		self.value = some_value
		return self

	func evaluate(_context: VisitorContext) -> Variant:
		return self.value


class FloatLiteralNode:
	extends ExpressionNode
	@export var value: float

	static func from_token(token: Token) -> FloatLiteralNode:
		assert(token.type == Token.Types.LITERAL_FLOAT)
		assert(token.literal.length() >= 2)
		var node := FloatLiteralNode.new()
		node.value = float(token.literal)
		node.tokens.append(token)
		assert(str(node.value) == token.literal, "%s != %s" % [
			str(node.value), token.literal,
		])
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
		assert(token.literal.length() >= 2)
		var node := StringLiteralNode.new()
		node.value = token.literal.substr(1, token.literal.length() - 2)
		node.value = node.value.replace("\\\"", "\"")
		node.value = node.value.replace("\\\\", "\\")
		node.tokens.append(token)
		return node

	#func with_value(some_value: String) -> StringLiteralNode:
		#self.value = some_value
		#return self

	func evaluate(_context: VisitorContext) -> Variant:
		return self.value


class OperatorNode:
	extends ExpressionNode
	func serialize(context: VisitorContext) -> String:
		return serialize_self(context)


class UnaryOperatorNode:
	extends OperatorNode
	func evaluate_unary(_operand: Variant) -> Variant:
		breakpoint  # you MUST override me !
		return ""
	func evaluate(context: VisitorContext) -> Variant:
		assert(self.children.size() == 1)
		return evaluate_unary(self.children[0].evaluate(context))
	func serialize(context: VisitorContext) -> String:
		return serialize_self(context)


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


class BinaryOperatorNode:
	extends OperatorNode
	
	@warning_ignore("unused_parameter")
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		breakpoint  # you MUST override me !
		return null
	
	func evaluate(context: VisitorContext) -> Variant:
		assert(self.children.size() == 2)
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
		return null


class StatementIdentifierNode:
	extends ExpressionNode
	@export var identifier: String
	
	func with_identifier(value: String) -> StatementIdentifierNode:
		self.identifier = value
		return self


class EchoNode:
	extends SyntaxNode
	# FIXME just use self.children with only one child allowed instead of this ?
	@export var expression: ExpressionNode

	func with_expression(value: ExpressionNode) -> EchoNode:
		self.expression = value
		return self

	func serialize(context: VisitorContext) -> String:
		return self.expression.serialize(context)


class StatementNode:
	extends SyntaxNode
	#@export var identifier: String
	@export var extension: StatementExtension
	func with_extension(value: StatementExtension) -> StatementNode:
		self.extension = value
		return self
	
	func serialize(context: VisitorContext) -> String:
		return self.extension.serialize(context, self)


class StatementExtension:
	extends Resource
	
	func get_statement_identifier() -> String:
		breakpoint  # override me !
		return ''
	
	func parse(
		_parser: Parser,
		_context: ParserContext,
		_identifier_token: Token,
	) -> StatementNode:
		breakpoint  # override me !
		return StatementNode.new().with_extension(self)
	
	func serialize(_context: VisitorContext, _node: StatementNode) -> String:
		return ""  # you probably want to override me
	
	func matches_statement_identifier(id_to_match: String) -> bool:
		return id_to_match == get_statement_identifier()


class IfElseStatementExtension:
	extends StatementExtension
	
	func get_statement_identifier() -> String:
		return 'if'
	
	func parse(
		parser: Parser,
		context: ParserContext,
		identifier_token: Token,
	) -> StatementNode:
		# The engine has already parsed the statement opener and identifier.
		# The rest is up to us, now.
		# {% if <some_condition> %} … {% else %} … {% end %}
		
		#var condition := parser.parse_expression()
		#parser.consume_type(Token.Types.STATEMENT_CLOSER)
		
		var condition := parser.parse_expression(context)
		context.consume_type(Token.Types.STATEMENT_CLOSER)
		
		var detect_else := context.detect_other_statement('else')
		var detect_end := context.detect_other_statement('end' + get_statement_identifier())
		var detect_else_or_end := func(pc: ParserContext) -> bool:
			return detect_else.call(pc) or detect_end.call(pc)
		
		var then_node := SyntaxNode.new()
		parser.parse_tokens_until(
			context,
			then_node,
			detect_else_or_end,
		)
		var found_else := detect_else.call(context) as bool
		context.consume_some_tokens(3)  # luck: else & end have the same shape
		
		var else_node: SyntaxNode
		if found_else:
			else_node = SyntaxNode.new()
			context.parser.parse_tokens_until(
				context,
				else_node,
				detect_end,
			)
			context.consume_some_tokens(3)
		
		var if_node := (
			StatementNode
			.new()
			.with_extension(self)
			.with_child(condition)
			.with_child(then_node)
		)
		if else_node:
			if_node.with_child(else_node)
		
		return if_node
	
	func serialize(context: VisitorContext, node: StatementNode) -> String:
		var condition := node.children[0] as ExpressionNode
		var then_node := node.children[1] as SyntaxNode
		var condition_evaluated: Variant = condition.evaluate(context)
		if condition_evaluated:
			return then_node.serialize(context)
		elif node.children.size() == 3:
			var else_node := node.children[2] as SyntaxNode
			return else_node.serialize(context)
		return ""


class VerbatimStatementExtension:
	extends StatementExtension
	
	func get_statement_identifier() -> String:
		return 'verbatim'
	
	func parse(
		_parser: Parser,
		context: ParserContext,
		identifier_token: Token,
	) -> StatementNode:
		context.consume_type(Token.Types.STATEMENT_CLOSER)
		var content_tokens := context.consume_until(
			func(token_index: int):
				return (
					context.tokens[token_index].type == Token.Types.STATEMENT_OPENER
					&&
					context.tokens[token_index+1].type == Token.Types.STATEMENT_IDENTIFIER
					&&
					context.tokens[token_index+1].literal == 'end' + get_statement_identifier()
					&&
					context.tokens[token_index+2].type == Token.Types.STATEMENT_CLOSER
				)
		)
		context.consume_some_tokens(3)  # {% end<statement_identifier> %}
		
		var content: String = content_tokens.reduce(
			func(acc: String, tk: Token):
				return acc + tk.whitespaces_before + tk.literal + tk.whitespaces_after
				,
			""
		)
		
		return (
			StatementNode.new()
			.with_extension(self)
			.with_child(RawDataNode.new().with_data(content))
		)
	
	func serialize(context: VisitorContext, node: StatementNode) -> String:
		if node.children.is_empty():
			return ""
		assert(node.children.size() == 1, "Why would there be more ?")
		return node.children[0].serialize(context)


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


## Holds the context of a parser's run.
## I thought we'd have to backtrack and therefore copy a context around,
## but we don't backtrack, and therefore this is somewhat redundant with Parser.
## Not sure if we'll ever end up backtracking…
## This makes the API in statement extensions horrible.  Let's work on it.
class ParserContext:
	extends Resource
	var statement_extensions: Array[StatementExtension]
	var parser: Parser
	var tokens: Array[Token]
	var current_token_index := 0
	
	func with_parser(value: Parser) -> ParserContext:
		self.parser = value
		return self
	
	func with_tokens(value: Array[Token]) -> ParserContext:
		self.tokens = value
		return self
	
	func with_statement_extensions(value: Array[StatementExtension]) -> ParserContext:
		self.statement_extensions = value
		return self
	
	func get_statement_extension(identifier: String) -> StatementExtension:
		var i := self.statement_extensions.find_custom(
			func(se: StatementExtension):
				return se.get_statement_identifier() == identifier
		)
		assert(i >= 0, "No statement extension found for `%s`." % identifier)
		return self.statement_extensions[i]
	
	func has_tokens_remaining(at_least := 1) -> bool:
		return (
			self.current_token_index + at_least
			<=
			self.tokens.size()
		)
	
	## Scans the current token and returns whether it matches the type.
	func scan_type(type: Token.Types, offset := 0) -> bool:
		return get_current_token(offset).type == type
	
	## Consumes the provided type or yells.
	func consume_type(type: Token.Types) -> void:
		if get_current_token().type == type:
			consume_token()
		else:
			assert(false, "Expected a token of type %s but got %s" % [
				type, get_current_token().type
			])
	
	## Scans the current token and consumes it if it matches the type.
	func match_type(type: Token.Types) -> bool:
		var matched := get_current_token().type == type
		if matched:
			consume_token()
		return matched
	
	func get_current_token(offset := 0) -> Token:
		return self.tokens[self.current_token_index + offset]
	func get_previous_token() -> Token:
		return get_current_token(-1)
	
	# TODO: rename pop_current_token()  ? (this method returns)
	func consume_current_token() -> Token:
		consume_token()
		return get_previous_token()
	
	func consume_token() -> void:
		self.current_token_index += 1
	
	func consume_some_tokens(amount: int) -> void:
		assert(amount >= 0)
		self.current_token_index += amount
	
	func consume_until_type(type: Token.Types) -> Array[Token]:
		var consumed: Array[Token] = []
		while has_tokens_remaining():
			var token := consume_current_token()
			if token.type == type:
				break
			consumed.append(token)
		return consumed
	
	func consume_until(condition: Callable) -> Array[Token]:
		var consumed: Array[Token] = []
		var found := false
		var cursor := -1
		while not found:
			cursor += 1
			if self.current_token_index + cursor >= self.tokens.size():
				assert(false, "Did not find the end condition.")
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
	
	func detect_other_statement(identifier: String) -> Callable:
		return (
			func(context: ParserContext) -> bool:
				return (
					context.get_current_token(0).type == Token.Types.STATEMENT_OPENER and
					context.get_current_token(1).type == Token.Types.STATEMENT_IDENTIFIER and
					context.get_current_token(1).literal == identifier
				)
		)


class Parser:
	extends RefCounted
	
	func parse(
		tokens: Array[Token],
		statement_extensions: Array[StatementExtension],
	) -> SyntaxTree:
		var body := BodyNode.new()
		var tree := SyntaxTree.new().with_body(body)
		var context := (
			ParserContext
			.new()
			.with_parser(self)
			.with_tokens(tokens)
			.with_statement_extensions(statement_extensions)
		)
		
		parse_tokens_until(
			context, body,
			func(c: ParserContext): return not c.has_tokens_remaining(),
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
		var token: Token = context.consume_current_token()
		match token.type:
			Token.Types.RAW_DATA:
				return RawDataNode.new().with_data(token.literal).with_token(token)
			Token.Types.ECHO_OPENER:
				#var tokens_subset := context.consume_until_type(Token.Types.ECHO_CLOSER)
				#var tokens_subset: Array[Token] = []
				var expression: ExpressionNode = parse_expression(context)
				if not context.match_type(Token.Types.ECHO_CLOSER):
					raise_error("Expected }}, but got something else: %s" % context.get_current_token())
				
				var echo_tokens: Array[Token] = []
				#echo_tokens.append(token)
				#echo_tokens.append_array(tokens_subset)  # FIXME
				#echo_tokens.append(context.get_current_token(-1))
				return EchoNode.new().with_expression(expression).with_tokens(echo_tokens)
			Token.Types.STATEMENT_OPENER:
				var identifier_token := context.consume_current_token()
				assert(identifier_token.type == Token.Types.STATEMENT_IDENTIFIER, "Expected statement identifier")
				var statement_extension := context.get_statement_extension(identifier_token.literal)
				return statement_extension.parse(self, context, identifier_token)
			Token.Types.COMMENT_OPENER:
				context.consume_some_tokens(2)
				return CommentNode.new()
			_:
				breakpoint  # implement your new "main" token type !
		
		# Somewhat safe fallback ; should not happen anyway.
		return RawDataNode.new().with_data("")

	# Grammar Quick Notes
	# -------------------
	# EXPRESSION = VARIABLE_IDENTIFIER
	#            | LITERAL
	#            | PREFIX_OPERATOR EXPRESSION
	#            | EXPRESSION INFIX_OPERATOR EXPRESSION
	#            | EXPRESSION "|" FILTER
	#            | "(" EXPRESSION ")"
	#
	# Stratified Grammar for Expressions
	# ----------------------------------
	# EXPRESSION = COMBINATION
	# COMBINATION = EQUALITY ( ( "n"?"and" | "x"?"or" ) EQUALITY )*
	# EQUALITY = COMPARISON ( ( "!=" | "==" ) COMPARISON )*
	# COMPARISON = ADDITION ( ( "<=" | "<" | ">=" | ">" ) ADDITION )*
	# ADDITION = MULTIPLICATION ( ( "+" | "-" ) MULTIPLICATION )*
	# MULTIPLICATION = MODULO ( ( "*" | "/" ) MODULO )*
	# MODULO = FILTER ( "%" FILTER )*   → NO: conflicts with %}
	# FILTER = UNARY ( "|" FILTER_NAME )*
	# UNARY = ( "+" | "-" | "!" ) UNARY
	#       | PRIMARY
	# PRIMARY = LITERAL
	#         | VARIABLE_IDENTIFIER
	#         | "(" EXPRESSION ")"
	func parse_expression(context: ParserContext) -> ExpressionNode:
		assert(context.has_tokens_remaining(), "Expected an expression, but got nothing.")
		return parse_combination(context)

	func parse_combination(context: ParserContext) -> ExpressionNode:
		var node := parse_equality(context)
		while (
			context.match_type(Token.Types.COMBINATOR_AND) or
			context.match_type(Token.Types.COMBINATOR_OR) or
			context.match_type(Token.Types.COMBINATOR_NAND) or
			context.match_type(Token.Types.COMBINATOR_XOR)
		):
			#prints("parser token", context.get_previous_token())
			#var combinator_token := context.get_previous_token()
			node = (
				CombinatorNode.new()
				#.with_token(combinator_token)
				.with_child(node)
				.with_child(parse_combination(context))
				.with_token(context.get_previous_token())
				as CombinatorNode
			)
		return node
	
	func parse_equality(context: ParserContext) -> ExpressionNode:
		var node := parse_comparison(context)
		while (
			context.match_type(Token.Types.COMPARATOR_EQUAL) or
			context.match_type(Token.Types.COMPARATOR_INEQUAL)
		):
			match context.get_previous_token().type:
				Token.Types.COMPARATOR_EQUAL:
					node = (
						EqualityComparatorNode.new()
						.with_child(node)
						.with_child(parse_equality(context))
					)
				Token.Types.COMPARATOR_INEQUAL:
					node = (
						InequalityComparatorNode.new()
						.with_child(node)
						.with_child(parse_equality(context))
					)
		return node
	
	func parse_comparison(context: ParserContext) -> ExpressionNode:
		var node := parse_addition(context)
		while (
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
					)
				Token.Types.COMPARATOR_LESS_EQUAL:
					node = (
						LessOrEqualComparatorNode.new()
						.with_child(node)
						.with_child(parse_comparison(context))
					)
				Token.Types.COMPARATOR_GREATER:
					node = (
						GreaterComparatorNode.new()
						.with_child(node)
						.with_child(parse_comparison(context))
					)
				Token.Types.COMPARATOR_GREATER_EQUAL:
					node = (
						GreaterOrEqualComparatorNode.new()
						.with_child(node)
						.with_child(parse_comparison(context))
					)
		return node
	
	func parse_addition(context: ParserContext) -> ExpressionNode:
		var node := parse_multiplication(context)
		while (
			context.match_type(Token.Types.OPERATOR_ADDITION) or
			context.match_type(Token.Types.OPERATOR_SUBTRACTION)
		):
			match context.get_previous_token().type:
				Token.Types.OPERATOR_ADDITION:
					node = (
						AdditionOperatorNode.new()
						.with_child(node)
						.with_child(parse_addition(context))
					)
				Token.Types.OPERATOR_SUBTRACTION:
					node = (
						SubtractionOperatorNode.new()
						.with_child(node)
						.with_child(parse_addition(context))
					)
				_:
					breakpoint
		return node
	
	func parse_multiplication(context: ParserContext) -> ExpressionNode:
		var node := parse_unary(context)
		while (
			context.match_type(Token.Types.OPERATOR_MULTIPLICATION) or
			context.match_type(Token.Types.OPERATOR_DIVISION)
		):
			match context.get_previous_token().type:
				Token.Types.OPERATOR_MULTIPLICATION:
					node = (
						MultiplicationOperatorNode.new()
						.with_child(node)
						.with_child(parse_multiplication(context))
					)
				Token.Types.OPERATOR_DIVISION:
					node = (
						DivisionOperatorNode.new()
						.with_child(node)
						.with_child(parse_multiplication(context))
					)
		return node
	
	# FIXME: use a filter for modulo, not the % symbol
	#func parse_modulo(context: ParserContext) -> ExpressionNode:
		#var node := parse_unary(context)
		#while (context.match_type(Token.Types.OPERATOR_MODULO)):
			#node = (
				#ModuloOperatorNode.new()
				#.with_child(node)
				#.with_child(parse_modulo(context))
			#)
		#return node
	
	#func parse_filter(context: ParserContext) -> ExpressionNode:
	
	func parse_unary(context: ParserContext) -> ExpressionNode:
		var node: ExpressionNode
		if (
			context.match_type(Token.Types.OPERATOR_NOT) or
			context.match_type(Token.Types.OPERATOR_ADDITION) or
			context.match_type(Token.Types.OPERATOR_SUBTRACTION)
		):
			match context.get_previous_token().type:
				Token.Types.OPERATOR_ADDITION:
					node = PositiveUnaryOperatorNode.new()
				Token.Types.OPERATOR_SUBTRACTION:
					node = NegativeUnaryOperatorNode.new()
				Token.Types.OPERATOR_NOT:
					node = NotUnaryOperatorNode.new()
		
		if node:
			node.with_child(parse_primary(context))
			node.with_token(context.get_previous_token())
		else:
			node = parse_primary(context)
		
		return node
	
	func parse_primary(context: ParserContext) -> ExpressionNode:
		if context.get_current_token().type == Token.Types.EXPRESSION_GROUP_OPENER:
			context.consume_token()
			var inside := parse_expression(context)
			if not context.match_type(Token.Types.EXPRESSION_GROUP_CLOSER):
				raise_error("Expected `)` but got `%s`." % [
					context.get_current_token(),
				])
			return inside
		return parse_literal(context)
	
	func parse_literal(context: ParserContext) -> ExpressionNode:
		var token := context.consume_current_token()
		match token.type:
			Token.Types.VARIABLE_IDENTIFIER:
				return VariableIdentifierNode.new().with_identifier(token.literal).with_token(token) as VariableIdentifierNode
			Token.Types.LITERAL_BOOLEAN_TRUE:
				return BooleanLiteralNode.new().with_value(true).with_token(token)
			Token.Types.LITERAL_BOOLEAN_FALSE:
				return BooleanLiteralNode.new().with_value(false).with_token(token)
			Token.Types.LITERAL_INTEGER:
				return IntegerLiteralNode.new().with_value(int(token.literal)).with_token(token)
			Token.Types.LITERAL_FLOAT:
				#return FloatLiteralNode.new().with_value(float(token.literal)).with_token(token)
				return FloatLiteralNode.from_token(token)
			Token.Types.LITERAL_STRING:
				return StringLiteralNode.from_token(token)
				#return StringLiteralNode.new().with_value(token.literal.substr(1, token.literal.length()-2)).with_token(token)
			_:
				raise_error("Parser expected a literal, but got `%s`." % token)
				return ExpressionNode.new()
	
	func raise_error(message: String):
		printerr(message)
		assert(false, message)


class VisitorContext:
	extends Resource
	## identifier:String => value:Variant
	var variables := {}


## Outputs the serialized template with variables replaced and logic applied.
class EvaluatorVisitor:
	extends RefCounted
	
	var output: String
	
	func reset() -> void:
		self.output = ""
	
	func visit(tree: SyntaxTree, context: VisitorContext) -> String:
		reset()
		visit_node(tree, context)
		return self.output
	
	func visit_node(node: SyntaxNode, context: VisitorContext) -> void:
		self.output += node.serialize(context)

#class CompilerVisitor:  # TODO: cache the template as pure GdScript
#class HighlighterVisitor:  # TODO: syntax highlighting for Godot's code editor

## Renders a source template using the variables.
## The main method of the string template engine.
func render(source: String, variables: Dictionary) -> String:
	
	var tokenizer := Tokenizer.new()
	var tokens := tokenizer.tokenize(source)
	
	#prints("Source:", source)
	#prints("Variables:", variables)
	#prints("Tokens:", tokens)
	
	var parser := Parser.new()
	var syntax_tree := parser.parse(tokens, self.statement_extensions)
	
	var visitor_context := VisitorContext.new()
	visitor_context.variables = variables
	
	var visitor := EvaluatorVisitor.new()
	var output := visitor.visit(syntax_tree, visitor_context)
	
	return output
