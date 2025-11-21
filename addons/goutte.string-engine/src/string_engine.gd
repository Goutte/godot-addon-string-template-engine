# Vanilla GDScript "port" of Inja, Twig, Jinja…
# If you need perfs, try the Ginja addon using a gdextension to wrap Inja.
# Used to generate shaders, dynamic dialogues, HTML pages…

## A String Template Engine, super slow but flexible.
class_name StringEngine
extends RefCounted

# TENTATIVE GRAMMAR
# -----------------
# LITERAL_STRING = /["]TODO["]/
# LITERAL_NUMBER = LITERAL_INTEGER | LITERAL_FLOAT
# LITERAL = LITERAL_NUMBER | LITERAL_STRING
# VARIABLE_IDENTIFIER = /[a-zA-Z_][a-zA-Z0-9_]*/
# ECHO = ECHO_OPENER EXPRESSION ECHO_CLOSER
# EXPRESSION = VARIABLE_IDENTIFIER
#            | LITERAL
#            | PREFIX_OPERATOR EXPRESSION
#            | EXPRESSION INFIX_OPERATOR EXPRESSION
#            | EXPRESSION FILTER_SYMBOL FILTER
#            | "(" EXPRESSION ")"
# EXPRESSIONS = EXPRESSION
#             | EXPRESSION "," EXPRESSIONS
# FILTER = FILTER_NAME
#        | FILTER_NAME "(" FILTER_ARGUMENTS ")"
# FILTER_ARGUMENTS = EXPRESSIONS | ""

var statement_extensions: Array[StatementExtension] = [
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
		ECHO_OPENER,              ## Usually {{
		ECHO_CLOSER,              ## Usually }}
		STATEMENT_OPENER,         ## Usually {%
		STATEMENT_CLOSER,         ## Usually %}
		STATEMENT_IDENTIFIER,     ## Examples: for, if, verbatim…
		VARIABLE_IDENTIFIER,      ## The Token only knows the name of the variable, not its value.
		LITERAL_INTEGER,          ## 42
		LITERAL_FLOAT,            ## 1.618
		OPERATOR_ADDITION,        ## +
		OPERATOR_SUBTRACTION,     ## -
		OPERATOR_MULTIPLICATION,  ## *
		OPERATOR_DIVISION,        ## /
		OPERATOR_MODULO,          ## %
		OPERATOR_NOT,             ## !
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


## The goal is/was to copy less strings.
## But regex search using offset ignores line anchors like ^, so we copy anyway.
class StringView:
	extends Resource
	@export var string: String
	@export var start: int
	@export var len: int
	
	func _init(
		string := "",  # Provide this, 'tis only optional to please Godot IIRC
		start := -1,   # Defaults to 0
		len := -1,     # Defaults to the string's length
	) -> void:
		self.string = string
		if start < 0:
			start = 0
		self.start = start
		if len < 0:
			len = self.string.length()
		self.len = len
		sanitize_delimiters()
	
	func _to_string() -> String:
		return get_as_string()
	
	func get_as_string() -> String:
		return self.string.substr(self.start, self.len)
	
	func length() -> int:
		return self.len
	
	func sanitize_delimiters() -> void:
		self.start = clampi(self.start, 0, self.string.length())
		self.len = clampi(self.len, 0, self.string.length()-self.start)
	
	func search_with_regex(regex: RegEx) -> RegExMatch:
		# This is faster but breaks on line anchor metacharacters like ^ and $
		return regex.search(self.string, self.start, self.start + self.len)
	
	func search_with_regex_using_anchors(regex: RegEx) -> RegExMatch:
		# Slower (by how much?), but it "Just Works".
		return regex.search(self.get_as_string())
	
	func shrink_from_start_by(amount: int) -> void:
		self.start = self.start + amount
		self.len = self.len - amount
		sanitize_delimiters()
	
	func read_character_at(relative_position: int) -> String:  # razor sharp
		return self.string[self.start + relative_position]


## Probably closer to a Lexer now, as its states are kinda tied to our grammar.
class Tokenizer:
	extends RefCounted
	enum States {
		RAW_DATA,
		ECHO,
		STATEMENT,
	}
	
	# Public configuration
	var symbol_clear_whitespace := '-'  # as in {{- for example
	var symbol_clear_line_whitespace := '~'  # as in {{~ for example
	var symbol_echo_opener := '{{'
	var symbol_echo_closer := '}}'
	var symbol_statement_opener := '{%'
	var symbol_statement_closer := '%}'
	var symbol_operator_modulo := '%'
	
	# Privates
	var state: States
	var tokens: Array[Token]  # TODO: make a TokenStream class
	var source: String  # Immutable whole source, we work on a view of it
	var source_view: StringView  # Our view on the source template.
	
	# @protected
	func reset():
		state = States.RAW_DATA
		tokens = []  # do NOT clear() ; copy is needed ← output of tokenize() !
		source = ""
		source_view = null
		reset_regexes()
	
	var openers_regex: RegEx
	var variable_identifier_regex: RegEx
	var integer_literal_regex: RegEx
	var float_literal_regex: RegEx
	
	# Line start anchor breaks with regex's search offset, yet we use a StringView.
	# We might want to get rid of it and switch to solution B ; depends on the benchmarks.
	# A: keep it, keep searching on a substr (copy!) of source on each token (we're doing it)
	# B: remove it, use the search offset, hope regex is fast (feels like it'll scale worse)
	const LINE_START_ANCHOR := "^"
	
	func reset_regexes():
		var regex_has_compiled: int
		
		self.openers_regex = RegEx.new()
		regex_has_compiled = self.openers_regex.compile(
			"(?<symbol>" +
			escape_for_regex(self.symbol_echo_opener) +
			"|" +
			escape_for_regex(self.symbol_statement_opener) +
			")" +
			"(?<clear_whitespace>" +
			escape_for_regex(self.symbol_clear_whitespace) +
			"|" +
			escape_for_regex(self.symbol_clear_line_whitespace) +
			"|" +
			")"
		)
		assert(regex_has_compiled == OK, "Did you change some symbols, perhaps?")
		
		self.variable_identifier_regex = RegEx.new()
		regex_has_compiled = self.variable_identifier_regex.compile(
			LINE_START_ANCHOR +
			# This is just BAD DESIGN ; FIXME: use an allowlist
			#("[^0-9%s][^%s]*" % [self.forbidden_identifier_chars, self.forbidden_identifier_chars])
			# Safer, but ASCII only
			"[a-zA-Z_][a-zA-Z0-9_]*"
			#"[\\w_][\\w0-9_]*"
		)
		assert(regex_has_compiled == OK, "Did you change the forbidden identifier characters?")
		
		self.integer_literal_regex = RegEx.new()
		regex_has_compiled = self.integer_literal_regex.compile(
			LINE_START_ANCHOR +
			"[0-9]+"
		)
		assert(regex_has_compiled == OK, "Numbers must be ßr0k3n")
		
		#new Regex(  "^(" +
		#/*Hex*/ @"0x[0-9a-f]+"  + "|" +
		#/*Bin*/ @"0b[01]+"      + "|" + 
		#/*Oct*/ @"0[0-7]*"      + "|" +
		#/*Dec*/ @"((?!0)|[-+]|(?=0+\.))(\d*\.)?\d+(e\d+)?" + 
		#")$" );
		self.float_literal_regex = RegEx.new()
		regex_has_compiled = self.float_literal_regex.compile(
			LINE_START_ANCHOR +
			"(?:" +
			"[0-9]+[.][0-9]*" +  # 2.0 or 2.
			"|" +
			"[0-9]*[.][0-9]+" +  # .2
			")"
		)
		assert(regex_has_compiled == OK, "Numbers must be ßr0k3n")
	
	
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
					consume_whitespaces_into_previous_token()
					tokenize_expression()
					#tokenize_expression_until_type(Token.Types.ECHO_CLOSER)
					consume_whitespaces_into_previous_token()
					tokenize_echo_closer()
					set_state(States.RAW_DATA)
				States.STATEMENT:
					consume_whitespaces_into_previous_token()
					tokenize_statement_identifier()
					# FIXME: ask the statement extension on how to tokenize here
					consume_whitespaces_into_previous_token()
					tokenize_statement_closer()
					set_state(States.RAW_DATA)
				_:
					breakpoint # unknown state (implement it!)
		
		return self.tokens
	
	func tokenize_raw_data() -> void:
		# There is no way to discriminate raw data by itself.
		# We're going to advance to the first found OPENER of our syntax.
		# With the default configuration, OPENERs are `{{`, `{%` and `{#`.
		
		var openers_match := self.source_view.search_with_regex_using_anchors(self.openers_regex)
		
		var source_remaining := source_view.get_as_string()
		if openers_match == null:
			add_token(Token.Types.RAW_DATA, source_remaining)
			consume_source(source_remaining.length())
		else:
			var match_start := openers_match.get_start()
			var raw_data_contents := source_remaining.substr(0, match_start)
			
			add_token(Token.Types.RAW_DATA, raw_data_contents)
			consume_source(match_start)
			
			var whole_match := openers_match.get_string()
			var opener_symbol := openers_match.get_string(&'symbol')
			var clear_whitespace_symbol := openers_match.get_string(&'clear_whitespace')  # TODO
			
			match opener_symbol:
				symbol_echo_opener:
					set_state(States.ECHO)
					add_token(Token.Types.ECHO_OPENER, whole_match)
				symbol_statement_opener:
					set_state(States.STATEMENT)
					add_token(Token.Types.STATEMENT_OPENER, whole_match)
				_:
					breakpoint  # implementation is missing, get to work!
			
			consume_source(whole_match.length())

	func tokenize_echo_closer() -> void:
		tokenize_closer(self.symbol_echo_closer, Token.Types.ECHO_CLOSER)
	
	func tokenize_statement_closer() -> void:
		tokenize_closer(self.symbol_statement_closer, Token.Types.STATEMENT_CLOSER)

	func tokenize_closer(symbol: String, token_type: Token.Types) -> void:
		var compiled: int
		var close_regex := RegEx.new()
		compiled = close_regex.compile(
			LINE_START_ANCHOR +
			"(?<clear_whitespace>" +
			escape_for_regex(self.symbol_clear_whitespace) +
			"|" +
			escape_for_regex(self.symbol_clear_line_whitespace) +
			"|" +
			")" +
			"(?<symbol>" +
			escape_for_regex(symbol) +
			")"
		)
		if compiled != OK:
			breakpoint
		
		var close_match := self.source_view.search_with_regex_using_anchors(close_regex)
		if close_match == null:
			assert(false, "Expected closer token `%s`, got `%s` instead" % [
				symbol,
				self.source_view.read_character_at(0) + 
				self.source_view.read_character_at(1)
			])
			consume_source(self.source_view.length())  # infinite loop prevention
		else:
			var whole_match := close_match.get_string()
			var close_symbol := close_match.get_string(&'symbol')
			var clear_whitespace_symbol := close_match.get_string(&'clear_whitespace')
			add_token(token_type, whole_match)
			consume_source(whole_match.length())
	
	# FIXME: worst design in the history of designs, hf w/ rtl !
	var forbidden_identifier_chars := " \\\\/#%?!<>{}()\\[\\]^'\"`|:;,.~+*-"  # MUST end with -
	
	func tokenize_statement_identifier() -> void:
		var regex_compiled: int
		var statement_identifier_regex := RegEx.new()
		regex_compiled = statement_identifier_regex.compile(
			"^" +
			("[^0-9%s][^%s]*" % [forbidden_identifier_chars, forbidden_identifier_chars])
			#"[a-zA-Z_][a-zA-Z0-9_]*"  # safer, ascii-only
		)
		if regex_compiled != OK:
			breakpoint
		
		var regex_match := self.source_view.search_with_regex_using_anchors(statement_identifier_regex)
		if regex_match == null:
			assert(false, "Not a statement identifier")
		else:
			var whole_match := regex_match.get_string()
			add_token(Token.Types.STATEMENT_IDENTIFIER, whole_match)
			consume_source(whole_match.length())
	
	func tokenize_expression() -> void:
		var status := OK
		while status == OK:
			status = tokenize_expression_once()
			consume_whitespaces_into_previous_token()
		
	func tokenize_expression_once() -> int:
		# The order matters.  Eg: float MUST be tokenized BEFORE int
		if tokenize_variable_identifier() == OK: return OK
		if tokenize_float_literal() == OK: return OK
		if tokenize_integer_literal() == OK: return OK
		if tokenize_addition_operator() == OK: return OK
		if tokenize_subtraction_operator() == OK: return OK
		if tokenize_multiplication_operator() == OK: return OK
		if tokenize_division_operator() == OK: return OK
		if tokenize_modulo_operator() == OK: return OK
		if tokenize_not_operator() == OK: return OK
		return ERR_DOES_NOT_EXIST
	
	func tokenize_variable_identifier() -> int:
		var regex_match := self.source_view.search_with_regex_using_anchors(variable_identifier_regex)
		if regex_match == null:
			return ERR_INVALID_DATA
		
		var whole_match := regex_match.get_string()
		add_token(Token.Types.VARIABLE_IDENTIFIER, whole_match)
		consume_source(whole_match.length())
		return OK
	
	const addition_operator_symbol := '+'  # maximum one rune
	const subtraction_operator_symbol := '-'  # maximum one rune
	const multiplication_operator_symbol := '*'  # TODO: support × as well?
	const division_operator_symbol := '/'  # TODO: support ÷ as well?
	const not_operator_symbol := '!'  # TODO: support `not` as well?
	
	func tokenize_addition_operator() -> int:
		if self.source_view.read_character_at(0) != addition_operator_symbol:
			return ERR_INVALID_DATA
		add_token(Token.Types.OPERATOR_ADDITION, addition_operator_symbol)
		consume_source(1)
		return OK
	
	func tokenize_subtraction_operator() -> int:
		if self.source_view.read_character_at(0) != subtraction_operator_symbol:
			return ERR_INVALID_DATA
		add_token(Token.Types.OPERATOR_SUBTRACTION, subtraction_operator_symbol)
		consume_source(1)
		return OK
	
	func tokenize_multiplication_operator() -> int:
		if self.source_view.read_character_at(0) != multiplication_operator_symbol:
			return ERR_INVALID_DATA
		add_token(Token.Types.OPERATOR_MULTIPLICATION, multiplication_operator_symbol)
		consume_source(1)
		return OK
	
	func tokenize_division_operator() -> int:
		if self.source_view.read_character_at(0) != division_operator_symbol:
			return ERR_INVALID_DATA
		add_token(Token.Types.OPERATOR_DIVISION, division_operator_symbol)
		consume_source(1)
		return OK
	
	func tokenize_modulo_operator() -> int:
		if self.source_view.read_character_at(0) != self.symbol_operator_modulo:
			return ERR_INVALID_DATA
		add_token(Token.Types.OPERATOR_MODULO, self.symbol_operator_modulo)
		consume_source(self.symbol_operator_modulo.length())
		return OK
	
	func tokenize_not_operator() -> int:
		if self.source_view.read_character_at(0) != not_operator_symbol:
			return ERR_INVALID_DATA
		add_token(Token.Types.OPERATOR_NOT, not_operator_symbol)
		consume_source(1)
		return OK
	
	func tokenize_integer_literal() -> int:
		var regex_match := self.source_view.search_with_regex_using_anchors(integer_literal_regex)
		if regex_match == null:
			return ERR_INVALID_DATA
		
		var whole_match := regex_match.get_string()
		add_token(Token.Types.LITERAL_INTEGER, whole_match)
		consume_source(whole_match.length())
		return OK
	
	func tokenize_float_literal() -> int:
		var regex_match := self.source_view.search_with_regex_using_anchors(float_literal_regex)
		if regex_match == null:
			return ERR_INVALID_DATA
		
		var whole_match := regex_match.get_string()
		add_token(Token.Types.LITERAL_FLOAT, whole_match)
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
		
		var whitespaces_match := self.source_view.search_with_regex_using_anchors(whitespaces_regex)
		if whitespaces_match != null:
			consume_source(whitespaces_match.get_string().length())
			return whitespaces_match.get_string()
		
		return ""
	
	func consume_source(amount: int) -> void:
		assert(amount >= 0, "Cannot consume a source negatively. … For now.  We can do it.")
		if amount < 0:
			return
		if amount == 0:  # just to see if/when that happens, remove me at will
			breakpoint
			return
		
		self.source_view.shrink_from_start_by(amount)
	
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
		)


## Main element of our (Abstract) Syntax Tree.
class SyntaxNode:
	extends Resource
	@export var children: Array[SyntaxNode] = []
	@export var tokens: Array[Token] = []
	
	func with_token(token: Token) -> SyntaxNode:
		self.tokens.append(token)
		return self
	
	func with_tokens(tokens: Array[Token]) -> SyntaxNode:
		self.tokens.append_array(tokens)
		return self
	
	func with_child(child: SyntaxNode) -> SyntaxNode:
		self.children.append(child)
		return self
	
	func with_children(children: Array[SyntaxNode]) -> SyntaxNode:
		self.children.append_array(children)
		return self
	
	func evaluate(context: VisitorContext) -> Variant:
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
		return self


class BodyNode:
	extends SyntaxNode


class RawDataNode:
	extends SyntaxNode
	@export var data: String = ""
	
	func with_data(value: String) -> RawDataNode:
		self.data += value
		return self
	
	func serialize_self(context: VisitorContext) -> String:
		return self.data


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


class IntegerLiteralNode:
	extends ExpressionNode
	@export var value: int
	
	func with_value(value: int) -> IntegerLiteralNode:
		self.value = value
		return self

	func evaluate(context: VisitorContext) -> Variant:
		return self.value


class FloatLiteralNode:
	extends ExpressionNode
	@export var value: float
	
	func with_value(value: float) -> FloatLiteralNode:
		self.value = value
		return self

	func evaluate(context: VisitorContext) -> Variant:
		return self.value


class OperatorNode:
	extends ExpressionNode
	func serialize(context: VisitorContext) -> String:
		return serialize_self(context)


class UnaryOperatorNode:
	extends OperatorNode
	func evaluate_unary(operand: Variant) -> Variant:
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
	
	#@export var operand_left: ExpressionNode
	#@export var operand_right: ExpressionNode
	#func with_left(operand: ExpressionNode) -> BinaryOperatorNode:
		#self.operand_left = operand
		#return self
	#func with_right(operand: ExpressionNode) -> BinaryOperatorNode:
		#self.operand_right = operand
		#return self
	
	func evaluate_binary(left: Variant, right: Variant) -> Variant:
		breakpoint  # you MUST override me !
		return null
	
	func evaluate(context: VisitorContext) -> Variant:
		assert(self.children.size() == 2)
		return evaluate_binary(
			self.children[0].evaluate(context),
			self.children[1].evaluate(context),
		)
		#return evaluate_binary(
			#self.operand_left.evaluate(context),
			#self.operand_right.evaluate(context),
		#)


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
		return left % right


class StatementIdentifierNode:
	extends ExpressionNode
	@export var identifier: String
	
	func with_identifier(value: String) -> StatementIdentifierNode:
		self.identifier = value
		return self


class EchoNode:
	extends SyntaxNode
	# FIXME Perhaps just use children with only one child allowed instead of this ?
	@export var expression: ExpressionNode

	func with_expression(value: ExpressionNode) -> EchoNode:
		self.expression = value
		return self

	func serialize(context: VisitorContext) -> String:
		return self.expression.serialize(context)


class StatementNode:
	extends SyntaxNode
	@export var identifier: String


class StatementExtension:
	extends Resource

	func get_statement_identifier() -> String:
		breakpoint  # override me !
		return ''
	
	func matches_statement_identifier(id_to_match: String) -> bool:
		return id_to_match == get_statement_identifier()


class VerbatimStatementExtension:
	extends StatementExtension
	
	func get_statement_identifier() -> String:
		return 'verbatim'
	
	func parse(
		identifier_token: Token,
		arguments_tokens: Array[Token],
		parser: Parser,
		context: ParserContext,
	) -> StatementNode:
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
		context.consume_current_token()  # {%
		context.consume_current_token()  # end<statement_identifier>
		context.consume_current_token()  # %}
		
		var content := content_tokens.reduce(
			func(acc: String, tk: Token):
				return acc + tk.whitespaces_before + tk.literal + tk.whitespaces_after
				,
			""
		)
		var content_node := RawDataNode.new().with_data(content)
		return StatementNode.new().with_children([content_node])
	
	func serialize(node: StatementNode, context: VisitorContext) -> String:
		if self.children.is_empty():
			return ""
		assert(self.children.size() == 1, "Why would there be more ?")
		return self.children[0].serialize(context)


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
	#func getCurrent() -> Token:
		#return self.tokens[self.cursor]


class ParserContext:
	extends Resource
	var statement_extensions: Array[StatementExtension]
	var tokens: Array[Token]
	var current_token_index := 0
	
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
	
	## Checks and consumes if matched.
	func match_type(type: Token.Types) -> bool:
		var matched := get_current_token().type == type
		if matched:
			consume_token()
		return matched
	
	func get_current_token(offset := 0) -> Token:
		return self.tokens[self.current_token_index + offset]
	func get_previous_token() -> Token:
		return get_current_token(-1)
	
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
			.with_tokens(tokens)
			.with_statement_extensions(statement_extensions)
		)
		
		while context.has_tokens_remaining():
			var token: Token = context.consume_current_token()
			var node: SyntaxNode = parse_token(token, context)
			body.children.append(node)
		
		return tree

	func parse_token(token: Token, context: ParserContext) -> SyntaxNode:
		match token.type:
			Token.Types.RAW_DATA:
				return RawDataNode.new().with_data(token.literal).with_token(token)
			Token.Types.ECHO_OPENER:
				#var tokens_subset := context.consume_until_type(Token.Types.ECHO_CLOSER)
				var tokens_subset: Array[Token] = []
				var expression: ExpressionNode = parse_expression(context)
				if not context.match_type(Token.Types.ECHO_CLOSER):
					raise_error("Expected }}, but got something else: %s" % context.get_current_token())
				
				var echo_tokens: Array[Token] = []
				#echo_tokens.append(token)
				#echo_tokens.append_array(tokens_subset)  # FIXME
				#echo_tokens.append(context.get_current_token(-1))
				return EchoNode.new().with_expression(expression).with_tokens(echo_tokens)
			Token.Types.STATEMENT_OPENER:
				var tokens_subset := context.consume_until_type(Token.Types.STATEMENT_CLOSER)
				var identifier_token := tokens_subset.pop_front()
				assert(identifier_token.type == Token.Types.STATEMENT_IDENTIFIER, "Expected statement identifier")
				var statement_extension := context.get_statement_extension(identifier_token.literal)
				return statement_extension.parse(identifier_token, tokens_subset, self, context)
			Token.Types.VARIABLE_IDENTIFIER:
				return VariableIdentifierNode.new().with_identifier(token.literal).with_tokens([token])
			Token.Types.STATEMENT_IDENTIFIER:
				return StatementIdentifierNode.new().with_identifier(token.literal).with_tokens([token])
			_:
				breakpoint  # implement your new token type !
		
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
	# EXPRESSION = EQUALITY
	# EQUALITY = COMPARISON ( ( "!=" | "==" ) COMPARISON )*
	# COMPARISON = ADDITION ( ( "<=" | "<" | ">=" | ">" ) ADDITION )*
	# ADDITION = MULTIPLICATION ( ( "+" | "-" ) MULTIPLICATION )*
	# MULTIPLICATION = MODULO ( ( "*" | "/" ) MODULO )*
	# MODULO = FILTER ( "%" FILTER )*
	# FILTER = UNARY ( "|" FILTER_NAME )*
	# UNARY = ( "+" | "-" | "!" ) UNARY
	#       | PRIMARY
	# PRIMARY = LITERAL
	#         | VARIABLE_IDENTIFIER
	#         | "(" EXPRESSION ")"
	func parse_expression(context: ParserContext) -> ExpressionNode:
		assert(context.has_tokens_remaining(), "Expected an expression, but got nothing.")
		return parse_equality(context)

	func parse_equality(context: ParserContext) -> ExpressionNode:
		# TODO
		return parse_comparison(context)
	
	func parse_comparison(context: ParserContext) -> ExpressionNode:
		# TODO
		return parse_addition(context)
	
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
		var node := parse_modulo(context)
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
				_:
					breakpoint
		return node
	
	func parse_modulo(context: ParserContext) -> ExpressionNode:
		var node := parse_unary(context)
		while (context.match_type(Token.Types.OPERATOR_MODULO)):
			node = (
				ModuloOperatorNode.new()
				.with_child(node)
				.with_child(parse_modulo(context))
			)
		return node
	
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
				_:
					breakpoint
		
		if node:
			node.with_child(parse_primary(context))
			node.with_token(context.get_previous_token())
		else:
			node = parse_primary(context)
		
		return node
	
	func parse_primary(context: ParserContext) -> ExpressionNode:
		# TODO: add parentheses
		return parse_literal(context)
	
	func parse_literal(context: ParserContext) -> ExpressionNode:
		var token := context.consume_current_token()
		match token.type:
			Token.Types.VARIABLE_IDENTIFIER:
				return VariableIdentifierNode.new().with_identifier(token.literal).with_token(token)
			Token.Types.LITERAL_INTEGER:
				return IntegerLiteralNode.new().with_value(int(token.literal)).with_token(token)
			Token.Types.LITERAL_FLOAT:
				return FloatLiteralNode.new().with_value(float(token.literal)).with_token(token)
			_:
				raise_error("Expected a literal, got `%s`." % token)
				return ExpressionNode.new()
	
	func raise_error(message: String):
		printerr(message)
		#push_error(message)
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

## The main method of the string template engine.
func render(source: String, variables: Dictionary) -> String:
	
	var tokenizer := Tokenizer.new()
	var tokens := tokenizer.tokenize(source)
	
	prints("Source:", source)
	prints("Variables:", variables)
	prints("Tokens:", tokens)
	
	var parser := Parser.new()
	var syntax_tree := parser.parse(tokens, self.statement_extensions)
	
	var visitor_context := VisitorContext.new()
	visitor_context.variables = variables
	
	var visitor := EvaluatorVisitor.new()
	var output := visitor.visit(syntax_tree, visitor_context)
	
	return output
