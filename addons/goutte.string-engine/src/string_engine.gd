# Vanilla GDScript "port" of Inja, Twig, Jinja…
# If you need perfs, try the Ginja addon using a gdextension to wrap Inja.

## A String Template Engine, super slow but flexible.
class_name StringEngine
extends RefCounted


# LITERAL_STRING = /["]TODO["]/
# LITERAL_NUMBER = LITERAL_INTEGER | LITERAL_FLOAT
# LITERAL = LITERAL_NUMBER | LITERAL_STRING
# VARIABLE_IDENTIFIER = /[a-zA-Z_][a-zA-Z0-9_]*/
# EXPRESSION = VARIABLE_IDENTIFIER | LITERAL
# ECHO = ECHO_OPEN EXPRESSION ECHO_CLOSE

class Token:
	extends Resource
	enum Types {
		UNKNOWN,
		RAW_DATA,
		ECHO_OPENER,  # {{
		ECHO_CLOSER,  # }}
		VARIABLE_IDENTIFIER,
	}
	
	@export var type := Types.UNKNOWN
	@export var literal := ""
	
	func with_type(value: Types) -> Token:
		type = value
		return self
	func with_literal(value: String) -> Token:
		literal = value
		return self
	
	func _to_string() -> String:
		return literal


class Tokenizer:
	extends RefCounted
	enum States {
		RAW_DATA,
		ECHO_BODY,
	}
	
	var symbol_clear_whitespace := '-'  # as in {{- for example
	var symbol_clear_line_whitespace := '~'  # as in {{~ for example
	var symbol_echo_opener := '{{'
	var symbol_echo_closer := '}}'
	#var symbol_open_statement := '{%'
	#var symbol_close_statement := '%}'
	
	var state: States
	var tokens: Array[Token]
	var source: String
	# TODO: If possible do not use this variable, instead use a character cursor (int)
	var source_remaining: String
	
	# @protected
	func reset():
		state = States.RAW_DATA
		tokens = []  # do NOT clear() ; copy is needed ← output of tokenize() !
		source = ""
		source_remaining = ""
	
	## The main job of a Tokenizer is to create a stream of tokens from a source.
	func tokenize(template: String) -> Array[Token]:
		reset()
		self.source = template
		self.source_remaining = template
		
		while not self.source_remaining.is_empty():
			if States.RAW_DATA == self.state:
				tokenize_raw_data()
			elif States.ECHO_BODY == self.state:
				consume_whitespaces()
				tokenize_expression()
				consume_whitespaces()
				tokenize_echo_close()
				set_state(States.RAW_DATA)
			else:
				breakpoint # unknown state (implement it!)
		
		return self.tokens
	
	# @protected
	func tokenize_raw_data() -> void:
		# There is no way to categorize raw data by itself.
		# We're going to advance to the first found OPEN of our grammar.
		# With the default configuration, OPENs are `{{`, `{%` and `{#`.
		# We need to account for backslashes prefixs, the escape sequences,
		# and therefore consider as raw data the escaped openings such as `\{{`.
		var compiled: int
		var openers_regex := RegEx.new()
		compiled = openers_regex.compile(
			"(?<symbol>" +
			escape_for_regex(self.symbol_echo_opener) +
			#"|" +
			#escape_for_regex(symbol_open_statement) +
			")" +
			"(?<clear_whitespace>" +
			escape_for_regex(self.symbol_clear_whitespace) +
			"|" +
			escape_for_regex(self.symbol_clear_line_whitespace) +
			"|" +
			")"
		)
		if compiled != OK:
			breakpoint
		
		var openers_match := openers_regex.search(source_remaining)
		if openers_match == null:
			add_token(Token.Types.RAW_DATA, source_remaining)
			consume_source(source_remaining.length())
		else:
			var match_start := openers_match.get_start()
			add_token(Token.Types.RAW_DATA, source_remaining.substr(0, match_start))
			consume_source(match_start)
			
			var whole_match := openers_match.get_string()
			var opener_symbol := openers_match.get_string(&'symbol')
			var clear_whitespace_symbol := openers_match.get_string(&'clear_whitespace')  # TODO
			if opener_symbol == self.symbol_echo_opener:
				set_state(States.ECHO_BODY)
				add_token(Token.Types.ECHO_OPENER, whole_match)
			else:
				breakpoint  # unsupported opener symbol (implement it!)
			consume_source(whole_match.length())

	func tokenize_echo_close() -> void:
		tokenize_close(self.symbol_echo_closer, Token.Types.ECHO_CLOSER)

	func tokenize_close(symbol: String, token_type: Token.Types) -> void:
		var compiled: int
		var close_regex := RegEx.new()
		compiled = close_regex.compile(
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
		
		var close_match := close_regex.search(source_remaining)
		if close_match == null:
			assert(false, "Expected close token, got (TODO) instead")
			consume_source(source_remaining.length())  # no infinite loops for you
		else:
			var whole_match := close_match.get_string()
			var open_symbol := close_match.get_string(&'symbol')
			var clear_whitespace_symbol := close_match.get_string(&'clear_whitespace')
			add_token(token_type, whole_match)
			consume_source(whole_match.length())
	
	func tokenize_expression() -> void:
		var regex_compiled: int
		var variable_identifier_regex := RegEx.new()
		regex_compiled = variable_identifier_regex.compile(
			"^" +
			"[a-zA-Z_][a-zA-Z0-9_]*"
		)
		if regex_compiled != OK:
			breakpoint
		
		var regex_match := variable_identifier_regex.search(source_remaining)
		if regex_match == null:
			assert(false, "Not a variable identifier")
		else:
			var whole_match := regex_match.get_string()
			add_token(Token.Types.VARIABLE_IDENTIFIER, whole_match)
			consume_source(whole_match.length())
	
	func set_state(value: States) -> void:
		self.state = value
	
	func add_token(type: Token.Types, literal: String) -> void:
		tokens.append(Token.new().with_type(type).with_literal(literal))
	
	func consume_whitespaces() -> void:
		self.source_remaining = self.source_remaining.lstrip(" \t")
	
	func consume_source(amount: int) -> void:
		if amount <= 0:
			breakpoint # just to see, remove me
			return
		self.source_remaining = self.source_remaining.substr(amount)
	
	# No RegEx.escape() support yet, see https://github.com/godotengine/godot-proposals/issues/7995
	func escape_for_regex(input: String) -> String:
		input = input.replace("\\", "\\\\")
		input = input.replace(".", "\\.")
		input = input.replace("^", "\\^")
		input = input.replace("$", "\\$")
		input = input.replace("*", "\\*")
		input = input.replace("+", "\\+")
		input = input.replace("?", "\\?")
		input = input.replace("(", "\\(")
		input = input.replace(")", "\\)")
		input = input.replace("[", "\\[")
		input = input.replace("]", "\\]")
		input = input.replace("{", "\\{")
		input = input.replace("}", "\\}")
		input = input.replace("|", "\\|")
		return input


class SyntaxNode:
	extends Resource
	@export var children: Array[SyntaxNode] = []
	
	func evaluate(context: VisitorContext) -> String:
		#breakpoint
		return evaluate_self(context) + evaluate_children(context)
	
	func evaluate_self(context: VisitorContext) -> String:
		return ""
	
	func evaluate_children(context: VisitorContext) -> String:
		var output := ""
		for child: SyntaxNode in self.children:
			output += child.evaluate(context)
		return output


class SyntaxTree:
	extends SyntaxNode
	@export var body: BodyNode
	
	func evaluate(context: VisitorContext) -> String:
		return self.body.evaluate(context)
	
	func with_body(value: BodyNode) -> SyntaxTree:
		self.body = value
		return self


class BodyNode:
	extends SyntaxNode


class RawDataNode:
	extends SyntaxNode
	@export var data: String = ""
	
	func with_data(value: String) -> RawDataNode:
		self.data = value
		return self
	
	func evaluate_self(context: VisitorContext) -> String:
		return data


class ExpressionNode:
	extends SyntaxNode


class VariableIdentifierNode:
	extends ExpressionNode
	@export var identifier: String
	
	func with_identifier(value: String) -> VariableIdentifierNode:
		self.identifier = value
		return self

	func evaluate(context: VisitorContext) -> String:
		return str(context.variables.get(self.identifier, ''))


class EchoNode:
	extends SyntaxNode
	# Perhaps just use children with only one child allowed instead of this ?
	@export var expression: ExpressionNode

	func with_expression(value: ExpressionNode) -> EchoNode:
		self.expression = value
		return self

	func evaluate(context: VisitorContext) -> String:
		return self.expression.evaluate(context)


class ParserContext:
	extends Resource
	
	var tokens: Array[Token]
	var current_token_index := 0
	
	func with_tokens(value: Array[Token]) -> ParserContext:
		self.tokens = value
		return self
	
	func has_tokens_remaining() -> bool:
		return (
			self.current_token_index
			<
			self.tokens.size()
		)
	
	func consume_current_token() -> Token:
		consume_token()
		return get_current_token(-1)
	
	func get_current_token(offset := 0) -> Token:
		return self.tokens[self.current_token_index + offset]
	
	func consume_token() -> void:
		self.current_token_index += 1
	
	func consume_until_type(type: Token.Types) -> Array[Token]:
		var consumed: Array[Token] = []
		while has_tokens_remaining():
			var token := consume_current_token()
			if token.type == type:
				break
			consumed.append(token)
		return consumed
	


class Parser:
	extends RefCounted
	
	func parse(tokens: Array[Token]) -> SyntaxTree:
		var tokens_amount := tokens.size()
		var body := BodyNode.new()
		var tree := SyntaxTree.new().with_body(body)
		var context := (
			ParserContext
			.new()
			.with_tokens(tokens)
		)
		
		while context.has_tokens_remaining():
			var token: Token = context.consume_current_token()
			
			#var node: SyntaxNode = token.parse(context)
			var node: SyntaxNode = parse_token(token, context)
			body.children.append(node)
		
		
		return tree

	func parse_token(token: Token, context: ParserContext) -> SyntaxNode:
		match token.type:
			Token.Types.RAW_DATA:
				return RawDataNode.new().with_data(token.literal)
			Token.Types.ECHO_OPENER:
				var tokens_subset := context.consume_until_type(Token.Types.ECHO_CLOSER)
				var expression: ExpressionNode = parse_expression(tokens_subset, context)
				return EchoNode.new().with_expression(expression)
			Token.Types.VARIABLE_IDENTIFIER:
				return VariableIdentifierNode.new().with_identifier(token.literal)
			_:
				breakpoint  # implement your new token type !
		
		# Somewhat safe fallback ; should not happen anyway.
		return RawDataNode.new().with_data("")

	func parse_expression(tokens_subset: Array[Token], context: ParserContext) -> ExpressionNode:
		assert(tokens_subset.size() > 0, "Expected an expression, got …")
		assert(tokens_subset.size() == 1)
		
		var token := tokens_subset[0]
		match token.type:
			Token.Types.VARIABLE_IDENTIFIER:
				return VariableIdentifierNode.new().with_identifier(token.literal)
			_:
				breakpoint
		
		return ExpressionNode.new()

class VisitorContext:
	extends Resource
	## identifier:String => value:Variant
	var variables := {}


#class CompilerVisitor:  # TODO: cache the template as pure GdScript
#class HighlighterVisitor:  # TODO: syntax highlighting for Godot's code editor

## Outputs the evaluated template with variables replaced and logic applied.
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
		self.output += node.evaluate(context)


func render(source: String, variables: Dictionary) -> String:
	
	var tokenizer := Tokenizer.new()
	var tokens := tokenizer.tokenize(source)
	#prints("Tokens:", tokens)
	
	var parser := Parser.new()
	var syntax_tree := parser.parse(tokens)
	
	var visitor_context := VisitorContext.new()
	visitor_context.variables = variables
	
	var visitor := EvaluatorVisitor.new()
	var output := visitor.visit(syntax_tree, visitor_context)
	
	return output
