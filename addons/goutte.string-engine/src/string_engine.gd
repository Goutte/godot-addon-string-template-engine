# Vanilla GDScript "port" of Inja, Twig, Jinja…
# If you need perfs, try the Ginja addon using a gdextension to wrap Inja.

## A String Template Engine, super slow but flexible.
class_name StringEngine
extends RefCounted

# TENTATIVE GRAMMAR
# -----------------
# LITERAL_STRING = /["]TODO["]/
# LITERAL_NUMBER = LITERAL_INTEGER | LITERAL_FLOAT
# LITERAL = LITERAL_NUMBER | LITERAL_STRING
# VARIABLE_IDENTIFIER = /[a-zA-Z_][a-zA-Z0-9_]*/
# EXPRESSION = VARIABLE_IDENTIFIER | LITERAL
# ECHO = ECHO_OPENER EXPRESSION ECHO_CLOSER


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
		RAW_DATA,                 ## Most of the stuff in the source, all that is not our DSL.
		ECHO_OPENER,              ## Usually {{
		ECHO_CLOSER,              ## Usually }}
		STATEMENT_OPENER,         ## Usually {%
		STATEMENT_CLOSER,         ## Usually %}
		STATEMENT_IDENTIFIER,     ## Examples: for, if, verbatim…
		VARIABLE_IDENTIFIER,      ## The Token only knows the name of the variable, not its value.
		LITERAL_INTEGER,          ## 42
		OPERATOR_ADDITION,        ## +
		OPERATOR_SUBTRACTION,     ## -
		OPERATOR_MULTIPLICATION,  ## *
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


class StringView:
	extends Resource
	@export var string: String
	@export var start: int
	@export var len: int
	
	func _init(
		string := "",
		start := -1,
		len:= -1,
	) -> void:
		self.string = string
		if start < 0:
			start = 0
		self.start = start
		if len < 0:
			len = self.string.length()
		self.len = len
		sanitize_delimiters()
	
	func sanitize_delimiters() -> void:
		self.start = clampi(self.start, 0, self.string.length())
		self.len = clampi(self.len, 0, self.string.length())
	
	func search_with_regex(regex: RegEx) -> RegExMatch:
		# This is faster but breaks on line anchor metacharacters like ^ and $
		return regex.search(self.string, self.start, self.start + self.len)
	
	func search_with_regex_using_anchors(regex: RegEx) -> RegExMatch:
		# Much slower, but works.
		return regex.search(self.get_as_string())
	
	func length() -> int:
		return self.len
	
	func shrink_from_start_by(amount: int) -> void:
		self.start = self.start + amount
		self.len = self.len - amount
		sanitize_delimiters()
	
	func get_as_string() -> String:
		return self.string.substr(self.start, self.len)
	
	func _to_string() -> String:
		return get_as_string()


## Probably closer to a Lexer now as its states are tied to our grammar.
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
	
	# Privates
	var state: States
	var tokens: Array[Token]  # TODO: make a TokenStream class
	var source: String     # Immutable whole source, we work on a view of it (StringView class?)
	#var source_start: int  # Start index of our view on the source, inclusive
	#var source_end: int    # End index of our view on the source, exclusive
	var source_view: StringView
	# TODO: If possible do not use this variable, instead use start/end cursors (ints)
	#var source_remaining: String
	
	# @protected
	func reset():
		state = States.RAW_DATA
		tokens = []  # do NOT clear() ; copy is needed ← output of tokenize() !
		source = ""
		#source_start = 0
		#source_end = 0
		source_view = null
		#source_remaining = ""
	
	## The main job of a Tokenizer is to create a stream of tokens from a source.
	func tokenize(template: String) -> Array[Token]:
		reset()
		self.source = template
		#self.source_start = 0
		#self.source_end = self.source.length()
		self.source_view = StringView.new(template, 0, template.length())
		#self.source_remaining = template
		
		#while not self.source_remaining.is_empty():
		while self.source_view.length() > 0:
			match self.state:
				States.RAW_DATA:
					tokenize_raw_data()
				States.ECHO:
					consume_whitespaces_into_previous_token()
					tokenize_expression()
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
	
	# @protected
	func tokenize_raw_data() -> void:
		# There is no way to discriminate raw data by itself.
		# We're going to advance to the first found OPENER of our syntax.
		# With the default configuration, OPENERs are `{{`, `{%` and `{#`.
		# We need to account for backslashes prefixs, the escape sequences,
		# and therefore consider as raw data the escaped openings such as `\{{`.
		var compiled: int
		var openers_regex := RegEx.new()
		compiled = openers_regex.compile(
			#"(?<![\\\\]([\\\\][\\\\])*)" +  # no lookbehind in Godot  T_T
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
		if compiled != OK:
			breakpoint
		
		#var openers_match := openers_regex.search(source_remaining, search_starts_at)
		prints("Searching for openers", self.source_view.start, self.source_view.len)
		var openers_match := self.source_view.search_with_regex_using_anchors(openers_regex)
		#var openers_match := self.source_view.search_with_regex(openers_regex)
		
		#var openers_match := openers_regex.search(
			#self.source,
			#self.source_start,
			#self.source_end,
		#)
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
		
		var close_match := self.source_view.search_with_regex(close_regex)
		#var close_match := close_regex.search(source_remaining)
		if close_match == null:
			assert(false, "Expected close token, got (TODO) instead")
			consume_source(source_view.length())  # no infinite loops for you
		else:
			var whole_match := close_match.get_string()
			var close_symbol := close_match.get_string(&'symbol')
			var clear_whitespace_symbol := close_match.get_string(&'clear_whitespace')
			add_token(token_type, whole_match)
			consume_source(whole_match.length())
	
	var forbidden_identifier_chars := " #%<>{}()\\[\\]^'\"|.~+*-"  # MUST end with -
	
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
		
		#var regex_match := statement_identifier_regex.search(source_remaining)
		var regex_match := self.source_view.search_with_regex_using_anchors(statement_identifier_regex)
		if regex_match == null:
			assert(false, "Not a statement identifier")
		else:
			var whole_match := regex_match.get_string()
			add_token(Token.Types.STATEMENT_IDENTIFIER, whole_match)
			consume_source(whole_match.length())
	
	func tokenize_expression() -> int:
		if tokenize_variable_identifier() == OK: return OK
		breakpoint
		return ERR_METHOD_NOT_FOUND
	
	func tokenize_variable_identifier() -> int:
		var regex_compiled: int
		var variable_identifier_regex := RegEx.new()
		regex_compiled = variable_identifier_regex.compile(
			"^" +
			("[^0-9%s][^%s]*" % [forbidden_identifier_chars, forbidden_identifier_chars])
			#"[a-zA-Z_][a-zA-Z0-9_]*"  # safer, ascii-only
		)
		if regex_compiled != OK:
			breakpoint
			return ERR_PRINTER_ON_FIRE
		
		#var regex_match := variable_identifier_regex.search(source_remaining)
		var regex_match := self.source_view.search_with_regex_using_anchors(variable_identifier_regex)
		if regex_match == null:
			#assert(false, "Not a variable identifier")
			return ERR_INVALID_DATA
		
		var whole_match := regex_match.get_string()
		add_token(Token.Types.VARIABLE_IDENTIFIER, whole_match)
		consume_source(whole_match.length())
		return OK
	
	func set_state(value: States) -> void:
		self.state = value
	
	func add_token(type: Token.Types, literal: String) -> void:
		tokens.append(Token.new().with_type(type).with_literal(literal))
	
	func consume_whitespaces_into_previous_token() -> void:
		assert(not self.tokens.is_empty())
		var consumed_whitespaces := consume_whitespaces()
		self.tokens[-1].whitespaces_after = consumed_whitespaces
		
	func consume_whitespaces() -> String:
		#var whitespaces_consumed := ""
		#var source_afterwards = self.source_remaining.lstrip(" \t")
		#var amount := self.source_view.length() - source_afterwards.length()
		#whitespaces_consumed += self.source_remaining.substr(0, amount)
		#self.source_remaining = source_afterwards
		
		var regex_compiled: int
		var whitespaces_regex := RegEx.new()
		regex_compiled = whitespaces_regex.compile(
			"^[ ]+"
		)
		if regex_compiled != OK:
			breakpoint  # regex broke ; dev intrigued
			return ""
		
		var whitespaces_match := self.source_view.search_with_regex_using_anchors(whitespaces_regex)
		if whitespaces_match != null:
			prints("Tokenizer", "Consuming %d whitespaces" % whitespaces_match.get_string().length())
			consume_source(whitespaces_match.get_string().length())
			return whitespaces_match.get_string()
		
		#breakpoint
		
		return ""
	
	func consume_source(amount: int) -> void:
		assert(amount >= 0)
		if amount < 0:
			return
		
		if amount == 0:  # just to see if/when that happens, remove me at will
			breakpoint
			return
		
		#self.source_remaining = self.source_remaining.substr(amount)
		prints('consuming source view:', source_view)
		prints('amount', amount)
		self.source_view.shrink_from_start_by(amount)
		prints('source view after    :', source_view)
	
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
	@export var literal: String
	@export var tokens: Array[Token] = []
	
	func with_tokens(tokens: Array[Token]) -> SyntaxNode:
		self.tokens.append_array(tokens)
		return self
	func with_children(children: Array[SyntaxNode]) -> SyntaxNode:
		self.children.append_array(children)
		return self
	
	func evaluate(context: VisitorContext) -> String:
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


class StatementIdentifierNode:
	extends ExpressionNode
	@export var identifier: String
	
	func with_identifier(value: String) -> StatementIdentifierNode:
		self.identifier = value
		return self


class EchoNode:
	extends SyntaxNode
	# Perhaps just use children with only one child allowed instead of this ?
	@export var expression: ExpressionNode

	func with_expression(value: ExpressionNode) -> EchoNode:
		self.expression = value
		return self

	func evaluate(context: VisitorContext) -> String:
		return self.expression.evaluate(context)


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
	
	func evaluate(node: StatementNode, context: VisitorContext) -> String:
		if self.children.is_empty():
			return ""
		assert(self.children.size() == 1, "Why would there be more ?")
		return self.children[0].evaluate(context)


#class VerbatimStatementNode:
	#extends StatementNode
	#func evaluate(context: VisitorContext) -> String:
		#if self.children.is_empty():
			#return ""
		#assert(self.children.size() == 1, "Why would there be more ?")
		#return self.children[0].evaluate(context)



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
	
	func consume_current_token() -> Token:
		consume_token()
		return get_current_token(-1)
	
	func get_current_token(offset := 0) -> Token:
		return self.tokens[self.current_token_index + offset]
	
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
		var tokens_amount := tokens.size()
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
				return RawDataNode.new().with_data(token.literal).with_tokens([token])
			Token.Types.ECHO_OPENER:
				var tokens_subset := context.consume_until_type(Token.Types.ECHO_CLOSER)
				var expression: ExpressionNode = parse_expression(tokens_subset, context)
				var echo_tokens: Array[Token] = []
				echo_tokens.append(token)
				echo_tokens.append_array(tokens_subset)
				echo_tokens.append(context.get_current_token(-1))
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

	func parse_expression(tokens_subset: Array[Token], context: ParserContext) -> ExpressionNode:
		# 
		assert(tokens_subset.size() > 0, "Expected an expression, got …")
		#assert(tokens_subset.size() == 1)
		
		if tokens_subset.size() == 1:
			var token := tokens_subset[0]
			match token.type:
				Token.Types.VARIABLE_IDENTIFIER:
					return VariableIdentifierNode.new().with_identifier(token.literal)
				_:
					breakpoint  # unexpected token
					return ExpressionNode.new()
		
		
		
		return ExpressionNode.new()


class VisitorContext:
	extends Resource
	## identifier:String => value:Variant
	var variables := {}


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

#class CompilerVisitor:  # TODO: cache the template as pure GdScript
#class HighlighterVisitor:  # TODO: syntax highlighting for Godot's code editor

## The main method of the string template engine.
func render(source: String, variables: Dictionary) -> String:
	
	prints("Template", source)
	
	var tokenizer := Tokenizer.new()
	var tokens := tokenizer.tokenize(source)
	
	prints("Tokens", tokens)
	
	var parser := Parser.new()
	var syntax_tree := parser.parse(tokens, self.statement_extensions)
	
	var visitor_context := VisitorContext.new()
	visitor_context.variables = variables
	
	var visitor := EvaluatorVisitor.new()
	var output := visitor.visit(syntax_tree, visitor_context)
	
	return output
