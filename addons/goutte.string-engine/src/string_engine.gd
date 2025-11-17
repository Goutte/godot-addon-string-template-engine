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

class Token:
	extends Resource
	enum Types {
		UNKNOWN,               ## Usually means there was a failure somewhere.
		RAW_DATA,              ## Most of the stuff in the source, all that is not our DSL.
		WHITESPACES,           ## Tokenize whitespaces to be able to rebuild the source as it was.
		ECHO_OPENER,           ## Usually {{
		ECHO_CLOSER,           ## Usually }}
		STATEMENT_OPENER,      ## Usually {%
		STATEMENT_CLOSER,      ## Usually %}
		STATEMENT_IDENTIFIER,  ## Examples: for, if, verbatim…
		VARIABLE_IDENTIFIER,   ## The Token only knows the name of the variable, not its value.
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
		STATEMENT_BODY,
	}
	
	# Public configuration
	var symbol_clear_whitespace := '-'  # as in {{- for example
	var symbol_clear_line_whitespace := '~'  # as in {{~ for example
	var symbol_echo_opener := '{{'
	var symbol_echo_closer := '}}'
	var symbol_statement_opener := '{%'
	var symbol_statement_closer := '%}'
	#var symbol_escaper := '\\'  # not an option (we might not feature escaping at all)
	
	# Privates
	var state: States
	var tokens: Array[Token]
	var source: String
	# TODO: If possible do not use this variable, instead use start/end cursors (ints)
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
			match self.state:
				States.RAW_DATA:
					tokenize_raw_data()
				States.ECHO_BODY:
					#consume_whitespaces()
					tokenize_whitespaces()
					tokenize_expression()
					tokenize_whitespaces()
					#consume_whitespaces()
					tokenize_echo_closer()
					set_state(States.RAW_DATA)
				States.STATEMENT_BODY:
					consume_whitespaces()
					tokenize_statement_identifier()
					consume_whitespaces()
					# FIXME: ask the statement extension on how to tokenize here
					consume_whitespaces()
					tokenize_statement_closer()
					set_state(States.RAW_DATA)
				_:
					breakpoint # unknown state (implement it!)
		
		return self.tokens
	
	func tokenize_whitespaces() -> void:
		var regex_compiled: int
		var whitespaces_regex := RegEx.new()
		regex_compiled = whitespaces_regex.compile(
			"^[\\s\\t]+"
		)
		if regex_compiled != OK:
			breakpoint
		
		var whitespaces_match := whitespaces_regex.search(source_remaining)
		if whitespaces_match == null:
			return
		
		add_token(Token.Types.WHITESPACES, whitespaces_match.get_string())
		consume_source(whitespaces_match.get_string().length())
	
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
		
		# remove me
		var escapers_regex := RegEx.new()
		compiled = escapers_regex.compile(
			"[\\\\](?<backslashes>(?:[\\\\][\\\\])*)$"
		)
		if compiled != OK:
			breakpoint
		
		
		var token_created := false
		var search_starts_at := 0
		
		while not token_created:
			var openers_match := openers_regex.search(source_remaining, search_starts_at)
			if openers_match == null:
				add_token(Token.Types.RAW_DATA, source_remaining)
				consume_source(source_remaining.length())
				token_created = true
			else:
				var match_start := openers_match.get_start()
				var raw_data_contents := source_remaining.substr(0, match_start)
				
				# Idea: Consider escaped openers like \{{ as raw data, not openers.
				# What about \\{{ ?  Or \\\\\\\\\{{ ?  Do we even escape backslashes ?  Urgh.
				#var escapers_match := escapers_regex.search(raw_data_contents)
				#if escapers_match != null:
					#search_starts_at = openers_match.get_end()
					#continue
				
				add_token(Token.Types.RAW_DATA, raw_data_contents)
				consume_source(match_start)
				token_created = true
				
				var whole_match := openers_match.get_string()
				var opener_symbol := openers_match.get_string(&'symbol')
				var clear_whitespace_symbol := openers_match.get_string(&'clear_whitespace')  # TODO
				
				match opener_symbol:
					symbol_echo_opener:
						set_state(States.ECHO_BODY)
						add_token(Token.Types.ECHO_OPENER, whole_match)
					symbol_statement_opener:
						set_state(States.STATEMENT_BODY)
						add_token(Token.Types.STATEMENT_OPENER, whole_match)
					_:
						breakpoint  # implementation missing
				
				#if opener_symbol == self.symbol_echo_opener:
					#set_state(States.ECHO_BODY)
					#add_token(Token.Types.ECHO_OPENER, whole_match)
				#else:
					#breakpoint  # unsupported opener symbol (implement it!)
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
		
		var close_match := close_regex.search(source_remaining)
		if close_match == null:
			assert(false, "Expected close token, got (TODO) instead")
			consume_source(source_remaining.length())  # no infinite loops for you
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
		
		var regex_match := statement_identifier_regex.search(source_remaining)
		if regex_match == null:
			assert(false, "Not a statement identifier")
		else:
			var whole_match := regex_match.get_string()
			add_token(Token.Types.STATEMENT_IDENTIFIER, whole_match)
			consume_source(whole_match.length())
	
	func tokenize_expression() -> void:
		var regex_compiled: int
		var variable_identifier_regex := RegEx.new()
		regex_compiled = variable_identifier_regex.compile(
			"^" +
			("[^0-9%s][^%s]*" % [forbidden_identifier_chars, forbidden_identifier_chars])
			#"[a-zA-Z_][a-zA-Z0-9_]*"  # safer, ascii-only
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
				return acc + tk.literal
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
		#if i == -1:
			#i = self.statement_extensions.find_custom(
				#func(se: StatementExtension):
					#return 'end' + se.get_statement_identifier() == identifier
			#)
		assert(i >= 0, "No statement extension found with identifier %s" % identifier)
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
	
	static var DEFAULT_STATEMENT_EXTENSIONS: Array[StatementExtension] = [
		VerbatimStatementExtension.new(),
	]
	
	func parse(
		tokens: Array[Token],
		statement_extensions: Array[StatementExtension] = DEFAULT_STATEMENT_EXTENSIONS,
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
				return EchoNode.new().with_expression(expression).with_tokens(
					echo_tokens
					#([token] + tokens_subset + [context.get_current_token(-1)]) as Array[Token]
				)
			
			Token.Types.STATEMENT_OPENER:
				var tokens_subset := context.consume_until_type(Token.Types.STATEMENT_CLOSER)
				var identifier_token := tokens_subset.pop_front()
				assert(identifier_token.type == Token.Types.STATEMENT_IDENTIFIER, "Expected statement identifier")
				var statement_extension := context.get_statement_extension(identifier_token.literal)
				return statement_extension.parse(identifier_token, tokens_subset, self, context)
				#return StatementNode.new()
				
			Token.Types.VARIABLE_IDENTIFIER:
				return VariableIdentifierNode.new().with_identifier(token.literal).with_tokens([token])
			Token.Types.STATEMENT_IDENTIFIER:
				return StatementIdentifierNode.new().with_identifier(token.literal).with_tokens([token])
			_:
				breakpoint  # implement your new token type !
		
		# Somewhat safe fallback ; should not happen anyway.
		return RawDataNode.new().with_data("")

	func parse_expression(tokens_subset: Array[Token], context: ParserContext) -> ExpressionNode:
		tokens_subset = tokens_subset.filter(func(t: Token): return t.type != Token.Types.WHITESPACES)
		
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
