use crate::Span;
use crate::lexer::{LexError, Token, TokenKind};

pub fn pretty_error(src: &str, err: &ParseError) -> String
{
	let Span { start, end, .. } = &err.span;
	let lines: Vec<&str> = src.lines().collect();
	let line_text = lines.get(start.line as usize - 1).copied().unwrap_or("");
	let width = (end.column.saturating_sub(start.column)).max(1) as usize;
	let indent = " ".repeat(start.column as usize - 1);
	let caret = "^".repeat(width);

	format!(
		"Parse error at {}\n{}\n{}{}\n{}",
		start, line_text, indent, caret, err.message
	)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Quant
{
	/// `+`  one or more
	One,
	/// `*`  zero or more
	Many,
	/// `?`  zero or one
	Opt,
}

/// A single pattern node.
#[derive(Debug, Clone)]
pub enum Pattern
{
	/// `"str"`
	Str(String, Span),
	/// `'c'`
	Char(char, Span),
	/// `(a | b | c)`  – alternation
	Alt(Vec<Pattern>, Span),
	/// `a & b`        – sequence / concatenation
	Seq(Vec<Pattern>, Span),
	/// `pat+` / `pat*` / `pat?`
	/// `name` is the optional count-capture identifier.
	Repeat
	{
		pat: Box<Pattern>,
		quant: Quant,
		name: Option<String>,
		span: Span,
	},
	/// `-> label? atom`  – consume-until
	/// `label` names the captured span (usable in `%slice(label)` / call-func args).
	Until
	{
		label: Option<String>,
		end: Box<Pattern>,
		span: Span,
	},
}

impl Pattern
{
	pub fn span(&self) -> &Span
	{
		match self {
			Pattern::Str(_, s) => s,
			Pattern::Char(_, s) => s,
			Pattern::Alt(_, s) => s,
			Pattern::Seq(_, s) => s,
			Pattern::Repeat { span, .. } => span,
			Pattern::Until { span, .. } => span,
		}
	}
}

/// A parsed modifier argument.
#[derive(Debug, Clone)]
pub enum ModifierAst
{
	/// `%reserved`
	Reserved,
	/// `%slice` or `%slice(label)`
	Slice(Option<String>),
	/// `%call_func(func_name(arg1, arg2, ...))`
	CallFunc
	{
		func: String, args: Vec<String>
	},
}

/// A top-level definition produced by the parser.
#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Definition
{
	/// `@keyword(name, "str", modifiers*)`
	Keyword
	{
		name: String,
		pattern: Pattern,
		modifiers: Vec<ModifierAst>,
		span: Span,
	},
	/// `@token(name, pattern, modifiers*)`
	Token
	{
		name: String,
		pattern: Pattern,
		modifiers: Vec<ModifierAst>,
		span: Span,
	},
	/// `@literal(name, pattern, modifiers*)`
	Literal
	{
		name: String,
		pattern: Pattern,
		modifiers: Vec<ModifierAst>,
		span: Span,
	},
	/// `@ignore(pattern)`
	Ignore
	{
		pattern: Pattern, span: Span
	},
	/// `@comment(name, pattern, modifiers*)`
	Comment
	{
		name: String,
		pattern: Pattern,
		modifiers: Vec<ModifierAst>,
		span: Span,
	},
	/// `@c_func("Token lexer_next(Lexer* lx)")`
	CFunc
	{
		func_definition: String, span: Span
	},
	/// `@c_type("typedef Lexer { SliceString src; size_t offset; } Lexer;")`
	CType
	{
		type_definition: String, span: Span
	},
}

#[derive(Debug, Clone)]
pub struct ParseError
{
	pub message: String,
	pub span: Span,
}

impl ParseError
{
	fn new(msg: impl Into<String>, span: Span) -> Self
	{
		Self {
			message: msg.into(),
			span,
		}
	}
}

impl std::fmt::Display for ParseError
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "Parse error at {}: {}", self.span.start, self.message)
	}
}

impl From<LexError> for ParseError
{
	fn from(e: LexError) -> Self
	{
		Self {
			message: e.message,
			span: e.span,
		}
	}
}

type PResult<T> = Result<T, ParseError>;

pub struct Parser
{
	tokens: Vec<Token>,
	cursor: usize,
}

impl Parser
{
	pub fn new(tokens: Vec<Token>) -> Self
	{
		Self { tokens, cursor: 0 }
	}

	fn peek(&self) -> &Token
	{
		&self.tokens[self.cursor]
	}

	fn advance(&mut self) -> &Token
	{
		let tok = &self.tokens[self.cursor];
		if self.cursor + 1 < self.tokens.len() {
			self.cursor += 1;
		}
		tok
	}

	fn at_eof(&self) -> bool
	{
		self.peek().kind == TokenKind::Eof
	}

	fn span(&self) -> Span
	{
		self.peek().span.clone()
	}

	fn expect(&mut self, kind: &TokenKind) -> PResult<&Token>
	{
		if &self.peek().kind == kind {
			Ok(self.advance())
		} else {
			Err(ParseError::new(
				format!("Expected {:?}, got {:?}", kind, self.peek().kind),
				self.span(),
			))
		}
	}

	fn expect_span(&mut self, kind: &TokenKind) -> PResult<Span>
	{
		self.expect(kind).map(|t| t.span.clone())
	}

	fn expect_ident(&mut self) -> PResult<String>
	{
		match self.peek().kind.clone() {
			TokenKind::Ident(s) => {
				self.advance();
				Ok(s)
			}
			_ => Err(ParseError::new(
				format!("Expected identifier, got {:?}", self.peek().kind),
				self.span(),
			)),
		}
	}

	fn eat_comma(&mut self)
	{
		if self.peek().kind == TokenKind::Comma {
			self.advance();
		}
	}

	pub fn parse_file(&mut self) -> PResult<Vec<Definition>>
	{
		let mut defs = Vec::new();
		while !self.at_eof() {
			defs.push(self.parse_definition()?);
		}
		Ok(defs)
	}

	fn parse_definition(&mut self) -> PResult<Definition>
	{
		match self.peek().kind.clone() {
			TokenKind::Definition(crate::lexer::Definition::Keyword) => self.parse_keyword(),
			TokenKind::Definition(crate::lexer::Definition::Token) => self.parse_token(),
			TokenKind::Definition(crate::lexer::Definition::Literal) => self.parse_literal(),
			TokenKind::Definition(crate::lexer::Definition::Ignore) => self.parse_ignore(),
			TokenKind::Definition(crate::lexer::Definition::Comment) => self.parse_comment(),
			TokenKind::Definition(crate::lexer::Definition::CFunc) => self.parse_cfunc(),
			TokenKind::Definition(crate::lexer::Definition::CType) => self.parse_ctype(),
			_ => Err(ParseError::new(
				format!(
					"Expected a definition (@keyword, @token, etc.), got {:?}",
					self.peek().kind
				),
				self.span(),
			)),
		}
	}

	fn parse_keyword(&mut self) -> PResult<Definition>
	{
		let start = self.span();
		self.advance(); // consume @keyword
		self.expect(&TokenKind::ParenOpen)?;

		let name = self.expect_ident()?;
		self.expect(&TokenKind::Comma)?;

		let pattern = self.parse_pattern()?;
		self.eat_comma();

		let modifiers = self.parse_modifiers()?;
		self.eat_comma();

		let end = self.expect_span(&TokenKind::ParenClose)?;
		let span = Span::new(start.start, end.end);

		Ok(Definition::Keyword {
			name,
			pattern,
			modifiers,
			span,
		})
	}

	fn parse_token(&mut self) -> PResult<Definition>
	{
		let start = self.span();
		self.advance(); // consume @token
		self.expect(&TokenKind::ParenOpen)?;

		let name = self.expect_ident()?;
		self.expect(&TokenKind::Comma)?;

		let pattern = self.parse_pattern()?;
		self.eat_comma();

		let modifiers = self.parse_modifiers()?;
		self.eat_comma();

		let end = self.expect_span(&TokenKind::ParenClose)?;
		let span = Span::new(start.start, end.end);

		Ok(Definition::Token {
			name,
			pattern,
			modifiers,
			span,
		})
	}

	fn parse_literal(&mut self) -> PResult<Definition>
	{
		let start = self.span();
		self.advance(); // consume @literal
		self.expect(&TokenKind::ParenOpen)?;

		let name = self.expect_ident()?;
		self.expect(&TokenKind::Comma)?;

		let pattern = self.parse_pattern()?;
		self.eat_comma();

		let modifiers = self.parse_modifiers()?;
		self.eat_comma();

		let end = self.expect_span(&TokenKind::ParenClose)?;
		let span = Span::new(start.start, end.end);

		Ok(Definition::Literal {
			name,
			pattern,
			modifiers,
			span,
		})
	}

	fn parse_ignore(&mut self) -> PResult<Definition>
	{
		let start = self.span();
		self.advance(); // consume @ignore
		self.expect(&TokenKind::ParenOpen)?;

		let pattern = self.parse_pattern()?;
		self.eat_comma();

		let end = self.expect_span(&TokenKind::ParenClose)?;
		let span = Span::new(start.start, end.end);

		Ok(Definition::Ignore { pattern, span })
	}

	fn parse_comment(&mut self) -> PResult<Definition>
	{
		let start = self.span();
		self.advance(); // consume @comment
		self.expect(&TokenKind::ParenOpen)?;

		let name = self.expect_ident()?;
		self.expect(&TokenKind::Comma)?;

		let pattern = self.parse_pattern()?;
		self.eat_comma();

		let modifiers = self.parse_modifiers()?;
		self.eat_comma();

		let end = self.expect_span(&TokenKind::ParenClose)?;
		let span = Span::new(start.start, end.end);

		Ok(Definition::Comment {
			name,
			pattern,
			modifiers,
			span,
		})
	}

	fn parse_cfunc(&mut self) -> PResult<Definition>
	{
		let start = self.span();
		self.advance(); // consume @c_func
		self.expect(&TokenKind::ParenOpen)?;

		let tok = self.advance();
		let TokenKind::Str(func_definition) = tok.kind.clone() else {
			return Err(ParseError {
				message: "expected String".to_string(),
				span: tok.span.clone(),
			});
		};

		let end = self.expect_span(&TokenKind::ParenClose)?;
		let span = Span::new(start.start, end.end);

		Ok(Definition::CFunc { func_definition, span })
	}

	fn parse_ctype(&mut self) -> PResult<Definition>
	{
		let start = self.span();
		self.advance(); // consume @c_type
		self.expect(&TokenKind::ParenOpen)?;

		let tok = self.advance();
		let TokenKind::Str(type_definition) = tok.kind.clone() else {
			return Err(ParseError {
				message: "expected String".to_string(),
				span: tok.span.clone(),
			});
		};

		let end = self.expect_span(&TokenKind::ParenClose)?;
		let span = Span::new(start.start, end.end);

		Ok(Definition::CType { type_definition, span })
	}

	fn parse_modifiers(&mut self) -> PResult<Vec<ModifierAst>>
	{
		let mut mods = Vec::new();
		while let TokenKind::Modifier(_) = &self.peek().kind {
			mods.push(self.parse_one_modifier()?);
			self.eat_comma();
		}
		Ok(mods)
	}

	fn parse_one_modifier(&mut self) -> PResult<ModifierAst>
	{
		match self.peek().kind.clone() {
			TokenKind::Modifier(crate::lexer::Modifier::Reserved) => {
				self.advance();
				Ok(ModifierAst::Reserved)
			}
			TokenKind::Modifier(crate::lexer::Modifier::Slice) => self.parse_slice_modifier(),
			TokenKind::Modifier(crate::lexer::Modifier::CallFunc) => self.parse_call_func_modifier(),
			TokenKind::Modifier(crate::lexer::Modifier::Type) => Err(ParseError::new(
				"%type has been removed — use %slice or %call_func instead",
				self.span(),
			)),
			ref other => Err(ParseError::new(
				format!(
					"Unknown modifier {:?} — valid modifiers are %reserved, %slice, %slice(label), %call_func(fn(args))",
					other
				),
				self.span(),
			)),
		}
	}

	fn parse_call_func_modifier(&mut self) -> PResult<ModifierAst>
	{
		let span = self.span();
		// consume the %call_func modifier token
		match &self.peek().kind {
			TokenKind::Modifier(crate::lexer::Modifier::CallFunc) => {
				self.advance();
			}
			_ => {
				return Err(ParseError::new(
					format!("Expected %call_func, got {:?}", self.peek().kind),
					span,
				));
			}
		}

		// `(`
		self.expect(&TokenKind::ParenOpen)?;

		// function name
		let func = self.expect_ident()?;

		// `(args*)`
		self.expect(&TokenKind::ParenOpen)?;
		let args = self.parse_call_args()?;
		self.expect(&TokenKind::ParenClose)?;

		// `)`
		self.expect(&TokenKind::ParenClose)?;

		Ok(ModifierAst::CallFunc { func, args })
	}

	fn parse_slice_modifier(&mut self) -> PResult<ModifierAst>
	{
		self.advance(); // consume %slice token

		if self.peek().kind == TokenKind::ParenOpen {
			self.advance();
			let label = self.expect_ident()?;
			self.expect(&TokenKind::ParenClose)?;
			Ok(ModifierAst::Slice(Some(label)))
		} else {
			Ok(ModifierAst::Slice(None))
		}
	}

	fn parse_call_args(&mut self) -> PResult<Vec<String>>
	{
		let mut args = Vec::new();
		loop {
			match &self.peek().kind {
				TokenKind::ParenClose => break,
				TokenKind::Ident(_) => {
					args.push(self.expect_ident()?);
					self.eat_comma();
				}
				_ => {
					return Err(ParseError::new(
						format!(
							"Expected identifier or ')' in argument list, got {:?}",
							self.peek().kind
						),
						self.span(),
					));
				}
			}
		}
		Ok(args)
	}

	pub fn parse_pattern(&mut self) -> PResult<Pattern>
	{
		self.parse_alternation()
	}

	fn parse_alternation(&mut self) -> PResult<Pattern>
	{
		let start = self.span();
		let first = self.parse_sequence()?;

		if self.peek().kind != TokenKind::Pipe {
			return Ok(first);
		}

		let mut alts = vec![first];
		while self.peek().kind == TokenKind::Pipe {
			self.advance(); // consume '|'
			alts.push(self.parse_sequence()?);
		}

		let end = alts.last().unwrap().span().end;
		Ok(Pattern::Alt(alts, Span::new(start.start, end)))
	}

	fn parse_sequence(&mut self) -> PResult<Pattern>
	{
		let start = self.span();
		let first = self.parse_term()?;

		let mut seq = vec![first];
		loop {
			// consume optional explicit '&'
			let explicit = self.peek().kind == TokenKind::Ampersand;
			if explicit {
				self.advance();
			}

			// check whether the next token can start an atom
			let can_start = matches!(
				self.peek().kind,
				TokenKind::Str(_) | TokenKind::Char(_) | TokenKind::ParenOpen | TokenKind::Arrow
			);

			if !can_start {
				// if we consumed a '&' but nothing follows, that's an error
				if explicit {
					return Err(ParseError::new("Expected a pattern after '&'", self.span()));
				}
				break;
			}

			seq.push(self.parse_term()?);
		}

		if seq.len() == 1 {
			return Ok(seq.remove(0));
		}

		let end = seq.last().unwrap().span().end;
		Ok(Pattern::Seq(seq, Span::new(start.start, end)))
	}

	fn parse_term(&mut self) -> PResult<Pattern>
	{
		let start = self.span();
		let atom = self.parse_atom()?;

		// optional quantifier
		let quant = match self.peek().kind {
			TokenKind::Plus => Some(Quant::One),
			TokenKind::Star => Some(Quant::Many),
			TokenKind::Question => Some(Quant::Opt),
			_ => None,
		};

		if let Some(quant) = quant {
			self.advance(); // consume +/*/?

			// optional count-capture name:  +name
			let name = match &self.peek().kind {
				TokenKind::Ident(_) => {
					let n = self.expect_ident()?;
					Some(n)
				}
				_ => None,
			};

			let end = self.span().start;
			let span = Span::new(start.start, end);
			Ok(Pattern::Repeat {
				pat: Box::new(atom),
				quant,
				name,
				span,
			})
		} else {
			Ok(atom)
		}
	}

	fn parse_atom(&mut self) -> PResult<Pattern>
	{
		let start = self.span();
		match self.peek().kind.clone() {
			// string literal
			TokenKind::Str(s) => {
				self.advance();
				let span = Span::new(start.start, self.span().start);
				Ok(Pattern::Str(s, span))
			}

			// char literal
			TokenKind::Char(c) => {
				self.advance();
				let span = Span::new(start.start, self.span().start);
				Ok(Pattern::Char(c, span))
			}

			// grouped pattern
			TokenKind::ParenOpen => {
				self.advance(); // '('
				let inner = self.parse_pattern()?;
				self.expect(&TokenKind::ParenClose)?;
				let span = Span::new(start.start, self.span().start);
				Ok(match inner {
					Pattern::Alt(arms, _) => Pattern::Alt(arms, span),
					other => other,
				})
			}

			// consume-until:  -> label? atom
			TokenKind::Arrow => {
				self.advance(); // '->'

				let label = match &self.peek().kind {
					TokenKind::Ident(_) => Some(self.expect_ident()?),
					_ => None,
				};

				let end_pat = self.parse_atom()?;
				let span = Span::new(start.start, end_pat.span().end);
				Ok(Pattern::Until {
					label,
					end: Box::new(end_pat),
					span,
				})
			}

			other => Err(ParseError::new(
				format!("Expected a pattern (string, char, '(' or '->'), got {:?}", other),
				start,
			)),
		}
	}
}

impl std::fmt::Display for Pattern
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		match self {
			Pattern::Str(s, _) => write!(f, "{:?}", s),
			Pattern::Char(c, _) => write!(f, "'{}'", c),
			Pattern::Alt(arms, _) => {
				write!(f, "(")?;
				for (i, arm) in arms.iter().enumerate() {
					if i > 0 {
						write!(f, " | ")?;
					}
					write!(f, "{}", arm)?;
				}
				write!(f, ")")
			}
			Pattern::Seq(parts, _) => {
				for (i, p) in parts.iter().enumerate() {
					if i > 0 {
						write!(f, " & ")?;
					}
					write!(f, "{}", p)?;
				}
				Ok(())
			}
			Pattern::Repeat { pat, quant, name, .. } => {
				let q = match quant {
					Quant::One => '+',
					Quant::Many => '*',
					Quant::Opt => '?',
				};
				write!(f, "{}{}", pat, q)?;
				if let Some(n) = name {
					write!(f, "{}", n)?;
				}
				Ok(())
			}
			Pattern::Until { label, end, .. } => {
				write!(f, "->")?;
				if let Some(l) = label {
					write!(f, "{} ", l)?;
				}
				write!(f, "{}", end)
			}
		}
	}
}

impl std::fmt::Display for ModifierAst
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		match self {
			ModifierAst::Reserved => write!(f, "%reserved"),
			ModifierAst::Slice(None) => write!(f, "%slice"),
			ModifierAst::Slice(Some(l)) => write!(f, "%slice({})", l),
			ModifierAst::CallFunc { func, args } => {
				write!(f, "%call_func({}(", func)?;
				for (i, a) in args.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", a)?;
				}
				write!(f, "))")
			}
		}
	}
}
