use crate::Origin;
use crate::Position;
use crate::Span;
use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Definition
{
	Keyword,
	Ignore,
	Token,
	Literal,
	StartWith,
	Comment,
	Macro,
	CFunc,
	CType,
}

impl Definition
{
	fn parse(s: &str) -> Option<Self>
	{
		match s {
			"keyword" => Some(Self::Keyword),
			"ignore" => Some(Self::Ignore),
			"token" => Some(Self::Token),
			"literal" => Some(Self::Literal),
			"start_with" => Some(Self::StartWith),
			"comment" => Some(Self::Comment),
			"macro" => Some(Self::Macro),
			"c_func" => Some(Self::CFunc),
			"c_type" => Some(Self::CType),
			_ => None,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Modifier
{
	CallFunc,
	Type,
	Reserved,
	Slice,
}

impl Modifier
{
	fn parse(s: &str) -> Option<Self>
	{
		match s {
			"call_func" => Some(Self::CallFunc),
			"type" => Some(Self::Type),
			"reserved" => Some(Self::Reserved),
			"slice" => Some(Self::Slice),
			_ => None,
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind
{
	Number(i64),
	Str(String),
	Definition(Definition),
	Ident(String),
	Char(char),
	Modifier(Modifier),
	BraceOpen,
	BraceClose,
	ParenOpen,
	ParenClose,
	Colon,
	Semicolon,
	Comma,
	Dot,
	DotDot,
	Star,
	Plus,
	Minus,
	Arrow,
	Slash,
	Pipe,
	Ampersand,
	Question,
	Eof,
}

#[derive(Debug, Clone)]
pub struct Token
{
	pub kind: TokenKind,
	pub span: Span,
}

impl Token
{
	fn new(kind: TokenKind, span: Span) -> Self
	{
		Self { kind, span }
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind
{
	Lex,
}

#[derive(Debug, Clone)]
pub struct LexError
{
	pub kind: ErrorKind,
	pub message: String,
	pub span: Span,
}

impl LexError
{
	fn lex(pos: Position, msg: impl Into<String>) -> Self
	{
		Self {
			kind: ErrorKind::Lex,
			message: msg.into(),
			span: Span::point(pos),
		}
	}
}

impl fmt::Display for LexError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{:?} error at {}: {}", self.kind, self.span.start, self.message)
	}
}

pub fn pretty_error(src: &str, err: &LexError) -> String
{
	let Span { start, end, .. } = &err.span;
	let lines: Vec<&str> = src.lines().collect();
	let line_text = lines.get(start.line as usize - 1).copied().unwrap_or("");
	let width = (end.column.saturating_sub(start.column)).max(1) as usize;
	let indent = " ".repeat(start.column as usize - 1);
	let caret = "^".repeat(width);

	format!(
		"{:?} at {}\n{}\n{}{}\n{}",
		err.kind, start, line_text, indent, caret, err.message
	)
}

type MacroTable = HashMap<String, String>;

pub struct Lexer<'src>
{
	pos: Position,
	chars: Peekable<Chars<'src>>,
	macros: MacroTable,
}

impl<'src> Lexer<'src>
{
	pub fn new(src: &'src str) -> Self
	{
		Self {
			// src,
			pos: Position::START,
			chars: src.chars().peekable(),
			macros: MacroTable::new(),
		}
	}

	fn peek(&mut self) -> Option<char>
	{
		self.chars.peek().copied()
	}

	fn bump(&mut self) -> Option<char>
	{
		let c = self.chars.next()?;
		self.pos = self.pos.advance(c);
		Some(c)
	}

	fn take_while(&mut self, pred: impl Fn(char) -> bool) -> String
	{
		let mut buf = String::new();
		while self.peek().map(&pred).unwrap_or(false) {
			buf.push(self.bump().unwrap());
		}
		buf
	}

	fn skip_whitespace(&mut self)
	{
		self.take_while(|c| c.is_whitespace());
	}

	fn span_here(&self) -> Span
	{
		Span::point(self.pos)
	}

	fn span_from(&self, start: Position) -> Span
	{
		Span::new(start, self.pos)
	}

	pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError>
	{
		let mut tokens = Vec::new();
		loop {
			self.skip_whitespace();
			match self.peek() {
				None => {
					tokens.push(Token::new(TokenKind::Eof, self.span_here()));
					break;
				}
				Some(c) => {
					let mut new_tokens = self.next_tokens(c)?;
					tokens.append(&mut new_tokens);
				}
			}
		}
		Ok(tokens)
	}

	fn next_tokens(&mut self, c: char) -> Result<Vec<Token>, LexError>
	{
		let start = self.pos;
		match c {
			c if c.is_ascii_digit() => {
				let digits = self.take_while(|c| c.is_ascii_digit());
				let n: i64 = digits
					.parse()
					.map_err(|_| LexError::lex(start, format!("Integer overflow: {digits}")))?;
				Ok(vec![Token::new(TokenKind::Number(n), self.span_from(start))])
			}

			c if c.is_alphabetic() || c == '_' => {
				let ident = self.take_while(is_ident_char);
				Ok(vec![Token::new(TokenKind::Ident(ident), self.span_from(start))])
			}

			'@' => self.lex_at(start),
			'$' => self.lex_dollar(start),
			'%' => self.lex_percent(start),
			'"' => {
				let tok = self.lex_string(start)?;
				Ok(vec![tok])
			}
			'\'' => {
				let tok = self.lex_char(start)?;
				Ok(vec![tok])
			}
			_ => self.lex_punct(start),
		}
	}

	fn lex_at(&mut self, start: Position) -> Result<Vec<Token>, LexError>
	{
		self.bump(); // consume '@'
		let name = self.take_while(is_ident_char);

		match Definition::parse(&name) {
			Some(Definition::Macro) => {
				self.skip_whitespace();
				let macro_name = self.take_while(is_ident_char);
				if macro_name.is_empty() {
					return Err(LexError::lex(start, "Expected macro name after @macro"));
				}
				self.skip_whitespace();
				let body = self.collect_macro_body(start)?;
				self.macros.insert(macro_name, body);
				Ok(vec![])
			}

			Some(def) => {
				let span = self.span_from(start);
				Ok(vec![Token::new(TokenKind::Definition(def), span)])
			}

			None => {
				let end = self.pos;
				match self.macros.get(&name).cloned() {
					Some(body) => {
						let call_site = Span::new(start, end);
						self.expand_macro(&name, &body, call_site)
					}
					None => Err(LexError::lex(start, format!("Unknown definition @{name}"))),
				}
			}
		}
	}

	fn lex_dollar(&mut self, start: Position) -> Result<Vec<Token>, LexError>
	{
		self.bump(); // consume '$'
		let name = self.take_while(is_ident_char);
		if name.is_empty() {
			return Err(LexError::lex(start, "Expected identifier after '$'"));
		}
		let end = self.pos;
		match self.macros.get(&name).cloned() {
			Some(body) => {
				let call_site = Span::new(start, end);
				self.expand_macro(&name, &body, call_site)
			}
			None => Err(LexError::lex(start, format!("Unknown macro ${name}"))),
		}
	}

	fn expand_macro(&self, name: &str, body: &str, call_site: Span) -> Result<Vec<Token>, LexError>
	{
		let origin = Origin::MacroExpansion(name.to_owned(), Box::new(call_site));
		let mut sub = Lexer::new(body);
		sub.macros = self.macros.clone();
		let mut tokens = sub.tokenize()?;
		tokens.retain(|t| t.kind != TokenKind::Eof);
		for tok in &mut tokens {
			tok.span = tok.span.clone().with_origin(origin.clone());
		}
		Ok(tokens)
	}

	fn lex_percent(&mut self, start: Position) -> Result<Vec<Token>, LexError>
	{
		self.bump(); // consume '%'
		let name = self.take_while(is_ident_char);
		if name.is_empty() {
			return Err(LexError::lex(start, "Expected identifier after '%'"));
		}
		match Modifier::parse(&name) {
			Some(m) => Ok(vec![Token::new(TokenKind::Modifier(m), self.span_from(start))]),
			None => Err(LexError::lex(start, format!("Unknown modifier %{name}"))),
		}
	}

	fn lex_string(&mut self, start: Position) -> Result<Token, LexError>
	{
		self.bump(); // opening '"'
		let mut buf = String::new();
		loop {
			match self.bump() {
				None => return Err(LexError::lex(start, "Unterminated string")),
				Some('"') => break,
				Some('\\') => {
					let c = self
						.bump()
						.ok_or_else(|| LexError::lex(start, "Unterminated escape in string"))?;
					buf.push(translate_escape(c));
				}
				Some(c) => buf.push(c),
			}
		}
		Ok(Token::new(TokenKind::Str(buf), self.span_from(start)))
	}

	fn lex_char(&mut self, start: Position) -> Result<Token, LexError>
	{
		self.bump(); // opening '\''
		let value = match self.bump() {
			None => return Err(LexError::lex(start, "Unterminated char literal")),
			Some('\\') => {
				let c = self
					.bump()
					.ok_or_else(|| LexError::lex(start, "Unterminated escape in char literal"))?;
				translate_escape(c)
			}
			Some(c) => c,
		};
		match self.bump() {
			Some('\'') => Ok(Token::new(TokenKind::Char(value), self.span_from(start))),
			_ => Err(LexError::lex(start, "Char literal must be exactly one character")),
		}
	}

	fn lex_punct(&mut self, start: Position) -> Result<Vec<Token>, LexError>
	{
		let c = self.bump().unwrap();
		let kind = match c {
			'{' => TokenKind::BraceOpen,
			'}' => TokenKind::BraceClose,
			'(' => TokenKind::ParenOpen,
			')' => TokenKind::ParenClose,
			':' => TokenKind::Colon,
			';' => TokenKind::Semicolon,
			',' => TokenKind::Comma,
			'*' => TokenKind::Star,
			'+' => TokenKind::Plus,
			'|' => TokenKind::Pipe,
			'&' => TokenKind::Ampersand,
			'?' => TokenKind::Question,
			'.' => {
				if self.peek() == Some('.') {
					self.bump();
					TokenKind::DotDot
				} else {
					TokenKind::Dot
				}
			}
			'-' => {
				if self.peek() == Some('>') {
					self.bump();
					TokenKind::Arrow
				} else {
					TokenKind::Minus
				}
			}
			'/' => {
				if self.peek() == Some('/') {
					self.take_while(|c| c != '\n');
					return Ok(vec![]);
				}
				TokenKind::Slash
			}
			other => {
				return Err(LexError::lex(start, format!("Unknown character '{other}'")));
			}
		};
		Ok(vec![Token::new(kind, self.span_from(start))])
	}

	fn collect_macro_body(&mut self, def_start: Position) -> Result<String, LexError>
	{
		if self.peek() == Some('{') {
			self.bump(); // '{'
			let mut depth = 1usize;
			let mut buf = String::new();
			loop {
				match self.bump() {
					None => return Err(LexError::lex(def_start, "Unterminated macro body (missing '}')")),
					Some('{') => {
						depth += 1;
						buf.push('{');
					}
					Some('}') => {
						depth -= 1;
						if depth == 0 {
							break;
						}
						buf.push('}');
					}
					Some(c) => buf.push(c),
				}
			}
			Ok(buf)
		} else {
			Ok(self.take_while(|c| c != '\n'))
		}
	}
}

fn is_ident_char(c: char) -> bool
{
	c.is_alphanumeric() || c == '_'
}

fn translate_escape(c: char) -> char
{
	match c {
		'n' => '\n',
		't' => '\t',
		'r' => '\r',
		'\\' => '\\',
		'"' => '"',
		'\'' => '\'',
		'0' => '\0',
		other => other,
	}
}
