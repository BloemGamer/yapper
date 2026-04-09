#![allow(clippy::needless_return)]

use std::{env, fmt};

mod dfa;
mod generator;
mod lexer;
mod nfa;
mod parser;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position
{
	pub line: u32,
	pub column: u32,
	pub offset: u32,
}

impl Position
{
	pub const START: Self = Self {
		line: 1,
		column: 1,
		offset: 0,
	};

	pub fn advance(self, c: char) -> Self
	{
		if c == '\n' {
			Self {
				line: self.line + 1,
				column: 1,
				offset: self.offset + 1,
			}
		} else {
			Self {
				column: self.column + 1,
				offset: self.offset + 1,
				..self
			}
		}
	}

	pub fn advance_str(self, s: &str) -> Self
	{
		s.chars().fold(self, Self::advance)
	}
}

impl fmt::Display for Position
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{}:{}", self.line, self.column)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Origin
{
	Original,
	MacroExpansion(String, Box<Span>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span
{
	pub start: Position,
	pub end: Position,
	pub origin: Origin,
}

impl Span
{
	pub fn new(start: Position, end: Position) -> Self
	{
		Self {
			start,
			end,
			origin: Origin::Original,
		}
	}

	pub fn point(pos: Position) -> Self
	{
		Self::new(pos, pos)
	}

	pub fn with_origin(self, origin: Origin) -> Self
	{
		Self { origin, ..self }
	}
}

fn main()
{
	let args: Vec<String> = env::args().collect();

	// Expect exactly 3 args: program name + 2 arguments
	if args.len() != 3 {
		eprintln!("Usage: {} <input_file> <output_file>", args[0]);
		std::process::exit(1);
	}

	let input_file = &args[1];
	let output_file = &args[2];

	let src = match std::fs::read_to_string(input_file) {
		Ok(s) => s,
		Err(e) => {
			eprintln!("Error reading file: {e}");
			std::process::exit(1);
		}
	};

	let tokens = {
		let mut lexer = lexer::Lexer::new(&src);
		match lexer.tokenize() {
			Ok(t) => t,
			Err(err) => {
				eprintln!("{}", lexer::pretty_error(&src, &err));
				std::process::exit(1);
			}
		}
	};

	let mut parser = parser::Parser::new(tokens);
	let defs = match parser.parse_file() {
		Ok(defs) => defs,
		Err(err) => {
			eprintln!("{}", parser::pretty_error(&src, &err));
			std::process::exit(1);
		}
	};

	let generated = generator::generate(&defs);

	if let Err(e) = std::fs::create_dir_all("output") {
		eprintln!("Error creating output directory: {e}");
		std::process::exit(1);
	}
	if let Err(e) = std::fs::write(output_file, &generated) {
		eprintln!("Error writing {output_file}: {e}");
		std::process::exit(1);
	}
	println!("Written to {output_file}");
}
