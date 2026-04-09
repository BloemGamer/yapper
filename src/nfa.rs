use std::fmt;

use crate::parser::{Pattern, Quant};

type StateId = usize;

#[derive(Debug, Clone)]
pub enum Transition
{
	Char(char, StateId),
	Any(StateId),
	Epsilon(StateId),
}

#[derive(Debug, Clone)]
pub struct State
{
	pub transitions: Vec<Transition>,
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone)]
pub struct NFA
{
	pub start: StateId,
	pub accept: StateId,
	pub states: Vec<State>,
}

impl fmt::Display for NFA
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		writeln!(f, "NFA {{")?;
		writeln!(f, "  start: {}", self.start)?;
		writeln!(f, "  accept: {}", self.accept)?;
		writeln!(f, "  states:")?;

		for (i, state) in self.states.iter().enumerate() {
			let mut marker = String::new();
			if i == self.start {
				marker.push_str(" [start]");
			}
			if i == self.accept {
				marker.push_str(" [accept]");
			}

			writeln!(f, "    {}{}:", i, marker)?;

			if state.transitions.is_empty() {
				writeln!(f, "      (no transitions)")?;
				continue;
			}

			for t in &state.transitions {
				match t {
					Transition::Char(c, to) => {
						writeln!(f, "      -- '{:?}' --> {}", c, to)?;
					}
					Transition::Any(to) => {
						writeln!(f, "      -- ANY --> {}", to)?;
					}
					Transition::Epsilon(to) => {
						writeln!(f, "      -- ε --> {}", to)?;
					}
				}
			}
		}

		writeln!(f, "}}")
	}
}

struct NFABuilder
{
	states: Vec<State>,
}

impl NFABuilder
{
	fn new() -> Self
	{
		Self { states: Vec::new() }
	}

	fn new_state(&mut self) -> StateId
	{
		let id = self.states.len();
		self.states.push(State { transitions: vec![] });
		id
	}

	fn add_transition(&mut self, from: StateId, t: Transition)
	{
		self.states[from].transitions.push(t);
	}

	fn build(self, start: StateId, accept: StateId) -> NFA
	{
		NFA {
			start,
			accept,
			states: self.states,
		}
	}
}

pub fn pattern_to_nfa(p: &Pattern) -> NFA
{
	let mut builder = NFABuilder::new();
	let (start, accept) = build_pattern(p, &mut builder);
	builder.build(start, accept)
}

pub fn pattern_has_until(p: &Pattern) -> bool
{
	match p {
		Pattern::Until { .. } => true,
		Pattern::Seq(parts, _) | Pattern::Alt(parts, _) => parts.iter().any(pattern_has_until),
		Pattern::Repeat { pat, .. } => pattern_has_until(pat),
		Pattern::Str(..) | Pattern::Char(..) => false,
	}
}

fn build_pattern(p: &Pattern, b: &mut NFABuilder) -> (StateId, StateId)
{
	match p {
		Pattern::Char(c, _) => {
			let s = b.new_state();
			let e = b.new_state();
			b.add_transition(s, Transition::Char(*c, e));
			(s, e)
		}

		Pattern::Str(s, _) => {
			let mut chars = s.chars();

			let first = chars.next().expect("empty string not supported yet");
			let (start, mut prev_end) = build_pattern(&Pattern::Char(first, p.span().clone()), b);

			for c in chars {
				let (s2, e2) = build_pattern(&Pattern::Char(c, p.span().clone()), b);
				b.add_transition(prev_end, Transition::Epsilon(s2));
				prev_end = e2;
			}

			(start, prev_end)
		}

		Pattern::Seq(parts, _) => {
			let mut iter = parts.iter();

			let (start, mut prev_end) = build_pattern(iter.next().unwrap(), b);

			for part in iter {
				let (s2, e2) = build_pattern(part, b);
				b.add_transition(prev_end, Transition::Epsilon(s2));
				prev_end = e2;
			}

			(start, prev_end)
		}

		Pattern::Alt(parts, _) => {
			let start = b.new_state();
			let end = b.new_state();

			for part in parts {
				let (s, e) = build_pattern(part, b);
				b.add_transition(start, Transition::Epsilon(s));
				b.add_transition(e, Transition::Epsilon(end));
			}

			(start, end)
		}

		Pattern::Repeat { pat, quant, .. } => {
			match quant {
				Quant::One => {
					// a+
					let (s, e) = build_pattern(pat, b);
					let end = b.new_state();

					b.add_transition(e, Transition::Epsilon(s)); // loop
					b.add_transition(e, Transition::Epsilon(end));

					(s, end)
				}

				Quant::Many => {
					// a*
					let start = b.new_state();
					let end = b.new_state();

					let (s, e) = build_pattern(pat, b);

					b.add_transition(start, Transition::Epsilon(s));
					b.add_transition(start, Transition::Epsilon(end));

					b.add_transition(e, Transition::Epsilon(s));
					b.add_transition(e, Transition::Epsilon(end));

					(start, end)
				}

				Quant::Opt => {
					// a?
					let start = b.new_state();
					let end = b.new_state();

					let (s, e) = build_pattern(pat, b);

					b.add_transition(start, Transition::Epsilon(s));
					b.add_transition(start, Transition::Epsilon(end));

					b.add_transition(e, Transition::Epsilon(end));

					(start, end)
				}
			}
		}

		Pattern::Until { end, .. } => {
			let start = b.new_state();
			let accept = b.new_state();

			let (end_start, end_accept) = build_pattern(end, b);

			b.add_transition(start, Transition::Epsilon(end_start));

			b.add_transition(end_accept, Transition::Epsilon(accept));

			let consume = b.new_state();
			b.add_transition(start, Transition::Any(consume));
			b.add_transition(consume, Transition::Epsilon(start));

			(start, accept)
		}
	}
}
