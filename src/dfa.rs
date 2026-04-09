use crate::nfa::{NFA, Transition};
use std::collections::{BTreeSet, HashMap, VecDeque};
use std::fmt;

pub type DfaStateId = usize;

#[derive(Debug, Clone)]
pub struct DfaState
{
	/// transitions: input char → next DFA state
	pub transitions: HashMap<char, DfaStateId>,
	/// wildcard (`.` / Any) → next DFA state (only fires for chars NOT in `transitions`)
	pub any_transition: Option<DfaStateId>,
	pub is_accept: bool,
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone)]
pub struct DFA
{
	pub start: DfaStateId,
	pub states: Vec<DfaState>,
}

fn epsilon_closure(nfa: &NFA, seeds: impl IntoIterator<Item = usize>) -> BTreeSet<usize>
{
	let mut closure = BTreeSet::new();
	let mut queue: VecDeque<usize> = seeds.into_iter().collect();
	while let Some(s) = queue.pop_front() {
		if !closure.insert(s) {
			continue;
		}
		for t in &nfa.states[s].transitions {
			if let Transition::Epsilon(next) = t {
				queue.push_back(*next);
			}
		}
	}
	closure
}

fn move_on_char(nfa: &NFA, nfa_set: &BTreeSet<usize>, c: char) -> BTreeSet<usize>
{
	let mut reached = BTreeSet::new();
	for &s in nfa_set {
		for t in &nfa.states[s].transitions {
			if let Transition::Char(tc, next) = t
				&& *tc == c
			{
				reached.insert(*next);
			}
		}
	}
	epsilon_closure(nfa, reached)
}

fn move_on_any_for_char(nfa: &NFA, nfa_set: &BTreeSet<usize>, c: char) -> BTreeSet<usize>
{
	let mut reached = BTreeSet::new();
	for &s in nfa_set {
		let has_concrete_for_c = nfa.states[s]
			.transitions
			.iter()
			.any(|t| matches!(t, Transition::Char(tc, _) if *tc == c));
		if has_concrete_for_c {
			continue;
		}
		for t in &nfa.states[s].transitions {
			if let Transition::Any(next) = t {
				reached.insert(*next);
			}
		}
	}
	epsilon_closure(nfa, reached)
}

fn move_on_any_unconstrained(nfa: &NFA, nfa_set: &BTreeSet<usize>) -> BTreeSet<usize>
{
	let mut reached = BTreeSet::new();
	for &s in nfa_set {
		for t in &nfa.states[s].transitions {
			if let Transition::Any(next) = t {
				reached.insert(*next);
			}
		}
	}
	epsilon_closure(nfa, reached)
}

fn alphabet(nfa: &NFA) -> Vec<char>
{
	let mut chars: Vec<char> = nfa
		.states
		.iter()
		.flat_map(|s| s.transitions.iter())
		.filter_map(|t| if let Transition::Char(c, _) = t { Some(*c) } else { None })
		.collect();
	chars.sort_unstable();
	chars.dedup();
	chars
}

fn get_or_insert(
	nfa: &NFA,
	state_map: &mut HashMap<BTreeSet<usize>, DfaStateId>,
	dfa_states: &mut Vec<DfaState>,
	queue: &mut VecDeque<BTreeSet<usize>>,
	set: BTreeSet<usize>,
) -> DfaStateId
{
	if let Some(&id) = state_map.get(&set) {
		return id;
	}
	let id = dfa_states.len();
	state_map.insert(set.clone(), id);
	dfa_states.push(DfaState {
		transitions: HashMap::new(),
		any_transition: None,
		is_accept: set.contains(&nfa.accept),
	});
	queue.push_back(set);
	id
}

pub fn nfa_to_dfa(nfa: &NFA) -> DFA
{
	let alpha = alphabet(nfa);

	let mut state_map: HashMap<BTreeSet<usize>, DfaStateId> = HashMap::new();
	let mut dfa_states: Vec<DfaState> = Vec::new();
	let mut queue: VecDeque<BTreeSet<usize>> = VecDeque::new();

	let start_set = epsilon_closure(nfa, [nfa.start]);
	get_or_insert(nfa, &mut state_map, &mut dfa_states, &mut queue, start_set);

	while let Some(current_set) = queue.pop_front() {
		let current_id = *state_map.get(&current_set).unwrap();

		for &c in &alpha {
			let concrete = move_on_char(nfa, &current_set, c);
			let via_any = move_on_any_for_char(nfa, &current_set, c);

			let mut next_set = concrete;
			next_set.extend(via_any);

			if next_set.is_empty() {
				continue;
			}

			let next_id = get_or_insert(nfa, &mut state_map, &mut dfa_states, &mut queue, next_set);
			dfa_states[current_id].transitions.insert(c, next_id);
		}

		let any_set = move_on_any_unconstrained(nfa, &current_set);
		if !any_set.is_empty() {
			let next_id = get_or_insert(nfa, &mut state_map, &mut dfa_states, &mut queue, any_set);
			dfa_states[current_id].any_transition = Some(next_id);
		}
	}

	DFA {
		start: 0,
		states: dfa_states,
	}
}

impl fmt::Display for DFA
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		writeln!(f, "DFA {{")?;
		writeln!(f, "  start: {}", self.start)?;
		writeln!(f, "  states:")?;

		for (i, state) in self.states.iter().enumerate() {
			let mut marker = String::new();
			if i == self.start {
				marker.push_str(" [start]");
			}
			if state.is_accept {
				marker.push_str(" [accept]");
			}
			writeln!(f, "    {}{}:", i, marker)?;

			if state.transitions.is_empty() && state.any_transition.is_none() {
				writeln!(f, "      (no transitions)")?;
				continue;
			}

			let mut chars: Vec<char> = state.transitions.keys().copied().collect();
			chars.sort_unstable();
			for c in chars {
				let to = state.transitions[&c];
				writeln!(f, "      -- {:?} --> {}", c, to)?;
			}
			if let Some(to) = state.any_transition {
				writeln!(f, "      -- ANY --> {}", to)?;
			}
		}

		writeln!(f, "}}")
	}
}
