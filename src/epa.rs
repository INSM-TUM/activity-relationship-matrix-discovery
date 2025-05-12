use std::collections::{HashMap, HashSet};

use crate::{event::Event, state::State};

#[derive(Debug)]
pub struct ExtendedPrefixAutomaton {
    pub states: HashMap<String, State>,
    pub transitions: Vec<(String, char, String)>,
    pub activities: HashSet<char>,
    pub root: String,
}

impl Default for ExtendedPrefixAutomaton {
    fn default() -> Self {
        Self::new()
    }
}

impl ExtendedPrefixAutomaton {
    pub fn new() -> Self {
        let root_id = "root".to_string();
        let mut states = HashMap::new();
        states.insert(
            root_id.clone(),
            State {
                partition: None,
                sequences: HashSet::new(),
            },
        );

        Self {
            states,
            transitions: Vec::new(),
            activities: HashSet::new(),
            root: root_id,
        }
    }

    pub fn build(plain_log: Vec<Vec<Event>>) -> Self {
        let mut epa = Self::new();
        let mut last_at: HashMap<String, String> = HashMap::new();

        for trace in plain_log {
            for event in trace {
                let pred_at = event
                    .predecessor
                    .as_ref()
                    .and_then(|case| last_at.get(case))
                    .unwrap_or(&epa.root)
                    .to_string();

                let current_at = if let Some(target) = epa
                    .transitions
                    .iter()
                    .find(|(source, act, _)| source == &pred_at && *act == event.activity)
                    .map(|(_, _, target)| target.to_string())
                {
                    target
                } else {
                    let new_state_id = format!("s{}", epa.states.len());
                    let current_c = if pred_at == epa.root {
                        1
                    } else if epa
                        .transitions
                        .iter()
                        .any(|(source, _, _)| source == &pred_at)
                    {
                        epa.states
                            .values()
                            .filter_map(|s| s.partition)
                            .max()
                            .unwrap_or(0)
                            + 1
                    } else {
                        epa.states[&pred_at].partition.unwrap_or(0)
                    };

                    epa.states.insert(
                        new_state_id.clone(),
                        State {
                            partition: Some(current_c),
                            sequences: HashSet::new(),
                        },
                    );

                    epa.transitions
                        .push((pred_at, event.activity, new_state_id.clone()));
                    epa.activities.insert(event.activity);

                    new_state_id
                };

                if let Some(state) = epa.states.get_mut(&current_at) {
                    state.sequences.insert(event.clone());
                }

                last_at.insert(event.case.clone(), current_at);
            }
        }

        epa
    }

    pub fn variant_entropy(&self) -> f64 {
        let s = (self.states.len() as f64).max(1.0);
        let s = if s > 1.0 { s - 1.0 } else { s };

        let partition_sizes: HashMap<usize, usize> = self
            .states
            .values()
            .filter_map(|state| state.partition)
            .fold(HashMap::new(), |mut acc, partition| {
                *acc.entry(partition).or_insert(0) += 1;
                acc
            });

        let sum_term: f64 = partition_sizes
            .values()
            .map(|&size| {
                let size_f64 = size as f64;
                size_f64 * size_f64.log10()
            })
            .sum();

        s * s.log10() - sum_term
    }

    pub fn normalized_variant_entropy(&self) -> f64 {
        let e_v = self.variant_entropy();
        let s = (self.states.len() as f64).max(1.0);
        let s = if s > 1.0 { s - 1.0 } else { s };

        if s * s.log10() == 0.0 {
            0.0 // Avoid division by zero
        } else {
            e_v / (s * s.log10())
        }
    }
}
