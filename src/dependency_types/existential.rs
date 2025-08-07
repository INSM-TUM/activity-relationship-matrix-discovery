use super::temporal::DependencyType as TemporalDependencyType;
use super::loop_detection::{Loop, detect_loops, activities_in_same_loop, get_loop_for_activity};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExistentialDependency {
    pub from: String,
    pub to: String,
    pub dependency_type: DependencyType,
    pub direction: Direction,
}

impl ExistentialDependency {
    pub fn new(
        from: &str,
        to: &str,
        dependency_type: DependencyType,
        direction: Direction,
    ) -> Self {
        ExistentialDependency {
            from: from.to_string(),
            to: to.to_string(),
            dependency_type,
            direction,
        }
    }
}

impl std::fmt::Display for ExistentialDependency {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.dependency_type == DependencyType::Implication {
            match &self.direction {
                Direction::Forward => write!(f, "=>"),
                Direction::Backward => write!(f, "<="),
                Direction::Both => panic!("Invalid direction for Implication"),
            }
        } else {
            write!(f, "{}", self.dependency_type)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Direction {
    Forward,
    Backward,
    Both,
}

#[allow(dead_code)]
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord)]
pub enum DependencyType {
    Implication,
    Equivalence,
    NegatedEquivalence,
    Nand,
    Or,
}

impl std::fmt::Display for DependencyType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            DependencyType::Implication => write!(f, "⇒"),
            DependencyType::Equivalence => write!(f, "⇔"),
            DependencyType::NegatedEquivalence => write!(f, "⇎"),
            DependencyType::Nand => write!(f, "⊼"),
            DependencyType::Or => write!(f, "∨"),
        }
    }
}

// TODO: NAND and OR dependencies
/// Checks for an existential dependency between two activities within a set of traces.
///
/// This function analyzes the given traces to determine if there is an existential dependency
/// between the `from` and `to` activities based on the specified threshold. It considers
/// implications, equivalences, and negated equivalences to identify the type and direction
/// of the dependency (in that order).
///
/// # Arguments
///
/// * `from` - The name of the starting activity.
/// * `to` - The name of the target activity.
/// * `traces` - A vector of `Trace` objects representing the sequence of events.
/// * `threshold` - A threshold value to determine if the dependency is significant.
///
/// # Returns
///
/// An `Option` containing an `ExistentialDependency` if a dependency is found, otherwise `None`.
pub fn check_existential_dependency(
    from: &str,
    to: &str,
    traces: &[Vec<&str>],
    threshold: f64,
) -> Option<ExistentialDependency> {
    let loops = detect_loops(traces);
    check_existential_dependency_with_loops(from, to, traces, threshold, &loops)
}

/// Checks for existential dependencies with loop context
pub fn check_existential_dependency_with_loops(
    from: &str,
    to: &str,
    traces: &[Vec<&str>],
    threshold: f64,
    loops: &[Loop],
) -> Option<ExistentialDependency> {
    assert!((0.0..=1.0).contains(&threshold), "Threshold must be between 0 and 1");

    let from_in_loop = get_loop_for_activity(from, loops);
    let _to_in_loop = get_loop_for_activity(to, loops);
    let same_loop = activities_in_same_loop(from, to, loops);

    // Special handling for self-loops
    if from == to {
        return handle_self_loop_existential(from, traces, from_in_loop);
    }

    // Special handling for activities in the same structural loop
    if same_loop {
        return handle_same_loop_existential(from, to, traces, threshold);
    }

    handle_regular_existential(from, to, traces, threshold)
}

/// Handles existential dependencies for self-loops
fn handle_self_loop_existential(
    activity: &str,
    traces: &[Vec<&str>],
    _loop_info: Option<&Loop>,
) -> Option<ExistentialDependency> {
    let temporal_pattern = detect_self_loop_temporal_pattern(activity, traces);
    
    match temporal_pattern {
        Some(TemporalDependencyType::Direct) => {
            // Always direct after itself -> (temporal forward direct, implication backwards)
            Some(ExistentialDependency {
                from: activity.to_string(),
                to: activity.to_string(),
                dependency_type: DependencyType::Implication,
                direction: Direction::Backward,
            })
        }
        Some(TemporalDependencyType::Eventual) => {
            // Self loop but not always direct -> (eventual forward, equivalence both)
            Some(ExistentialDependency {
                from: activity.to_string(),
                to: activity.to_string(),
                dependency_type: DependencyType::Equivalence,
                direction: Direction::Both,
            })
        }
        _ => {
            // No self-loops or independence -> (independence, independence)
            None
        }
    }
}

/// Handles existential dependencies for activities in the same structural loop
fn handle_same_loop_existential(
    from: &str,
    to: &str,
    traces: &[Vec<&str>],
    threshold: f64,
) -> Option<ExistentialDependency> {
    handle_regular_existential(from, to, traces, threshold)
}

/// Handles regular (non-loop) existential dependencies
fn handle_regular_existential(
    from: &str,
    to: &str,
    traces: &[Vec<&str>],
    threshold: f64,
) -> Option<ExistentialDependency> {
    let forward_implication_holds = has_implication(from, to, traces, threshold);
    let backward_implication_holds = has_implication(to, from, traces, threshold);

    if forward_implication_holds || backward_implication_holds {
        let final_dependency_type;
        let final_direction;

        if forward_implication_holds && backward_implication_holds {
            let mut counts_match_for_equivalence = true;
            if from != to {
                for trace in traces.iter() {
                    let from_present_in_trace = trace.contains(&from);
                    let to_present_in_trace = trace.contains(&to);

                    if from_present_in_trace && to_present_in_trace {
                        let count_from = trace.iter().filter(|&&act| act == from).count();
                        let count_to = trace.iter().filter(|&&act| act == to).count();
                        if count_from != count_to {
                            counts_match_for_equivalence = false;
                            break;
                        }
                    }
                }
            }

            if counts_match_for_equivalence {
                final_dependency_type = DependencyType::Equivalence;
                final_direction = Direction::Both;
            } else {
                // Both presence implications hold, but counts mismatch. Determine implication direction.
                final_dependency_type = DependencyType::Implication;
                
                let mut any_from_strictly_less_than_to = false;
                let mut any_from_strictly_greater_than_to = false;

                for trace in traces.iter() {
                    // Consider only traces where both activities are actually present
                    let count_from_in_trace = trace.iter().filter(|&&act| act == from).count();
                    let count_to_in_trace = trace.iter().filter(|&&act| act == to).count();

                    if count_from_in_trace > 0 && count_to_in_trace > 0 { // Both present
                        if count_from_in_trace < count_to_in_trace {
                            any_from_strictly_less_than_to = true;
                        }
                        if count_from_in_trace > count_to_in_trace {
                            any_from_strictly_greater_than_to = true;
                        }
                    }
                }

                if any_from_strictly_less_than_to && !any_from_strictly_greater_than_to {
                    // 'from' count is less than 'to' count in all cases of inequality
                    final_direction = Direction::Forward; // from => to
                } else if !any_from_strictly_less_than_to && any_from_strictly_greater_than_to {
                    // 'from' count is greater than 'to' count in all cases of inequality
                    final_direction = Direction::Backward; // from <= to
                } else {
                    // Mixed inequalities (e.g., some traces from < to, others from > to),
                    // or no inequalities found despite counts_match_for_equivalence being false (should not happen for from != to).
                    // Defaulting to Forward as a fallback.
                    final_direction = Direction::Forward;
                }
            }
        } else if forward_implication_holds {
            // Only forward implication holds
            final_dependency_type = DependencyType::Implication;
            final_direction = Direction::Forward;
        } else {
            // Only backward implication holds (since the outer if condition ensures one of them is true)
            final_dependency_type = DependencyType::Implication;
            final_direction = Direction::Backward;
        }

        return Some(ExistentialDependency {
            from: from.to_string(),
            to: to.to_string(),
            dependency_type: final_dependency_type,
            direction: final_direction,
        });
    }

    // If no positive dependency (implication or equivalence) was found, check for negated equivalence.
    let negated_equivalence_holds = negated_equivalence(from, to, traces, threshold);
    if negated_equivalence_holds {
        return Some(ExistentialDependency {
            from: from.to_string(),
            to: to.to_string(),
            dependency_type: DependencyType::NegatedEquivalence,
            direction: Direction::Forward, // Conventionally Forward for NegatedEquivalence
        });
    }

    None
}

/// Checks if there is an implication relationship between two events within a set of event traces.
///
/// # Parameters
/// - `from`: The event that implies the occurrence of another event.
/// - `to`: The event that is implied by the occurrence of the `from` event.
/// - `event_names`: A vector of vectors, where each inner vector represents a sequence of event names (a trace).
/// - `threshold`: A threshold value between 0 and 1 that determines the minimum proportion of valid traces required to confirm the implication.
///
/// # Returns
/// - `true` if the proportion of valid traces is greater than or equal to the threshold, indicating that the implication holds.
/// - `false` otherwise.
fn has_implication(from: &str, to: &str, event_names: &[Vec<&str>], threshold: f64) -> bool {
    let total_traces = event_names.len();
    let valid_traces = event_names
        .iter()
        .filter(|trace| {
            if trace.contains(&from) {
                trace.contains(&to)
            } else {
                true
            }
        })
        .count();
    valid_traces as f64 / total_traces as f64 >= threshold
}

fn negated_equivalence(from: &str, to: &str, event_names: &[Vec<&str>], threshold: f64) -> bool {
    let filtered_traces: Vec<_> = event_names
        .iter()
        .filter(|&trace| trace.contains(&from) || trace.contains(&to))
        .collect();
    let valid_traces = filtered_traces
        .iter()
        .filter(|trace| {
            if trace.contains(&from) {
                !trace.contains(&to)
            } else {
                trace.contains(&to)
            }
        })
        .count();
    valid_traces as f64 / filtered_traces.len() as f64 >= threshold
}

/// Helper function to determine temporal pattern for self-loops
/// Returns the temporal dependency type that would be detected for this self-loop
fn detect_self_loop_temporal_pattern(activity: &str, traces: &[Vec<&str>]) -> Option<TemporalDependencyType> {
    let mut has_any_transitions = false;
    let mut all_transitions_are_direct = true;
    
    for trace in traces.iter() {
        let positions: Vec<usize> = trace
            .iter()
            .enumerate()
            .filter(|(_, &act)| act == activity)
            .map(|(i, _)| i)
            .collect();
        
        if positions.len() > 1 {
            for window in positions.windows(2) {
                has_any_transitions = true;
                if window[1] != window[0] + 1 {
                    all_transitions_are_direct = false;
                }
            }
        }
    }
    
    if !has_any_transitions {
        return None; // No self-loops found (independence)
    }
    
    if all_transitions_are_direct {
        Some(TemporalDependencyType::Direct)
    } else {
        Some(TemporalDependencyType::Eventual)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_has_implication() {
        let event_names = vec![
            vec!["A", "B", "C", "D"],
            vec!["A", "C", "B", "D"],
            vec!["A", "E", "D"],
            vec!["A", "D"],
        ];
        let activities = ["A", "B", "C", "D", "E"];
        let pairs = vec![
            ("A", "D"),
            ("B", "A"),
            ("B", "C"),
            ("B", "D"),
            ("C", "A"),
            ("C", "B"),
            ("C", "D"),
            ("D", "A"),
            ("E", "A"),
            ("E", "D"),
        ];
        activities.iter().for_each(|from| {
            activities.iter().for_each(|to| {
                if from != to {
                    if pairs.contains(&(from, to)) {
                        assert!(has_implication(from, to, &event_names, 1.0));
                    } else {
                        assert!(!has_implication(from, to, &event_names, 1.0));
                    }
                }
            });
        });
    }

    #[test]
    fn test_has_implication_with_noise() {
        let event_names = vec![
            vec!["A", "B", "C", "D"],
            vec!["A", "C", "B", "D"],
            vec!["A", "E", "D"],
            vec!["A", "D"],
            vec!["A", "C"], // Noise: D is missing
        ];
        assert!(has_implication("A", "D", &event_names, 0.8));
        assert!(!has_implication("A", "D", &event_names, 1.0));
    }

    #[test]
    fn test_same_activity_existential_1() {
        let traces = vec![vec!["A", "B", "C", "C", "A"]];
        let expected = Some(ExistentialDependency {
            from: "A".to_string(),
            to: "A".to_string(),
            dependency_type: DependencyType::Equivalence,
            direction: Direction::Both,
        });
        let actual = check_existential_dependency("A", "A", &traces, 1.0);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_self_loop_always_direct_existential() {
        let traces = vec![vec!["A", "A", "A", "B", "C"]];
        let expected = Some(ExistentialDependency {
            from: "A".to_string(),
            to: "A".to_string(),
            dependency_type: DependencyType::Implication,
            direction: Direction::Backward,
        });
        let actual = check_existential_dependency("A", "A", &traces, 1.0);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_self_loop_not_always_direct_existential() {
        let traces = vec![vec!["A", "A", "B", "A"]];
        let expected = Some(ExistentialDependency {
            from: "A".to_string(),
            to: "A".to_string(),
            dependency_type: DependencyType::Equivalence,
            direction: Direction::Both,
        });
        let actual = check_existential_dependency("A", "A", &traces, 1.0);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_self_loop_independence_existential() {
        let traces = vec![vec!["A", "B", "C"]];
        let actual = check_existential_dependency("A", "A", &traces, 1.0);
        assert_eq!(None, actual);
    }

    // #[test]
    // fn test_same_activity_existential_2() {
    //     let traces = vec![vec!["A", "B", "C", "A", "A"]];
    //     let expected = Some(ExistentialDependency {
    //         from: "A".to_string(),
    //         to: "A".to_string(),
    //         dependency_type: DependencyType::Implication,
    //         direction: Direction::Forward,
    //     });
    //     let actual = check_existential_dependency("A", "A", &traces, 1.0);
    //     assert_eq!(expected, actual);
    // }

    // TODO: add more tests
}
