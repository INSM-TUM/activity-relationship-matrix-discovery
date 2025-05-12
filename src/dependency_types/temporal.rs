use std::cmp::Ordering;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TemporalDependency {
    pub from: String,
    pub to: String,
    pub dependency_type: DependencyType,
    pub direction: Direction,
}

impl TemporalDependency {
    pub fn new(
        from: &str,
        to: &str,
        dependency_type: DependencyType,
        direction: Direction,
    ) -> TemporalDependency {
        TemporalDependency {
            from: from.to_string(),
            to: to.to_string(),
            dependency_type,
            direction,
        }
    }
}

impl std::fmt::Display for TemporalDependency {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.direction {
            Direction::Forward => write!(f, "≺{}", self.dependency_type),
            Direction::Backward => write!(f, "≻{}", self.dependency_type),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Direction {
    Forward,
    Backward,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DependencyType {
    Direct,
    Eventual,
}

impl std::fmt::Display for DependencyType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            DependencyType::Direct => write!(f, "d"),
            DependencyType::Eventual => write!(f, ""),
        }
    }
}

/// Checks for temporal dependencies between two activities across multiple traces.
///
/// # Parameters
/// - `from`: The starting activity in the dependency.
/// - `to`: The ending activity in the dependency.
/// - `traces`: A list of traces where each trace is an ordered sequence of activities.
/// - `threshold`: The ratio threshold for considering the dependency direction.
///    (for example, a threshold of 0.8 would mean that the dependency would be considered
///    a Direct dependency if it is found in at least 80% of the traces)
///
/// # Returns
/// An `Option` containing the `TemporalDependency` if a dependency is found; otherwise, `None`.
pub fn check_temporal_dependency(
    from: &str,
    to: &str,
    traces: &[Vec<&str>],
    threshold: f64,
) -> Option<TemporalDependency> {
    //println!("Checking temporal dependency for {} -> {}", from, to);
    let mut dependencies = Vec::new();

    for (_, trace) in traces.iter().enumerate() {
        //println!("Checking trace {}: {:?}", i, trace);
        let trace_deps = check_trace_dependency(from, to, trace);
        //println!("{trace_deps:?}");
        //println!("Trace {} dependencies: {:?}", i, trace_deps);
        dependencies.extend(trace_deps);
    }

    //println!("All dependencies: {:?}", dependencies);
    let result = classify_dependencies(from, to, dependencies, threshold);
    //println!("Final result: {:?}", result);
    result
}

/// Checks the dependencies between two activities within a single trace.
///
/// # Parameters
/// - `from`: The starting activity in the dependency.
/// - `to`: The ending activity in the dependency.
/// - `trace`: A single trace (ordered sequence of activities).
///
/// # Returns
/// A vector of tuples where each tuple contains the `DependencyType` and `Direction`.
///
/// Note: this is where the logic for determining the types and directions of the dependencies
/// is implemented.
fn check_trace_dependency(
    from: &str,
    to: &str,
    trace: &[&str],
) -> Vec<(DependencyType, Direction)> {
    let mut result = Vec::new();
    let mut from_positions: Vec<usize> = Vec::new();
    let mut to_positions: Vec<usize> = Vec::new();

    // get the indexes of each `from` and each `to` activities
    for (i, activity) in trace.iter().enumerate() {
        if activity == &from {
            from_positions.push(i);
        } else if activity == &to {
            to_positions.push(i);
        }
    }

    if from_positions.is_empty() || to_positions.is_empty() {
        return vec![];
    }

    let mut from_index = 0;
    let mut to_index = 0;

    // edge case for when `from` and `to` are the same
    if from == to {
        // check >2
        if from_positions.len() > 2 {
            result.push((DependencyType::Eventual, Direction::Forward));
        } else if from_positions[0] + 1 == from_positions[1] {
            // check if activity in between
            result.push((DependencyType::Direct, Direction::Forward));
        }
    }

    // iterate through the `from` and `to` positions except for the last one
    while from_index < from_positions.len() && to_index < to_positions.len() {
        let from_pos = from_positions[from_index];
        let to_pos = to_positions[to_index];

        match from_pos.cmp(&to_pos) {
            Ordering::Less => {
                let dependency_type = if to_pos - from_pos == 1 {
                    DependencyType::Direct
                } else {
                    DependencyType::Eventual
                };
                result.push((dependency_type, Direction::Forward));
                from_index += 1;
                to_index += 1;
            }
            Ordering::Greater => {
                let dependency_type = if from_pos - to_pos == 1 {
                    DependencyType::Direct
                } else {
                    DependencyType::Eventual
                };
                result.push((dependency_type, Direction::Backward));
                to_index += 1;
            }
            Ordering::Equal => unreachable!(),
        }
    }

    // handle remaining 'from' activities
    while from_index < from_positions.len() {
        if to_positions
            .last()
            .map_or(false, |&last_to| last_to > from_positions[from_index])
        {
            result.push((DependencyType::Eventual, Direction::Forward));
        }
        from_index += 1;
    }

    // handle remaining 'to' activities
    while to_index < to_positions.len() {
        if from_positions
            .last()
            .map_or(false, |&last_from| last_from < to_positions[to_index])
        {
            result.push((DependencyType::Eventual, Direction::Forward));
        } else {
            result.push((DependencyType::Eventual, Direction::Backward));
        }
        to_index += 1;
    }

    result
}

/// Classifies the dependencies based on their ratio to determine the overall dependency.
///
/// # Parameters
/// - `from`: The starting activity in the dependency.
/// - `to`: The ending activity in the dependency.
/// - `dependencies`: A vector of dependencies found in the traces.
/// - `threshold`: The ratio threshold for determining the direction of the dependency.
///
/// # Returns
/// An `Option` containing the `TemporalDependency` if a dependency direction meets the threshold; otherwise, `None`.
fn classify_dependencies(
    from: &str,
    to: &str,
    dependencies: Vec<(DependencyType, Direction)>,
    threshold: f64,
) -> Option<TemporalDependency> {
    if dependencies.is_empty() {
        return None;
    }

    let total_count = dependencies.len() as f64;
    let forward_count = dependencies
        .iter()
        .filter(|(_, dir)| *dir == Direction::Forward)
        .count() as f64;
    let backward_count = total_count - forward_count;

    let forward_ratio = forward_count / total_count;
    let backward_ratio = backward_count / total_count;

    let direction = if forward_ratio >= threshold {
        Direction::Forward
    } else if backward_ratio >= threshold {
        Direction::Backward
    } else {
        return None; // if neither direction meets the threshold, it's independent
    };

    let dependency_type = if dependencies
        .iter()
        .any(|(dep, _)| *dep == DependencyType::Eventual)
    {
        DependencyType::Eventual
    } else {
        DependencyType::Direct
    };

    Some(TemporalDependency::new(
        from,
        to,
        dependency_type,
        direction,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_with_loops_general() {
        let event_names = vec![
            vec!["A", "B", "C", "B", "D", "B"],
            vec!["A", "C", "B", "D"],
            vec!["B", "A", "C", "D", "B"],
        ];
        let mut pairs_and_deps = HashMap::new();

        pairs_and_deps.insert(
            ("A", "B"),
            check_temporal_dependency("A", "B", &event_names, 1.0),
        );
        pairs_and_deps.insert(
            ("A", "C"),
            check_temporal_dependency("A", "C", &event_names, 1.0),
        );
        pairs_and_deps.insert(
            ("B", "C"),
            check_temporal_dependency("B", "C", &event_names, 1.0),
        );
        pairs_and_deps.insert(
            ("C", "D"),
            check_temporal_dependency("C", "D", &event_names, 1.0),
        );
        pairs_and_deps.insert(
            ("B", "D"),
            check_temporal_dependency("B", "D", &event_names, 1.0),
        );
        // FIXME: fix me :3
        // pairs_and_deps.insert(("B", "B"), check_temporal_dependency("B", "B", &event_names, 1.0));

        let expected = HashMap::from([
            (("A", "B"), None),
            (
                ("A", "C"),
                Some(TemporalDependency::new(
                    "A",
                    "C",
                    DependencyType::Eventual,
                    Direction::Forward,
                )),
            ),
            (("B", "C"), None),
            (
                ("B", "D"),
                Some(TemporalDependency::new(
                    "B",
                    "D",
                    DependencyType::Eventual,
                    Direction::Forward,
                )),
            ),
            (
                ("C", "D"),
                Some(TemporalDependency::new(
                    "C",
                    "D",
                    DependencyType::Eventual,
                    Direction::Forward,
                )),
            ),
            (
                ("B", "B"),
                Some(TemporalDependency::new(
                    "B",
                    "B",
                    DependencyType::Eventual,
                    Direction::Forward,
                )),
            ),
        ]);

        pairs_and_deps.iter().for_each(|(key, value)| {
            assert_eq!(value, expected.get(key).unwrap());
        });
    }

    #[test]
    fn test_with_loop_1() {
        let traces = vec![vec!["A", "B", "C", "A", "C"]];
        let trace = &traces[0];
        let expected = vec![
            (DependencyType::Eventual, Direction::Forward),
            (DependencyType::Direct, Direction::Forward),
        ];
        assert_eq!(expected, check_trace_dependency("A", "C", trace));

        let expected = Some(TemporalDependency::new(
            "A",
            "C",
            DependencyType::Eventual,
            Direction::Forward,
        ));
        let actual = check_temporal_dependency("A", "C", &traces, 1.0);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_independence() {
        let traces = vec![vec!["A", "B", "C", "C", "A"]];
        let trace = &traces[0];
        let expected = vec![
            (DependencyType::Eventual, Direction::Forward),
            (DependencyType::Direct, Direction::Backward),
        ];
        assert_eq!(expected, check_trace_dependency("A", "C", trace));

        let actual = check_temporal_dependency("A", "C", &traces, 1.0);
        assert_eq!(None, actual);
    }

    #[test]
    fn test_with_loop_2() {
        let traces = vec![vec!["A", "C", "B", "C"]];
        let trace = &traces[0];
        let expected = vec![
            (DependencyType::Direct, Direction::Forward),
            (DependencyType::Eventual, Direction::Forward),
        ];
        assert_eq!(expected, check_trace_dependency("A", "C", trace));

        let actual = check_temporal_dependency("A", "C", &traces, 1.0);
        let expected = Some(TemporalDependency::new(
            "A",
            "C",
            DependencyType::Eventual,
            Direction::Forward,
        ));
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_with_loop_3() {
        let traces = vec![vec!["C", "A", "C"]];
        let expected = vec![
            (DependencyType::Direct, Direction::Backward),
            (DependencyType::Direct, Direction::Forward),
        ];
        assert_eq!(expected, check_trace_dependency("A", "C", &traces[0]));

        let actual = check_temporal_dependency("A", "C", &traces, 1.0);
        assert_eq!(None, actual);
    }

    #[test]
    fn test_same_activity_temporal_1() {
        let traces = vec![vec!["A", "A", "C", "A", "C"]];
        let expected = Some(TemporalDependency::new(
            "A",
            "A",
            DependencyType::Eventual,
            Direction::Forward,
        ));
        let actual = check_temporal_dependency("A", "A", &traces, 1.0);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_same_activity_temporal_2() {
        let traces = vec![vec!["B", "C", "A", "A", "C"]];
        let expected = Some(TemporalDependency::new(
            "A",
            "A",
            DependencyType::Direct,
            Direction::Forward,
        ));
        let actual = check_temporal_dependency("A", "A", &traces, 1.0);
        assert_eq!(expected, actual);
    }

    // #[test]
    // fn test_exclusive_choice() {
    //     let traces = vec![vec!["a", "b"], vec!["a", "c"]];
    //     let expected = Some(TemporalDependency::new(
    //             "A",
    //             "B",
    //             DependencyType::Direct,
    //             Direction::Forward,
    //     ));
    //     let actual = check_temporal_dependency("a", "c", &traces, 1.0);
    //     assert_eq!(expected, actual);
    // }
}
