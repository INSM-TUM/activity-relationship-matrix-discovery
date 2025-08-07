use super::loop_detection::{Loop, detect_loops, activities_in_same_loop, get_loop_for_activity};

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
    TrueEventual,
}

impl std::fmt::Display for DependencyType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            DependencyType::Direct => write!(f, "d"),
            DependencyType::Eventual => write!(f, ""),
            DependencyType::TrueEventual => write!(f, "t"),
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
    // Detect loops first
    let loops = detect_loops(traces);
    check_temporal_dependency_with_loops(from, to, traces, threshold, &loops)
}

/// Checks for temporal dependencies between two activities with loop context.
pub fn check_temporal_dependency_with_loops(
    from: &str,
    to: &str,
    traces: &[Vec<&str>],
    threshold: f64,
    loops: &[Loop],
) -> Option<TemporalDependency> {
    //println!("Checking temporal dependency for {} -> {}", from, to);
    let mut dependencies = Vec::new();

    for (_, trace) in traces.iter().enumerate() {
        //println!("Checking trace {}: {:?}", i, trace);
        let trace_deps = check_trace_dependency_with_loops(from, to, trace, loops);
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
/// This implementation first checks for a strict direct dependency. A "Direct Forward"
/// dependency is only recognized if EVERY occurrence of the `to` activity is immediately
/// preceded by the `from` activity. If this strict check fails, it then checks for
/// eventual dependencies.
///
/// # Parameters
/// - `from`: The starting activity in the dependency.
/// - `to`: The ending activity in the dependency.
/// - `trace`: A single trace (ordered sequence of activities).
///
/// # Returns
/// A vector of tuples where each tuple contains the `DependencyType` and `Direction`.
fn check_trace_dependency(from: &str, to: &str, trace: &[&str]) -> Vec<(DependencyType, Direction)> {
    let loops = detect_loops(&[trace.to_vec()]);
    check_trace_dependency_with_loops(from, to, trace, &loops)
}

/// Checks the dependencies between two activities within a single trace with loop context.
fn check_trace_dependency_with_loops(
    from: &str, 
    to: &str, 
    trace: &[&str],
    loops: &[Loop],
) -> Vec<(DependencyType, Direction)> {
    let from_positions: Vec<usize> = trace
        .iter()
        .enumerate()
        .filter(|(_, &activity)| activity == from)
        .map(|(i, _)| i)
        .collect();

    let to_positions: Vec<usize> = trace
        .iter()
        .enumerate()
        .filter(|(_, &activity)| activity == to)
        .map(|(i, _)| i)
        .collect();

    if from_positions.is_empty() || to_positions.is_empty() {
        return vec![];
    }

    let from_in_loop = get_loop_for_activity(from, loops);
    let _to_in_loop = get_loop_for_activity(to, loops);
    let same_loop = activities_in_same_loop(from, to, loops);

    // Handle self-loops (e.g., A -> A)
    if from == to {
        return handle_self_loop_temporal(from, &from_positions, from_in_loop);
    }

    // Handle activities in the same structural loop
    if same_loop {
        return handle_same_loop_temporal(from, to, &from_positions, &to_positions, trace);
    }

    handle_regular_temporal(from, to, &from_positions, &to_positions, trace)
}

/// Handles temporal dependencies for self-loops
fn handle_self_loop_temporal(
    _activity: &str,
    positions: &[usize],
    _loop_info: Option<&Loop>,
) -> Vec<(DependencyType, Direction)> {
    let mut result = Vec::new();
    
    if positions.len() > 1 {
        // Check each pair of consecutive occurrences of the activity
        for window in positions.windows(2) {
            if window[1] == window[0] + 1 {
                result.push((DependencyType::Direct, Direction::Forward));
            } else {
                result.push((DependencyType::Eventual, Direction::Forward));
            }
        }
    }
    
    result
}

/// Handles temporal dependencies for activities in the same structural loop
fn handle_same_loop_temporal(
    _from: &str,
    _to: &str,
    from_positions: &[usize],
    to_positions: &[usize],
    _trace: &[&str],
) -> Vec<(DependencyType, Direction)> {
    // For activities in the same loop, they have eventual forward dependency
    // due to the cyclic nature of the loop
    let mut result = Vec::new();
    
    let any_forward = from_positions
        .iter()
        .any(|&from_pos| to_positions.iter().any(|&to_pos| to_pos > from_pos));

    if any_forward {
        result.push((DependencyType::Eventual, Direction::Forward));
    }

    let any_backward = to_positions
        .iter()
        .any(|&to_pos| from_positions.iter().any(|&from_pos| from_pos > to_pos));

    if any_backward {
        result.push((DependencyType::Eventual, Direction::Backward));
    }

    result
}

/// Handles temporal dependencies for regular (non-loop) cases
fn handle_regular_temporal(
    from: &str,
    to: &str,
    from_positions: &[usize],
    to_positions: &[usize],
    trace: &[&str],
) -> Vec<(DependencyType, Direction)> {
    // Strict Direct Forward Check: Every `to` must be immediately preceded by a `from`.
    let is_direct_forward = !to_positions.is_empty()
        && to_positions
            .iter()
            .all(|&to_pos| to_pos > 0 && trace[to_pos - 1] == from);

    if is_direct_forward {
        // If the condition is met, this trace represents a single, clear Direct Forward dependency.
        return vec![(DependencyType::Direct, Direction::Forward)];
    }

    // Strict Direct Backward Check: Every `from` must be immediately preceded by a `to`.
    let is_direct_backward = !from_positions.is_empty()
        && from_positions
            .iter()
            .all(|&from_pos| from_pos > 0 && trace[from_pos - 1] == to);

    if is_direct_backward {
        return vec![(DependencyType::Direct, Direction::Backward)];
    }

    // If no strict direct dependency was found, check for eventual dependencies.
    let mut result = Vec::new();
    let any_forward = from_positions
        .iter()
        .any(|&from_pos| to_positions.iter().any(|&to_pos| to_pos > from_pos));

    if any_forward {
        result.push((DependencyType::Eventual, Direction::Forward));
    }

    let any_backward = to_positions
        .iter()
        .any(|&to_pos| from_positions.iter().any(|&from_pos| from_pos > to_pos));

    if any_backward {
        result.push((DependencyType::Eventual, Direction::Backward));
    }

    result
}


/// Classifies the dependencies based on their ratio to determine the overall dependency.
///
/// This function first determines the overall direction (Forward/Backward) based on a threshold.
/// Then, it determines the dependency type (Direct/Eventual). The type is considered `Direct`
/// if the ratio of direct dependencies within the chosen direction also meets the threshold.
///
/// # Parameters
/// - `from`: The starting activity in the dependency.
/// - `to`: The ending activity in the dependency.
/// - `dependencies`: A vector of dependencies found in the traces.
/// - `threshold`: The ratio threshold for determining the direction and type of the dependency.
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

    let forward_ratio = if total_count > 0.0 { forward_count / total_count } else { 0.0 };
    let backward_ratio = if total_count > 0.0 { backward_count / total_count } else { 0.0 };

    let direction = if forward_ratio >= threshold {
        Direction::Forward
    } else if backward_ratio >= threshold {
        Direction::Backward
    } else {
        return None; // if neither direction meets the threshold, it's independent
    };

    let total_deps_in_direction = dependencies
        .iter()
        .filter(|(_, dir)| *dir == direction)
        .count() as f64;

    let direct_deps_in_direction = dependencies
        .iter()
        .filter(|(dep, dir)| *dir == direction && *dep == DependencyType::Direct)
        .count() as f64;

    // Check if there are any direct dependencies at all in the chosen direction
    let has_any_direct_in_direction = dependencies
        .iter()
        .any(|(dep, dir)| *dir == direction && *dep == DependencyType::Direct);
    
    let dependency_type = if total_deps_in_direction > 0.0 && (direct_deps_in_direction / total_deps_in_direction) >= threshold {
        DependencyType::Direct
    } else if !has_any_direct_in_direction {
        DependencyType::TrueEventual
    } else {
        DependencyType::Eventual
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
            check_temporal_dependency("B", "D", &event_names, 0.5),
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
                    DependencyType::TrueEventual,
                    Direction::Forward,
                )),
            ),
            (("B", "C"), None),
            (
                ("B", "D"),
                Some(TemporalDependency::new(
                    "B",
                    "D",
                    DependencyType::TrueEventual,
                    Direction::Forward,
                )),
            ),
            (
                ("C", "D"),
                Some(TemporalDependency::new(
                    "C",
                    "D",
                    DependencyType::TrueEventual,
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
            (DependencyType::Eventual, Direction::Backward),
        ];
        assert_eq!(expected, check_trace_dependency("A", "C", trace));

        let actual = check_temporal_dependency("A", "C", &traces, 1.0);
        assert_eq!(None, actual); // Should be None because of backward dependency
    }

    #[test]
    fn test_independence() {
        let traces = vec![vec!["A", "B", "C", "C", "A"]];
        let trace = &traces[0];
        let expected = vec![
            (DependencyType::Eventual, Direction::Forward),
            (DependencyType::Eventual, Direction::Backward),
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
            (DependencyType::Eventual, Direction::Forward)
        ];
        assert_eq!(expected, check_trace_dependency("A", "C", trace));

        let actual = check_temporal_dependency("A", "C", &traces, 1.0);
        let expected = Some(TemporalDependency::new(
            "A",
            "C",
            DependencyType::TrueEventual,
            Direction::Forward,
        ));
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_with_loop_3() {
        let traces = vec![vec!["C", "A", "C"]];
        let expected = vec![
            (DependencyType::Eventual, Direction::Forward),
            (DependencyType::Eventual, Direction::Backward),
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

    #[test]
    fn test_self_loop_always_direct() {
        // Test case: a,a,a,b,c (all A->A transitions are direct)
        let traces = vec![vec!["A", "A", "A", "B", "C"]];
        let expected = Some(TemporalDependency::new(
            "A",
            "A",
            DependencyType::Direct,
            Direction::Forward,
        ));
        let actual = check_temporal_dependency("A", "A", &traces, 1.0);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_self_loop_not_always_direct() {
        // Test case: a,a,b,a (some direct, some eventual)
        let traces = vec![vec!["A", "A", "B", "A"]];
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
    fn test_self_loop_independence() {
        // Test case: a,b,c (no A->A transitions)
        let traces = vec![vec!["A", "B", "C"]];
        let actual = check_temporal_dependency("A", "A", &traces, 1.0);
        assert_eq!(None, actual);
    }

    #[test]
    fn test_loop_detection_system() {
        // Test case with complex loops
        let traces = vec![
            vec!["A", "B", "C", "A"],  // Structural loop: A -> B -> C -> A
            vec!["D", "D", "E"],       // Self-loop: D -> D
            vec!["A", "C", "B", "A"],  // Different path in the same loop
        ];
        
        // Test temporal dependencies within the structural loop
        let ab_dep = check_temporal_dependency("A", "B", &traces, 0.5);
        let bc_dep = check_temporal_dependency("B", "C", &traces, 0.5);
        let ca_dep = check_temporal_dependency("C", "A", &traces, 0.5);
        
        // Test self-loop dependency
        let dd_dep = check_temporal_dependency("D", "D", &traces, 1.0);
        
        // Verify loop detection is working
        assert!(ab_dep.is_some());
        assert!(bc_dep.is_some());
        assert!(ca_dep.is_some());
        assert!(dd_dep.is_some());
        
        // Self-loop should be direct (consecutive D->D)
        let dd_dep = dd_dep.unwrap();
        assert_eq!(dd_dep.dependency_type, DependencyType::Direct);
        assert_eq!(dd_dep.direction, Direction::Forward);
    }

    #[test]
    fn test_loop_vs_non_loop_dependencies() {
        let traces = vec![
            vec!["A", "B", "A", "B"],  // A and B in a loop
            vec!["C", "D", "E"],       // C, D, E in linear sequence
        ];
        
        // Loop dependencies should be different from linear dependencies
        let ab_dep = check_temporal_dependency("A", "B", &traces, 1.0);
        let cd_dep = check_temporal_dependency("C", "D", &traces, 1.0);
        
        assert!(ab_dep.is_some());
        assert!(cd_dep.is_some());
        
        let ab_dep = ab_dep.unwrap();
        let cd_dep = cd_dep.unwrap();
        
        // Linear dependency should be direct
        assert_eq!(cd_dep.dependency_type, DependencyType::Direct);
        
        // Loop dependency behavior depends on the specific pattern
        assert_eq!(ab_dep.direction, Direction::Forward);
    }
}
