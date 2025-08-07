use matrix_discovery::{
    dependency_types::{
        existential::{check_existential_dependency, DependencyType as ExistentialDependencyType, Direction as ExistentialDirection},
        temporal::{check_temporal_dependency, DependencyType as TemporalDependencyType, Direction as TemporalDirection},
    },
};

fn main() {
    println!("Testing self-loop logic...");
    
    let traces1 = vec![vec!["A", "A", "B", "C", "A", "A"]];
    test_case(&traces1, "Always direct", 
              Some((TemporalDependencyType::Direct, TemporalDirection::Forward)),
              Some((ExistentialDependencyType::Implication, ExistentialDirection::Backward)));
    
    let traces2 = vec![vec!["A", "A", "B", "A"]];
    test_case(&traces2, "Not always direct", 
              Some((TemporalDependencyType::Eventual, TemporalDirection::Forward)),
              Some((ExistentialDependencyType::Equivalence, ExistentialDirection::Both)));
    
    let traces3 = vec![vec!["A", "B", "C"]];
    test_case(&traces3, "No self-loops", 
              None,
              None);
}

fn test_case(
    traces: &[Vec<&str>], 
    description: &str,
    expected_temporal: Option<(TemporalDependencyType, TemporalDirection)>,
    expected_existential: Option<(ExistentialDependencyType, ExistentialDirection)>
) {
    println!("\n--- Test case: {} ---", description);
    println!("Traces: {:?}", traces);
    
    let temporal_result = check_temporal_dependency("A", "A", traces, 1.0);
    let existential_result = check_existential_dependency("A", "A", traces, 1.0);
    
    println!("Temporal result: {:?}", temporal_result);
    println!("Existential result: {:?}", existential_result);
    
    match (expected_temporal, &temporal_result) {
        (Some((exp_type, exp_dir)), Some(actual)) => {
            assert_eq!(actual.dependency_type, exp_type, "Temporal dependency type mismatch");
            assert_eq!(actual.direction, exp_dir, "Temporal direction mismatch");
            println!("Temporal expectation met");
        }
        (None, None) => {
            println!("Temporal independence as expected");
        }
        _ => {
            panic!("Temporal expectation mismatch! Expected: {:?}, Got: {:?}", expected_temporal, temporal_result);
        }
    }
    
    match (expected_existential, &existential_result) {
        (Some((exp_type, exp_dir)), Some(actual)) => {
            assert_eq!(actual.dependency_type, exp_type, "Existential dependency type mismatch");
            assert_eq!(actual.direction, exp_dir, "Existential direction mismatch");
            println!("Existential expectation met");
        }
        (None, None) => {
            println!("Existential independence as expected");
        }
        _ => {
            panic!("Existential expectation mismatch! Expected: {:?}, Got: {:?}", expected_existential, existential_result);
        }
    }
    
    println!("Test case passed!");
}
