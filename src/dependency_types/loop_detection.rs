use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub struct Loop {
    pub activities: HashSet<String>,
    pub loop_type: LoopType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LoopType {
    SelfLoop,        // Single activity repeating (A -> A)
    StructuralLoop,  // Multiple activities in a cycle (A -> B -> C -> A)
}

/// Detects all loops in the given traces
pub fn detect_loops(traces: &[Vec<&str>]) -> Vec<Loop> {
    let mut loops = Vec::new();
    
    let self_loops = detect_self_loops(traces);
    loops.extend(self_loops);
    
    let structural_loops = detect_structural_loops(traces);
    loops.extend(structural_loops);
    
    loops
}

/// Detects self-loops (activities that immediately follow themselves)
fn detect_self_loops(traces: &[Vec<&str>]) -> Vec<Loop> {
    let mut self_loop_activities = HashSet::new();
    
    for trace in traces {
        for window in trace.windows(2) {
            if window[0] == window[1] {
                self_loop_activities.insert(window[0].to_string());
            }
        }
    }
    
    self_loop_activities
        .into_iter()
        .map(|activity| Loop {
            activities: vec![activity].into_iter().collect(),
            loop_type: LoopType::SelfLoop,
        })
        .collect()
}

/// Detects structural loops (cycles in the activity graph)
fn detect_structural_loops(traces: &[Vec<&str>]) -> Vec<Loop> {
    let mut graph = HashMap::new();
    let mut all_activities = HashSet::new();
    
    for trace in traces {
        for window in trace.windows(2) {
            let from = window[0];
            let to = window[1];
            
            all_activities.insert(from);
            all_activities.insert(to);
            
            graph.entry(from).or_insert_with(HashSet::new).insert(to);
        }
    }
    
    let sccs = find_strongly_connected_components(&graph, &all_activities);
    
    // Convert SCCs with more than one activity to structural loops
    sccs.into_iter()
        .filter(|scc| scc.len() > 1)
        .map(|scc| Loop {
            activities: scc.into_iter().map(|s| s.to_string()).collect(),
            loop_type: LoopType::StructuralLoop,
        })
        .collect()
}

/// Tarjan's algorithm for finding strongly connected components
fn find_strongly_connected_components<'a>(
    graph: &'a HashMap<&'a str, HashSet<&'a str>>,
    all_activities: &'a HashSet<&'a str>,
) -> Vec<HashSet<&'a str>> {
    let mut index = 0;
    let mut stack = Vec::new();
    let mut indices = HashMap::new();
    let mut lowlinks = HashMap::new();
    let mut on_stack = HashSet::new();
    let mut sccs = Vec::new();
    
    for &activity in all_activities {
        if !indices.contains_key(activity) {
            strongconnect(
                activity,
                graph,
                &mut index,
                &mut stack,
                &mut indices,
                &mut lowlinks,
                &mut on_stack,
                &mut sccs,
            );
        }
    }
    
    sccs
}

fn strongconnect<'a>(
    v: &'a str,
    graph: &HashMap<&'a str, HashSet<&'a str>>,
    index: &mut usize,
    stack: &mut Vec<&'a str>,
    indices: &mut HashMap<&'a str, usize>,
    lowlinks: &mut HashMap<&'a str, usize>,
    on_stack: &mut HashSet<&'a str>,
    sccs: &mut Vec<HashSet<&'a str>>,
) {
    indices.insert(v, *index);
    lowlinks.insert(v, *index);
    *index += 1;
    stack.push(v);
    on_stack.insert(v);
    
    if let Some(neighbors) = graph.get(v) {
        for &w in neighbors {
            if !indices.contains_key(w) {
                strongconnect(w, graph, index, stack, indices, lowlinks, on_stack, sccs);
                let w_lowlink = *lowlinks.get(w).unwrap();
                let v_lowlink = *lowlinks.get(v).unwrap();
                lowlinks.insert(v, v_lowlink.min(w_lowlink));
            } else if on_stack.contains(w) {
                let w_index = *indices.get(w).unwrap();
                let v_lowlink = *lowlinks.get(v).unwrap();
                lowlinks.insert(v, v_lowlink.min(w_index));
            }
        }
    }
    
    if lowlinks.get(v) == indices.get(v) {
        let mut scc = HashSet::new();
        loop {
            let w = stack.pop().unwrap();
            on_stack.remove(w);
            scc.insert(w);
            if w == v {
                break;
            }
        }
        if !scc.is_empty() {
            sccs.push(scc);
        }
    }
}

/// Checks if two activities are in the same loop
pub fn activities_in_same_loop(activity1: &str, activity2: &str, loops: &[Loop]) -> bool {
    loops.iter().any(|loop_| {
        loop_.activities.contains(activity1) && loop_.activities.contains(activity2)
    })
}
 
/// Gets the loop that contains the given activity (if any)
pub fn get_loop_for_activity<'a>(activity: &str, loops: &'a [Loop]) -> Option<&'a Loop> {
    loops.iter().find(|loop_| loop_.activities.contains(activity))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_self_loop_detection() {
        let traces = vec![
            vec!["A", "A", "B", "C"],
            vec!["A", "B", "B", "C"],
        ];
        
        let loops = detect_loops(&traces);
        let self_loops: Vec<_> = loops.iter().filter(|l| l.loop_type == LoopType::SelfLoop).collect();
        
        assert_eq!(self_loops.len(), 2);
        assert!(self_loops.iter().any(|l| l.activities.contains("A")));
        assert!(self_loops.iter().any(|l| l.activities.contains("B")));
    }

    #[test]
    fn test_structural_loop_detection() {
        let traces = vec![
            vec!["A", "B", "C", "A"],  // A -> B -> C -> A (loop)
            vec!["A", "B", "C", "D"],  // Linear
        ];
        
        let loops = detect_loops(&traces);
        let structural_loops: Vec<_> = loops.iter().filter(|l| l.loop_type == LoopType::StructuralLoop).collect();
        
        assert_eq!(structural_loops.len(), 1);
        let loop_ = &structural_loops[0];
        assert!(loop_.activities.contains("A"));
        assert!(loop_.activities.contains("B"));
        assert!(loop_.activities.contains("C"));
    }
}
