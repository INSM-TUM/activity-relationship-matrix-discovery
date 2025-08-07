use matrix_discovery::dependency_types::existential;
use matrix_discovery::{
    dependency_types::{dependency::Dependency, temporal},
    parser::parse_into_traces,
};
use petgraph::{algo::tarjan_scc, graph::DiGraph};
use std::collections::{BTreeSet, HashMap, HashSet};

fn main() {
    if let Ok(deps) = get_adjacency_matrix("./sample-data/classification/Log01_structured.xes") {
        println!("unique_independent_equivalences:");

        let independent_equivalences = filter_dependencies_by_existential_dependency_type(
            &deps,
            existential::DependencyType::Equivalence,
        );
        let unique_independent_equivalences = filter_unique_dependencies(&independent_equivalences);
        unique_independent_equivalences
            .iter()
            .for_each(|d| println!("{} -> {}", d.from, d.to)); // Added

        println!("-------------------");

        println!("unique_independent_negated_equivalences:");
        let independent_negated_equivalences = filter_dependencies_by_existential_dependency_type(
            &deps,
            existential::DependencyType::NegatedEquivalence,
        );
        let unique_independent_negated_equivalences =
            filter_unique_dependencies(&independent_negated_equivalences);
        unique_independent_negated_equivalences
            .iter()
            .for_each(|d| println!("{} -> {}", d.from, d.to));

        println!("-------------------");

        println!("direct_deps:");
        let direct_deps = filter_direct_deps(&deps);
        direct_deps
            .iter()
            .for_each(|d| println!("{} -> {}", d.from, d.to));

        println!("-------------------");

        println!("Strongly Connected Components (SCCs):");
        let graph = create_direct_graph(&direct_deps);

        // Store node weights (names) associated with NodeIndex
        let node_names: HashMap<_, _> = graph
            .node_indices()
            .map(|idx| (idx, graph[idx].clone()))
            .collect();

        let sccs = tarjan_scc(&graph);

        for (i, scc) in sccs.iter().enumerate() {
            // Map NodeIndex back to node names (Strings)
            let component_names: Vec<String> = scc
                .iter()
                .map(|node_index| node_names[node_index].clone())
                .collect();
            // Sort names for consistent output
            let mut sorted_names = component_names;
            sorted_names.sort();
            println!("SCC {}: {:?}", i + 1, sorted_names);
        }
        //
        // New block processing
        println!("\nDesired Blocks:");
        let blocks = identify_blocks(
            &unique_independent_equivalences,
            &unique_independent_negated_equivalences,
            &direct_deps,
        );
        print_blocks(&blocks);
    } else {
        eprintln!("Error getting adjacency matrix and dependencies.");
    }
}

fn identify_blocks(
    equivalences: &[Dependency],
    negated_equivalences: &[Dependency],
    direct_deps: &[Dependency],
) -> Vec<BTreeSet<String>> {
    let mut uf = UnionFind::new();

    for dep in equivalences {
        uf.union(&dep.from, &dep.to);
    }

    let mut blocks: Vec<BTreeSet<String>> = uf.get_classes();

    let mut new_blocks = Vec::new();
    for dep in negated_equivalences {
        if let Some(target_class) = blocks.iter().find(|b| b.contains(&dep.to)) {
            let mut new_block = target_class.clone();
            new_block.insert(dep.from.clone());
            new_blocks.push(new_block);
        }
    }
    blocks.extend(new_blocks);

    let mut unique = HashSet::new();
    blocks.retain(|b| unique.insert(b.clone()));
    blocks.sort_by(|a, b| {
        a.len()
            .cmp(&b.len())
            .then_with(|| a.iter().next().cmp(&b.iter().next()))
    });

    let all_nodes: HashSet<_> = direct_deps
        .iter()
        .flat_map(|d| vec![d.from.clone(), d.to.clone()])
        .collect();

    for node in all_nodes {
        if !blocks.iter().any(|b| b.contains(&node)) {
            blocks.push(BTreeSet::from_iter([node]));
        }
    }

    blocks
}

fn print_blocks(blocks: &[BTreeSet<String>]) {
    let mut block_labels = HashMap::new();
    for (i, block) in blocks.iter().enumerate() {
        let label = format!("B{}", i + 1);
        block_labels.insert(block, label.clone());

        let sub_blocks: Vec<_> = blocks[..i].iter().filter(|b| b.is_subset(block)).collect();

        if !sub_blocks.is_empty() {
            let mut decomposition = Vec::new();
            for sb in &sub_blocks {
                decomposition.push(block_labels[*sb].clone());
            }

            let remaining: Vec<_> = block
                .iter()
                .filter(|n| !sub_blocks.iter().any(|sb| sb.contains(n.as_str())))
                .collect();

            print!(
                "{}: {:?} (can be decomposed into {}",
                label,
                block,
                decomposition.join(", ")
            );
            if !remaining.is_empty() {
                print!(" and {}a: {:?}", label, remaining);
            }
            println!(")");
        } else {
            println!("{}: {:?}", label, block);
        }
    }
}

struct UnionFind {
    parent: HashMap<String, String>,
}

impl UnionFind {
    fn new() -> Self {
        Self {
            parent: HashMap::new(),
        }
    }

    fn find(&mut self, node: &str) -> String {
        let root = self
            .parent
            .get(node)
            .cloned()
            .unwrap_or_else(|| node.to_string());
        if root != node {
            let true_root = self.find(&root);
            self.parent.insert(node.to_string(), true_root.clone());
            true_root
        } else {
            root
        }
    }

    fn union(&mut self, a: &str, b: &str) {
        let root_a = self.find(a);
        let root_b = self.find(b);
        if root_a != root_b {
            self.parent.insert(root_b, root_a);
        }
    }

    fn get_classes(&mut self) -> Vec<BTreeSet<String>> {
        let mut classes: HashMap<String, BTreeSet<String>> = HashMap::new();
        let mut all_nodes = HashSet::new();
        all_nodes.extend(self.parent.keys().cloned());
        all_nodes.extend(self.parent.values().cloned());

        for node in all_nodes {
            let root = self.find(&node);
            classes.entry(root).or_default().insert(node);
        }

        classes.into_values().collect()
    }
}

fn get_adjacency_matrix(path: &str) -> Result<Vec<Dependency>, Box<dyn std::error::Error>> {
    let traces = parse_into_traces(Some(path), None)?;
    let deps = generate_dependencies_from_traces(traces, 1.0, 1.0);
    Ok(deps)
}

fn generate_dependencies_from_traces(
    traces: Vec<Vec<String>>,
    existential_threshold: f64,
    temporal_threshold: f64,
) -> Vec<Dependency> {
    use matrix_discovery::dependency_types::{
        existential::check_existential_dependency,
        temporal::check_temporal_dependency,
    };
    use std::collections::HashSet;

    let traces_str: Vec<Vec<&str>> = traces
        .iter()
        .map(|trace| trace.iter().map(|s| s.as_str()).collect())
        .collect();

    let activities: HashSet<String> = traces
        .iter()
        .flat_map(|trace| trace.iter().cloned())
        .collect();

    let mut dependencies = Vec::new();

    for from in &activities {
        for to in &activities {
            let temporal_dependency =
                check_temporal_dependency(from, to, &traces_str, temporal_threshold);
            let existential_dependency =
                check_existential_dependency(from, to, &traces_str, existential_threshold);

            dependencies.push(Dependency::new(
                from.clone(),
                to.clone(),
                temporal_dependency,
                existential_dependency,
            ));
        }
    }

    dependencies
}

fn create_direct_graph(deps: &[Dependency]) -> DiGraph<String, ()> {
    let mut graph = DiGraph::new();
    let mut node_indices = HashMap::new();

    for dep in deps {
        if !node_indices.contains_key(&dep.from) {
            let idx = graph.add_node(dep.from.clone());
            node_indices.insert(dep.from.clone(), idx);
        }
        if !node_indices.contains_key(&dep.to) {
            let idx = graph.add_node(dep.to.clone());
            node_indices.insert(dep.to.clone(), idx);
        }
    }

    for dep in deps {
        let from_idx = node_indices[&dep.from];
        let to_idx = node_indices[&dep.to];
        graph.add_edge(from_idx, to_idx, ());
    }

    graph
}

fn filter_direct_deps(deps: &[Dependency]) -> Vec<Dependency> {
    deps.iter()
        .filter(|d| {
            if let Some(td) = &d.temporal_dependency {
                return td.dependency_type == temporal::DependencyType::Direct;
            }
            false
        })
        .cloned()
        .collect()
}

/// Removes dependencies that are the same just in reverse direction
/// For example: (a, b) is the same as (b, a) just in the opposite direction
fn filter_unique_dependencies(deps: &[Dependency]) -> Vec<Dependency> {
    let mut unique_deps = Vec::new();
    let mut seen_canonical = HashSet::new();

    for dep in deps {
        let canonical_dep = dep.canonical();
        if !seen_canonical.contains(&canonical_dep) {
            seen_canonical.insert(canonical_dep);
            unique_deps.push(dep.clone());
        }
    }
    unique_deps
}

fn filter_dependencies_by_existential_dependency_type(
    deps: &[Dependency],
    existential_dependency_type: existential::DependencyType,
) -> Vec<Dependency> {
    deps.iter()
        .filter(|d| {
            if let Some(ed) = &d.existential_dependency {
                return ed.dependency_type == existential_dependency_type
                    && d.temporal_dependency.is_none();
            }
            false
        })
        .cloned()
        .collect()
}
