use chrono::{DateTime, Duration, Utc};
use dependency_types::{
    dependency::Dependency, 
    existential::{check_existential_dependency_with_loops, Direction as ExistentialDirection, DependencyType as ExistentialType},
    temporal::{check_temporal_dependency_with_loops, Direction as TemporalDirection, DependencyType as TemporalType},
    loop_detection::{detect_loops, Loop, LoopType},
};
use std::collections::{HashMap, HashSet};
use serde::{Deserialize, Serialize};

pub mod dependency_types;
pub mod epa;
pub mod evaluation;
pub mod event;
pub mod parser;
pub mod routes;
pub mod state;

#[derive(Serialize, Deserialize, Debug)]
pub struct AdjacencyMatrix {
    pub metadata: Metadata,
    pub activity_statistics: ActivityStatistics,
    pub trace_statistics: TraceStatistics,
    pub loop_information: LoopInformation,
    pub strongly_connected_components: Vec<StronglyConnectedComponent>,
    pub dependencies: Vec<DependencyEntry>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Metadata {
    pub format_version: String,
    pub description: String,
    pub activities: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ActivityStatistics {
    pub total_activities: usize,
    pub activity_frequencies: HashMap<String, ActivityFrequency>,
    pub self_loop_counts: HashMap<String, usize>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ActivityFrequency {
    pub total_occurrences: usize,
    pub average_per_trace: f64,
    pub traces_present_in: usize,
    pub min_per_trace: usize,
    pub max_per_trace: usize,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct TraceStatistics {
    pub total_traces: usize,
    pub average_trace_length: f64,
    pub min_trace_length: usize,
    pub max_trace_length: usize,
    pub total_events: usize,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct LoopInformation {
    pub self_loops: Vec<SelfLoop>,
    pub structural_loops: Vec<StructuralLoop>,
    pub total_loops: usize,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct SelfLoop {
    pub activity: String,
    pub occurrences: usize,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct StructuralLoop {
    pub activities: Vec<String>,
    pub loop_id: usize,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct StronglyConnectedComponent {
    pub activities: Vec<String>,
    pub component_id: usize,
    pub size: usize,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct DependencyEntry {
    pub from: String,
    pub to: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub temporal: Option<TemporalSpec>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub existential: Option<ExistentialSpec>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct TemporalSpec {
    #[serde(rename = "type")]
    pub type_: String,
    pub symbol: String,
    pub direction: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ExistentialSpec {
    #[serde(rename = "type")]
    pub type_: String,
    pub symbol: String,
    pub direction: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActivityPatternAnalysis {
    pub activity_frequencies_per_trace_display: Vec<String>,
    pub overall_activity_stats_display: Vec<String>,
    pub direct_self_loop_counts_display: Vec<String>,
    pub conditional_freq_display: Vec<String>,
    pub average_activity_frequency_per_trace_display: Vec<String>,
}

pub fn analyze_activity_patterns(
    traces: &[Vec<String>],
    conditional_target_activity: &str,
    conditional_preceding_activity: &str,
) -> ActivityPatternAnalysis {
    let mut activity_frequencies_per_trace_display: Vec<String> = Vec::new();
    let mut activity_counts_for_overall_stats: HashMap<String, Vec<usize>> = HashMap::new();
    let mut total_activity_counts: HashMap<String, usize> = HashMap::new();
    let num_traces = traces.len();

    for (i, trace) in traces.iter().enumerate() {
        if trace.is_empty() {
            activity_frequencies_per_trace_display.push(format!("- Trace {}: (empty)", i + 1));
            continue;
        }
        let mut current_trace_counts: HashMap<String, usize> = HashMap::new();
        for activity in trace {
            *current_trace_counts.entry(activity.clone()).or_insert(0) += 1;
        }

        let mut trace_display_parts: Vec<String> = Vec::new();
        for (activity, count) in &current_trace_counts {
            trace_display_parts.push(format!("{} ({})", activity, count));
            activity_counts_for_overall_stats.entry(activity.clone()).or_default().push(*count);
            *total_activity_counts.entry(activity.clone()).or_insert(0) += count;
        }
        activity_frequencies_per_trace_display
            .push(format!("- Trace {}: {}", i + 1, trace_display_parts.join(", ")));
    }

    let mut overall_activity_stats_display: Vec<String> = Vec::new();
    let mut sorted_overall_activities: Vec<String> =
        activity_counts_for_overall_stats.keys().cloned().collect();
    sorted_overall_activities.sort_by(|a, b| a.cmp(b));

    for activity_key in &sorted_overall_activities {
        // if let Some(counts_vec) = activity_counts_for_overall_stats.get(&activity_key) {
        if let Some(counts_vec) = activity_counts_for_overall_stats.get(activity_key) {
            if counts_vec.is_empty() {
                continue;
            }
            let total_occurrences: usize = counts_vec.iter().sum();
            let num_traces_present = counts_vec.len();
            let min_in_trace = *counts_vec.iter().min().unwrap_or(&0);
            let max_in_trace = *counts_vec.iter().max().unwrap_or(&0);
            let avg_per_trace = if num_traces_present > 0 {
                total_occurrences as f64 / num_traces_present as f64
            } else {
                0.0
            };
            overall_activity_stats_display.push(format!(
                "- '{}': Total: {}, Appears in {} traces (avg {:.2} per trace where present), Min/Trace: {}, Max/Trace: {}",
                activity_key,
                total_occurrences,
                num_traces_present,
                avg_per_trace,
                min_in_trace,
                max_in_trace
            ));
        }
    }

    let mut average_activity_frequency_per_trace_display: Vec<String> = Vec::new();
    let mut sorted_average_activities: Vec<String> = total_activity_counts.keys().cloned().collect();
    sorted_average_activities.sort();

    for activity_key in sorted_average_activities {
        if let Some(total_count) = total_activity_counts.get(&activity_key) {
            let average_frequency = if num_traces > 0 {
                *total_count as f64 / num_traces as f64
            } else {
                0.0
            };
            average_activity_frequency_per_trace_display.push(format!(
                "- \'{}\': {:.2} times on average per trace",
                activity_key, average_frequency
            ));
        }
    }

    // Direct Self-Loop Counts
    let mut direct_self_loop_counts: HashMap<String, usize> = HashMap::new();
    for trace in traces {
        if trace.len() < 2 {
            continue;
        }
        for i in 0..(trace.len() - 1) {
            if trace[i] == trace[i + 1] {
                *direct_self_loop_counts
                    .entry(trace[i].clone())
                    .or_insert(0) += 1;
            }
        }
    }
    let mut direct_self_loop_counts_display: Vec<String> = Vec::new();
    // Use sorted_overall_activities to ensure all activities are listed
    for activity_key in &sorted_overall_activities { 
        let count = direct_self_loop_counts.get(activity_key).unwrap_or(&0);
        direct_self_loop_counts_display
            .push(format!("- '{}': {} times", activity_key, count));
    }

    // Basic Conditional Frequencies
    let mut conditional_freq_target_after_preceding = 0;
    let mut occurrences_of_preceding = 0;
    let mut conditional_freq_display: Vec<String> = Vec::new();

    let target_activity_str = conditional_target_activity.trim();
    let preceding_activity_str = conditional_preceding_activity.trim();

    if target_activity_str.is_empty() || preceding_activity_str.is_empty() {
        conditional_freq_display.push(
            " (Please specify both preceding and target activities for conditional frequency analysis)"
                .to_string(),
        );
    } else {
        for trace in traces {
            for i in 0..trace.len() {
                if trace[i] == preceding_activity_str {
                    occurrences_of_preceding += 1;
                    if i + 1 < trace.len() && trace[i + 1] == target_activity_str {
                        conditional_freq_target_after_preceding += 1;
                    }
                }
            }
        }
        if occurrences_of_preceding > 0 {
            conditional_freq_display.push(format!(
                "- Activity '{}' directly followed '{}': {} times (out of {} occurrences of '{}')",
                target_activity_str,
                preceding_activity_str,
                conditional_freq_target_after_preceding,
                occurrences_of_preceding,
                preceding_activity_str
            ));
        } else {
            conditional_freq_display.push(format!(
                " (Pattern '{}' then '{}' not applicable or '{}' not found)",
                preceding_activity_str, target_activity_str, preceding_activity_str
            ));
        }
    }
    conditional_freq_display.push(
        "(Note: More complex conditional loop analysis can be implemented with specific rules.)"
            .to_string(),
    );

    ActivityPatternAnalysis {
        activity_frequencies_per_trace_display,
        overall_activity_stats_display,
        direct_self_loop_counts_display,
        conditional_freq_display,
        average_activity_frequency_per_trace_display,
    }
}

pub fn generate_xes(text: &str) -> String {
    let mut output = String::with_capacity(text.len() * 8); // Estimate capacity

    // Parse the text to get traces and their frequencies
    let traces_with_frequencies = get_traces_with_frequencies(text);

    output.push_str("<log xes.version=\"1.0\" xes.features=\"nested-attributes\" openxes.version=\"1.0RC7\" xmlns=\"http://www.xes-standard.org/\">\n");

    for (trace, frequency) in traces_with_frequencies {
        // Repeat the trace based on the frequency
        for _ in 0..frequency {
            output.push_str("<trace>\n");

            let mut timestamp = DateTime::<Utc>::default();
            const EVENT_INTERVAL: i64 = 1000;

            for event in &trace {
                timestamp = timestamp
                    .checked_add_signed(Duration::milliseconds(EVENT_INTERVAL))
                    .expect("Time overflow occurred");

                output.push_str(&format!(
                    "<event>\n\
                    <string key=\"concept:name\" value=\"{}\"/>\n\
                    <date key=\"time:timestamp\" value=\"{}\"/>\n\
                    </event>\n",
                    event,
                    timestamp.to_rfc3339()
                ));
            }

            output.push_str("</trace>\n");
        }
    }

    output.push_str("</log>\n");

    output
}

/// Parse the text to get traces with their frequencies
fn get_traces_with_frequencies(text: &str) -> Vec<(Vec<&str>, usize)> {
    text.lines()
        .filter_map(|line| {
            let line = line.trim();
            if line.is_empty() {
                return None;
            }

            // Check if the line contains a frequency specified, and fallback to 1 if it doesn't
            let (trace_part, freq_part) = line.rsplit_once(':').unwrap_or((line, "1"));
            let frequency = freq_part.trim().parse::<usize>().unwrap_or(1);

            let trace: Vec<&str> = trace_part
                .split(',')
                .filter(|activity| !activity.trim().is_empty())
                .collect();

            if trace.is_empty() {
                None
            } else {
                Some((trace, frequency))
            }
        })
        .collect()
}

pub fn generate_adj_matrix_from_traces(
    traces: Vec<Vec<String>>,
    existential_threshold: f64,
    temporal_threshold: f64,
) -> (
    String,
    usize,
    usize,
    usize,
    usize,
    usize,
    HashMap<String, usize>,
    String,
) {
    let activities: HashSet<String> = traces
        .iter()
        .flat_map(|trace| trace.iter().cloned())
        .collect();

    generate_adj_matrix_from_activities_and_traces(
        &activities,
        traces,
        existential_threshold,
        temporal_threshold,
    )
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct MatrixTableData {
    pub activities: Vec<String>,
    pub activity_frequencies: HashMap<String, f64>,
    pub dependencies: Vec<Vec<String>>,
}

pub fn generate_adj_matrix_table_data(
    traces: Vec<Vec<String>>,
    existential_threshold: f64,
    temporal_threshold: f64,
) -> MatrixTableData {
    let activities: HashSet<String> = traces
        .iter()
        .flat_map(|trace| trace.iter().cloned())
        .collect();
    
    let mut activities_sorted: Vec<_> = activities.iter().cloned().collect();
    activities_sorted.sort();
    
    let mut total_activity_counts: HashMap<String, usize> = HashMap::new();
    let num_traces = traces.len();
    for trace in &traces {
        for activity in trace {
            *total_activity_counts.entry(activity.clone()).or_insert(0) += 1;
        }
    }

    let mut average_frequencies: HashMap<String, f64> = HashMap::new();
    for (activity, total_count) in &total_activity_counts {
        let avg_freq = if num_traces > 0 {
            *total_count as f64 / num_traces as f64
        } else {
            0.0
        };
        average_frequencies.insert(activity.clone(), avg_freq);
    }
    
    let converted_traces: Vec<Vec<&str>> = traces
        .iter()
        .map(|v| v.iter().map(|s| s.as_str()).collect())
        .collect();

    let loops = detect_loops(&converted_traces);
    
    let mut dependencies_matrix = Vec::new();
    
    for from in &activities_sorted {
        let mut row = Vec::new();
        for to in &activities_sorted {
            let temporal_dependency =
                check_temporal_dependency_with_loops(from, to, &converted_traces, temporal_threshold, &loops);
            let existential_dependency = check_existential_dependency_with_loops(
                from,
                to,
                &converted_traces,
                existential_threshold,
                &loops,
            );

            let dependency = Dependency::new(
                from.to_string(),
                to.to_string(),
                temporal_dependency,
                existential_dependency,
            );
            
            row.push(format!("{}", dependency));
        }
        dependencies_matrix.push(row);
    }
    
    MatrixTableData {
        activities: activities_sorted,
        activity_frequencies: average_frequencies,
        dependencies: dependencies_matrix,
    }
}

pub fn generate_adj_matrix_from_activities_and_traces(
    activities: &HashSet<String>,
    traces: Vec<Vec<String>>,
    existential_threshold: f64,
    temporal_threshold: f64,
) -> (
    String,
    usize,
    usize,
    usize,
    usize,
    usize,
    HashMap<String, usize>,
    String,
) {
    const MAX_DEPENDENCY_WIDTH: usize = 15;

    let mut output =
        String::with_capacity(activities.len() * activities.len() * MAX_DEPENDENCY_WIDTH);
    let mut metrics = MatrixMetrics::default();
    let mut dependency_entries = Vec::new();

    // Calculate average activity frequencies
    let mut total_activity_counts: HashMap<String, usize> = HashMap::new();
    let num_traces = traces.len();
    for trace in &traces {
        for activity in trace {
            *total_activity_counts.entry(activity.clone()).or_insert(0) += 1;
        }
    }

    let mut average_frequencies: HashMap<String, f64> = HashMap::new();
    for (activity, total_count) in &total_activity_counts {
        let avg_freq = if num_traces > 0 {
            *total_count as f64 / num_traces as f64
        } else {
            0.0
        };
        average_frequencies.insert(activity.clone(), avg_freq);
    }

    // Header
    output.push_str(&format!("{:<MAX_DEPENDENCY_WIDTH$}", " "));

    let mut activities_sorted: Vec<_> = activities.iter().collect();
    activities_sorted.sort();

    for activity_name in &activities_sorted {
        let avg_freq = average_frequencies.get(*activity_name).unwrap_or(&0.0);
        let header_display = format!("{} ({:.2})", activity_name, avg_freq);
        output.push_str(&format!("{:<MAX_DEPENDENCY_WIDTH$}", header_display));
    }
    output.push('\n');

    let format_dependency = |dep: &Dependency| {
        format!(
            "{:<width$}",
            format!("{}", dep),
            width = MAX_DEPENDENCY_WIDTH
        )
    };

    let converted_traces: Vec<Vec<&str>> = traces
        .iter()
        .map(|v| v.iter().map(|s| s.as_str()).collect())
        .collect();

    let loops = detect_loops(&converted_traces);

    for from in &activities_sorted {
        output.push_str(&format!("{:<MAX_DEPENDENCY_WIDTH$}", from));

        for to in &activities_sorted {
            let temporal_dependency =
                check_temporal_dependency_with_loops(from, to, &converted_traces, temporal_threshold, &loops);
            let existential_dependency = check_existential_dependency_with_loops(
                from,
                to,
                &converted_traces,
                existential_threshold,
                &loops,
            );

            let dependency = Dependency::new(
                from.to_string(),
                to.to_string(),
                temporal_dependency.clone(),
                existential_dependency.clone(),
            );

            let temporal_spec = temporal_dependency.as_ref().map(|dep| TemporalSpec {
                type_: match dep.dependency_type {
                    TemporalType::Direct => "direct".to_string(),
                    TemporalType::Eventual => "eventual".to_string(),
                    TemporalType::TrueEventual => "eventual".to_string(),
                },
                symbol: format!("{}", dep),
                direction: match dep.direction {
                    TemporalDirection::Forward => "forward".to_string(),
                    TemporalDirection::Backward => "backward".to_string(),
                },
            });

            let existential_spec = existential_dependency.as_ref().map(|dep| ExistentialSpec {
                type_: match dep.dependency_type {
                    ExistentialType::Implication => "implication".to_string(),
                    ExistentialType::Equivalence => "equivalence".to_string(),
                    ExistentialType::NegatedEquivalence => "negated_equivalence".to_string(),
                    ExistentialType::Nand => "nand".to_string(),
                    ExistentialType::Or => "or".to_string(),
                },
                symbol: format!("{}", dep),
                direction: match dep.direction {
                    ExistentialDirection::Forward => "forward".to_string(),
                    ExistentialDirection::Backward => "backward".to_string(),
                    ExistentialDirection::Both => "both".to_string(),
                },
            });

            if temporal_spec.is_some() || existential_spec.is_some() {
                dependency_entries.push(DependencyEntry {
                    from: (*from).clone(),
                    to: (*to).clone(),
                    temporal: temporal_spec,
                    existential: existential_spec,
                });
            }

            metrics.update(&temporal_dependency, &existential_dependency);
            output.push_str(&format_dependency(&dependency));
        }
        output.push('\n');
    }

    let activity_statistics = generate_activity_statistics(
        &traces,
        &total_activity_counts,
        activities,
    );

    let trace_statistics = generate_trace_statistics(&traces);

    let loop_information = generate_loop_information(&loops, &converted_traces);

    let sccs = generate_strongly_connected_components(&dependency_entries, activities);

    let adj_matrix_yaml = AdjacencyMatrix {
        metadata: Metadata {
            format_version: "1.0".to_string(),
            description: "Automatically generated adjacency matrix with comprehensive analysis".to_string(),
            activities: activities_sorted.iter().map(|s| s.to_string()).collect(),
        },
        activity_statistics,
        trace_statistics,
        loop_information,
        strongly_connected_components: sccs,
        dependencies: dependency_entries,
    };

    let yaml_string = serde_yaml::to_string(&adj_matrix_yaml).unwrap_or_default();

    (
        output,
        metrics.full_independences,
        metrics.pure_existences,
        metrics.eventual_equivalences,
        metrics.direct_equivalences,
        activities.len(),
        metrics.relationship_counts,
        yaml_string,
    )
}

fn generate_activity_statistics(
    traces: &[Vec<String>],
    total_activity_counts: &HashMap<String, usize>,
    activities: &HashSet<String>,
) -> ActivityStatistics {
    let mut activity_frequencies = HashMap::new();
    let mut self_loop_counts = HashMap::new();

    for activity in activities {
        let mut traces_present_in = 0;
        let mut occurrence_counts = Vec::new();
        
        for trace in traces {
            let count = trace.iter().filter(|&a| a == activity).count();
            if count > 0 {
                traces_present_in += 1;
                occurrence_counts.push(count);
            }
        }

        let total_occurrences = total_activity_counts.get(activity).unwrap_or(&0);
        let average_per_trace = if traces_present_in > 0 {
            *total_occurrences as f64 / traces_present_in as f64
        } else {
            0.0
        };

        let min_per_trace = *occurrence_counts.iter().min().unwrap_or(&0);
        let max_per_trace = *occurrence_counts.iter().max().unwrap_or(&0);

        activity_frequencies.insert(activity.clone(), ActivityFrequency {
            total_occurrences: *total_occurrences,
            average_per_trace,
            traces_present_in,
            min_per_trace,
            max_per_trace,
        });
    }

    for trace in traces {
        for window in trace.windows(2) {
            if window[0] == window[1] {
                *self_loop_counts.entry(window[0].clone()).or_insert(0) += 1;
            }
        }
    }

    ActivityStatistics {
        total_activities: activities.len(),
        activity_frequencies,
        self_loop_counts,
    }
}

fn generate_trace_statistics(traces: &[Vec<String>]) -> TraceStatistics {
    let total_traces = traces.len();
    let total_events: usize = traces.iter().map(|t| t.len()).sum();
    let average_trace_length = if total_traces > 0 {
        total_events as f64 / total_traces as f64
    } else {
        0.0
    };
    
    let min_trace_length = traces.iter().map(|t| t.len()).min().unwrap_or(0);
    let max_trace_length = traces.iter().map(|t| t.len()).max().unwrap_or(0);

    TraceStatistics {
        total_traces,
        average_trace_length,
        min_trace_length,
        max_trace_length,
        total_events,
    }
}

fn generate_loop_information(loops: &[Loop], traces: &[Vec<&str>]) -> LoopInformation {
    let mut self_loops = Vec::new();
    let mut structural_loops = Vec::new();

    for (loop_id, loop_info) in loops.iter().enumerate() {
        match loop_info.loop_type {
            LoopType::SelfLoop => {
                if let Some(activity) = loop_info.activities.iter().next() {
                    let mut occurrences = 0;
                    for trace in traces {
                        for window in trace.windows(2) {
                            if window[0] == window[1] && window[0] == activity {
                                occurrences += 1;
                            }
                        }
                    }
                    self_loops.push(SelfLoop {
                        activity: activity.clone(),
                        occurrences,
                    });
                }
            }
            LoopType::StructuralLoop => {
                let mut activities: Vec<String> = loop_info.activities.iter().cloned().collect();
                activities.sort();
                structural_loops.push(StructuralLoop {
                    activities,
                    loop_id,
                });
            }
        }
    }

    LoopInformation {
        self_loops,
        structural_loops,
        total_loops: loops.len(),
    }
}

fn generate_strongly_connected_components(
    dependency_entries: &[DependencyEntry],
    activities: &HashSet<String>,
) -> Vec<StronglyConnectedComponent> {
    use std::collections::HashMap;
    
    let mut graph: HashMap<String, HashSet<String>> = HashMap::new();
    
    for activity in activities {
        graph.insert(activity.clone(), HashSet::new());
    }
    
    for dep in dependency_entries {
        if let Some(temporal) = &dep.temporal {
            if temporal.type_ == "direct" {
                graph.entry(dep.from.clone())
                    .or_default()
                    .insert(dep.to.clone());
            }
        }
    }
    
    let sccs = find_sccs_simple(&graph, activities);
    
    sccs.into_iter()
        .enumerate()
        .map(|(component_id, mut activities)| {
            activities.sort();
            let size = activities.len();
            StronglyConnectedComponent {
                activities,
                component_id,
                size,
            }
        })
        .filter(|scc| scc.size > 1)
        .collect()
}

fn find_sccs_simple(graph: &HashMap<String, HashSet<String>>, activities: &HashSet<String>) -> Vec<Vec<String>> {
    let mut visited = HashSet::new();
    let mut sccs = Vec::new();
    
    for activity in activities {
        if !visited.contains(activity) {
            let mut scc = Vec::new();
            dfs_scc(activity, graph, &mut visited, &mut scc);
            if scc.len() > 1 {
                sccs.push(scc);
            }
        }
    }
    
    sccs
}

fn dfs_scc(
    node: &str,
    graph: &HashMap<String, HashSet<String>>,
    visited: &mut HashSet<String>,
    scc: &mut Vec<String>,
) {
    if visited.contains(node) {
        return;
    }
    
    visited.insert(node.to_string());
    scc.push(node.to_string());
    
    if let Some(neighbors) = graph.get(node) {
        for neighbor in neighbors {
            dfs_scc(neighbor, graph, visited, scc);
        }
    }
}

#[derive(Default)]
struct MatrixMetrics {
    full_independences: usize,
    pure_existences: usize,
    eventual_equivalences: usize,
    direct_equivalences: usize,
    relationship_counts: HashMap<String, usize>,
}

impl MatrixMetrics {
    fn update(
        &mut self,
        temporal_dependency: &Option<dependency_types::temporal::TemporalDependency>,
        existential_dependency: &Option<dependency_types::existential::ExistentialDependency>,
    ) {
        use dependency_types::{
            existential::DependencyType as EDType, temporal::DependencyType as TDType,
        };

        let temporal_type = match temporal_dependency {
            Some(td) => match td.dependency_type {
                TDType::Eventual => "eventual",
                TDType::Direct => "direct",
                TDType::TrueEventual => "true_eventual",
            },
            None => {
                self.pure_existences += 1;
                "none"
            }
        };

        let existential_type = match existential_dependency {
            Some(ed) => match ed.dependency_type {
                EDType::Equivalence => "equivalence",
                EDType::Implication => "implication",
                EDType::NegatedEquivalence => "negated equivalence",
                _ => "other",
            },
            None => {
                if temporal_type == "none" {
                    self.full_independences += 1;
                }
                "none"
            }
        };

        // Record relationship type
        let relationship_type = format!("({}, {})", temporal_type, existential_type);
        *self
            .relationship_counts
            .entry(relationship_type)
            .or_insert(0) += 1;

        // Check for equivalences
        if let Some(ed) = existential_dependency {
            if ed.dependency_type == EDType::Equivalence {
                if let Some(td) = temporal_dependency {
                    match td.dependency_type {
                        TDType::Eventual => self.eventual_equivalences += 1,
                        TDType::Direct => self.direct_equivalences += 1,
                        TDType::TrueEventual => self.eventual_equivalences += 1, // True eventuals count as eventual for equivalence statistics
                    }
                }
            }
        }
    }
}

pub fn get_activities_and_traces(text: &str) -> (Vec<String>, Vec<Vec<&str>>) {
    let traces = get_traces(text);
    let activities: HashSet<String> = traces
        .iter()
        .flat_map(|trace| trace.iter().map(|&s| s.to_string()))
        .collect();

    (activities.into_iter().collect(), traces)
}

pub fn get_traces(text: &str) -> Vec<Vec<&str>> {
    text.lines()
        .filter_map(|line| {
            let trace: Vec<&str> = line
                .split(',')
                .filter(|&activity| !activity.trim().is_empty())
                .collect();

            if !trace.is_empty() {
                Some(trace)
            } else {
                None
            }
        })
        .collect()
}

// TODO: fix tests so they can be ran simultaneously and don't interfere with each other
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_activities_and_traces() {
        let traces = "
activity 3,activity 3,activity 3,activity 3,activity 3,activity 1,activity 1,activity 2,
activity 3,activity 1,activity 2,
activity 1,activity 1,activity 1,activity 1,activity 3,activity 1,activity 1,activity 2,
activity 3,activity 1,activity 1,activity 2,
";
        let (activities, traces) = get_activities_and_traces(traces);
        let expected_activities: HashSet<_> = vec!["activity 1", "activity 2", "activity 3"]
            .into_iter()
            .map(String::from)
            .collect();
        assert_eq!(expected_activities, activities.into_iter().collect());

        let expected_traces = vec![
            vec![
                "activity 3",
                "activity 3",
                "activity 3",
                "activity 3",
                "activity 3",
                "activity 1",
                "activity 1",
                "activity 2",
            ],
            vec!["activity 3", "activity 1", "activity 2"],
            vec![
                "activity 1",
                "activity 1",
                "activity 1",
                "activity 1",
                "activity 3",
                "activity 1",
                "activity 1",
                "activity 2",
            ],
            vec!["activity 3", "activity 1", "activity 1", "activity 2"],
        ];
        assert_eq!(expected_traces, traces);
    }

    #[test]
    fn test_get_traces() {
        let traces = "
activity 3,activity 3,activity 3,activity 3,activity 3,activity 1,activity 1,activity 2,
activity 3,activity 1,activity 2,
activity 1,activity 1,activity 1,activity 1,activity 3,activity 1,activity 1,activity 2,
activity 3,activity 1,activity 1,activity 2,
";
        let expected_traces = vec![
            vec![
                "activity 3",
                "activity 3",
                "activity 3",
                "activity 3",
                "activity 3",
                "activity 1",
                "activity 1",
                "activity 2",
            ],
            vec!["activity 3", "activity 1", "activity 2"],
            vec![
                "activity 1",
                "activity 1",
                "activity 1",
                "activity 1",
                "activity 3",
                "activity 1",
                "activity 1",
                "activity 2",
            ],
            vec!["activity 3", "activity 1", "activity 1", "activity 2"],
        ];
        assert_eq!(expected_traces, get_traces(traces));
    }
}
