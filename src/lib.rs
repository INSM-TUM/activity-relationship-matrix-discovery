use chrono::{DateTime, Duration, Utc};
use dependency_types::{
    dependency::Dependency, existential::check_existential_dependency,
    temporal::check_temporal_dependency,
};
use std::collections::{HashMap, HashSet};

pub mod dependency_types;
pub mod epa;
pub mod evaluation;
pub mod event;
pub mod parser;
pub mod routes;
pub mod state;

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
) {
    const MAX_DEPENDENCY_WIDTH: usize = 15;

    let mut output =
        String::with_capacity(activities.len() * activities.len() * MAX_DEPENDENCY_WIDTH);
    let mut metrics = MatrixMetrics::default();

    // Header
    output.push_str(&format!("{:<MAX_DEPENDENCY_WIDTH$}", " "));

    let mut activities_sorted: Vec<_> = activities.iter().collect();
    activities_sorted.sort();

    for activity in &activities_sorted {
        output.push_str(&format!("{:<MAX_DEPENDENCY_WIDTH$}", activity));
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

    for from in &activities_sorted {
        output.push_str(&format!("{:<MAX_DEPENDENCY_WIDTH$}", from));

        for to in &activities_sorted {
            if to != from {
                let temporal_dependency =
                    check_temporal_dependency(from, to, &converted_traces, temporal_threshold);
                let existential_dependency = check_existential_dependency(
                    from,
                    to,
                    &converted_traces,
                    existential_threshold,
                );

                let dependency = Dependency::new(
                    from.to_string(),
                    to.to_string(),
                    temporal_dependency.clone(),
                    existential_dependency.clone(),
                );

                metrics.update(&temporal_dependency, &existential_dependency);

                output.push_str(&format_dependency(&dependency));
            } else {
                output.push_str(&format!("{:<MAX_DEPENDENCY_WIDTH$}", "TODO"));
            }
        }
        output.push('\n');
    }

    (
        output,
        metrics.full_independences,
        metrics.pure_existences,
        metrics.eventual_equivalences,
        metrics.direct_equivalences,
        activities.len(),
        metrics.relationship_counts,
    )
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
