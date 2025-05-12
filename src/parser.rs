use chrono::{DateTime, Utc};
use std::collections::{HashMap, HashSet};

use process_mining::{
    event_log::{import_xes::XESParseError, AttributeValue},
    import_xes_file, import_xes_slice, XESImportOptions,
};

#[derive(Debug, Clone)]
struct Event {
    activity: String,
    date: DateTime<Utc>,
}

impl Event {
    fn new(activity: String, date: DateTime<Utc>) -> Event {
        Event { activity, date }
    }
}

// Helper function to extract relevant attributes
fn extract_event_attributes(
    attributes: &[process_mining::event_log::Attribute],
) -> (Option<String>, Option<DateTime<Utc>>) {
    let mut name = None;
    let mut date = None;

    for attribute in attributes {
        match attribute.key.as_str() {
            "concept:name" => {
                if let AttributeValue::String(value) = &attribute.value {
                    name = Some(value.clone());
                }
            }
            "time:timestamp" => {
                if let AttributeValue::Date(value) = &attribute.value {
                    date = Some(*value);
                }
            }
            _ => continue,
        }
    }

    (name, date)
}

pub fn get_activities(path: &str) -> Option<HashSet<String>> {
    let event_log = import_xes_file(path, XESImportOptions::default()).ok()?;
    let traces = event_log.traces;
    let mut activities = HashSet::new();

    for trace in traces {
        // Check if there is a lifecycle:transition with value "complete" in the trace
        let has_complete = trace.events.iter().any(|event| {
            event.attributes.iter().any(|a| {
                a.key == "lifecycle:transition"
                    && a.value == AttributeValue::String("complete".to_string())
            })
        });

        for event in trace.events {
            let mut name = None;

            // If "complete" is present, we only consider events with "complete" transition
            if !has_complete
                || event.attributes.iter().any(|a| {
                    a.key == "lifecycle:transition"
                        && a.value == AttributeValue::String("complete".to_string())
                })
            {
                // Extract the "concept:name" (activity name) if it exists
                for attribute in event.attributes {
                    if attribute.key == "concept:name" {
                        if let AttributeValue::String(value) = &attribute.value {
                            name = Some(value.clone());
                        }
                    }
                }

                if let Some(name) = name {
                    activities.insert(name);
                }
            }
        }
    }

    Some(activities)
}

pub fn parse_into_traces(
    path: Option<&str>,
    content: Option<&str>,
) -> Result<Vec<Vec<String>>, XESParseError> {
    let traces = match (path, content) {
        (Some(path), _) => {
            let event_log = import_xes_file(path, XESImportOptions::default())?;
            event_log.traces
        }
        (None, Some(content)) => {
            let event_log =
                import_xes_slice(content.as_bytes(), false, XESImportOptions::default())?;
            event_log.traces
        }
        _ => panic!("Either path or content must be provided, not both"),
    };

    let mut result = Vec::new();

    for trace in traces {
        let mut events: Vec<Event> = Vec::new();

        // first check if there is a lifecycle:transition with value complete anywhere in the trace
        let has_complete = trace.events.iter().any(|event| {
            event.attributes.iter().any(|a| {
                a.key == "lifecycle:transition"
                    && a.value == AttributeValue::String("complete".to_string())
            })
        });

        for event in trace.events {
            let (name, date) = extract_event_attributes(&event.attributes);

            if !has_complete
                || event.attributes.iter().any(|a| {
                    a.key == "lifecycle:transition"
                        && a.value == AttributeValue::String("complete".to_string())
                })
            {
                if let (Some(name), Some(date)) = (name, date) {
                    events.push(Event::new(name, date));
                }
            }
        }

        events.sort_by(|a, b| a.date.cmp(&b.date)); // sort events by date

        let activity_list: Vec<String> = events.into_iter().map(|event| event.activity).collect();
        result.push(activity_list);
    }

    Ok(result)
}

pub fn variants_of_traces(traces: Vec<Vec<&str>>) -> HashMap<Vec<&str>, usize> {
    traces.into_iter().fold(HashMap::new(), |mut acc, trace| {
        *acc.entry(trace).or_insert(0) += 1;
        acc
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_activities() {
        let activities = get_activities("./sample-data/exercise2.xes").unwrap();
        assert_eq!(activities.len(), 5);
        let actual_activities = ["A", "B", "C", "D", "E"];
        actual_activities
            .into_iter()
            .for_each(|a| assert!(activities.contains(a)));
    }

    #[test]
    fn test_parse_into_traces() {
        let traces = parse_into_traces(Some("./sample-data/exercise2.xes"), None).unwrap();
        assert_eq!(traces.len(), 2);
        assert_eq!(traces[0].len(), 3);
        assert_eq!(traces[1].len(), 3);

        assert_eq!(traces[0], ["B", "C", "E"]);
        assert_eq!(traces[1], ["A", "C", "D"]);
    }

    // #[test]
    // fn test_parse_into_traces_dups() {
    //     let traces =
    //         parse_into_traces(Some("./sample-data/Example_SemiStructured.xes"), None).unwrap();
    //     println!("{:?}", traces);
    //     assert_eq!(traces.len(), 2);
    // }

    #[test]
    fn test_variants_of_traces() {
        let traces = vec![
            vec!["A", "B", "C"],
            vec!["A", "B", "C"],
            vec!["B", "C", "D"],
            vec!["A", "B", "C"],
            vec!["B", "C", "D"],
            vec!["E", "F", "G"],
        ];

        let result = variants_of_traces(traces);
        assert_eq!(result.len(), 3);
        assert_eq!(result[&vec!["A", "B", "C"]], 3);
        assert_eq!(result[&vec!["B", "C", "D"]], 2);
        assert_eq!(result[&vec!["E", "F", "G"]], 1);
    }

    //     #[test]
    //     fn test_failing_event_logs() {
    //         // let foo = parse_into_traces(Some("./sample-data/PrepaidTravelCost.xes"), None);
    //         // let foo = parse_into_traces(Some("./sample-data/RequestForPayment.xes"), None);
    //         // let foo = parse_into_traces(Some("./sample-data/BPI_Challenge_2013_incidents.xes"), None);
    //         let foo = parse_into_traces(Some("./sample-data/exercise2.xes"), None);
    //         println!("{:?}", foo);
    //         assert_eq!(2, 3);
    //     }
}
