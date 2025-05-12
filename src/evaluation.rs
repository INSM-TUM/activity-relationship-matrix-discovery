use std::collections::HashSet;

use crate::{
    dependency_types::{
        dependency::{convert_to_dependencies, Dependency},
        existential::{self, check_existential_dependency},
        temporal::{self, check_temporal_dependency},
    },
    parser::parse_into_traces,
};
use once_cell::sync::Lazy;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{File, FileReader, HtmlInputElement, MouseEvent};
use yew::prelude::*;

pub enum Msg {
    AddRelationship,
    UpdateFrom(String),
    UpdateTo(String),
    UpdateTemporal(String),
    UpdateTemporalDirection(String),
    UpdateExistential(String),
    UpdateExistentialDirection(String),
    FileImport(Option<File>),
    FileLoaded(Result<String, String>),
}

#[derive(Clone)]
pub struct Relationship {
    from: String,
    to: String,
    temporal_type: Option<temporal::DependencyType>,
    temporal_direction: Option<temporal::Direction>,
    existential_type: Option<existential::DependencyType>,
    existential_direction: Option<existential::Direction>,
}

#[derive(Clone, PartialEq)]
pub struct RelationInput {
    pub from: String,
    pub to: String,
    pub temporal_type: Option<temporal::DependencyType>,
    pub temporal_direction: Option<temporal::Direction>,
    pub existential_type: Option<existential::DependencyType>,
    pub existential_direction: Option<existential::Direction>,
}

impl Default for RelationInput {
    fn default() -> Self {
        Self {
            from: String::new(),
            to: String::new(),
            temporal_type: None,
            temporal_direction: None,
            existential_type: None,
            existential_direction: None,
        }
    }
    
}

impl RelationInput {
    pub fn to_dependency(&self) -> Option<Dependency> {
        let temporal_dependency = match (
            self.temporal_type.as_ref(),
            self.temporal_direction.as_ref(),
        ) {
            (Some(t_type), Some(t_dir)) => Some(temporal::TemporalDependency::new(
                &self.from,
                &self.to,
                t_type.clone(),
                t_dir.clone(),
            )),
            _ => None,
        };

        let existential_dependency = match (
            self.existential_type.as_ref(),
            self.existential_direction.as_ref(),
        ) {
            (Some(e_type), Some(e_dir)) => Some(existential::ExistentialDependency::new(
                &self.from,
                &self.to,
                e_type.clone(),
                e_dir.clone(),
            )),
            _ => None,
        };

        // Always return Some with a new Dependency, bc otherwise the
        // evaluation will fail (?)
        Some(Dependency::new(
            self.from.clone(),
            self.to.clone(),
            temporal_dependency,
            existential_dependency,
        ))
    }
}

pub struct Evaluation {
    relationships: Vec<Relationship>,
    current: Relationship,
    matrix: Option<String>,
    file_reader: Option<FileReader>,
    file_content: Option<String>,
}

impl Component for Evaluation {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self {
            relationships: Vec::new(),
            current: Relationship {
                from: String::new(),
                to: String::new(),
                temporal_type: None,
                temporal_direction: None,
                existential_type: None,
                existential_direction: None,
            },
            matrix: None,
            file_reader: None,
            file_content: None,
        }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::AddRelationship => {
                if !self.current.from.is_empty() && !self.current.to.is_empty() {
                    self.relationships.push(self.current.clone());
                    self.current = Relationship {
                        from: String::new(),
                        to: String::new(),
                        temporal_type: None,
                        temporal_direction: None,
                        existential_type: None,
                        existential_direction: None,
                    };
                }
                true
            }
            Msg::UpdateFrom(value) => {
                self.current.from = value;
                true
            }
            Msg::UpdateTo(value) => {
                self.current.to = value;
                true
            }
            Msg::UpdateTemporal(value) => {
                self.current.temporal_type = match value.as_str() {
                    "direct" => Some(temporal::DependencyType::Direct),
                    "eventual" => Some(temporal::DependencyType::Eventual),
                    _ => None,
                };
                true
            }
            Msg::UpdateTemporalDirection(value) => {
                self.current.temporal_direction = match value.as_str() {
                    "forward" => Some(temporal::Direction::Forward),
                    "backward" => Some(temporal::Direction::Backward),
                    _ => None,
                };
                true
            }
            Msg::UpdateExistential(value) => {
                self.current.existential_type = match value.as_str() {
                    "implication" => Some(existential::DependencyType::Implication),
                    "equivalence" => Some(existential::DependencyType::Equivalence),
                    "negated_equivalence" => Some(existential::DependencyType::NegatedEquivalence),
                    "nand" => Some(existential::DependencyType::Nand),
                    "or" => Some(existential::DependencyType::Or),
                    _ => None,
                };
                self.current.existential_direction = None; // Reset direction when type changes
                true
            }
            Msg::UpdateExistentialDirection(value) => {
                self.current.existential_direction = match value.as_str() {
                    "forward" => Some(existential::Direction::Forward),
                    "backward" => Some(existential::Direction::Backward),
                    "both" => Some(existential::Direction::Both),
                    _ => None,
                };
                true
            }
            _ => false,
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        html! {
            <RelationForm />
        }
    }
}

#[function_component(RelationForm)]
pub fn relation_form() -> Html {
    let current_relation = use_state(RelationInput::default);
    let relations = use_state(Vec::<RelationInput>::new);
    let file_content = use_state(|| None::<String>);
    let evaluation_result = use_state(|| None::<(usize, usize, usize, usize)>);

    let onsubmit = {
        let current = current_relation.clone();
        let relations_state = relations.clone();
        Callback::from(move |e: FocusEvent| {
            e.prevent_default();
            let mut new_relations = (*relations_state).clone();
            if !(*current).from.is_empty() && !(*current).to.is_empty() {
                new_relations.push((*current).clone());
                relations_state.set(new_relations);
                // Reset all select elements to default values
                if let Some(document) = web_sys::window().and_then(|w| w.document()) {
                    // Reset temporal dependency select
                    if let Ok(Some(select)) = document.query_selector("select[id='temporal-type']") {
                        select.unchecked_into::<HtmlInputElement>().set_value("");
                    }
                    // Reset temporal direction select
                    if let Ok(Some(select)) = document.query_selector("select[id='temporal-direction']") {
                        select.unchecked_into::<HtmlInputElement>().set_value("");
                    }
                    // Reset existential dependency select
                    if let Ok(Some(select)) = document.query_selector("select[id='existential-type']") {
                        select.unchecked_into::<HtmlInputElement>().set_value("");
                    }
                    // Reset existential direction select
                    if let Ok(Some(select)) = document.query_selector("select[id='existential-direction']") {
                        select.unchecked_into::<HtmlInputElement>().set_value("");
                    }
                }
                current.set(RelationInput::default());
            }
        })
    };

    // File upload handler
    let onchange = {
        let file_content = file_content.clone();
        Callback::from(move |e: Event| {
            let input: HtmlInputElement = e.target_unchecked_into();
            if let Some(file) = input.files().and_then(|files| files.get(0)) {
                let file_content = file_content.clone();
                let reader = FileReader::new().unwrap();
                let reader_clone = reader.clone();
                let onload = Closure::wrap(Box::new(move |_: web_sys::ProgressEvent| {
                    if let Ok(result) = reader_clone.result() {
                        if let Some(content) = result.as_string() {
                            file_content.set(Some(content));
                        }
                    }
                })
                    as Box<dyn FnMut(web_sys::ProgressEvent)>);
                reader.set_onload(Some(onload.as_ref().unchecked_ref()));
                onload.forget();
                reader.read_as_text(&file).unwrap();
            }
        })
    };

    let evaluate = {
        let relations_state = relations.clone();
        let file_content = file_content.clone();
        let evaluation_result = evaluation_result.clone();
        Callback::from(move |_: MouseEvent| {
            if let Some(content) = (*file_content).as_ref() {
                let deps: Vec<Dependency> = (*relations_state)
                    .iter()
                    .filter_map(|r| r.to_dependency())
                    .collect();

                let result = evaluate_deps(&deps, content);
                evaluation_result.set(Some(result));
            }
        })
    };

    html! {
        <div class="container">
            <h2>{"Create Dependencies"}</h2>
            <p>{"Enter each pair of activities that you would like to be represented in your adjacency matrix."}</p>
            <p>{"The matrix will then be evaluated against the uploaded event log."}</p>
            <p>{"Please note that the evaluation is based on the dependencies you provide here (which means they are the ones that should be correct)."}</p>
            <form {onsubmit}>
                <div class="relationship-form">
                    <input
                        type="text"
                        placeholder="From Activity"
                        value={current_relation.from.clone()}
                        onchange={
                            let current = current_relation.clone();
                            Callback::from(move |e: Event| {
                                let input: HtmlInputElement = e.target_unchecked_into();
                                let mut new_relation = (*current).clone();
                                new_relation.from = input.value();
                                current.set(new_relation);
                            })
                        }
                    />
                    <input
                        type="text"
                        placeholder="To Activity"
                        value={current_relation.to.clone()}
                        onchange={
                            let current = current_relation.clone();
                            Callback::from(move |e: Event| {
                                let input: HtmlInputElement = e.target_unchecked_into();
                                let mut new_relation = (*current).clone();
                                new_relation.to = input.value();
                                current.set(new_relation);
                            })
                        }
                    />
                    <select 
                        id="temporal-type"
                        onchange={
                        let current = current_relation.clone();
                        Callback::from(move |e: Event| {
                            let select: HtmlInputElement = e.target_unchecked_into();
                            let mut new_relation = (*current).clone();
                            new_relation.temporal_type = match select.value().as_str() {
                                "direct" => {
                                    // Set Forward as default direction for Direct
                                    new_relation.temporal_direction = Some(temporal::Direction::Forward);
                                    Some(temporal::DependencyType::Direct)
                                }
                                "eventual" => {
                                    // Set Forward as default direction for Eventual
                                    new_relation.temporal_direction = Some(temporal::Direction::Forward);
                                    Some(temporal::DependencyType::Eventual)
                                }
                                "independent" => {
                                    new_relation.temporal_direction = None;
                                    None
                                }
                                _ => None,
                            };
                            current.set(new_relation);
                        })
                    }>
                        <option value="">{"Select Temporal Dependency"}</option>
                        <option value="direct">{"Direct"}</option>
                        <option value="eventual">{"Eventual"}</option>
                        <option value="independent">{"Independent"}</option>
                    </select>
                    
                    {if current_relation.temporal_type.is_some() {
                        html! {
                            <select 
                                id="temporal-direction"
                                value={current_relation.temporal_direction.clone().map_or("forward".to_string(), |d| match d {
                                    temporal::Direction::Forward => "forward".to_string(),
                                    temporal::Direction::Backward => "backward".to_string(),
                                })}
                                onchange={
                                let current = current_relation.clone();
                                Callback::from(move |e: Event| {
                                    let select: HtmlInputElement = e.target_unchecked_into();
                                    let mut new_relation = (*current).clone();
                                    new_relation.temporal_direction = match select.value().as_str() {
                                        "forward" => Some(temporal::Direction::Forward),
                                        "backward" => Some(temporal::Direction::Backward),
                                        _ => None,
                                    };
                                    current.set(new_relation);
                                })
                            }>
                                <option value="">{"Select Temporal Direction"}</option>
                                <option value="forward" selected=true>{"Forward"}</option>
                                <option value="backward">{"Backward"}</option>
                            </select>
                        }
                    } else {
                        html! {}
                    }}
                    <select 
                        id="existential-type"
                        onchange={
                        let current = current_relation.clone();
                        Callback::from(move |e: Event| {
                            let select: HtmlInputElement = e.target_unchecked_into();
                            let mut new_relation = (*current).clone();
                            new_relation.existential_type = match select.value().as_str() {
                                "implication" => {
                                    new_relation.existential_direction = Some(existential::Direction::Forward);
                                    Some(existential::DependencyType::Implication)
                                },
                                "equivalence" | "negated_equivalence" | "nand" | "or" => {
                                    new_relation.existential_direction = Some(existential::Direction::Both);
                                    match select.value().as_str() {
                                        "equivalence" => Some(existential::DependencyType::Equivalence),
                                        "negated_equivalence" => Some(existential::DependencyType::NegatedEquivalence),
                                        "nand" => Some(existential::DependencyType::Nand),
                                        "or" => Some(existential::DependencyType::Or),
                                        _ => None,
                                    }
                                },
                                "independent" => None,
                                _ => None,
                            };
                            current.set(new_relation);
                        })
                    }>
                        <option value="">{"Select Existential Dependency"}</option>
                        <option value="implication">{"Implication"}</option>
                        <option value="equivalence">{"Equivalence"}</option>
                        <option value="negated_equivalence">{"Negated Equivalence"}</option>
                        <option value="nand">{"Nand"}</option>
                        <option value="or">{"Or"}</option>
                        <option value="independent">{"Independent"}</option>
                    </select>

                    {if current_relation.existential_type.is_some() &&
                        matches!(current_relation.existential_type, Some(existential::DependencyType::Implication)) {
                        html! {
                            <select 
                                id="existential-direction"
                                value={current_relation.existential_direction.clone().map_or("".to_string(), |d| match d {
                                    existential::Direction::Forward => "forward".to_string(),
                                    existential::Direction::Backward => "backward".to_string(),
                                    existential::Direction::Both => "both".to_string(),
                                })}
                                onchange={
                                let current = current_relation.clone();
                                Callback::from(move |e: Event| {
                                    let select: HtmlInputElement = e.target_unchecked_into();
                                    let mut new_relation = (*current).clone();
                                    new_relation.existential_direction = match select.value().as_str() {
                                        "forward" => Some(existential::Direction::Forward),
                                        "backward" => Some(existential::Direction::Backward),
                                        "both" => Some(existential::Direction::Both),
                                        _ => None,
                                    };
                                    current.set(new_relation);
                                })
                            }>
                                <option value="">{"Select Existential Direction"}</option>
                                <option 
                                    value="forward" 
                                    selected={matches!(current_relation.existential_type, Some(existential::DependencyType::Implication))}
                                >
                                    {"Forward"}
                                </option>
                                <option value="backward">{"Backward"}</option>
                                <option 
                                    value="both"
                                    selected={matches!(
                                        current_relation.existential_type,
                                        Some(existential::DependencyType::Equivalence) |
                                        Some(existential::DependencyType::NegatedEquivalence) |
                                        Some(existential::DependencyType::Nand) |
                                        Some(existential::DependencyType::Or)
                                    )}
                                >
                                    {"Both"}
                                </option>
                            </select>
                        }
                    } else {
                        html! {}
                    }}
                </div>
                <button type="submit" disabled={current_relation.from.is_empty() || current_relation.to.is_empty()} style="margin-top: 10px;">
                    {"Add Relationship"}
                </button>
            </form>

            // Display existing relations
            <div class="relations-list">
                <table>
                    <thead>
                        <tr>
                            <th>{"From"}</th>
                            <th>{"To"}</th>
                            <th>{"Temporal Type"}</th>
                            <th>{"Temporal Direction"}</th>
                            <th>{"Existential Type"}</th>
                            <th>{"Existential Direction"}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {for (*relations).iter().map(|rel| html! {
                            <tr>
                                <td>{&rel.from}</td>
                                <td>{&rel.to}</td>
                                <td>{format!("{:?}", &rel.temporal_type)}</td>
                                <td>{format!("{:?}", &rel.temporal_direction)}</td>
                                <td>{format!("{:?}", &rel.existential_type)}</td>
                                <td>{format!("{:?}", &rel.existential_direction)}</td>
                            </tr>
                        })}
                    </tbody>
                </table>
            </div>

            // File upload and evaluation section
            <div class="evaluation-section" style="padding-top: 60px;">
                <p>{"Please upload an event log in .xes format."}</p>
                <input
                    type="file"
                    accept=".xes"
                    onchange={onchange}
                    style="display: block; margin: 10px 0;"
                />
                <button onclick={evaluate} disabled={file_content.is_none()}>
                    {"Evaluate Dependencies"}
                </button>

                // Display evaluation results
                {if let Some((correct_temporal, total_temporal, correct_existential, total_existential)) = *evaluation_result {
                    html! {
                        <div class="evaluation-results">
                            <h3>{"Evaluation Results"}</h3>
                            <p>{format!("Temporal Dependencies: {}/{} ({:.0}%)",
                                correct_temporal,
                                total_temporal,
                                (correct_temporal as f64 / total_temporal as f64 * 100.0)
                            )}</p>
                            <p>{format!("Existential Dependencies: {}/{} ({:.0}%)",
                                correct_existential,
                                total_existential,
                                (correct_existential as f64 / total_existential as f64 * 100.0)
                            )}</p>
                        </div>
                    }
                } else {
                    html! {}
                }}
            </div>
        </div>
    }
}

const STRINGS_1: &str = "
a,b:d,f i,b
a,c:e,f i,b
a,d:e,f e
a,e:d,f i,b
b,c:d,f e
b,d:e,f i,f
b,e:-,- ne
c,d:d,f i,f
c,e:-,- ne
d,e:d,b i,b
";
static DEPS_01: Lazy<Vec<Dependency>> = Lazy::new(|| convert_to_dependencies(STRINGS_1));

const STRINGS_2: &str = "
a,b:e,f e
a,c:e,f e
a,d:e,f e
a,e:e,f i,b
a,f:e,f i,b
b,c:-,- e
b,d:e,f e
b,e:e,f i,b
b,f:e,f i,b
c,d:e,f e
c,e:e,f i,b
c,f:e,f i,b
d,e:d,f i,b
d,f:d,f i,b
e,f:-,- ne
";
static DEPS_02: Lazy<Vec<Dependency>> = Lazy::new(|| convert_to_dependencies(STRINGS_2));

const STRINGS_3: &str = "
a,b:d,f -,-
a,c:e,f i,f
a,d:-,- ne
a,e:d,f -,-
a,f:-,- ne
b,c:d,f -,-
b,d:d,b -,-
b,e:-,- ne
b,f:d,f -,-
c,d:e,b -,-
c,e:d,b -,-
c,f:-,- ne
d,e:d,f -,-
d,f:e,f i,b
e,f:d,f -,-
";
static DEPS_03: Lazy<Vec<Dependency>> = Lazy::new(|| convert_to_dependencies(STRINGS_3));

const STRINGS_4: &str = "
a,b:e,f e
a,c:e,f e
a,d:e,f i,f
a,e:-,- ne
b,c:-,- e
b,d:e,f i,f
b,e:-,- ne
c,d:e,f i,f
c,e:-,- ne
d,e:d,b i,b
";
static DEPS_04: Lazy<Vec<Dependency>> = Lazy::new(|| convert_to_dependencies(STRINGS_4));

const STRINGS_5: &str = "
a,b:d,f i,b
a,c:-,- e
a,d:e,f e
a,e:d,f i,b
b,c:-,- i,f
b,d:e,f i,f
b,e:-,- ne
c,d:e,f e
c,e:-,- i,b
d,e:e,b i,b
";
static DEPS_05: Lazy<Vec<Dependency>> = Lazy::new(|| convert_to_dependencies(STRINGS_5));

const STRINGS_6: &str = "
a,b:e,f e
a,c:e,f e
a,d:-,- o
b,c:-,- e
b,d:-,- o
c,d:-,- o
";
static DEPS_06: Lazy<Vec<Dependency>> = Lazy::new(|| convert_to_dependencies(STRINGS_6));

const STRINGS_7: &str = "
a,b:-,- i,b
a,c:-,- i,b
a,d:e,f e
a,e:-,- e
b,c:-,- ne
b,d:e,f i,f
b,e:e,f i,f
c,d:e,f i,f
c,e:e,f i,f
d,e:-,- e
";
static DEPS_07: Lazy<Vec<Dependency>> = Lazy::new(|| convert_to_dependencies(STRINGS_7));

const STRINGS_8: &str = "
a,b:e,f e
a,c:e,f e
a,e:e,f i,b
b,c:-,- e
b,e:e,f i,b
c,e:e,f i,b
";
static DEPS_08: Lazy<Vec<Dependency>> = Lazy::new(|| convert_to_dependencies(STRINGS_8));

const STRINGS_9: &str = "
a,b:-,- ne
a,c:e,f i,f
a,d:e,f i,f
a,e:-,- ne
a,f:-,- ne
b,c:e,f i,f
b,d:e,f i,f
b,e:-,- ne
b,f:-,- ne
c,d:-,- e
c,e:-,- ne
c,f:-,- ne
d,e:-,- ne
d,f:-,- ne
e,f:d,f e
";
static DEPS_09: Lazy<Vec<Dependency>> = Lazy::new(|| convert_to_dependencies(STRINGS_9));

const STRINGS_10: &str = "
a,b:d,f i,b
a,c:d,f i,b
a,d:-,- ne
b,c:-,- ne
b,d:-,- ne
c,d:-,- ne
";
static DEPS_10: Lazy<Vec<Dependency>> = Lazy::new(|| convert_to_dependencies(STRINGS_10));

const STRINGS_11: &str = "
a,b:-,- ne
a,d:-,- n
a,e:d,b i,f
a,f:-,- n
b,d:-,- n
b,e:d,b i,f
b,f:-,- n
d,e:d,b i,f
d,f:-,- ne
e,f:d,b i,b
";
static DEPS_11: Lazy<Vec<Dependency>> = Lazy::new(|| convert_to_dependencies(STRINGS_11));

//fn main() {
//    let logs = [
//        (
//            "L01",
//            &DEPS_01,
//            "./sample-data/synthetic-log/event_log_01.xes",
//        ),
//        (
//            "L02",
//            &DEPS_02,
//            "./sample-data/synthetic-log/event_log_02.xes",
//        ),
//        (
//            "L03",
//            &DEPS_03,
//            "./sample-data/synthetic-log/event_log_03.xes",
//        ),
//        (
//            "L04",
//            &DEPS_04,
//            "./sample-data/synthetic-log/event_log_04.xes",
//        ),
//        (
//            "L05",
//            &DEPS_05,
//            "./sample-data/synthetic-log/event_log_05.xes",
//        ),
//        (
//            "L06",
//            &DEPS_06,
//            "./sample-data/synthetic-log/event_log_06.xes",
//        ),
//        (
//            "L07",
//            &DEPS_07,
//            "./sample-data/synthetic-log/event_log_07.xes",
//        ),
//        (
//            "L08",
//            &DEPS_08,
//            "./sample-data/synthetic-log/event_log_08.xes",
//        ),
//        (
//            "L09",
//            &DEPS_09,
//            "./sample-data/synthetic-log/event_log_09.xes",
//        ),
//        (
//            "L10",
//            &DEPS_10,
//            "./sample-data/synthetic-log/event_log_10.xes",
//        ),
//        (
//            "L11",
//            &DEPS_11,
//            "./sample-data/synthetic-log/event_log_11.xes",
//        ),
//    ];
//
//    println!("Log | Temporal Dependencies | Existential Dependencies");
//    println!("----|-----------------------|-------------------------");
//
//    for (log_name, deps, log_path) in logs {
//        let (correct_temporal, total_temporal, correct_existential, total_existential) =
//            evaluate_deps(deps, log_path);
//
//        let temporal_ratio = format!(
//            "{}/{}: {:>3}%",
//            correct_temporal,
//            total_temporal,
//            (correct_temporal as f32 / total_temporal as f32 * 100.0).round() as u32
//        );
//
//        let existential_ratio = format!(
//            "{}/{}: {:>3}%",
//            correct_existential,
//            total_existential,
//            (correct_existential as f32 / total_existential as f32 * 100.0).round() as u32
//        );
//
//        let temporal_parts: Vec<&str> = temporal_ratio.split(':').collect();
//        let existential_parts: Vec<&str> = existential_ratio.split(':').collect();
//
//        println!(
//            "{:<3} | {:>10} : {:>8} | {:>10} : {:>4}",
//            log_name,
//            temporal_parts[0].trim(),
//            temporal_parts[1].trim(),
//            existential_parts[0].trim(),
//            existential_parts[1].trim()
//        );
//    }
//
//    debug_log(
//        "L06",
//        &DEPS_06,
//        "./sample-data/synthetic-log/event_log_03.xes",
//        true,
//    );
//    debug_log(
//        "L06",
//        &DEPS_06,
//        "./sample-data/synthetic-log/event_log_03.xes",
//        false,
//    );
//
//    debug_log(
//        "L11",
//        &DEPS_11,
//        "./sample-data/synthetic-log/event_log_03.xes",
//        true,
//    );
//    debug_log(
//        "L11",
//        &DEPS_11,
//        "./sample-data/synthetic-log/event_log_03.xes",
//        false,
//    );
//}

pub fn evaluate_deps(deps: &[Dependency], event_log_content: &str) -> (usize, usize, usize, usize) {
    let traces: Vec<Vec<String>> = parse_into_traces(None, Some(event_log_content))
        .expect("Failed to parse event log content");

    let traces_str: Vec<Vec<&str>> = traces
        .iter()
        .map(|trace| trace.iter().map(|s| s.as_str()).collect())
        .collect();

    let activities: HashSet<String> = traces
        .iter()
        .flat_map(|trace| trace.iter().cloned())
        .collect();

    let temporal_threshold = 1.0;
    let existential_threshold = 1.0;

    let estimated_capacity = activities.len() * (activities.len() - 1);
    let mut adj_matrix = Vec::with_capacity(estimated_capacity);

    for from in &activities {
        for to in &activities {
            if to != from {
                let temporal_dependency =
                    check_temporal_dependency(from, to, &traces_str, temporal_threshold);
                let existential_dependency =
                    check_existential_dependency(from, to, &traces_str, existential_threshold);

                adj_matrix.push(Dependency::new(
                    from.clone(),
                    to.clone(),
                    temporal_dependency,
                    existential_dependency,
                ));
            }
        }
    }

    let mut seen = HashSet::with_capacity(adj_matrix.len() / 2);
    let mut adj_matrix_sorted = Vec::with_capacity(adj_matrix.len() / 2);

    adj_matrix.sort_by(|a, b| a.from.cmp(&b.from).then(a.to.cmp(&b.to)));

    for dep in adj_matrix {
        let forward_pair = (dep.from.clone(), dep.to.clone());
        let reverse_pair = (dep.to.clone(), dep.from.clone());

        if !seen.contains(&forward_pair) && !seen.contains(&reverse_pair) {
            seen.insert(forward_pair);
            adj_matrix_sorted.push(dep);
        }
    }

    let mut correct_temporal = 0;
    let mut correct_existential = 0;

    // The key issue is here - we need to better handle existential dependency evaluation
    for dep in deps {
        // Find the corresponding dependency in adj_matrix_sorted
        let matching_dep = adj_matrix_sorted.iter().find(|d| {
            (d.from == dep.from && d.to == dep.to) || (d.from == dep.to && d.to == dep.from)
        });

        if let Some(actual_dep) = matching_dep {
            // Check temporal dependency
            if dep.temporal_dependency == actual_dep.temporal_dependency {
                correct_temporal += 1;
            }

            // Check existential dependency
            match (
                &dep.existential_dependency,
                &actual_dep.existential_dependency,
            ) {
                (Some(expected), Some(actual)) => {
                    if expected.dependency_type == existential::DependencyType::Equivalence
                        || expected.dependency_type
                            == existential::DependencyType::NegatedEquivalence
                    {
                        // For equivalence types, we only need to check the type
                        if expected.dependency_type == actual.dependency_type {
                            correct_existential += 1;
                        }
                    } else if expected.dependency_type == existential::DependencyType::Or {
                        // For OR relationships, direction doesn't matter
                        if expected.dependency_type == actual.dependency_type {
                            correct_existential += 1;
                        }
                    } else if expected == actual {
                        // For other types, check the full dependency
                        correct_existential += 1;
                    }
                }
                (None, None) => {
                    correct_existential += 1;
                }
                _ => {} // No match if one is Some and the other is None
            }
        }
    }

    (
        correct_temporal,
        deps.len(),
        correct_existential,
        deps.len(),
    )
}

// FIXME: i think the logic is wrong here
fn _debug_log(log_name: &str, deps: &[Dependency], event_log_path: &str, check_temporal: bool) {
    let traces: Vec<Vec<String>> = parse_into_traces(Some(event_log_path), None).unwrap();

    let traces_str: Vec<Vec<&str>> = traces
        .iter()
        .map(|trace| trace.iter().map(|s| s.as_str()).collect())
        .collect();

    let activities: HashSet<String> = traces
        .iter()
        .flat_map(|trace| trace.iter().cloned())
        .collect();

    let temporal_threshold = 1.0;
    let existential_threshold = 1.0;

    let estimated_capacity = activities.len() * (activities.len() - 1);
    let mut adj_matrix = Vec::with_capacity(estimated_capacity);

    for from in &activities {
        for to in &activities {
            if to != from {
                let temporal_dependency =
                    check_temporal_dependency(from, to, &traces_str, temporal_threshold);
                let existential_dependency =
                    check_existential_dependency(from, to, &traces_str, existential_threshold);

                adj_matrix.push(Dependency::new(
                    from.clone(),
                    to.clone(),
                    temporal_dependency,
                    existential_dependency,
                ));
            }
        }
    }

    let mut seen = HashSet::with_capacity(adj_matrix.len() / 2);
    let mut adj_matrix_sorted = Vec::with_capacity(adj_matrix.len() / 2);

    adj_matrix.sort_by(|a, b| a.from.cmp(&b.from).then(a.to.cmp(&b.to)));

    for dep in adj_matrix {
        let forward_pair = (dep.from.clone(), dep.to.clone());
        let reverse_pair = (dep.to.clone(), dep.from.clone());

        if !seen.contains(&forward_pair) && !seen.contains(&reverse_pair) {
            seen.insert(forward_pair);
            adj_matrix_sorted.push(dep);
        }
    }

    println!("{:-<10}---{:-<18}---{:-<20}", "", "", "");
    println!("Debugging log: {}", log_name);
    println!(
        "{:<10} | {:<14} | {:<20}",
        "Dependency", "Expected", "Actual"
    );
    println!("{:-<10}---{:-<18}---{:-<20}", "", "", "");

    for (i, dep) in deps.iter().enumerate() {
        let expected = if check_temporal {
            match &dep.temporal_dependency {
                Some(value) => format!("{}", value),
                None => "None".to_string(),
            }
        } else {
            match &dep.existential_dependency {
                Some(value) => format!("{}", value),
                None => "None".to_string(),
            }
        };

        let actual = if check_temporal {
            match &adj_matrix_sorted[i].temporal_dependency {
                Some(value) => format!("{}", value),
                None => "None".to_string(),
            }
        } else {
            match &adj_matrix_sorted[i].existential_dependency {
                Some(value) => format!("{}", value),
                None => "None".to_string(),
            }
        };

        if expected != actual {
            println!(
                "{:<10} | {:<14} | {:<20}",
                format!("{} -> {}", dep.from, dep.to),
                expected,
                actual
            );
        }
    }
}

#[cfg(test)]
mod tests {

    use rstest::rstest;

    use crate::dependency_types::{
        dependency::Dependency,
        existential::{self, check_existential_dependency},
        temporal::check_temporal_dependency,
    };
    use crate::parser::parse_into_traces;
    use std::collections::HashSet;

    use super::*;

    fn test_dependencies(deps: &[Dependency], event_log_path: &str) {
        let traces: Vec<Vec<String>> = parse_into_traces(Some(event_log_path), None).unwrap();

        let traces_str: Vec<Vec<&str>> = traces
            .iter()
            .map(|trace| trace.iter().map(|s| s.as_str()).collect())
            .collect();

        let activities: HashSet<String> = traces
            .iter()
            .flat_map(|trace| trace.iter().cloned())
            .collect();

        let temporal_threshold = 1.0;
        let existential_threshold = 1.0;

        let estimated_capacity = activities.len() * (activities.len() - 1);
        let mut adj_matrix = Vec::with_capacity(estimated_capacity);

        for from in &activities {
            for to in &activities {
                if to != from {
                    let temporal_dependency =
                        check_temporal_dependency(from, to, &traces_str, temporal_threshold);
                    let existential_dependency =
                        check_existential_dependency(from, to, &traces_str, existential_threshold);

                    adj_matrix.push(Dependency::new(
                        from.clone(),
                        to.clone(),
                        temporal_dependency,
                        existential_dependency,
                    ));
                }
            }
        }

        let mut seen = HashSet::with_capacity(adj_matrix.len() / 2);
        let mut adj_matrix_sorted = Vec::with_capacity(adj_matrix.len() / 2);

        adj_matrix.sort_by(|a, b| a.from.cmp(&b.from).then(a.to.cmp(&b.to)));

        for dep in adj_matrix {
            let forward_pair = (dep.from.clone(), dep.to.clone());
            let reverse_pair = (dep.to.clone(), dep.from.clone());

            if !seen.contains(&forward_pair) && !seen.contains(&reverse_pair) {
                seen.insert(forward_pair);
                adj_matrix_sorted.push(dep);
            }
        }

        assert_eq!(deps.len(), adj_matrix_sorted.len());

        for (i, dep) in deps.iter().enumerate() {
            assert_eq!(dep.from, adj_matrix_sorted[i].from);
            assert_eq!(dep.to, adj_matrix_sorted[i].to);
            assert_eq!(
                dep.temporal_dependency,
                adj_matrix_sorted[i].temporal_dependency
            );

            if let Some(existential_dependency) = dep.existential_dependency.as_ref() {
                if existential_dependency.dependency_type
                    == existential::DependencyType::Equivalence
                    || existential_dependency.dependency_type
                        == existential::DependencyType::NegatedEquivalence
                {
                    assert_eq!(
                        dep.existential_dependency.as_ref().unwrap().dependency_type,
                        adj_matrix_sorted[i]
                            .existential_dependency
                            .as_ref()
                            .unwrap()
                            .dependency_type
                    );
                } else {
                    assert_eq!(
                        dep.existential_dependency,
                        adj_matrix_sorted[i].existential_dependency
                    );
                }
            }
        }
    }

    #[rstest]
    #[case(&DEPS_01, "./sample-data/synthetic-log/event_log_01.xes")]
    #[case(&DEPS_02, "./sample-data/synthetic-log/event_log_02.xes")]
    #[case(&DEPS_03, "./sample-data/synthetic-log/event_log_03.xes")]
    #[case(&DEPS_04, "./sample-data/synthetic-log/event_log_04.xes")]
    #[case(&DEPS_05, "./sample-data/synthetic-log/event_log_05.xes")]
    #[case(&DEPS_06, "./sample-data/synthetic-log/event_log_06.xes")]
    #[case(&DEPS_07, "./sample-data/synthetic-log/event_log_07.xes")]
    #[case(&DEPS_08, "./sample-data/synthetic-log/event_log_08.xes")]
    #[case(&DEPS_09, "./sample-data/synthetic-log/event_log_09.xes")]
    #[case(&DEPS_10, "./sample-data/synthetic-log/event_log_10.xes")]
    #[case(&DEPS_11, "./sample-data/synthetic-log/event_log_11.xes")]
    fn test_dependencies_general(#[case] deps: &[Dependency], #[case] event_log_path: &str) {
        test_dependencies(deps, event_log_path);
    }
}
