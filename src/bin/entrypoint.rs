use matrix_discovery::{evaluation::Evaluation, routes::Route};
use matrix_discovery::{
    analyze_activity_patterns,
    generate_adj_matrix_from_traces, generate_xes, generate_adj_matrix_table_data, MatrixTableData,
    parser::{parse_into_traces, variants_of_traces},
};
use wasm_bindgen::{closure::Closure, JsCast, JsValue, UnwrapThrowExt};
use web_sys::{File, FileReader, HtmlAnchorElement, HtmlInputElement, HtmlTextAreaElement};
use yew::prelude::*;
use yew_router::prelude::*;

struct Main;

impl Component for Main {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self
    }

    fn view(&self, _ctx: &Context<Self>) -> Html {
        html! {
            <BrowserRouter>
                <Switch<Route> render={Switch::render(switch)} />
            </BrowserRouter>
        }
    }
}

fn switch(routes: &Route) -> Html {
    match routes {
        Route::Home => html! {
            <App />
        },
        Route::MatrixDiscovery => html! {
            <App />
        },
        Route::Evaluation => html! {
            <Evaluation />
        },
    }
}

#[derive(Debug, thiserror::Error, PartialEq)]
enum AppError {
    #[error("Error downloading file: {0}")]
    WebSys(String),
    #[error("Failed to read file: {0}")]
    FileReaderError(String),
    #[error("Failed to convert file content to string")]
    FileContentToStringError,
    #[error("Error parsingfile: {0}")]
    ParseError(String),
}

impl From<JsValue> for AppError {
    fn from(value: JsValue) -> Self {
        AppError::WebSys(format!("{:?}", value))
    }
}

type AppResult<T> = Result<T, AppError>;

enum Msg {
    TextInput(String),
    XESImport(Option<File>),
    ExistentialThresholdInput(String),
    TemporalThresholdInput(String),
    XESLoaded(AppResult<String>),
    ConvertToXES,
    DownloadXES,
    DownloadYAML,
    ConditionalTargetActivityInput(String),
    ConditionalPrecedingActivityInput(String),
    PrintVariants,
}

#[derive(Clone, PartialEq)]
struct AppState {
    text: String,
    processed: bool,
    existential_threshold: f64,
    temporal_threshold: f64,
    conditional_target_activity: String,
    conditional_preceding_activity: String,
    last_xes_content: Option<String>,
    last_matrix_content: Option<String>,
    matrix_table_data: Option<MatrixTableData>,
}

struct App {
    state: AppState,
    file_reader_closure: Option<Closure<dyn FnMut(web_sys::ProgressEvent)>>,
}

impl Component for App {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self {
            state: AppState {
                text: String::new(),
                processed: false,
                existential_threshold: 1.0,
                temporal_threshold: 1.0,
                conditional_preceding_activity: "B".to_string(),
                conditional_target_activity: "A".to_string(),
                last_xes_content: None,
                last_matrix_content: None,
                matrix_table_data: None,
            },
            file_reader_closure: None,
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::TextInput(text) => {
                self.state.text = text;
                self.state.processed = false;
                true
            }
            Msg::XESImport(file_option) => {
                if let Some(file) = file_option {
                    self.load_xes_file(ctx, file);
                }
                false
            }
            Msg::ExistentialThresholdInput(value) => {
                if let Ok(threshold) = value.parse::<f64>() {
                    self.state.existential_threshold = threshold;
                }
                false
            }
            Msg::TemporalThresholdInput(value) => {
                if let Ok(threshold) = value.parse::<f64>() {
                    self.state.temporal_threshold = threshold;
                }
                false
            }
            Msg::ConditionalTargetActivityInput(activity) => {
                self.state.conditional_target_activity = activity;
                false
            }
            Msg::ConditionalPrecedingActivityInput(activity) => {
                self.state.conditional_preceding_activity = activity;
                false
            }
            Msg::PrintVariants => {
                match self.print_variants_from_current_content() {
                    Ok(variants_text) => {
                        self.state.text = variants_text;
                        self.state.processed = false;
                    }
                    Err(e) => {
                        self.state.text = format!("Error extracting variants: {}", e);
                    }
                }
                true
            }
            Msg::XESLoaded(result) => {
                match result {
                    Ok(content) => {
                        // Store the original XES content for debugging
                        self.state.last_xes_content = Some(content.clone());
                        
                        match self.process_xes_content(&content) {
                            Ok(processed_text) => {
                                self.state.text = processed_text;
                            }
                            Err(e) => {
                                self.state.text = format!("Processing error: {}", e);
                            }
                        }
                    },
                    Err(e) => {
                        self.state.text = format!("Error loading XES file: {}", e);
                    }
                }
                true
            }
            Msg::ConvertToXES => {
                match self.generate_xes_output() {
                    Ok(xes_text) => {
                        self.state.text = xes_text;
                        self.state.processed = true;
                    }
                    Err(e) => {
                        self.state.text = format!("Conversion to XES failed: {}", e);
                    }
                }
                true
            }
            Msg::DownloadXES => {
                if self.state.processed {
                    if let Err(e) = self.download_xes() {
                        self.state.text = format!("Download error: {}", e);
                    }
                }
                false
            }
            Msg::DownloadYAML => {
                if let Some(yaml_content) = &self.state.last_matrix_content {
                    if let Err(e) = self.download_yaml(yaml_content) {
                        self.state.text = format!("Download error: {}", e);
                    }
                }
                false
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let oninput = ctx.link().callback(|e: InputEvent| {
            let input: HtmlTextAreaElement = e.target_unchecked_into();
            Msg::TextInput(input.value())
        });

        let onxesimport = ctx.link().callback(|e: Event| {
            let input: HtmlInputElement = e.target_unchecked_into();
            Msg::XESImport(input.files().and_then(|files| files.get(0)))
        });

        let onexistential_threshold_input = ctx.link().callback(|e: InputEvent| {
            let input: HtmlInputElement = e.target_unchecked_into();
            Msg::ExistentialThresholdInput(input.value())
        });

        let ontemporal_threshold_input = ctx.link().callback(|e: InputEvent| {
            let input: HtmlInputElement = e.target_unchecked_into();
            Msg::TemporalThresholdInput(input.value())
        });

        let onconditional_target_activity_input = ctx.link().callback(|e: InputEvent| {
            let input: HtmlInputElement = e.target_unchecked_into();
            Msg::ConditionalTargetActivityInput(input.value())
        });

        let onconditional_preceding_activity_input = ctx.link().callback(|e: InputEvent| {
            let input: HtmlInputElement = e.target_unchecked_into();
            Msg::ConditionalPrecedingActivityInput(input.value())
        });

        let onprocess = ctx.link().callback(|_| Msg::ConvertToXES);
        let ondownload = ctx.link().callback(|_| Msg::DownloadXES);
        let ondownload_yaml = ctx.link().callback(|_| Msg::DownloadYAML);
        let onprint_variants = ctx.link().callback(|_| Msg::PrintVariants);

        let content_area = if let Some(matrix_data) = &self.state.matrix_table_data {
            // Show matrix as HTML bc I can do that :sunglasses:
            html! {
                <div style="flex-grow: 1; width: 99%; background-color: #393939; color: white; padding: 10px; font-size: 14px; overflow: auto;">
                    <h3 style="color: white; margin-bottom: 15px;">{"Activity Relationship Matrix"}</h3>
                    <table style="border-collapse: collapse; width: 100%; background-color: #2d2d2d; margin-bottom: 20px;">
                        <thead>
                            <tr style="background-color: #4a4a4a;">
                                <th style="border: 1px solid #666; padding: 8px; text-align: center; font-weight: bold; min-width: 120px;"></th>
                                {
                                    matrix_data.activities.iter().map(|activity| {
                                        let freq = matrix_data.activity_frequencies.get(activity).unwrap_or(&0.0);
                                        html! {
                                            <th style="border: 1px solid #666; padding: 8px; text-align: center; font-weight: bold; min-width: 100px;">
                                                {format!("{} ({:.2})", activity, freq)}
                                            </th>
                                        }
                                    }).collect::<Html>()
                                }
                            </tr>
                        </thead>
                        <tbody>
                            {
                                matrix_data.activities.iter().enumerate().map(|(i, from_activity)| {
                                    let from_freq = matrix_data.activity_frequencies.get(from_activity).unwrap_or(&0.0);
                                    html! {
                                        <tr style="background-color: #393939;">
                                            <td style="border: 1px solid #666; padding: 8px; text-align: left; font-weight: bold; background-color: #4a4a4a;">
                                                {format!("{} ({:.2})", from_activity, from_freq)}
                                            </td>
                                            {
                                                matrix_data.dependencies[i].iter().map(|dep| {
                                                    html! {
                                                        <td style="border: 1px solid #666; padding: 8px; text-align: center; font-family: monospace;">
                                                            {dep.clone()}
                                                        </td>
                                                    }
                                                }).collect::<Html>()
                                            }
                                        </tr>
                                    }
                                }).collect::<Html>()
                            }
                        </tbody>
                    </table>
                    <div style="margin-bottom: 15px; padding: 10px; background-color: #2d2d2d; border-radius: 5px;">
                        <h4 style="color: white; margin-bottom: 10px;">{"Legend:"}</h4>
                        <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 10px; font-size: 12px;">
                            <div>{"≺d = Direct forward temporal"}</div>
                            <div>{"≺ = Eventual forward temporal"}</div>
                            <div>{"≺t = True eventual forward temporal"}</div>
                            <div>{"≻d = Direct backward temporal"}</div>
                            <div>{"≻ = Eventual backward temporal"}</div>
                            <div>{"≻t = True eventual backward temporal"}</div>
                            <div>{"⇒ = Forward implication"}</div>
                            <div>{"⇐ = Backward implication"}</div>
                            <div>{"⇔ = Equivalence"}</div>
                            <div>{"⇎ = Negated equivalence"}</div>
                            <div>{"⊼ = NAND"}</div>
                            <div>{"∨ = OR"}</div>
                            <div>{"None = No dependency"}</div>
                        </div>
                    </div>
                    
                    // Display the rest of the analysis text below the table
                    <div style="background-color: #2d2d2d; padding: 15px; border-radius: 5px; white-space: pre-wrap; font-family: monospace; font-size: 13px; line-height: 1.4;">
                        {self.state.text.clone()}
                    </div>
                </div>
            }
        } else {
            html! {
                <textarea
                    value={self.state.text.clone()}
                    oninput={oninput}
                    placeholder="Enter your text here"
                    style="flex-grow: 1; width: 99%; background-color: #393939; color: white; padding: 10px; font-size: 16px; resize: none;"
                />
            }
        };

        html! {
            <div style="height: 90vh; display: flex; flex-direction: column;">
                {content_area}
                <div style="display: flex; flex-wrap: wrap; padding: 10px; align-items: center;">
                    <div style="display: flex; align-items: center; margin-right: 20px;">
                        <label for="temporal-threshold" style="margin-right: 10px; font-size: 14px;">
                            {"Temporal Threshold:"}
                        </label>
                        <input
                            id="temporal-threshold"
                            type="number"
                            min="0.1"
                            max="1.0"
                            step="0.1"
                            value={self.state.temporal_threshold.to_string()}
                            oninput={ontemporal_threshold_input}
                            style="width: 70px; padding: 5px; font-size: 14px; border-radius: 4px; border: 1px solid #ccc;"
                        />
                    </div>
                    <div style="display: flex; align-items: center; margin-right: auto;">
                        <label for="existential-threshold" style="margin-right: 10px; font-size: 14px;">
                            {"Existential Threshold:"}
                        </label>
                        <input
                            id="existential-threshold"
                            type="number"
                            min="0.1"
                            max="1.0"
                            step="0.1"
                            value={self.state.existential_threshold.to_string()}
                            oninput={onexistential_threshold_input}
                            style="width: 70px; padding: 5px; font-size: 14px; border-radius: 4px; border: 1px solid #ccc;"
                        />
                    </div>
                    <div style="display: flex; align-items: center; margin-right: 20px; margin-top: 5px;">
                        <label for="conditional-preceding-activity" style="margin-right: 10px; font-size: 14px;">
                            {"Preceding Activity (Conditional):"}
                        </label>
                        <input
                            id="conditional-preceding-activity"
                            type="text"
                            value={self.state.conditional_preceding_activity.clone()}
                            oninput={onconditional_preceding_activity_input}
                            style="width: 70px; padding: 5px; font-size: 14px; border-radius: 4px; border: 1px solid #ccc;"
                        />
                    </div>
                    <div style="display: flex; align-items: center; margin-right: auto; margin-top: 5px;">
                        <label for="conditional-target-activity" style="margin-right: 10px; font-size: 14px;">
                            {"Target Activity (Conditional):"}
                        </label>
                        <input
                            id="conditional-target-activity"
                            type="text"
                            value={self.state.conditional_target_activity.clone()}
                            oninput={onconditional_target_activity_input}
                            style="width: 70px; padding: 5px; font-size: 14px; border-radius: 4px; border: 1px solid #ccc;"
                        />
                    </div>
                    <div style="display: flex; margin-left: auto;">
                        <input type="file" id="xes-file" accept=".xes" onchange={onxesimport} style="display: none;" />
                        <label for="xes-file" style="padding: 10px 20px; font-size: 16px; margin-right: 10px; background-color: #4CAF50; color: white; cursor: pointer; border-radius: 5px;">
                            {"Import XES"}
                        </label>
                        <button onclick={onprint_variants} style="padding: 10px 20px; font-size: 16px; margin-right: 10px; background-color: #FF9800; color: white; border: none; border-radius: 5px; cursor: pointer;">
                            {"Print Variants"}
                        </button>
                        <button onclick={onprocess} disabled={self.state.processed} style="padding: 10px 20px; font-size: 16px; margin-right: 10px;">
                            {"Convert To XES"}
                        </button>
                        <button onclick={ondownload} disabled={!self.state.processed} style="padding: 10px 20px; font-size: 16px; margin-right: 10px;">
                            {"Download XES"}
                        </button>
                        <button onclick={ondownload_yaml} disabled={self.state.last_matrix_content.is_none()} style="padding: 10px 20px; font-size: 16px;">
                            {"Download YAML"}
                        </button>
                    </div>
                </div>
                <div style="color: white; font-size: 16px; margin-top: 10px; margin-right: 10px; text-align: right;">
                    <Link<Route> to={Route::Evaluation}>{ "Evaluation" }</Link<Route>>
                </div>
            </div>
        }
    }
}

impl App {
    fn print_variants_from_current_content(&self) -> AppResult<String> {
        let xes_content = if let Some(ref stored_content) = self.state.last_xes_content {
            stored_content
        } else if self.state.text.trim().starts_with("<?xml") || self.state.text.trim().starts_with("<log") {
            &self.state.text
        } else {
            return Err(AppError::ParseError("No XES content available. Please import an XES file first.".to_string()));
        };

        let traces = parse_into_traces(None, Some(xes_content))
            .map_err(|e| AppError::ParseError(format!("Failed to parse XES content: {:?}", e)))?;
        
        if traces.is_empty() {
            return Ok("No traces found in the XES content.".to_string());
        }
        
        let trace_refs: Vec<Vec<&str>> = traces
            .iter()
            .map(|trace| trace.iter().map(|s| s.as_str()).collect())
            .collect();
        
        let variants = variants_of_traces(trace_refs);
        
        if variants.is_empty() {
            return Ok("No variants found.".to_string());
        }
        
        let mut output = String::new();
        output.push_str(&format!("Total unique variants found: {}\n", variants.len()));
        output.push_str(&format!("Total traces: {}\n\n", traces.len()));
        
        let mut sorted_variants: Vec<_> = variants.iter().collect();
        sorted_variants.sort_by(|a, b| b.1.cmp(a.1));
        
        for (i, (variant, frequency)) in sorted_variants.iter().enumerate() {
            let freq_val = **frequency;
            let frequency_percentage = (freq_val as f64 / traces.len() as f64) * 100.0;
            
            if variant.is_empty() {
                output.push_str(&format!(
                    "Variant {}: (empty) - appears {} times ({:.1}%)\n", 
                    i + 1, freq_val, frequency_percentage
                ));
            } else {
                output.push_str(&format!(
                    "Variant {}: [{}] - appears {} times ({:.1}%)\n", 
                    i + 1, variant.join(" -> "), freq_val, frequency_percentage
                ));
            }
        }
        
        Ok(output)
    }

    fn load_xes_file(&mut self, ctx: &Context<Self>, file: File) {
        let link = ctx.link().clone();
        let reader = FileReader::new().unwrap_throw();
        let reader_clone = reader.clone();

        let onload = Closure::once(move |_event: web_sys::ProgressEvent| {
            let result = reader_clone
                .result()
                .map_err(|e| AppError::FileReaderError(format!("{:?}", e)))
                .and_then(|result| result.as_string().ok_or(AppError::FileContentToStringError));
            link.send_message(Msg::XESLoaded(result));
        });

        reader.set_onload(Some(onload.as_ref().unchecked_ref()));
        self.file_reader_closure = Some(onload);

        if let Err(e) = reader.read_as_text(&file) {
            let error_link = ctx.link().clone(); // Clone link here for the error case
            error_link.send_message(Msg::XESLoaded(Err(AppError::FileReaderError(format!(
                "{:?}",
                e
            )))));
        }
    }

    fn process_xes_content(&mut self, content: &str) -> AppResult<String> {
        let traces = parse_into_traces(None, Some(content))
            .map_err(|e| AppError::ParseError(format!("{:?}", e)))?;

        // Generate table data for matrix visualization
        let table_data = generate_adj_matrix_table_data(
            traces.clone(),
            self.state.existential_threshold,
            self.state.temporal_threshold,
        );
        self.state.matrix_table_data = Some(table_data);

        let (
            adj_matrix,
            _full_independences,
            _pure_existences,
            _eventual_equivalences,
            _direct_equivalences,
            number_of_activities,
            _relationship_counts,
            yaml_string,
        ) = generate_adj_matrix_from_traces(
            traces.clone(),
            self.state.existential_threshold,
            self.state.temporal_threshold,
        );
        self.state.last_matrix_content = Some(yaml_string);
        let relations = number_of_activities * number_of_activities;

        let analysis_results = analyze_activity_patterns(
            &traces,
            &self.state.conditional_target_activity,
            &self.state.conditional_preceding_activity,
        );

        let mut output_parts: Vec<String> = vec![
            format!("#relations: {}", relations),
        ];

        output_parts.push("\n--- Activity Frequency Analysis ---".to_string());
        
        output_parts.push("\nActivity Frequencies per Trace:".to_string());
        if analysis_results.activity_frequencies_per_trace_display.is_empty() {
            output_parts.push(" (No traces or traces are empty)".to_string());
        } else {
            output_parts.extend(analysis_results.activity_frequencies_per_trace_display);
        }

        output_parts.push("\nOverall Activity Statistics:".to_string());
        if analysis_results.overall_activity_stats_display.is_empty() {
            output_parts.push(" (No activities found)".to_string());
        } else {
            output_parts.extend(analysis_results.overall_activity_stats_display);
        }

        output_parts.push("\nAverage Activity Frequency Per Trace:".to_string());
        if analysis_results.average_activity_frequency_per_trace_display.is_empty() {
            output_parts.push(" (No activities found)".to_string());
        } else {
            output_parts.extend(analysis_results.average_activity_frequency_per_trace_display);
        }

        output_parts.push("\nDirect Self-Loops (e.g., A->A):".to_string());
        if analysis_results.direct_self_loop_counts_display.is_empty() {
            output_parts.push(" (No direct self-loops found)".to_string());
        } else {
            output_parts.extend(analysis_results.direct_self_loop_counts_display);
        }
        
        output_parts.push("\nBasic Conditional Frequencies:".to_string());
        if analysis_results.conditional_freq_display.is_empty() {
            output_parts.push(" (No conditional frequency data or not applicable)".to_string());
        } else {
            output_parts.extend(analysis_results.conditional_freq_display);
        }

        let _original_commented_metrics = "\n\n\
            /* #independence / #relations:        {:<10.4}\n\
            #temporal independence / #relations: {:<10.4}\n\
            max. frequency of variants / total #traces: {:<10.4}\n\
            #variants / total #traces:          {:<10.4}\n\
            #(Eventual, <=>):                    {:<10}\n\
            #(Direct, <=>):                      {:<10}\n\
            #variants:                          {:<10}\n\
            max. frequency of variants / #variants:     {:<10.4}\n\
            Variant Entropy:                     {:<10.4}\n\
            Normalized Variant Entropy:          {:<10.4}\n\n\
            Relationship Type Frequencies: {} */";
            
        // output_parts.push(_original_commented_metrics.to_string());

        Ok(output_parts.join("\n"))
    }

    fn generate_xes_output(&self) -> AppResult<String> {
        Ok(generate_xes(&self.state.text))
    }

    fn download_xes(&self) -> AppResult<()> {
        let window = web_sys::window().ok_or(AppError::WebSys("No window object".to_string()))?;
        let document = window
            .document()
            .ok_or(AppError::WebSys("No document object".to_string()))?;

        let blob = web_sys::Blob::new_with_str_sequence_and_options(
            &js_sys::Array::of1(&JsValue::from_str(&self.state.text)),
            web_sys::BlobPropertyBag::new().type_("text/plain"),
        )?;

        let url = web_sys::Url::create_object_url_with_blob(&blob)?;

        let anchor: HtmlAnchorElement = document
            .create_element("a")?
            .dyn_into()
            .map_err(|e| AppError::WebSys(format!("{:?}", e)))?;

        anchor.set_href(&url);
        anchor.set_download("event_log.xes");
        anchor.click();

        web_sys::Url::revoke_object_url(&url)?;
        Ok(())
    }

    fn download_yaml(&self, yaml_content: &str) -> AppResult<()> {
        let window = web_sys::window().ok_or(AppError::WebSys("No window object".to_string()))?;
        let document = window
            .document()
            .ok_or(AppError::WebSys("No document object".to_string()))?;

        let blob = web_sys::Blob::new_with_str_sequence_and_options(
            &js_sys::Array::of1(&JsValue::from_str(yaml_content)),
            web_sys::BlobPropertyBag::new().type_("text/yaml"),
        )?;

        let url = web_sys::Url::create_object_url_with_blob(&blob)?;

        let anchor: HtmlAnchorElement = document
            .create_element("a")?
            .dyn_into()
            .map_err(|e| AppError::WebSys(format!("{:?}", e)))?;

        anchor.set_href(&url);
        anchor.set_download("matrix.yaml");
        anchor.click();

        web_sys::Url::revoke_object_url(&url)?;
        Ok(())
    }
}

fn main() {
    yew::start_app::<Main>();
}
