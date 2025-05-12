use matrix_discovery::{evaluation::Evaluation, routes::Route};
use matrix_discovery::{
    generate_adj_matrix_from_traces, generate_xes,
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
}

#[derive(Clone, PartialEq)]
struct AppState {
    text: String,
    processed: bool,
    existential_threshold: f64,
    temporal_threshold: f64,
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
            Msg::XESLoaded(result) => {
                match result {
                    Ok(content) => match self.process_xes_content(&content) {
                        Ok(processed_text) => {
                            self.state.text = processed_text;
                        }
                        Err(e) => {
                            self.state.text = format!("Processing error: {}", e);
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

        let onprocess = ctx.link().callback(|_| Msg::ConvertToXES);
        let ondownload = ctx.link().callback(|_| Msg::DownloadXES);

        html! {
            <div style="height: 90vh; display: flex; flex-direction: column;">
                <textarea
                    value={self.state.text.clone()}
                    oninput={oninput}
                    placeholder="Enter your text here"
                    style="flex-grow: 1; width: 99%; background-color: #393939; color: white; padding: 10px; font-size: 16px; resize: none;"
                />
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
                    <div style="display: flex; margin-left: auto;">
                        <input type="file" id="xes-file" accept=".xes" onchange={onxesimport} style="display: none;" />
                        <label for="xes-file" style="padding: 10px 20px; font-size: 16px; margin-right: 10px; background-color: #4CAF50; color: white; cursor: pointer; border-radius: 5px;">
                            {"Import XES"}
                        </label>
                        <button onclick={onprocess} disabled={self.state.processed} style="padding: 10px 20px; font-size: 16px; margin-right: 10px;">
                            {"Convert To XES"}
                        </button>
                        <button onclick={ondownload} disabled={!self.state.processed} style="padding: 10px 20px; font-size: 16px;">
                            {"Download XES"}
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

    fn process_xes_content(&self, content: &str) -> AppResult<String> {
        let traces = parse_into_traces(None, Some(content))
            .map_err(|e| AppError::ParseError(format!("{:?}", e)))?;

        let (
            adj_matrix,
            full_independences,
            pure_existences,
            _eventual_equivalences,
            _direct_equivalences,
            number_of_activities,
            _relationship_counts,
        ) = generate_adj_matrix_from_traces(
            traces.clone(),
            self.state.existential_threshold,
            self.state.temporal_threshold,
        );
        let relations = number_of_activities * number_of_activities;
        let _independences_per_relations = full_independences as f64 / relations as f64;
        let _temporal_independences_per_relations = pure_existences as f64 / relations as f64;
        let traces_as_str: Vec<Vec<&str>> = traces
            .iter()
            .map(|trace| trace.iter().map(|s| s.as_str()).collect())
            .collect();
        let variants = variants_of_traces(traces_as_str);
        let max_variant_frequency = *variants.values().max().unwrap() as f64 / traces.len() as f64;
        let _variants_per_traces = variants.len() as f64 / traces.len() as f64;
        let _freq_over_variants = max_variant_frequency / variants.len() as f64;

        let plain_log: Vec<Vec<matrix_discovery::event::Event>> = traces
            .clone()
            .into_iter()
            .enumerate()
            .map(|(case_idx, trace)| {
                trace
                    .into_iter()
                    .enumerate()
                    .map(|(event_idx, activity)| matrix_discovery::event::Event {
                        case: format!("case_{}", case_idx),
                        activity: activity.chars().next().unwrap(),
                        predecessor: if event_idx > 0 {
                            Some(format!("case_{}", case_idx))
                        } else {
                            None
                        },
                    })
                    .collect()
            })
            .collect();

        let epa = matrix_discovery::epa::ExtendedPrefixAutomaton::build(plain_log);
        let _variant_entropy = epa.variant_entropy();
        let _normalized_variant_entropy = epa.normalized_variant_entropy();

        Ok(format!(
            "{}\n\n\
            #relations: {}",
            //#independence / #relations:        {:<10.4}\n\
            //#temporal independence / #relations: {:<10.4}\n\
            //max. frequency of variants / total #traces: {:<10.4}\n\
            //#variants / total #traces:          {:<10.4}\n\
            //#(Eventual, <=>):                    {:<10}\n\
            //#(Direct, <=>):                      {:<10}\n\
            //#variants:                          {:<10}\n\
            //max. frequency of variants / #variants:     {:<10.4}\n\
            //Variant Entropy:                     {:<10.4}\n\
            //Normalized Variant Entropy:          {:<10.4}\n\n\
            //Relationship Type Frequencies:\n{}",
            adj_matrix,
            relations,
            //independences_per_relations,
            //temporal_independences_per_relations,
            //max_variant_frequency,
            //variants_per_traces,
            //eventual_equivalences,
            //direct_equivalences,
            //variants.len() as f64,
            //freq_over_variants,
            //variant_entropy,
            //normalized_variant_entropy,
            //relationship_counts
            //    .iter()
            //    .map(|(k, v)| format!("{}: {}", k, v))
            //    .collect::<Vec<String>>()
            //    .join("\n")
        ))
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
}

fn main() {
    yew::start_app::<Main>();
}
