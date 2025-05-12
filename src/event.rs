#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Event {
    pub case: String,
    pub activity: char,
    pub predecessor: Option<String>,
}
