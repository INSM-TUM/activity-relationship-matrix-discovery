use std::collections::HashSet;

use crate::event::Event;

#[derive(Debug)]
pub struct State {
    pub partition: Option<usize>,
    pub sequences: HashSet<Event>,
}
