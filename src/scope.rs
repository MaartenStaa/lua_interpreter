use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
pub struct DraftScope {
    variables: HashSet<String>,
    labels: HashSet<String>,
}

impl Default for DraftScope {
    fn default() -> Self {
        Self::new()
    }
}

impl DraftScope {
    pub fn new() -> Self {
        Self {
            variables: HashSet::new(),
            labels: HashSet::new(),
        }
    }

    pub fn register_variable(&mut self, name: &str) {
        self.variables.insert(name.to_string());
    }

    pub fn has_variable(&self, name: &str) -> bool {
        self.variables.contains(name)
    }

    pub fn has_label(&self, label: &str) -> bool {
        self.labels.contains(label)
    }

    pub fn register_label(&mut self, label: &str) {
        self.labels.insert(label.to_string());
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NameLocation {
    /// Which scope the name is defined in/can be found in. 0 means the current
    /// scope, 1 means the parent scope, and so on.
    pub scope_offset: usize,
}
