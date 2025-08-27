pub use regex as normal;
pub use fancy_regex as fancy;

#[derive(Clone)]
pub enum RegexEngine {
    Normal(normal::Regex),
    Fancy(fancy::Regex),
}

#[derive(Clone)]
pub struct Match {
    pub text: String,
    pub start: usize,
    pub end: usize,
}

impl Match {
    pub fn as_str(&self) -> &str {
        &self.text
    }
}

#[derive(Clone)]
pub struct Captures {
    pub groups: Vec<Option<String>>,
    pub names: Vec<Option<String>>,
}

impl Captures {
    pub fn get(&self, i: usize) -> Option<&str> {
        self.groups.get(i).and_then(|opt| opt.as_deref())
    }

    pub fn name(&self, name: &str) -> Option<&str> {
        if let Some(idx) = self.names.iter().position(|n| n.as_deref() == Some(name)) {
            self.get(idx)
        } else {
            None
        }
    }
}

impl RegexEngine {
    pub fn find(&self, text: &str) -> Option<Match> {
        match self {
            RegexEngine::Normal(re) => re.find(text).map(|m| Match {
                text: m.as_str().to_string(),
                start: m.start(),
                end: m.end(),
            }),
            RegexEngine::Fancy(re) => re.find(text).ok().flatten().map(|m| Match {
                text: m.as_str().to_string(),
                start: m.start(),
                end: m.end(),
            }),
        }
    }

    pub fn replace_all(&self, text: &str, replacement: &str) -> String {
        match self {
            RegexEngine::Normal(re) => re.replace_all(text, replacement).into_owned(),
            RegexEngine::Fancy(re) => re.replace_all(text, replacement).into_owned(),
        }
    }

    pub fn is_match(&self, text: &str) -> bool {
        match self {
            RegexEngine::Normal(re) => re.is_match(text),
            RegexEngine::Fancy(re) => re.is_match(text).unwrap_or(false),
        }
    }

    pub fn find_iter(&self, text: &str) -> Vec<Match> {
        match self {
            RegexEngine::Normal(re) => re.find_iter(text)
                .map(|m| Match { text: m.as_str().to_string(), start: m.start(), end: m.end() })
                .collect(),
            RegexEngine::Fancy(re) => re.find_iter(text)
                .filter_map(|res| res.ok())
                .map(|m| Match { text: m.as_str().to_string(), start: m.start(), end: m.end() })
                .collect(),
        }
    }

    pub fn split(&self, text: &str) -> Vec<String> {
        match self {
            RegexEngine::Normal(re) => re.split(text).map(|s| s.to_string()).collect(),
            RegexEngine::Fancy(re) => re.split(text).filter_map(|res| res.ok()).map(|s| s.to_string()).collect(),
        }
    }

    pub fn captures(&self, text: &str) -> Option<Captures> {
        match self {
            RegexEngine::Normal(re) => re.captures(text).map(|caps| {
                let groups = (0..caps.len())
                    .map(|i| caps.get(i).map(|m| m.as_str().to_string()))
                    .collect();
                let names = re.capture_names().map(|n| n.map(|s| s.to_string())).collect();
                Captures { groups, names }
            }),
            RegexEngine::Fancy(re) => re.captures(text).ok().flatten().map(|caps| {
                let groups = (0..caps.len())
                    .map(|i| caps.get(i).map(|m| m.as_str().to_string()))
                    .collect();
                let names = re.capture_names().map(|n| n.map(|s| s.to_string())).collect();
                Captures { groups, names }
            }),
        }
    }

    pub fn capture_names(&self) -> Vec<Option<String>> {
        match self {
            RegexEngine::Normal(re) => re.capture_names().map(|s| s.map(|s| s.to_string())).collect(),
            RegexEngine::Fancy(re) => re.capture_names().map(|s| s.map(|s| s.to_string())).collect(),
        }
    }
}
