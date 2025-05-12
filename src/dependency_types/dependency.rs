use std::str::FromStr;

use crate::dependency_types::existential::ExistentialDependency;
use crate::dependency_types::temporal::TemporalDependency;

use super::{existential, temporal};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Dependency {
    pub from: String,
    pub to: String,
    pub temporal_dependency: Option<TemporalDependency>,
    pub existential_dependency: Option<ExistentialDependency>,
}

impl Dependency {
    pub fn new(
        from: String,
        to: String,
        temporal_dependency: Option<TemporalDependency>,
        existential_dependency: Option<ExistentialDependency>,
    ) -> Self {
        Self {
            from,
            to,
            temporal_dependency,
            existential_dependency,
        }
    }
}

impl std::fmt::Display for Dependency {
    /// Formats the object using the given formatter.
    ///
    /// This method checks for the presence of `temporal_dependency` and `existential_dependency`
    /// and formats the output accordingly:
    /// - If both dependencies are present, it writes them separated by a comma.
    /// - If only `temporal_dependency` is present, it writes it followed by a comma and a dash.
    /// - If only `existential_dependency` is present, it writes a dash followed by the dependency.
    /// - If neither dependency is present, it writes "None".
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let temporal_dep = self.temporal_dependency.as_ref().map(|dep| dep.to_string());
        let existential_dep = self
            .existential_dependency
            .as_ref()
            .map(|dep| dep.to_string());

        match (temporal_dep, existential_dep) {
            (Some(t), Some(e)) => write!(f, "{},{}", t, e),
            (Some(t), None) => write!(f, "{},-", t),
            (None, Some(e)) => write!(f, "-,{}", e),
            (None, None) => write!(f, "None"),
        }
    }
}

impl FromStr for Dependency {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split(&[',', ':', ' '][..]).collect();
        let mut iter = parts.iter();
        let from = iter
            .next()
            .ok_or("Could not parse from activity".to_string())?
            .to_string();
        let to = iter
            .next()
            .ok_or("Could not parse to activity".to_string())?
            .to_string();

        // Manually dereference the iterator's items
        let temporal_dependency = match iter.next().copied() {
            Some("d") => Some(temporal::DependencyType::Direct),
            Some("e") => Some(temporal::DependencyType::Eventual),
            Some("-") => None,
            Some(s) => panic!("Invalid temporal dependency type {}", s),
            None => panic!("Missing dependency type"),
        };

        let direction = match iter.next().copied() {
            Some("b") => Some(temporal::Direction::Backward),
            Some("f") => Some(temporal::Direction::Forward),
            Some("-") => None,
            Some(s) => panic!("Invalid direction {}", s),
            None => panic!("Missing direction"),
        };

        let temporal_dependency =
            if let (Some(temp_dep), Some(dir)) = (temporal_dependency, direction) {
                Some(temporal::TemporalDependency::new(
                    from.as_str(),
                    to.as_str(),
                    temp_dep,
                    dir,
                ))
            } else {
                None
            };

        let existential_dependency = match iter.next().copied() {
            Some("i") => Some(existential::DependencyType::Implication),
            Some("e") => Some(existential::DependencyType::Equivalence),
            Some("ne") => Some(existential::DependencyType::NegatedEquivalence),
            Some("n") => Some(existential::DependencyType::Nand),
            Some("o") => Some(existential::DependencyType::Or),
            Some("-") => None,
            Some(s) => panic!("Invalid existential dependency type {}", s),
            None => panic!("Missing existential dependency type"),
        };

        let direction = match iter.next().copied() {
            Some("f") => Some(existential::Direction::Forward),
            Some("b") => Some(existential::Direction::Backward),
            Some("-") => None,
            None => {
                //println!("Assuming both directions");
                Some(existential::Direction::Both)
            }
            Some(s) => panic!("Invalid direction {}", s),
        };

        let existential_dependency =
            if let (Some(dep), Some(dir)) = (existential_dependency, direction) {
                Some(existential::ExistentialDependency::new(
                    from.as_str(),
                    to.as_str(),
                    dep,
                    dir,
                ))
            } else {
                None
            };

        Ok(Dependency {
            from,
            to,
            temporal_dependency,
            existential_dependency,
        })
    }
}

pub fn convert_to_dependencies(s: &str) -> Vec<Dependency> {
    let mut deps = Vec::new();
    for line in s.lines() {
        if line.is_empty() {
            continue;
        }
        let dep = Dependency::from_str(line).unwrap();
        deps.push(dep);
    }
    deps
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_str() {
        let d = Dependency::from_str("a,b:d,f i,b").unwrap();
        assert_eq!(d.from, "a");
        assert_eq!(d.to, "b");
        assert_eq!(
            d.temporal_dependency.unwrap(),
            TemporalDependency::new(
                "a",
                "b",
                temporal::DependencyType::Direct,
                temporal::Direction::Forward
            )
        );
        assert_eq!(
            d.existential_dependency.unwrap(),
            ExistentialDependency::new(
                "a",
                "b",
                existential::DependencyType::Implication,
                existential::Direction::Backward
            )
        );
    }

    #[test]
    fn test_from_str_no_direction() {
        let d = Dependency::from_str("a,b:d,f e").unwrap();
        assert_eq!(d.from, "a");
        assert_eq!(d.to, "b");
        assert_eq!(
            d.temporal_dependency.unwrap(),
            TemporalDependency::new(
                "a",
                "b",
                temporal::DependencyType::Direct,
                temporal::Direction::Forward
            )
        );
        assert_eq!(
            d.existential_dependency.unwrap(),
            ExistentialDependency::new(
                "a",
                "b",
                existential::DependencyType::Equivalence,
                existential::Direction::Both
            )
        );
    }

    const DEPS_1: &str = "
a,b:d,f i,b
a,c:e,f i,b
a,d:e,f e
a,e:d,f i,b
";

    #[test]
    fn test_convert_to_dependencies() {
        let deps = convert_to_dependencies(DEPS_1);
        assert_eq!(deps.len(), 4);
        assert_eq!(
            deps[0],
            Dependency::new(
                "a".to_string(),
                "b".to_string(),
                Some(TemporalDependency::new(
                    "a",
                    "b",
                    temporal::DependencyType::Direct,
                    temporal::Direction::Forward
                )),
                Some(ExistentialDependency::new(
                    "a",
                    "b",
                    existential::DependencyType::Implication,
                    existential::Direction::Backward
                ))
            )
        );
        assert_eq!(
            deps[1],
            Dependency::new(
                "a".to_string(),
                "c".to_string(),
                Some(TemporalDependency::new(
                    "a",
                    "c",
                    temporal::DependencyType::Eventual,
                    temporal::Direction::Forward
                )),
                Some(ExistentialDependency::new(
                    "a",
                    "c",
                    existential::DependencyType::Implication,
                    existential::Direction::Backward
                ))
            )
        );
        assert_eq!(
            deps[2],
            Dependency::new(
                "a".to_string(),
                "d".to_string(),
                Some(TemporalDependency::new(
                    "a",
                    "d",
                    temporal::DependencyType::Eventual,
                    temporal::Direction::Forward
                )),
                Some(ExistentialDependency::new(
                    "a",
                    "d",
                    existential::DependencyType::Equivalence,
                    existential::Direction::Both
                ))
            )
        );
        assert_eq!(
            deps[3],
            Dependency::new(
                "a".to_string(),
                "e".to_string(),
                Some(TemporalDependency::new(
                    "a",
                    "e",
                    temporal::DependencyType::Direct,
                    temporal::Direction::Forward
                )),
                Some(ExistentialDependency::new(
                    "a",
                    "e",
                    existential::DependencyType::Implication,
                    existential::Direction::Backward
                ))
            )
        );
    }
}
