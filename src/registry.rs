use anyhow::Result;
use async_trait::async_trait;
use std::collections::HashMap;

/// Metadata about a single argument on a command.
#[derive(Debug, Clone)]
pub struct ArgInfo {
    pub name: String,
    /// Display hint shown to the user (e.g. "volume name"). Falls back to `name` if empty.
    pub hint: String,
    /// Completer reference in "service.command" form (e.g. "block.list").
    pub completer: String,
    /// Human-readable description of this argument.
    pub description: String,
}

/// Metadata about a single command on a service.
#[derive(Debug, Clone)]
pub struct CommandInfo {
    pub name: String,
    pub args: Vec<ArgInfo>,
    pub description: String,
}

/// Trait that every service must implement. Use `#[nexus_service]` to auto-generate.
#[async_trait]
pub trait Service: Send + Sync + 'static {
    /// The service name used for dispatch (e.g., "volume").
    fn name(&self) -> &str;

    /// Human-readable description of the service (from doc comments on the impl block).
    fn description(&self) -> &str;

    /// List of commands this service supports.
    fn commands(&self) -> Vec<CommandInfo>;

    /// Execute a command by action name with positional string arguments.
    async fn execute(&self, action: &str, args: Vec<String>) -> Result<String>;
}

/// Holds registered services and dispatches commands to them.
pub struct Registry {
    services: HashMap<String, Box<dyn Service>>,
}

impl Registry {
    pub fn new() -> Self {
        Self {
            services: HashMap::new(),
        }
    }

    pub fn register<S: Service>(&mut self, service: S) {
        self.services
            .insert(service.name().to_string(), Box::new(service));
    }

    pub async fn execute(
        &self,
        service_name: &str,
        action: &str,
        args: Vec<String>,
    ) -> Result<String> {
        let service = self
            .services
            .get(service_name)
            .ok_or_else(|| anyhow::anyhow!("unknown service '{}'", service_name))?;
        service.execute(action, args).await
    }

    pub fn list_services(&self) -> Vec<(&str, &str, Vec<CommandInfo>)> {
        self.services
            .iter()
            .map(|(name, svc)| (name.as_str(), svc.description(), svc.commands()))
            .collect()
    }
}
