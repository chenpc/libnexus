pub mod registry;
pub mod server;
pub mod cli;

pub mod proto {
    tonic::include_proto!("nexus");
}

pub use registry::{ArgInfo, CommandInfo, Service};
pub use server::NexusServer;
pub use cli::NexusCli;
pub use nexus_derive::nexus_service;

/// A sorted map from name to value, serialized as a JSON object.
/// Use this as the return type for commands that list named resources.
pub type NamedMap<T> = std::collections::BTreeMap<String, T>;

/// Serialize a value to a pretty-printed JSON string.
/// Used by the `#[nexus_service]` macro to auto-serialize command return values.
pub fn serialize_response<T: serde::Serialize>(value: &T) -> anyhow::Result<String> {
    let mut buf = Vec::new();
    serde_json::to_writer_pretty(&mut buf, value)?;
    Ok(String::from_utf8(buf)?)
}

pub const DEFAULT_ENDPOINT: &str = "/tmp/nexus.sock";
