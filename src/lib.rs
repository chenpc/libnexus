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

pub const DEFAULT_ENDPOINT: &str = "/tmp/nexus.sock";
