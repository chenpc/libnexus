# libnexus Developer Guide

libnexus is a framework for building gRPC microservices with an interactive CLI. You define services as plain Rust structs, annotate methods with macros, and the framework handles gRPC transport, command dispatch, tab completion, and inline help.

## Quick Start

### Dependencies

**Server crate** (`Cargo.toml`):

```toml
[dependencies]
libnexus = { path = "../libnexus" }
tokio = { version = "1", features = ["full"] }
async-trait = "0.1"
anyhow = "1"
```

**CLI client crate** (`Cargo.toml`):

```toml
[dependencies]
libnexus = { path = "../libnexus" }
tokio = { version = "1", features = ["full"] }
anyhow = "1"
```

### Minimal Server

```rust
use libnexus::{nexus_service, NexusServer};

pub struct Greeter;

/// A simple greeting service.
#[nexus_service]
impl Greeter {
    /// Say hello.
    #[command]
    async fn hello(&self, name: String) -> anyhow::Result<String> {
        Ok(format!("Hello, {}!", name))
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    NexusServer::new()
        .register(Greeter)
        .serve(libnexus::DEFAULT_ENDPOINT)
        .await
}
```

### Minimal CLI Client

```rust
use libnexus::NexusCli;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    NexusCli::new(libnexus::DEFAULT_ENDPOINT).run().await
}
```

Run the server, then the client. Type `help` to see your service:

```
cli> help
Available commands:
  greeter: A simple greeting service.
    hello <name> - Say hello.
cli> greeter hello world
Hello, world!
```

## Defining a Service

A service is a struct with an `impl` block annotated with `#[nexus_service]`.

```rust
use libnexus::nexus_service;

pub struct MyService;

/// Description of your service (shown in `help`).
#[nexus_service]
impl MyService {
    // commands go here
}
```

The service name is derived from the struct name, lowercased. `MyService` becomes `myservice` in the CLI.

The doc comment on the `impl` block becomes the service description, displayed in `help` output and `help <service>`.

## Defining Commands

Mark methods with `#[command]`. Each command must:

- Take `&self` as the first parameter
- Be `async`
- Return `anyhow::Result<String>`
- Have all other parameters as `String`

```rust
/// Description of this command (shown in `help`).
#[command]
async fn my_command(&self, arg1: String, arg2: String) -> anyhow::Result<String> {
    Ok(format!("got {} and {}", arg1, arg2))
}
```

The doc comment on the method becomes the command description. Parameter names are used as default labels in the CLI help and hints.

Commands are invoked in the CLI as `<service> <command> [args...]`:

```
cli> myservice my_command foo bar
got foo and bar
```

## Argument Metadata with `#[arg(...)]`

Use `#[arg(...)]` on parameters to add CLI metadata. All fields are optional:

| Field      | Type     | Description                                          |
|------------|----------|------------------------------------------------------|
| `hint`     | `&str`   | Display label in CLI hints and help (instead of param name) |
| `doc`      | `&str`   | Description shown in `help <service>` output         |
| `complete` | `&str`   | Dynamic completer in `"service.command"` form         |

### hint — Display Label

Override the parameter name shown in inline hints and help:

```rust
#[command]
async fn create(
    &self,
    #[arg(hint = "volume name")] name: String,
) -> anyhow::Result<String> { ... }
```

Without hint, the CLI shows `<name>`. With hint, it shows `<volume name>`.

### doc — Argument Description

Add a description shown in `help <service>` output:

```rust
#[command]
async fn setip(
    &self,
    #[arg(hint = "ip", doc = "IP address (e.g. 10.0.0.1)")] ip: String,
    #[arg(hint = "mask", doc = "Subnet mask (e.g. 255.255.255.0)")] mask: String,
) -> anyhow::Result<String> { ... }
```

`help myservice` output:

```
  setip <ip> <mask>
    Set IP address and subnet mask.
    <ip> - IP address (e.g. 10.0.0.1)
    <mask> - Subnet mask (e.g. 255.255.255.0)
```

### complete — Dynamic Tab Completion

Reference another service's command to populate tab completions. The format is `"service.command"`. The CLI calls that command on the server and parses the comma-separated result as completion candidates.

```rust
#[command]
async fn delete(
    &self,
    #[arg(hint = "volume name", complete = "volume.list")] name: String,
) -> anyhow::Result<String> { ... }
```

When the user presses Tab on this argument, the CLI calls `volume list` on the server, splits the response by commas, and offers matching values.

The referenced command must:
- Take no arguments
- Return a comma-separated string (e.g. `"vol0, vol1, vol2"`)

You can reference commands from any registered service, including the current one:

```rust
// Self-referencing: volume.delete completes from volume.list
#[arg(complete = "volume.list")]

// Cross-service: volume.create completes disk from block.list
#[arg(complete = "block.list")]
```

### Combining All Fields

```rust
#[command]
async fn create(
    &self,
    #[arg(hint = "volume name", doc = "Name for the new volume")]
    name: String,
    #[arg(hint = "device", doc = "Block device to use", complete = "block.list")]
    disk: String,
) -> anyhow::Result<String> { ... }
```

## Registering Services

Register services with `NexusServer` using the builder pattern:

```rust
NexusServer::new()
    .register(Volume)
    .register(Block)
    .register(Network)
    .register(Pool)
    .serve(&addr)
    .await
```

## Transport

`NexusServer::serve()` and `NexusCli::new()` accept an address string:

- **Unix domain socket** (default): any path without `:` (e.g. `/tmp/nexus.sock`)
- **TCP**: address with `:` (e.g. `[::1]:50051`)

The default endpoint is available as `libnexus::DEFAULT_ENDPOINT` (`/tmp/nexus.sock`).

```rust
// Unix socket (default)
NexusServer::new().register(MyService).serve("/tmp/my.sock").await

// TCP
NexusServer::new().register(MyService).serve("[::1]:50051").await
```

## Project Layout

Recommended structure for a server crate:

```
my-daemon/
├── Cargo.toml
└── src/
    ├── main.rs
    └── services/
        ├── volume.rs
        ├── block.rs
        └── network.rs
```

No `mod.rs` needed — declare modules inline in `main.rs`:

```rust
mod services {
    pub mod block;
    pub mod network;
    pub mod volume;
}

use libnexus::NexusServer;
use services::{block::Block, network::Network, volume::Volume};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    NexusServer::new()
        .register(Volume)
        .register(Block)
        .register(Network)
        .serve(libnexus::DEFAULT_ENDPOINT)
        .await
}
```

To add a new service, create the `.rs` file and add one `pub mod` line.

## CLI Features

The built-in CLI client provides:

- **Tab completion** for service names (listed first), command names, and arguments with completers
- **Inline hints** showing `<param>` placeholders as grayed-out text
- **`help`** lists all services with descriptions
- **`help <service>`** shows detailed documentation for a service
- **Ctrl+C** cancels the current line (does not exit)
- **Ctrl+D** exits the CLI
- **Command history** via up/down arrows

## Complete Example

```rust
use libnexus::nexus_service;

pub struct Network;

/// Manage network interfaces and addressing.
#[nexus_service]
impl Network {
    /// List all network interfaces.
    #[command]
    async fn list(&self) -> anyhow::Result<String> {
        Ok("eth0, eth1, lo".to_string())
    }

    /// Show info for a network interface.
    #[command]
    async fn info(
        &self,
        #[arg(hint = "interface", doc = "Network interface to inspect", complete = "network.list")]
        iface: String,
    ) -> anyhow::Result<String> {
        Ok(format!("Interface '{}': ip=10.0.0.1, mask=255.255.255.0, state=UP", iface))
    }

    /// Set IP address and subnet mask on an interface.
    #[command]
    async fn setip(
        &self,
        #[arg(hint = "interface", doc = "Network interface to configure", complete = "network.list")]
        iface: String,
        #[arg(hint = "ip", doc = "IP address (e.g. 10.0.0.1)")]
        ip: String,
        #[arg(hint = "mask", doc = "Subnet mask (e.g. 255.255.255.0)")]
        mask: String,
    ) -> anyhow::Result<String> {
        Ok(format!("Set {}/{} on interface '{}'", ip, mask, iface))
    }
}
```
