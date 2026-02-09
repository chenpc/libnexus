use crate::proto::nexus_service_client::NexusServiceClient;
use crate::proto::{ArgDef, CommandRequest, ListServicesRequest, ServiceInfo};
use hyper_util::rt::TokioIo;
use rustyline::completion::{Completer, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::{Hint, Hinter};
use rustyline::validate::Validator;
use rustyline::{Context, Editor, Helper};
use std::collections::HashMap;
use tokio::net::UnixStream;
use tonic::transport::{Channel, Endpoint};
use tower::service_fn;

/// Inline hint shown as grayed-out text after the cursor.
struct ArgHint(String);

impl Hint for ArgHint {
    fn display(&self) -> &str {
        &self.0
    }

    fn completion(&self) -> Option<&str> {
        None
    }
}

/// Rustyline helper that provides tab-completion for service names, commands,
/// and argument values, plus inline hints showing expected argument placeholders.
struct NexusHelper {
    /// service name -> list of command names
    commands: HashMap<String, Vec<String>>,
    /// (service, command) -> argument definitions
    arg_info: HashMap<(String, String), Vec<ArgDef>>,
    /// gRPC client for dynamic completion calls.
    client: NexusServiceClient<Channel>,
    /// Tokio runtime handle for bridging async calls from the sync completer.
    handle: tokio::runtime::Handle,
    /// Length of the last input line seen (updated by the hinter on each keystroke).
    last_input_len: std::sync::Mutex<usize>,
}

impl NexusHelper {
    fn from_services(
        services: &[ServiceInfo],
        client: NexusServiceClient<Channel>,
        handle: tokio::runtime::Handle,
    ) -> Self {
        let mut commands = HashMap::new();
        let mut arg_info = HashMap::new();
        for svc in services {
            let cmds = svc.commands.iter().map(|c| c.name.clone()).collect();
            for cmd in &svc.commands {
                arg_info.insert(
                    (svc.name.clone(), cmd.name.clone()),
                    cmd.args.clone(),
                );
            }
            commands.insert(svc.name.clone(), cmds);
        }
        Self {
            commands,
            arg_info,
            client,
            handle,
            last_input_len: std::sync::Mutex::new(0),
        }
    }

    /// Display label for an argument: use hint if set, otherwise the param name.
    fn arg_label(arg: &ArgDef) -> &str {
        if arg.hint.is_empty() {
            &arg.name
        } else {
            &arg.hint
        }
    }

    /// Call a completer (e.g. "block.list") by executing the referenced service
    /// command on the server. Spawns a scoped thread to bridge sync -> async.
    fn fetch_completions(&self, completer: &str) -> Vec<String> {
        let Some((svc, cmd)) = completer.split_once('.') else {
            return vec![];
        };
        let mut client = self.client.clone();
        let request = CommandRequest {
            service: svc.to_string(),
            action: cmd.to_string(),
            args: vec![],
        };
        let handle = self.handle.clone();
        let result = std::thread::scope(|s| {
            s.spawn(|| {
                handle.block_on(async move { client.execute(request).await })
            })
            .join()
        });
        match result {
            Ok(Ok(resp)) => resp
                .into_inner()
                .message
                .split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect(),
            _ => vec![],
        }
    }
}

impl Completer for NexusHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Pair>)> {
        let line = &line[..pos];
        let parts: Vec<&str> = line.split_whitespace().collect();

        // Still typing the first word (or empty line): complete service names + builtins.
        if parts.is_empty() || (parts.len() == 1 && !line.ends_with(' ')) {
            let prefix = parts.first().copied().unwrap_or("");
            let start = pos - prefix.len();

            let mut services: Vec<Pair> = self
                .commands
                .keys()
                .filter(|s| s.starts_with(prefix))
                .map(|s| Pair {
                    display: s.clone(),
                    replacement: s.clone(),
                })
                .collect();
            services.sort_by(|a, b| a.display.cmp(&b.display));

            let builtins = ["help", "quit", "exit"];
            let mut builtin_pairs: Vec<Pair> = builtins
                .iter()
                .filter(|b| b.starts_with(prefix))
                .map(|b| Pair {
                    display: b.to_string(),
                    replacement: b.to_string(),
                })
                .collect();
            builtin_pairs.sort_by(|a, b| a.display.cmp(&b.display));

            let mut candidates = services;
            candidates.append(&mut builtin_pairs);
            return Ok((start, candidates));
        }

        // Typing the second word after "help": complete service names.
        if (parts.len() == 1 || (parts.len() == 2 && !line.ends_with(' ')))
            && parts[0] == "help"
        {
            let prefix = if parts.len() == 2 { parts[1] } else { "" };
            let start = pos - prefix.len();

            let mut candidates: Vec<Pair> = self
                .commands
                .keys()
                .filter(|s| s.starts_with(prefix))
                .map(|s| Pair {
                    display: s.clone(),
                    replacement: s.clone(),
                })
                .collect();
            candidates.sort_by(|a, b| a.display.cmp(&b.display));
            return Ok((start, candidates));
        }

        // Typing the second word: complete command names for the given service.
        if parts.len() == 1 || (parts.len() == 2 && !line.ends_with(' ')) {
            let service = parts[0];
            let prefix = if parts.len() == 2 { parts[1] } else { "" };
            let start = pos - prefix.len();

            if let Some(cmds) = self.commands.get(service) {
                let mut candidates: Vec<Pair> = cmds
                    .iter()
                    .filter(|c| c.starts_with(prefix))
                    .map(|c| Pair {
                        display: c.clone(),
                        replacement: c.clone(),
                    })
                    .collect();
                candidates.sort_by(|a, b| a.display.cmp(&b.display));
                return Ok((start, candidates));
            }
        }

        // Typing arguments: call the completer dynamically if one is declared.
        if parts.len() >= 2 {
            let service = parts[0];
            let command = parts[1];

            if let Some(args) = self.arg_info.get(&(service.to_string(), command.to_string())) {
                // Determine which arg position is being completed.
                let (arg_index, prefix) = if line.ends_with(' ') {
                    (parts.len() - 2, "")
                } else {
                    (parts.len() - 3, parts.last().copied().unwrap_or(""))
                };

                if let Some(arg_def) = args.get(arg_index) {
                    if !arg_def.completer.is_empty() {
                        let values = self.fetch_completions(&arg_def.completer);
                        let start = pos - prefix.len();
                        let candidates: Vec<Pair> = values
                            .iter()
                            .filter(|v| v.starts_with(prefix))
                            .map(|v| Pair {
                                display: v.clone(),
                                replacement: v.clone(),
                            })
                            .collect();
                        return Ok((start, candidates));
                    }
                }
            }
        }

        Ok((pos, vec![]))
    }
}

impl Hinter for NexusHelper {
    type Hint = ArgHint;

    fn hint(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Option<ArgHint> {
        *self.last_input_len.lock().unwrap() = line.len();
        let line = &line[..pos];
        let parts: Vec<&str> = line.split_whitespace().collect();

        if parts.len() < 2 {
            return None;
        }

        let service = parts[0];
        let command = parts[1];

        let args = self
            .arg_info
            .get(&(service.to_string(), command.to_string()))?;

        // How many args are already fully typed.
        let hint_start = parts.len() - 2;

        let remaining: Vec<String> = args
            .iter()
            .skip(hint_start)
            .map(|a| format!("<{}>", Self::arg_label(a)))
            .collect();

        if remaining.is_empty() {
            return None;
        }

        let hint = if line.ends_with(' ') {
            remaining.join(" ")
        } else {
            format!(" {}", remaining.join(" "))
        };

        Some(ArgHint(hint))
    }
}

impl Highlighter for NexusHelper {}
impl Validator for NexusHelper {}
impl Helper for NexusHelper {}

/// Interactive CLI shell that connects to a Nexus gRPC server.
pub struct NexusCli {
    addr: String,
}

impl NexusCli {
    pub fn new(addr: &str) -> Self {
        Self {
            addr: addr.to_string(),
        }
    }

    pub async fn run(self) -> anyhow::Result<()> {
        let mut client = if self.addr.contains(':') {
            let addr = if self.addr.starts_with("http://") || self.addr.starts_with("https://") {
                self.addr.clone()
            } else {
                format!("http://{}", self.addr)
            };
            NexusServiceClient::connect(addr).await?
        } else {
            let path = self.addr.clone();
            // The URI is not used for routing; the connector below ignores it.
            let channel = Endpoint::try_from("http://[::]:50051")?
                .connect_with_connector(service_fn(move |_| {
                    let path = path.clone();
                    async move {
                        UnixStream::connect(path).await.map(TokioIo::new)
                    }
                }))
                .await?;
            NexusServiceClient::new(channel)
        };

        // Fetch available services on startup.
        let services = client
            .list_services(ListServicesRequest {})
            .await?
            .into_inner()
            .services;

        println!("Connected. Type 'help' for available commands, 'quit' to exit.");

        let handle = tokio::runtime::Handle::current();
        let helper = NexusHelper::from_services(&services, client.clone(), handle);
        let mut rl = Editor::new()?;
        rl.set_helper(Some(helper));

        loop {
            let line = match rl.readline("cli> ") {
                Ok(line) => line,
                Err(ReadlineError::Interrupted) => {
                    // Move cursor up to the input line and position right after the
                    // text, then print ^C.  Column is 1-based: prompt "cli> " (5)
                    // + input length + 1.
                    let prompt_len = 5; // "cli> "
                    let input_len = rl
                        .helper()
                        .map(|h| *h.last_input_len.lock().unwrap())
                        .unwrap_or(0);
                    let col = prompt_len + input_len + 1;
                    print!("\x1b[A\x1b[{col}G^C\n");
                    continue;
                }
                Err(ReadlineError::Eof) => break,
                Err(e) => return Err(e.into()),
            };

            let line = line.trim();
            if line.is_empty() {
                continue;
            }

            let _ = rl.add_history_entry(line);

            if line == "quit" || line == "exit" {
                break;
            }

            let parts: Vec<&str> = line.split_whitespace().collect();

            if parts[0] == "help" {
                if parts.len() >= 2 {
                    print_service_help(&services, parts[1]);
                } else {
                    print_help(&services);
                }
                continue;
            }

            if parts.len() < 2 {
                println!("Usage: <service> <command> [args...]");
                continue;
            }

            let service = parts[0].to_string();
            let action = parts[1].to_string();
            let args: Vec<String> = parts[2..].iter().map(|s| s.to_string()).collect();

            let response = client
                .execute(CommandRequest {
                    service,
                    action,
                    args,
                })
                .await?
                .into_inner();

            if response.success {
                println!("{}", response.message);
            } else {
                println!("Error: {}", response.message);
            }
        }

        Ok(())
    }
}

fn print_service_help(services: &[ServiceInfo], name: &str) {
    let Some(svc) = services.iter().find(|s| s.name == name) else {
        println!("Unknown service '{}'. Type 'help' to list all services.", name);
        return;
    };
    if svc.description.is_empty() {
        println!("{}:", svc.name);
    } else {
        println!("{}: {}", svc.name, svc.description);
    }
    println!();
    for cmd in &svc.commands {
        let args_str = cmd
            .args
            .iter()
            .map(|a| {
                let label = if a.hint.is_empty() { &a.name } else { &a.hint };
                format!("<{}>", label)
            })
            .collect::<Vec<_>>()
            .join(" ");
        println!("  {} {}", cmd.name, args_str);
        if !cmd.description.is_empty() {
            println!("    {}", cmd.description);
        }
        for arg in &cmd.args {
            let label = if arg.hint.is_empty() { &arg.name } else { &arg.hint };
            let has_desc = !arg.description.is_empty();
            let has_comp = !arg.completer.is_empty();
            if has_desc || has_comp {
                let mut parts = vec![format!("    <{}>", label)];
                if has_desc {
                    parts.push(arg.description.clone());
                }
                if has_comp {
                    parts.push(format!("(completions from {})", arg.completer));
                }
                println!("{}", parts.join(" - "));
            }
        }
        println!();
    }
}

fn print_help(services: &[ServiceInfo]) {
    println!("Available commands:");
    for svc in services {
        if svc.description.is_empty() {
            println!("  {}:", svc.name);
        } else {
            println!("  {}: {}", svc.name, svc.description);
        }
        for cmd in &svc.commands {
            let args_str = cmd
                .args
                .iter()
                .map(|a| {
                    let label = if a.hint.is_empty() { &a.name } else { &a.hint };
                    format!("<{}>", label)
                })
                .collect::<Vec<_>>()
                .join(" ");
            let desc = if cmd.description.is_empty() {
                String::new()
            } else {
                format!(" - {}", cmd.description)
            };
            println!("    {} {}{}", cmd.name, args_str, desc);
        }
    }
}
