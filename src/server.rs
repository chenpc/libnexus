use crate::proto::nexus_service_server::{NexusService, NexusServiceServer};
use crate::proto::{
    ArgDef, CommandDef, CommandRequest, CommandResponse, ListServicesRequest, ListServicesResponse,
    ServiceInfo,
};
use crate::registry::{Registry, Service};
use std::sync::Arc;
use tokio::net::UnixListener;
use tokio_stream::wrappers::UnixListenerStream;
use tonic::{Request, Response, Status};

/// gRPC server wrapping a service registry.
pub struct NexusServer {
    registry: Arc<Registry>,
}

impl NexusServer {
    pub fn new() -> Self {
        Self {
            registry: Arc::new(Registry::new()),
        }
    }

    /// Register a service with the server. Must be called before `serve`.
    pub fn register<S: Service>(mut self, service: S) -> Self {
        Arc::get_mut(&mut self.registry)
            .expect("register must be called before serve")
            .register(service);
        self
    }

    /// Start the gRPC server on the given address.
    ///
    /// If `addr` contains `:` it is treated as a TCP socket address (e.g.
    /// `[::1]:50051`).  Otherwise it is treated as a Unix domain socket path
    /// (e.g. `/tmp/nexus.sock`).
    pub async fn serve(self, addr: &str) -> crate::Result<()> {
        let grpc_service = NexusGrpcService {
            registry: self.registry,
        };
        let svc = NexusServiceServer::new(grpc_service);

        if addr.contains(':') {
            let sock_addr = addr.parse()?;
            println!("Nexus server listening on {}", sock_addr);
            tonic::transport::Server::builder()
                .add_service(svc)
                .serve(sock_addr)
                .await?;
        } else {
            // Remove a stale socket file if it exists.
            let _ = std::fs::remove_file(addr);
            let uds = UnixListener::bind(addr)?;
            let stream = UnixListenerStream::new(uds);
            println!("Nexus server listening on {}", addr);
            tonic::transport::Server::builder()
                .add_service(svc)
                .serve_with_incoming(stream)
                .await?;
        }

        Ok(())
    }
}

struct NexusGrpcService {
    registry: Arc<Registry>,
}

#[tonic::async_trait]
impl NexusService for NexusGrpcService {
    async fn execute(
        &self,
        request: Request<CommandRequest>,
    ) -> Result<Response<CommandResponse>, Status> {
        let req = request.into_inner();
        match self.registry.execute(&req.service, &req.action, req.args).await {
            Ok(message) => Ok(Response::new(CommandResponse {
                success: true,
                message,
            })),
            Err(e) => Ok(Response::new(CommandResponse {
                success: false,
                message: e.to_string(),
            })),
        }
    }

    async fn list_services(
        &self,
        _request: Request<ListServicesRequest>,
    ) -> Result<Response<ListServicesResponse>, Status> {
        let services = self
            .registry
            .list_services()
            .into_iter()
            .map(|(name, description, commands)| ServiceInfo {
                name: name.to_string(),
                description: description.to_string(),
                commands: commands
                    .into_iter()
                    .map(|c| CommandDef {
                        name: c.name,
                        args: c.args
                            .into_iter()
                            .map(|a| ArgDef {
                                name: a.name,
                                hint: a.hint,
                                completer: a.completer,
                                description: a.description,
                            })
                            .collect(),
                        description: c.description,
                    })
                    .collect(),
            })
            .collect();

        Ok(Response::new(ListServicesResponse { services }))
    }
}
