use std::{
    convert::Infallible,
    env,
    io::{self, ErrorKind},
    path::PathBuf,
    time::Duration,
};

use rome_lsp::{ServerConnection, ServerFactory};
use tokio::{
    io::{split, Interest},
    net::{UnixListener, UnixStream},
    process::{Child, Command},
    time,
};

/// Returns the filesystem path of the global socket used to communicate with
/// the server daemon
fn get_socket_name() -> PathBuf {
    env::temp_dir().join("rome-socket")
}

/// Try to connect to the global socket and wait for the connection to become ready
async fn try_connect() -> io::Result<UnixStream> {
    let stream = UnixStream::connect(get_socket_name()).await?;
    stream
        .ready(Interest::READABLE | Interest::WRITABLE)
        .await?;
    Ok(stream)
}

/// Spawn the daemon server process in the background
fn spawn_daemon() -> io::Result<Child> {
    let binary = env::current_exe()?;

    let mut cmd = Command::new(binary);
    cmd.arg("__run_server");

    // Create a new session for the process and make it the leader, this will
    // ensures that the child process is fully detached from its parent and will
    // continue running in the background even after the parent process exits
    //
    // SAFETY: This closure runs in the forked child process before it starts
    // executing, this is a highly unsafe environment because the process isn't
    // running yet so seemingly innocuous operation like allocating memory may
    // hang indefinitely.
    // The only thing we do here is issuing a syscall, which is safe to do in
    // this state but still "unsafe" in Rust semantics because it's technically
    // mutating the shared global state of the process
    unsafe {
        cmd.pre_exec(|| {
            libc::setsid();
            Ok(())
        });
    }

    let child = cmd.spawn()?;
    Ok(child)
}

/// Open a connection to the daemon server process, returning [None] if the
/// server is not running
pub(crate) async fn open_socket() -> io::Result<Option<UnixStream>> {
    match try_connect().await {
        Ok(socket) => Ok(Some(socket)),
        Err(err) if err.kind() == ErrorKind::NotFound => Ok(None),
        Err(err) => Err(err),
    }
}

/// Ensure the server daemon is running and ready to receive connections and
/// print the global socket name in the standard output
pub(crate) async fn print_socket() -> io::Result<()> {
    let mut current_child: Option<Child> = None;

    loop {
        // Try to open a connection on the global socket
        match open_socket().await {
            // The connection is open and ready => exit to printing the socket name
            Ok(Some(_)) => break,
            // There's no process listening on the global socket
            Ok(None) => {
                if let Some(current_child) = &mut current_child {
                    // If we have a handle to the daemon process, wait for a few
                    // milliseconds for it to exit, or retry the connection
                    tokio::select! {
                        result = current_child.wait() => {
                            let _status = result?;
                            return Err(io::Error::new(
                                io::ErrorKind::ConnectionReset,
                                "the server process exited before the connection could be etablished",
                            ));
                        }
                        _ = time::sleep(Duration::from_millis(50)) => {}
                    }
                } else {
                    // Spawn the daemon process and wait a few milliseconds for
                    // it to become ready then retry the connection
                    current_child = Some(spawn_daemon()?);
                    time::sleep(Duration::from_millis(50)).await;
                }
            }
            Err(err) => return Err(err),
        }
    }

    println!("{}", get_socket_name().display());
    Ok(())
}

/// Start listening on the global socket and accepting connections with the
/// provided [ServerFactory]
pub(crate) async fn run_daemon(factory: ServerFactory) -> io::Result<Infallible> {
    let listener = UnixListener::bind(get_socket_name())?;

    loop {
        let (stream, _) = listener.accept().await?;
        let connection = factory.create();
        tokio::spawn(run_server(connection, stream));
    }
}

/// Async task driving a single client connection
async fn run_server(connection: ServerConnection, stream: UnixStream) {
    let (read, write) = split(stream);
    connection.accept(read, write).await;
}
