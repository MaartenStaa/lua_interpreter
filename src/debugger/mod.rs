mod breakpoint;
pub(crate) mod controller;
pub mod protocol;
pub(crate) mod source_map;
mod tui;

use std::sync::mpsc;

use crate::state::LuaState;

use controller::DebuggerController;
use protocol::{Breakpoint, DebugCommand, DebugEvent};

pub fn launch_debugger(mut state: LuaState, breakpoints: Vec<Breakpoint>, break_on_start: bool) {
    let (cmd_tx, cmd_rx) = mpsc::channel::<DebugCommand>();
    let (evt_tx, evt_rx) = mpsc::channel::<DebugEvent>();

    let controller = {
        let global = state.global.read().unwrap();
        DebuggerController::new(cmd_rx, evt_tx, breakpoints, break_on_start, &global.chunks)
    };
    state.debugger = Some(controller);

    let tui_handle = std::thread::spawn(move || tui::run_tui(&cmd_tx, &evt_rx));

    match state.run() {
        Ok(_) => {
            if let Some(ref dbg) = state.debugger {
                dbg.send_completed();
            }
        }
        Err(e) => {
            if let Some(ref dbg) = state.debugger {
                dbg.send_error(format!("{e}"));
            }
        }
    }

    match tui_handle.join() {
        Ok(Err(e)) => eprintln!("TUI error: {e}"),
        Err(_) => eprintln!("TUI thread panicked"),
        _ => {}
    }
}

#[must_use]
pub fn parse_breakpoint(s: &str) -> Option<Breakpoint> {
    if let Some(rest) = s.strip_prefix("c:") {
        let parts: Vec<&str> = rest.splitn(2, ':').collect();
        if parts.len() == 2 {
            let chunk = parts[0].parse().ok()?;
            let ip = parts[1].parse().ok()?;
            return Some(Breakpoint::Instruction { chunk, ip });
        }
        return None;
    }

    let parts: Vec<&str> = s.rsplitn(2, ':').collect();
    if parts.len() == 2 {
        let line = parts[0].parse().ok()?;
        let filename = parts[1].to_string();
        return Some(Breakpoint::SourceLine { filename, line });
    }

    None
}
