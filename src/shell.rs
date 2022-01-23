use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use clap::{Parser, Subcommand};
use once_cell::sync::Lazy;
use rustyline::Editor;

use crate::mos6502::Mos6502;
use crate::R;

const PROMPT_STR: &'static str = "$> ";

static SIGINT_RECEIVED: Lazy<Arc<AtomicBool>> = Lazy::new(Default::default);

#[derive(Parser, Debug)]
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    Step { instrs: u32 },
    Continue,
    Jump { addr: u16 },
    Regs,
    Peek { addr: u16 },
    Poke { addr: u16, value: u8 },
    DumpMem { start: u16, len: u16 },
    Quit,
    Irq,
    Nmi,
    Print,
    BreakRm { addr: u16 },
    BreakList,
    Break { addr: u16 },
}

pub fn run_shell(cpu: R<Mos6502>, interactive: bool) {
    signal_hook::flag::register(signal_hook::consts::SIGTERM, SIGINT_RECEIVED.clone())
        .expect("Couldn't register a SIGINT handler");

    let mut rl = Editor::<()>::new();

    // this is a macro instead of a closure because the add_history_entry call bothers borrowck
    macro_rules! prompt {
        () => {
            loop {
                match rl.readline(PROMPT_STR) {
                    Ok(line) => break line,
                    Err(_) => continue,
                }
            }
        };
    }

    let mut line = None;
    if interactive {
        line = Some(prompt!());
    }

    loop {
        match line.as_deref() {
            Some("") => (),
            None => {
                SIGINT_RECEIVED.store(false, Ordering::Relaxed);
                todo!()
            }
            Some(line) => {
                rl.add_history_entry(line);
                println!("{:?}", Cli::try_parse_from(line.split_whitespace()));
            }
        }
        line = Some(prompt!());
    }
}
