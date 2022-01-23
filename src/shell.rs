use std::collections::BTreeSet;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use clap::Parser;
use once_cell::sync::Lazy;
use rustyline::Editor;

use crate::mos6502::{Mos6502, StepResult};
use crate::R;

const PROMPT_STR: &'static str = "$> ";

static SIGINT_RECEIVED: Lazy<Arc<AtomicBool>> = Lazy::new(Default::default);
fn sigint_received() -> bool {
    SIGINT_RECEIVED.load(Ordering::Relaxed)
}

macro_rules! hex_int {
    ($wrapper:ident, $inner:ty) => {
        #[derive(Debug)]
        struct $wrapper($inner);
        impl std::str::FromStr for $wrapper {
            type Err = std::num::ParseIntError;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                <$inner>::from_str_radix(s, 16).map(Self)
            }
        }
    };
}

hex_int!(Hex16, u16);
hex_int!(Hex8, u8);

#[derive(Parser, Debug)]
enum Cli {
    /// Step the CPU by n instructions
    #[clap(alias = "s")]
    Step {
        #[clap(default_value = "1")]
        n: u32,
    },
    /// Continue program execution
    #[clap(alias = "c")]
    Continue,
    /// Set the program counter (PC) to addr
    #[clap(alias = "j")]
    Jump { addr: Hex16 },
    /// Print the values of each CPU register
    #[clap(alias = "r")]
    Regs,
    /// Print the byte at addr
    #[clap(alias = "pk")]
    Peek { addr: Hex16 },
    /// Set the byte at addr to value
    #[clap(alias = "po")]
    Poke { addr: Hex16, value: Hex8 },
    /// Dump the memory in the address range [start, start+length)
    #[clap(alias = "dm")]
    Dumpmem { start: Hex16, len: u16 },
    /// Quit this program
    #[clap(aliases = &["exit", "q"])]
    Quit,
    /// Raise an IRQ
    Irq,
    /// Raise an NMI
    Nmi,
    /// Print the current instruction
    #[clap(alias = "pr")]
    Print,
    /// Remove a breakpoint at addr
    #[clap(alias = "b-rm")]
    BreakRm { addr: Hex16 },
    /// List all active breakpoints
    #[clap(alias = "b-list")]
    BreakList,
    /// Set a breakpoint at addr
    #[clap(alias = "b")]
    Break { addr: Hex16 },
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

    let mut breakpoints = BTreeSet::new();

    let cpu = &mut *cpu.borrow_mut();

    loop {
        match line.as_deref() {
            Some("") => cmd_cont(cpu, &mut breakpoints),
            None => {
                SIGINT_RECEIVED.store(false, Ordering::Relaxed);
                todo!()
            }
            Some(line) => {
                rl.add_history_entry(line);
                let args = std::iter::once(">").chain(line.split_whitespace());
                match Cli::try_parse_from(args) {
                    Ok(cmd) => handle_cmd(cmd, cpu, &mut breakpoints),
                    Err(e) => e.print().expect("Error writing Error"),
                }
            }
        }
        line = Some(prompt!());
    }
}

fn handle_cmd(command: Cli, cpu: &mut Mos6502, breakpoints: &mut BTreeSet<u16>) {
    match command {
        Cli::Break { addr } => {
            let addr = addr.0;
            if !breakpoints.insert(addr) {
                eprintln!("  Breakpoint at ${addr:04X} already exists");
            }
        }
        Cli::BreakList => {
            for (c, addr) in breakpoints.iter().enumerate() {
                println!("  {c}: ${addr:04X}");
            }
        }
        Cli::BreakRm { addr } => {
            breakpoints.remove(&addr.0);
        }
        Cli::Step { n } => cmd_step(cpu, n, breakpoints),
        Cli::Jump { addr } => {
            cpu.pc = addr.0;
            print_pc_update(cpu);
        }
        Cli::Regs => cmd_regs(cpu),
        Cli::Peek { addr } => {
            let addr = addr.0;
            let val = cpu.bus.borrow_mut().read(addr);
            println!("  ${:04x}: {:02x}", addr, val);
        }
        Cli::Poke { addr, value } => cpu.bus.borrow_mut().write(addr.0, value.0),
        Cli::Dumpmem { start, len } => cmd_dumpmem(cpu, start.0, len),
        Cli::Irq => cpu.raise_irq(),
        Cli::Nmi => cpu.raise_nmi(),
        Cli::Print => cmd_print_instr(cpu),
        Cli::Continue => cmd_cont(cpu, breakpoints),
        Cli::Quit => {
            println!("  Quitting. Goodbye.");
            std::process::exit(0);
        }
    }
}

fn print_pc_update(cpu: &Mos6502) {
    let s = cpu.instr_repr(cpu.pc);
    println!("  PC now at ${:04x}: {s}", cpu.pc);
}

fn check_step_result(step_result: StepResult) {
    match step_result {
        StepResult::Success => (),
        StepResult::UnhandledVmcall => {
            println!("  Breaking due to unhandled VMCALL")
        }
        StepResult::IllegalInstruction => {
            eprintln!("  Illegal instruction")
        }
        StepResult::Vmbreak => {
            println!("  VMCALL breakpoint reached")
        }
    }
}

fn cmd_step(cpu: &mut Mos6502, n: u32, breakpoints: &mut BTreeSet<u16>) {
    let mut bp_hit = false;
    let mut last_pc = cpu.pc;
    let mut step_result = StepResult::Success;

    cpu.tk.borrow_mut().resume();
    for _ in 0..n {
        if breakpoints.contains(&cpu.pc) {
            bp_hit = true;
            break;
        } else if step_result != StepResult::Success || sigint_received() {
            break;
        }
        last_pc = cpu.pc;
        step_result = cpu.step();
    }
    cpu.tk.borrow_mut().pause();

    check_step_result(step_result);

    if bp_hit {
        println!("Breakpoint at ${:04x} reached", cpu.pc);
        breakpoints.remove(&cpu.pc);
    } else if step_result != StepResult::Success {
        cpu.pc = last_pc;
    }

    print_pc_update(cpu);
}

fn cmd_regs(cpu: &Mos6502) {
    println!("  PC -> 0x{:04x}", cpu.pc);
    println!("  SP -> 0x{:02x}", cpu.sp);
    println!("   A -> 0x{:02x}", cpu.a);
    println!("   X -> 0x{:02x}", cpu.x);
    println!("   Y -> 0x{:02x}", cpu.y);
    println!("   P -> 0x{:02x}", cpu.p.bits());
}

fn cmd_dumpmem(cpu: &Mos6502, addr: u16, count: u16) {
    for n in (0..count).step_by(4) {
        let a = addr + n;
        print!("  ${a:04x}:");
        for i in 0..4 {
            let v = cpu.bus.borrow_mut().read(a + i);
            print!(" {v:02x}");
        }
        println!();
    }
}

fn cmd_print_instr(cpu: &Mos6502) {
    let s = cpu.instr_repr(cpu.pc);
    println!(" ${:04x}: {s}", cpu.pc);
}

fn cmd_cont(cpu: &mut Mos6502, breakpoints: &mut BTreeSet<u16>) {
    let mut hit_bp = false;
    let mut last_pc = cpu.pc;
    let mut step_result = StepResult::Success;
    cpu.tk.borrow_mut().resume();
    loop {
        if breakpoints.contains(&cpu.pc) {
            hit_bp = true;
            break;
        } else if step_result != StepResult::Success || sigint_received() {
            break;
        }
        last_pc = cpu.pc;
        step_result = cpu.step();
    }
    cpu.tk.borrow_mut().pause();

    check_step_result(step_result);

    if hit_bp {
        println!("  Breakpoint at ${:04x} reached", cpu.pc);
        breakpoints.remove(&cpu.pc);
    } else if step_result != StepResult::Success {
        cpu.pc = last_pc;
    }

    print_pc_update(cpu);
}
