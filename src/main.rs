#![deny(rust_2018_idioms)]

use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

use clap::Parser;
use sdl2::Sdl;

mod membus;
mod memory;
mod mos6502;
mod nes;
mod reset_manager;
mod shell;
mod timekeeper;
mod util;

use memory::Memory;
use mos6502::Mos6502;
use reset_manager::ResetManager;
use timekeeper::Timekeeper;
use util::R;

const NES_NTSC_SYSCLK: f64 = 236.25 / 11. * 1_000_000.;
const HAWKNEST_MAGIC: [u8; 4] = [b'H', b'K', b'N', b'S'];
const INES_MAGIC: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];

fn hawknest_rom_load(f: File, rm: &R<ResetManager>, cpu: &mut Mos6502) -> io::Result<()> {
    cpu.paravirt = true;

    let bus = &mut *cpu.bus.borrow_mut();

    let mut cartrom = Memory::new(rm, 0x6000, false);
    cartrom.populate(f)?;
    cartrom.map(bus, 0xA000..=0xFFFF, 0);

    let ram = Memory::new(rm, 0x8000, true);
    ram.map(bus, 0x0000..=0x7FFF, 0);

    Ok(())
}

fn load_rom(
    path: &Path,
    sdl: &Sdl,
    rm: &R<ResetManager>,
    cpu: &mut Mos6502,
    palette_path: Option<&Path>,
    cscheme_path: Option<&Path>,
    scale: u32,
) -> io::Result<()> {
    let mut f = File::open(path)?;

    let mut magic = [0u8; 4];
    f.read_exact(&mut magic)?;
    if magic == HAWKNEST_MAGIC {
        hawknest_rom_load(f, rm, cpu)?;
        Ok(())
    } else if magic == INES_MAGIC {
        nes::ines::rom_load(f, sdl, rm, cpu, palette_path, cscheme_path, scale)?;
        Ok(())
    } else {
        Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("{path:?} does not appear to be in a valid ROM format"),
        ))
    }
}

#[derive(Parser, Debug)]
#[clap(version)]
struct Args {
    /// Start the shell immediately
    #[clap(short, long)]
    interactive: bool,
    /// Use the NES palette at <PATH>
    #[clap(short, long, value_name = "PATH")]
    palette: Option<PathBuf>,
    /// Use the NES controller scheme at <PATH>
    #[clap(short, long, value_name = "PATH")]
    cscheme: Option<PathBuf>,
    /// Scale the NES output by <INT>
    #[clap(short, long, value_name = "INT", default_value = "3")]
    scale: u32,

    rom_path: PathBuf,
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    let sdl = sdl2::init().expect("Couldn't initialize SDL");
    let events = sdl.event().expect("Couldn't initialize SDL events");

    let watch = events.add_event_watch(|ev| {
        if matches!(ev, sdl2::event::Event::Quit { .. }) {
            println!("Goodbye!");
            std::process::exit(0)
        }
    });
    std::mem::forget(watch);

    let rm = ResetManager::new();
    let tk = Timekeeper::new(&rm, NES_NTSC_SYSCLK);
    let mut cpu = Mos6502::new(&rm, &tk, &[/* TODO: catch the rest of the args */]);

    load_rom(
        &args.rom_path,
        &sdl,
        &rm,
        &mut cpu,
        args.palette.as_deref(),
        args.cscheme.as_deref(),
        args.scale,
    )?;

    rm.borrow_mut().issue_reset();
    cpu.reset();
    cpu.tk.borrow_mut().pause();
    shell::run_shell(&mut cpu, args.interactive);

    Ok(())
}
