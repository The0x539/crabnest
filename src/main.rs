#![deny(rust_2018_idioms)]
#![allow(dead_code)]

use std::cell::RefCell;
use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use clap::Parser;

mod ines;
mod membus;
mod memory;
mod mos6502;
mod reset_manager;
mod shell;
mod timekeeper;

mod nes {
    pub mod io_reg;
    pub mod ppu;
}

use memory::Memory;
use mos6502::Mos6502;
use reset_manager::ResetManager;
use timekeeper::Timekeeper;

type R<T> = Rc<RefCell<T>>;
fn r<T>(val: T) -> R<T> {
    Rc::new(RefCell::new(val))
}

const NES_NTSC_SYSCLK: f64 = 236.25 / 11. * 1_000_000.;
const HAWKNEST_MAGIC: [u8; 4] = [b'H', b'K', b'N', b'S'];
const INES_MAGIC: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];

fn hawknest_rom_load(
    mut f: File,
    _path: &Path,
    rm: &R<ResetManager>,
    cpu: &R<Mos6502>,
) -> io::Result<()> {
    let cpu = cpu.borrow_mut();
    let bus = &mut *cpu.bus.borrow_mut();

    let cartrom = Memory::new(rm, 0x6000, false);
    f.read_exact(&mut cartrom.borrow_mut().bytes)?;
    Memory::map(&cartrom, bus, 0xA000, 0x6000, 0);

    let ram = Memory::new(rm, 32768, true);
    Memory::map(&ram, bus, 0, 32768, 0);

    Ok(())
}

fn load_rom(
    path: &Path,
    rm: &R<ResetManager>,
    cpu: &R<Mos6502>,
    _palette_path: &Path,
    _cscheme_path: &Path,
    _scale: u32,
) -> io::Result<()> {
    let mut f = File::open(path)?;

    let mut magic = [0u8; 4];
    f.read_exact(&mut magic)?;
    if magic == HAWKNEST_MAGIC {
        hawknest_rom_load(f, path, rm, cpu)?;
        Ok(())
    } else if magic == INES_MAGIC {
        todo!()
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
    #[clap(short, long, value_name = "PATH", default_value = "palette")]
    palette: PathBuf,
    /// Use the NES controller scheme at <PATH>
    #[clap(short, long, value_name = "PATH", default_value = "cscheme")]
    cscheme: PathBuf,
    /// Scale the NES output by <INT>
    #[clap(short, long, value_name = "INT", default_value = "1")]
    scale: u32,

    rom_path: PathBuf,
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    let sdl = sdl2::init().expect("Couldn't initialize SDL");

    let rm = ResetManager::new();
    let tk = Timekeeper::new(&rm, 1.0 / NES_NTSC_SYSCLK);
    let cpu = Mos6502::new(&rm, &tk, &[/* TODO: catch the rest of the args */]);

    load_rom(
        &args.rom_path,
        &rm,
        &cpu,
        &args.palette,
        &args.cscheme,
        args.scale,
    )?;

    rm.borrow_mut().issue_reset();
    cpu.borrow_mut().reset();
    cpu.borrow().tk.borrow_mut().pause();
    shell::run_shell(cpu, args.interactive);

    Ok(())
}
