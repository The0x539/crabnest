mod a12_watcher;
pub(crate) mod apu;
pub(super) mod ines;
mod io_reg;
mod pageforty;
pub(crate) mod ppu;

mod mapper {
    pub(super) mod axrom;
    mod mmc1;
    pub(super) mod mmc3;
    pub(super) mod nrom;
    pub(super) mod sxrom;
    pub(super) mod uxrom;
}
