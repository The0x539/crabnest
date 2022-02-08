mod apu;
pub(super) mod ines;
mod io_reg;
mod pageforty;
mod ppu;

mod mapper {
    mod mmc1;
    pub(super) mod mmc3;
    pub(super) mod nrom;
    pub(super) mod sxrom;
}
