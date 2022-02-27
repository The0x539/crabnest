use super::ines::RomInfo;

macro_rules! mapper {
    ($id:literal, $setup_fn:ident) => {
        inventory::submit!($crate::nes::mapper::Mapper {
            id: $id,
            setup_fn: setup,
        });
    };
}

mod axrom;
mod mmc1;
pub(super) mod mmc3;
mod nrom;
mod sxrom;
mod uxrom;

type SetupFn = fn(RomInfo<'_>) -> Result<(), &'static str>;

#[derive(Copy, Clone)]
pub struct Mapper {
    id: u16,
    setup_fn: SetupFn,
}

inventory::collect!(Mapper);

impl Mapper {
    pub fn get(id: u16) -> Option<Self> {
        for mapper in inventory::iter::<Self> {
            if mapper.id == id {
                return Some(*mapper);
            }
        }
        None
    }

    pub fn setup(&self, info: RomInfo<'_>) -> Result<(), &'static str> {
        (self.setup_fn)(info)
    }
}
