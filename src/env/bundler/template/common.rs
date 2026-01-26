pub const BUNDLE_MAGIC: &[u8; 8] = b"LUCIA\0\0\0";
pub const BUNDLE_VERSION: u16 = 0xc200;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct BundleFooter {
    pub magic: [u8; 8],
    pub version: u16,
    pub lucia_version: [u8; 8],
    pub lucia_uuid: [u8; 16],
    pub flags: u16,
    pub data_offset: u64,
}

#[allow(dead_code)]
impl BundleFooter {
    pub fn new(data_offset: u64, lucia_version: [u8; 8], lucia_uuid: [u8; 16]) -> Self {
        Self {
            magic: *BUNDLE_MAGIC,
            version: BUNDLE_VERSION,
            lucia_version,
            lucia_uuid,
            flags: 0,
            data_offset,
        }
    }

    pub fn size() -> usize {
        std::mem::size_of::<Self>()
    }

    pub fn as_bytes(&self) -> [u8; std::mem::size_of::<Self>()] {
        unsafe { std::mem::transmute(*self) }
    }

    pub fn from_bytes(buf: &[u8]) -> Option<Self> {
        if buf.len() != Self::size() {
            return None;
        }
        let footer = unsafe {
            std::ptr::read_unaligned(buf.as_ptr() as *const Self)
        };
        if &footer.magic != BUNDLE_MAGIC {
            return None;
        }
        Some(footer)
    }
}
