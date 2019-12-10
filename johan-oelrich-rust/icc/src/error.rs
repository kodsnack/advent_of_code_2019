#[derive(Debug, Clone)]
pub enum Error {
  Address(i64),
  Operand(i64),
  // Input(i64),
  Mode,
}
impl std::fmt::Display for Error {
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
    match self {
      Error::Address(addr) => write!(fmt, "Error accessing address: {}.", addr),
      Error::Operand(op) => write!(fmt, "Error accessing operator {}.", op),
      //   Error::Input(addr) => write!(fmt, "Failed to read input at address {}.", addr),
      Error::Mode => write!(fmt, "Invalid addressing mode."),
    }
  }
}
