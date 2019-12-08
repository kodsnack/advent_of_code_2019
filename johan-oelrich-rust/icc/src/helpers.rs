use super::error::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum State {
  Output(usize, i64),
  Input,
  Running,
  Halted,
  Ready,
  Crashed,
}

pub enum Instruction {
  Add((Value, Value, Value)),
  Multiply((Value, Value, Value)),
  JumpIfTrue((Value, Value)),
  JumpIfFalse((Value, Value)),
  LessThan((Value, Value, Value)),
  Equals((Value, Value, Value)),
  Input(Value),
  Output(Value),
  Halt,
}

impl Instruction {
  pub fn get_op_modes(op: i64) -> Result<(OpCode, Mode, Mode, Mode), Error> {
    let op_val = op % 100;
    let mode0 = (op % 1000) / 100;
    let mode1 = (op % 10000) / 1000;
    let mode2 = (op % 100_000) / 10_000;
    Ok((
      OpCode::get_op(op_val)?,
      Mode::src_of_val(mode0)?,
      Mode::src_of_val(mode1)?,
      Mode::dest_of_val(mode2)?,
    ))
  }
}

#[derive(Debug, Clone)]
pub enum OpCode {
  Add = 1,
  Multiply = 2,
  Read = 3,
  Write = 4,
  JumpIfTrue = 5,
  JumpIfFalse = 6,
  LessThan = 7,
  Equals = 8,
  Halt = 99,
}
impl OpCode {
  pub fn get_op(val: i64) -> Result<OpCode, Error> {
    match val {
      1 => Ok(OpCode::Add),
      2 => Ok(OpCode::Multiply),
      3 => Ok(OpCode::Read),
      4 => Ok(OpCode::Write),
      5 => Ok(OpCode::JumpIfTrue),
      6 => Ok(OpCode::JumpIfFalse),
      7 => Ok(OpCode::LessThan),
      8 => Ok(OpCode::Equals),
      99 => Ok(OpCode::Halt),
      _ => Err(Error::Operand(val)),
    }
  }
}

#[derive(Debug, Clone)]
pub enum Mode {
  Direct,
  Stored,
}
impl Mode {
  pub fn dest_of_val(val: i64) -> Result<Mode, Error> {
    match val {
      0 => Ok(Mode::Direct),
      1 => Ok(Mode::Stored),
      _ => Err(Error::Mode),
    }
  }
  pub fn src_of_val(val: i64) -> Result<Mode, Error> {
    match val {
      0 => Ok(Mode::Stored),
      1 => Ok(Mode::Direct),
      _ => Err(Error::Mode),
    }
  }
}

#[derive(Debug, Clone)]
pub enum Value {
  Direct(i64),
  Stored(i64),
}
impl std::fmt::Display for Value {
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
    match self {
      Value::Direct(val) => write!(fmt, "Direct({})", val),
      Value::Stored(addr) => write!(fmt, "Stored({})", addr),
    }
  }
}
