package verifyTests.assembly

import chiselverify.assembly.RandomHelpers.BigRange
import chiselverify.assembly.Label.LabelRecord
import chiselverify.assembly.leros.{Leros, QuickAccessMemory}
//import chiselverify.assembly.rv32i.IntegerRegister
import chiselverify.assembly.{Constant, IllegalRegisterInitializer, LabelReference, Register, Signed, Unsigned, WidthViolation, leros}
import org.scalatest.flatspec.AnyFlatSpec
import chiselverify.assembly.Width

class FieldsTest extends AnyFlatSpec {
  behavior of "Constant field"

  implicit val context = chiselverify.assembly.GeneratorContext(Leros, Seq())

  it should "generate a random constant (unsigned)" in {
    val constant = Constant(Unsigned(8))(None)
    assert(constant >= 0 && constant < 256)
  }

  it should "generate a random constant (signed)" in {
    val constant = Constant(Signed(8))(None)
    assert(constant > -129 && constant < 127)
  }

  it should "accept initializer (unsigned)" in {
    val constant = Constant(Unsigned(8))(Some(BigInt(10)))
    assert(constant == 10)
  }

  it should "accept initializer (signed)" in {
    val constant = Constant(Signed(8))(Some(BigInt(-10)))
    assert(constant == -10)
  }

  it should "not allow an out of bounds initializer (unsigned)" in {
    var checker = true
    try {
      val constant = Constant(Unsigned(8))(Some(BigInt(256)))
      checker = false
    } catch {
      case e: WidthViolation =>
    }
    assert(checker)
  }

  it should "not allow an out of bounds initializer (signed)" in {
    var checker = true
    try {
      val constant = Constant(Signed(8))(Some(BigInt(-129)))
      checker = false
    } catch {
      case e: WidthViolation =>
    }
    assert(checker)
  }

  behavior of "Label reference field"

  it should "generate a random label" in {
    val lf = LabelReference(Unsigned(8))(None)
    assert(BigRange(Unsigned(8)).contains(BigInt(lf)))
  }

  it should "accept a label" in {
    val lf = LabelReference(Unsigned(8))(Some(LabelRecord("HelloWorld")))
    assert(lf == "HelloWorld")
  }

  behavior of "Register field"

  it should "produce a random register" in {
    val reg = Register(QuickAccessMemory)(None)
    assert(QuickAccessMemory.registers.indices.contains(reg))
  }

  it should "accept an initializer" in {
    val reg = Register(QuickAccessMemory)(Some(QuickAccessMemory.registers(10)))
    assert(reg == 10)
  }
/*
  it should "not allow an illegal initializer" in {
    var checker = true
    try {
      val reg = Register(leros.QuickAccessMemory)(Some(rv32i.IntegerRegister.zero))
      checker = false
    } catch {
      case e: IllegalRegisterInitializer =>
    }
    assert(checker)
  }
*/
}
