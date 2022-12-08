package verifyTests.assembly

import chiselverify.assembly.{Category, CategoryWhiteList, GeneratorContext, Instruction}
import chiselverify.assembly.leros.Leros
import org.scalatest.flatspec.AnyFlatSpec

class InstructionTest extends AnyFlatSpec {
  behavior of "Instruction producers"

  it should "select randomly between different instructions" in {
    import chiselverify.assembly.leros.Instructions.{Add, Br, Jal, Or}

    implicit val context = GeneratorContext(Leros, Seq())
    val options = Seq(Add(), Jal(), Br())
    val selected = Instruction.select(options:_*)
    val produced = selected.produce().head
    assert(produced.isInstanceOf[Add] || produced.isInstanceOf[Jal] || produced.isInstanceOf[Br])
    assert(!produced.isInstanceOf[Or])
  }

  it should "pick a random instruction of the correct category" in {
    implicit val context = GeneratorContext(Leros, Seq())
    val instr = Instruction.ofCategory(Category.Arithmetic).produce().head
    assert(instr.isOfCategory(Category.Arithmetic))
  }

  it should "produce a random sequence of instructions" in {
    implicit val context = GeneratorContext(Leros, Seq(CategoryWhiteList(Category.Arithmetic, Category.Logical)))
    val instr = Instruction.fill(10).produce()
    assert(instr.length == 10)
  }

  it should "produce a random sequence of instructions of the correct category" in {
    implicit val context = GeneratorContext(Leros, Seq(CategoryWhiteList(Category.Arithmetic, Category.Logical)))
    val instr = Instruction.fillWithCategory(10)(Category.Arithmetic).produce()
    assert(instr.foldLeft(true){ case (acc, i) => acc && i.isOfCategory(Category.Arithmetic) })
    assert(instr.length == 10)
  }
}
