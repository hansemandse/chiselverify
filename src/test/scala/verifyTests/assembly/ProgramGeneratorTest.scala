package verifyTests.assembly

import chiselverify.assembly.RandomHelpers.BigRange
import chiselverify.assembly.{IODistribution, MemoryDistribution, ProgramGenerator}
import chiselverify.assembly.leros.Leros
import org.scalatest.flatspec.AnyFlatSpec

class ProgramGeneratorTest extends AnyFlatSpec {
  behavior of "Program generator"

  it should "generate reproducible programs" in {
    val pg1 = ProgramGenerator(Leros)(
      MemoryDistribution(
        BigRange(0, 0x1000) -> 1,
        BigRange(0x7FFF0,0xFFFFF) -> 1
      ),
      IODistribution(
        BigRange(0, 0xF) -> 99,
        BigRange(0xFF) -> 1
      )
    )
    val prog1 = pg1.generate(300, 0xdeadbeef)

    val pg2 = ProgramGenerator(Leros)(
      MemoryDistribution(
        BigRange(0, 0x1000) -> 1,
        BigRange(0x7FFF0,0xFFFFF) -> 1
      ),
      IODistribution(
        BigRange(0, 0xF) -> 99,
        BigRange(0xFF) -> 1
      )
    )
    val prog2 = pg2.generate(300, 0xdeadbeef)

    prog1.instructions
      .zip(prog2.instructions)
      .zipWithIndex
      .foreach { case ((instr1, instr2), index) =>
        assert(instr1.toAsm == instr2.toAsm, s"got an invalid instruction at line $index")
      }
  }
}
