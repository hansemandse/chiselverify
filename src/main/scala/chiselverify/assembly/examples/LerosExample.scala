package chiselverify.assembly.examples

import chiselverify.assembly.RandomHelpers.BigRange
import chiselverify.assembly.leros.Leros
import chiselverify.assembly.leros.Leros.simulationWrapper
import chiselverify.assembly.{Category, CategoryBlackList, CategoryDistribution, IODistribution, Instruction, Label, MemoryDistribution, Pattern, ProgramGenerator, intToBigIntOption}

object LerosExample extends App {
  val pg = ProgramGenerator(Leros)(CategoryBlackList(Category.EnvironmentCall))

  val program = pg.generate(simulationWrapper(200))
  println(program.pretty)
}

object LerosExamplePatternBased extends App {
  import chiselverify.assembly.leros.Instructions._
  import chiselverify.assembly.leros.Leros.{read, write}

  val pattern = Pattern(implicit context => Seq(
    Label("Hello"), Add(), In(20), Instruction.fill(20), Instruction.fillWithCategory(10)(Category.Logical), read, write
  ))

  val pg = ProgramGenerator(Leros)(
    CategoryDistribution(
      Category.Arithmetic -> 6,
      Category.Logical -> 2,
      Category.Input -> 1,
      Category.Output -> 1
    ),
    MemoryDistribution(
      BigRange(100, 200) -> 1,
      BigRange(4000) -> 1
    ),
    IODistribution(
      BigRange(20, 30) -> 1,
      BigRange(0xFF) -> 1
    )
  )

  val program = pg.generate(simulationWrapper(pattern))
  println(program.pretty)
}
