package chiselverify.assembly

import chiselverify.assembly.Label.LabelRecord
import chiselverify.assembly.RandomHelpers.{BigRange, pow2, randSplit}

package object leros {

  // leros uses the lower part of main memory as a register file
  object QuickAccessMemory extends RegisterFile(Seq.fill(256)(new Register {}): _*)

  object Leros extends InstructionSet {
    import chiselverify.assembly.leros.Instructions._

    // wrapper patterns to ensure proper simulation of generated programs
    def simulationWrapper(body: InstructionFactory): Pattern = Pattern(implicit c => Seq(body, Scall(0)))
    def simulationWrapper(n: Int): Pattern = Pattern(implicit c => Seq(Instruction.fill(n), Scall(0)))

    // simple arithmetic instruction patterns
    val add  = Pattern(Category.Arithmetic)(implicit context => Seq(Add()))
    val addi = Pattern(Category.Arithmetic, Category.Immediate)(implicit context => Seq(Addi()))
    val sub  = Pattern(Category.Arithmetic)(implicit context => Seq(Sub()))
    val subi = Pattern(Category.Arithmetic, Category.Immediate)(implicit context => Seq(Subi()))
    val shr  = Pattern(Category.Arithmetic)(implicit context => Seq(Shr()))

    // simple logical instruction patterns
    val and  = Pattern(Category.Logical)(implicit context => Seq(And()))
    val andi = Pattern(Category.Logical, Category.Immediate)(implicit context => Seq(Andi()))
    val or   = Pattern(Category.Logical)(implicit context => Seq(Or()))
    val ori  = Pattern(Category.Logical, Category.Immediate)(implicit context => Seq(Ori()))
    val xor  = Pattern(Category.Logical)(implicit context => Seq(Xor()))
    val xori = Pattern(Category.Logical, Category.Immediate)(implicit context => Seq(Xori()))

    // simple load instruction patterns
    val loadLbl  = Pattern(Category.Load)(implicit context => Seq(LoadLbl()))
    val loadhLbl = Pattern(Category.Load)(implicit context => Seq(LoadhLbl()))
    val load     = Pattern(Category.Load)(implicit context => Seq(Load()))
    val loadi    = Pattern(Category.Load, Category.Immediate)(implicit context => Seq(Loadi()))
    val loadhi   = Pattern(Category.Load, Category.Immediate)(implicit context => Seq(Loadhi()))
    val loadh2i  = Pattern(Category.Load, Category.Immediate)(implicit context => Seq(Loadh2i()))
    val loadh3i  = Pattern(Category.Load, Category.Immediate)(implicit context => Seq(Loadh3i()))
    val ldaddr   = Pattern(Category.Load)(implicit context => Seq(Ldaddr()))
    val ldind    = Pattern(Category.Load, Category.Immediate)(implicit context => Seq(Ldind()))
    val lindbu   = Pattern(Category.Load, Category.Immediate)(implicit context => Seq(Ldindbu()))

    // simple store instruction patterns
    val store  = Pattern(Category.Store)(implicit context => Seq(Store()))
    val stind  = Pattern(Category.Store, Category.Immediate)(implicit context => Seq(Stind()))
    val stindb = Pattern(Category.Store, Category.Immediate)(implicit context => Seq(Stindb()))

    // simple I/O instruction patterns
    val in  = Pattern(Category.Input, Category.Immediate)(implicit context => Seq(In()))
    val out = Pattern(Category.Output, Category.Immediate)(implicit context => Seq(Out()))

    // simple system call instruction pattern
    val scall = Pattern(Category.EnvironmentCall)(implicit context => Seq(Scall()))

    // the read pattern sets the accumulator to a random base address and executes the load with offset
    val read = Pattern(Category.Load)(implicit context => {
      val address = context.nextMemoryAddress(Seq())
      val (base, offset) = randSplit(address, context.rng)(Unsigned(32), Signed(8))
      Seq(
        Loadi((base & 0xFF)),
        Loadhi(((base >> 8) & 0xFF)),
        Loadh2i(((base >> 16) & 0xFF)),
        Loadh3i(((base >> 24) & 0xFF)),
        Ldaddr(),
        Instruction.select(Ldind(offset), Ldindbu(offset))
      )
    })

    // the write pattern sets the accumulator to a random address and executes the store with offset
    val write = Pattern(Category.Store)(implicit context => {
      val address = context.nextMemoryAddress(Seq())
      val (base, offset) = randSplit(address, context.rng)(Unsigned(32), Signed(8))
      Seq(
        Loadi((base & 0xFF)),
        Loadhi(((base >> 8) & 0xFF)),
        Loadh2i(((base >> 16) & 0xFF)),
        Loadh3i(((base >> 24) & 0xFF)),
        Ldaddr(),
        Instruction.select(Stind(offset), Stindb(offset))
      )
    })

    // the jump and link pattern requests a jump target, sets the accumulator to the target address and executes the jump
    val jumpAndLink = Pattern(Category.JumpAndLink)(implicit context => {
      val target = context.nextJumpTarget()
      Seq(LoadLbl(target), LoadhLbl(target), Jal())
    })

    // branches rely on the generator context to deliver a target label
    val branch = Pattern(Category.Branch)(implicit context => {
      val target = context.nextJumpTarget()
      Seq(Instruction.select(Br(target), Brz(target), Brnz(target), Brp(target), Brn(target)))
    })

    // leros has full 16-bit address space
    override val memoryAddressSpace: BigRange = BigRange(0, pow2(16))

    // leros has a 8-bit IO port address space
    override val inputOutputAddressSpace: BigRange = BigRange(0, pow2(8))

    // all instructions
    override val instructions = Seq(
      // arithmetic instructions
      add,
      addi,
      sub,
      subi,
      shr,
      // logical instructions
      and,
      andi,
      or,
      ori,
      xor,
      xori,
      // load instructions
      load,
      loadi,
      loadhi,
      loadh2i,
      loadh3i,
      ldaddr,
      // store instructions
      store,
      // I/O instructions
      in,
      out,
      // other instruction patterns
      jumpAndLink,
      read,
      write,
      branch,
      scall
    )
  }
}
