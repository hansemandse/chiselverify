package chiselverify.assembly.leros

import chiselverify.assembly.Label.LabelRecord
import chiselverify.assembly.{GeneratorContext, Instruction}
import chiselverify.assembly.{Constant, LabelReference, Register, Unsigned, Signed}
import chiselverify.assembly.Category

object Instructions {
    // no-op instruction
    case class Nop()(implicit context: GeneratorContext) extends Instruction(Category.Nop) {
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm = "nop"
    }

    // arithmetic instructions
    case class Add(rsIn: Option[Register] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Arithmetic
    ) {
      val rs = Register(QuickAccessMemory)(rsIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"add r$rs"
    }

    case class Addi(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Arithmetic, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"addi $imm"
    }

    case class Sub(rsIn: Option[Register] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Arithmetic
    ) {
      val rs = Register(QuickAccessMemory)(rsIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"sub r$rs"
    }

    case class Subi(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Arithmetic, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"subi $imm"
    }

    case class Shr(rsIn: Option[Register] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Arithmetic
    ) {
      val rs = Register(QuickAccessMemory)(rsIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"sra r$rs"
    }

    // logical instructions
    case class And(rsIn: Option[Register] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Logical
    ) {
      val rs = Register(QuickAccessMemory)(rsIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"and r$rs"
    }

    case class Andi(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Logical, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"andi $imm"
    }

    case class Or(rsIn: Option[Register] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Logical
    ) {
      val rs = Register(QuickAccessMemory)(rsIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"or r$rs"
    }

    case class Ori(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Logical, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"ori $imm"
    }

    case class Xor(rsIn: Option[Register] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Logical
    ) {
      val rs = Register(QuickAccessMemory)(rsIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"xor r$rs"
    }

    case class Xori(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Logical, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"xori $imm"
    }

    // branch instructions
    case class Br(immIn: Option[LabelRecord] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Branch, Category.Immediate
    ) {
      val imm = LabelReference(Signed(12))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"br $imm"
    }

    case class Brz(immIn: Option[LabelRecord] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Branch
    ) {
      val imm = LabelReference(Signed(12))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"brz $imm"
    }

    case class Brnz(immIn: Option[LabelRecord] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Branch
    ) {
      val imm = LabelReference(Signed(12))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"brnz $imm"
    }

    case class Brp(immIn: Option[LabelRecord] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Branch
    ) {
      val imm = LabelReference(Signed(12))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"brp $imm"
    }

    case class Brn(immIn: Option[LabelRecord] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Branch
    ) {
      val imm = LabelReference(Signed(12))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"brn $imm"
    }

    // jump instruction
    case class Jal(rsIn: Option[Register] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.JumpAndLink
    ) {
      val rs = Register(QuickAccessMemory)(rsIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"jal r$rs"
    }
    
    // load instructions
    case class Load(rsIn: Option[Register] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Load
    ) {
      val rs = Register(QuickAccessMemory)(rsIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"load r$rs"
    }

    case class LoadLbl(immIn: Option[LabelRecord] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Load
    ) {
      val imm = LabelReference(Signed(12))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"load <$imm"
    }

    case class LoadhLbl(immIn: Option[LabelRecord] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Load
    ) {
      val imm = LabelReference(Signed(12))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"loadh >$imm"
    }

    case class Loadi(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Load, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"loadi $imm"
    }

    case class Loadhi(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Load, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"loadhi $imm"
    }

    case class Loadh2i(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Load, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"loadh2i $imm"
    }

    case class Loadh3i(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Load, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"loadh3i $imm"
    }

    case class Ldaddr() extends Instruction(Category.Load) {
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm = "ldaddr"
    }

    case class Ldind(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Load, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"ldind $imm"
    }

    case class Ldindbu(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Load, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"ldindbu $imm"
    }
    
    // store instructions
    case class Store(rsIn: Option[Register] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Store
    ) {
      val rs = Register(QuickAccessMemory)(rsIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"store r$rs"
    }

    case class Stind(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Store, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"stind $imm"
    }

    case class Stindb(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Store, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"stindb $imm"
    }

    // I/O instructions
    case class In(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Input, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"in $imm"
    }

    case class Out(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.Output, Category.Immediate
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm: String = s"out $imm"
    }

    // system call instruction
    case class Scall(immIn: Option[BigInt] = None)(implicit context: GeneratorContext) extends Instruction(
      Category.EnvironmentCall
    ) {
      val imm = Constant(Unsigned(8))(immIn)
      override def apply()(implicit context: GeneratorContext): Instruction = copy()
      override def toAsm = s"scall $imm"
    }
}
