package chiselverify.assembly


/**
  * Functions for creating symbolic labels in generated assembly code
  * they are represented by instruction objects whose assembly code is
  * the label itself
  */
object Label {

  case class LabelRecord(id: String)

  // auto generated label factory
  def apply(): InstructionFactory with Categorizable = {
    Pattern(Category.Label)(implicit context => {
      val lbl = s"RANDOM_LABEL_${context.labelCounter.inc()}"
      context.jumpTargets.append(LabelRecord(lbl))
      Seq(Label.create(lbl))
    })
  }
  // user defined labels factory
  def apply(lbl: String): Pattern = {
    Pattern(implicit context => {
      context.jumpTargets.append(LabelRecord(lbl))
      Seq(Label.create(lbl))
    })
  }

  // create a anonymous instruction instance with the label name set as the assembly representation
  private def create(id: String)(implicit context: GeneratorContext): Instruction = {
    new Instruction(Category.Label) {
      override def apply()(implicit context: GeneratorContext): Instruction = Label.create(id)
      override def toAsm: String = s"$id:"
    }
  }
}
