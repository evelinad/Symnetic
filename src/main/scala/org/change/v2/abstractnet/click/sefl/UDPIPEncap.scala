package org.change.v2.abstractnet.click.sefl

import org.change.v2.abstractnet.generic.{ConfigParameter, ElementBuilder, GenericElement, Port}
import org.change.v2.analysis.expression.concrete._
import org.change.v2.analysis.expression.concrete.nonprimitive.:@
import org.change.v2.analysis.processingmodels.instructions._
import org.change.v2.analysis.processingmodels.{Instruction, LocationId}
import org.change.v2.util.conversion.RepresentationConversion._
import org.change.v2.util.canonicalnames._
import org.change.v2.analysis.memory.TagExp._
import org.change.v2.analysis.memory.Tag
import org.change.v2.util.conversion.RepresentationConversion._


class UDPIPEncap(name: String,
                   elementType: String,
                   inputPorts: List[Port],
                   outputPorts: List[Port],
                   configParams: List[ConfigParameter])
  extends GenericElement(name,
    elementType,
    inputPorts,
    outputPorts,
    configParams) {

  /**
   * Every Click element modeled in Symnet has an "instructions" map.
   *
   * This maps every input port (identified by a LocationId) to the instruction performed.
   *
   * Knowing the input port id, one can get its corresponding LocationId by calling "inputPortName()".
   * The same holds true for output port names (used usually in conjuction with the "Forward instruction").
   *
   * Usually the sequence of instructions performed on a given input port ends with a "Forward". This
   * ensures the state gets propagated further, otherwise it becomes stuck (there are no instructions
   * to be executed).
   *
   * One can use the parameters provided in the Click file by issuing "configParams(id).value" where "id"
   * is the 0-based index of the parameter. For instance: "ipToNumber( configParams(0).value )" assumes the
   * first parameter of this click element is an IPv4 address and converts it to a long value.
   * @return
   */
  override def instructions: Map[LocationId, Instruction] = Map(
   inputPortName(0) -> InstructionBlock(

     CreateTag("L4", Tag("L5") - 64),

    // 67 and 68 ?
     Allocate(Tag("L4") + UDPSrcOffset, 16),
     Assign(Tag("L4") + UDPSrcOffset, ConstantValue(configParams(1).value.toInt)),

     Allocate(Tag("L4") + UDPDstOffset, 16),
     Assign(Tag("L4") + UDPDstOffset, ConstantValue(configParams(3).value.toInt)),

     Allocate(Tag("L4") + UDPLengthOffset, 16),
     Assign(Tag("L4") + UDPLengthOffset, SymbolicValue()),

     Allocate(Tag("L4") + UDPChecksumOffset, 16),
     Assign(Tag("L4") + UDPChecksumOffset, SymbolicValue()),

     CreateTag("L3", Tag("L4") - 160),

     Allocate(Tag("L3") + IPVersionOffset, 4),
     Assign(Tag("L3") + IPVersionOffset, SymbolicValue()),

     Allocate(Tag("L3") + IPHeaderLengthOffset, 4),
     Assign(Tag("L3") + IPHeaderLengthOffset, SymbolicValue()),

     Allocate(Tag("L3") + DSCPOffset, 6),
     Assign(Tag("L3") + DSCPOffset, SymbolicValue()),

     Allocate(Tag("L3") + ECNOffset, 2),
     Assign(Tag("L3") + ECNOffset, SymbolicValue()),

     Allocate(Tag("L3") + IPLengthOffset, 16),
     Assign(Tag("L3") + IPLengthOffset, ConstantValue(255)),

     Allocate(Tag("L3") + IPIDOffset, 16),
     Assign(Tag("L3") + IPIDOffset, SymbolicValue()),

     Allocate(Tag("L3") + IPFlagsOffset, 3),
     Assign(Tag("L3") + IPFlagsOffset, SymbolicValue()),

     Allocate(Tag("L3") + FragmentOffsetOffset,13),
     Assign(Tag("L3") + FragmentOffsetOffset, SymbolicValue()),

     Allocate(Tag("L3") + TTLOffset, 8),
     Assign(Tag("L3") + TTLOffset, SymbolicValue()),

     Allocate(Tag("L3") + ProtoOffset, 8),
     Assign(Tag("L3") + ProtoOffset, SymbolicValue()),

     Allocate(Tag("L3") + HeaderChecksumOffset, 16),
     Assign(Tag("L3") + HeaderChecksumOffset, SymbolicValue()),

      Allocate(Tag("L3") + IPSrcOffset, 32),
     Assign(Tag("L3") + IPSrcOffset, ConstantValue(ipToNumber(configParams(0).value))),

     Allocate(Tag("L3") + IPDstOffset, 32),
     Assign(Tag("L3") + IPDstOffset, ConstantValue(ipToNumber(configParams(2).value))),

     Forward(outputPortName(0))
  ))

  override def outputPortName(which: Int = 0): String = s"$name-$which-out"
}

class UDPIPEncapElementBuilder(name: String, elementType: String)
  extends ElementBuilder(name, elementType) {

  override def buildElement: GenericElement = {
    new UDPIPEncap(name, elementType, getInputPorts, getOutputPorts, getConfigParameters)
  }
}

object UDPIPEncap {
  private var unnamedCount = 0

  private val genericElementName = "udpipencap"

  private def increment {
    unnamedCount += 1
  }

  def getBuilder(name: String): UDPIPEncapElementBuilder = {
    increment ; new UDPIPEncapElementBuilder(name, "UDPIPEncap")
  }

  def getBuilder: UDPIPEncapElementBuilder =
    getBuilder(s"$genericElementName-$unnamedCount")
}
