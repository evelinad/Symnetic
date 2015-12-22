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


class DHCPClient(name: String,
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
     CreateTag("L5",0),

     Allocate(OPCode, 8),
     Assign(OPCode, ConstantValue(1)),

     Allocate(HwType, 8),
     Assign(HwType, ConstantValue(1)),

     Allocate(HwAl, 8),
     Assign(HwAl, ConstantValue(6)),

     Allocate(HopC, 8),
     Assign(HopC, ConstantValue(0)),

     Allocate(TrID, 32),
     Assign(TrID, SymbolicValue()),

     Allocate(NrSec, 16),
     Assign(NrSec, ConstantValue(0)),

     Allocate(Flags, 16),
     Assign(Flags, ConstantValue(0)),

     Allocate(ClientIP, 32),
     Assign(ClientIP, ConstantValue(0)),

     Allocate(YourIP, 32),
     Assign(YourIP, ConstantValue(0)),

     Allocate(ServerIP, 32),
     Assign(ServerIP, ConstantValue(0)),

     Allocate(GwIP, 32),
     Assign(GwIP, ConstantValue(0)),

     Allocate(ClientHwAddr, 128),
     Assign(ClientHwAddr, SymbolicValue()),

     Allocate(SrvHostName, 512),
     Assign(SrvHostName, SymbolicValue()),

     Allocate(BootFn, 1024),
     Assign(BootFn, SymbolicValue()),

     Allocate(Magic, 32),
     Assign(Magic, ConstantValue(1666417251)),

     Allocate(Options, 32),
     Assign(Options, ConstantValue(1)),

     //CreateTag("END", L4Tag + 12000)
     CreateTag("END", L5Tag + 12000),
     Forward(outputPortName(0))

  ))

  override def outputPortName(which: Int = 0): String = s"$name-$which-out"
}

class DHCPClientElementBuilder(name: String, elementType: String)
  extends ElementBuilder(name, elementType) {

  override def buildElement: GenericElement = {
    new DHCPClient(name, elementType, getInputPorts, getOutputPorts, getConfigParameters)
  }
}

object DHCPClient {
  private var unnamedCount = 0

  private val genericElementName = "dhcpclient"

  private def increment {
    unnamedCount += 1
  }

  def getBuilder(name: String): DHCPClientElementBuilder = {
    increment ; new DHCPClientElementBuilder(name, "DHCPClient")
  }

  def getBuilder: DHCPClientElementBuilder =
    getBuilder(s"$genericElementName-$unnamedCount")
}
