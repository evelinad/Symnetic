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


class DHCPServerOffer(name: String,
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
     Constrain(OPCode,:==:(ConstantValue(1))),
     Constrain(Options, :==:(ConstantValue(1))),

     DestroyTag("START"),
     Deallocate(EtherDst, 48),
     Deallocate(EtherSrc, 48),
     Deallocate(EtherType, 16),
     Deallocate(PCP, 3),
     Deallocate(DEI, 1),
     Deallocate(VLANTag, 12),

     Deallocate(IPHeaderLength, 4),
     Deallocate(DSCP, 6),
     Deallocate(ECN, 2),
     Deallocate(IPLength, 16),
     Deallocate(IPID, 16),
     Deallocate(IPFlags, 3),
     Deallocate(FragmentOffset, 13),
     Deallocate(TTL, 8),
     Deallocate(Proto, 8),
     Deallocate(HeaderChecksum, 16),
     Deallocate(IPSrc, 32),
     Deallocate(IPDst, 32),

     Deallocate(IPVersion, 4),
     Deallocate(Proto, 8),
     Deallocate(TTL, 8),
     Deallocate(IPLength, 16),
     Deallocate(IPHeaderLength, 4),
     Deallocate(HeaderChecksum,16),
     Deallocate(DSCP, 6),
     Deallocate(ECN, 2),
     Deallocate(IPID, 16),
     Deallocate(IPFlags, 3),
     Deallocate(FragmentOffset, 13),
     Deallocate(TTLOffset, 8),
     Deallocate(ProtoOffset, 8),
     Deallocate(HeaderChecksumOffset, 16),
     Deallocate(IPSrc, 32),
     Deallocate(IPDst, 32),

     Deallocate(UDPSrc, 16),
     Deallocate(UDPDst, 16),
     Deallocate(UDPLength, 16),
     Deallocate(UDPChecksum, 16),
     Deallocate(OPCode, 8),
     Deallocate(HwType, 8),
     Deallocate(HwAl, 8),
     Deallocate(HopC, 8),
     Deallocate(TrID, 32),
     Deallocate(NrSec, 16),
     Deallocate(Flags, 16),
     Deallocate(ClientIP, 32),
     Deallocate(YourIP, 32),
     Deallocate(ServerIP, 32),
     Deallocate(GwIP, 32),
     Deallocate(ClientHwAddr, 128),
     Deallocate(SrvHostName, 512),
     Deallocate(BootFn, 1024),
     Deallocate(Magic, 32),
     Deallocate(Options, 32),
     DestroyTag("END"),

     CreateTag("L5",0),

     Allocate(OPCode, 8),
     Assign(OPCode, ConstantValue(2)),

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
     Assign(YourIP, ConstantValue(ipToNumber(configParams(0).value))),

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
     Assign(Options, ConstantValue(2)),

     //CreateTag("END", L4Tag + 12000)
     CreateTag("END", L5Tag + 12000),
     Forward(outputPortName(0))
    )
  )
  override def outputPortName(which: Int = 0): String = s"$name-$which-out"
}

class DHCPServerOfferElementBuilder(name: String, elementType: String)
  extends ElementBuilder(name, elementType) {

  override def buildElement: GenericElement = {
    new DHCPServerOffer(name, elementType, getInputPorts, getOutputPorts, getConfigParameters)
  }
}

object DHCPServerOffer {
  private var unnamedCount = 0

  private val genericElementName = "dhcpserveroffer"

  private def increment {
    unnamedCount += 1
  }

  def getBuilder(name: String): DHCPServerOfferElementBuilder = {
    increment ; new DHCPServerOfferElementBuilder(name, "DHCPServerOffer")
  }

  def getBuilder: DHCPServerOfferElementBuilder =
    getBuilder(s"$genericElementName-$unnamedCount")
}
