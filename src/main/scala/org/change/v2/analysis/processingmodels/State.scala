package org.change.v2.analysis.processingmodels

import org.change.v2.analysis.memory.MemorySpace

import org.change.v2.util.canonicalnames._
import org.change.v2.analysis.processingmodels.instructions._
import org.change.v2.analysis.expression.concrete._
import org.change.v2.analysis.expression.concrete.nonprimitive._
import org.change.v2.analysis.memory.TagExp._
import org.change.v2.analysis.memory.Tag


/**
 * Author: Radu Stoenescu
 * Don't be a stranger,  symnetic.7.radustoe@spamgourmet.com
 */
case class State(memory: MemorySpace = MemorySpace.clean,
                 history: List[LocationId] = Nil,
                 errorCause: Option[ErrorCause] = None,
                 instructionHistory: List[Instruction] = Nil) {
  def location: LocationId = history.head
  def forwardTo(locationId: LocationId): State = State(memory, locationId :: history, errorCause, instructionHistory)
  def status = errorCause.getOrElse("OK")
  override def toString = s"Path ($status) {\n$memory\n} End Of Path Desc"
  def addInstructionToHistory(i: Instruction) = State(memory, history, errorCause, i :: instructionHistory)
}

object State {

 def clean = State(MemorySpace.clean)

 def bigBang: State = {
   val bigBang = State(MemorySpace.clean)

   val afterBigBang = InstructionBlock (
     CreateTag("START",0),

     CreateTag("L2", 0),

     Allocate(EtherDst, 48),
     Assign(EtherDst, SymbolicValue()),

     Allocate(EtherSrc, 48),
     Assign(EtherSrc, SymbolicValue()),

     Allocate(EtherType, 16),
     Assign(EtherType, SymbolicValue()),

     Allocate(PCP, 3),
     Assign(PCP, SymbolicValue()),

     Allocate(DEI, 1),
     Assign(DEI, SymbolicValue()),

     Allocate(VLANTag, 12),
     Assign(VLANTag, SymbolicValue()),

     //CreateTag("L3", 0),
     CreateTag("L3", L2Tag + 160),

     Allocate(IPVersion, 4),
     Assign(IPVersion, SymbolicValue()),

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
     Assign(Tag("L3") + IPSrcOffset, SymbolicValue()),
     Allocate(Tag("L3") + IPDstOffset, 32),
     Assign(Tag("L3") + IPDstOffset, SymbolicValue()),

     CreateTag("L4", L3Tag + 160),

    // 67 and 68 ?
     Allocate(UDPSrc, 16),
     Assign(UDPSrc, SymbolicValue()),

     Allocate(UDPDst, 16),
     Assign(UDPDst, SymbolicValue()),

     Allocate(UDPLength, 16),
     Assign(UDPLength, SymbolicValue()),

     Allocate(UDPChecksum, 16),
     Assign(UDPChecksum, SymbolicValue()),

     //Allocate(TcpSrc, 16),
     //Assign(TcpSrc, SymbolicValue()),

     //Allocate(TcpDst, 16),
     //Assign(TcpDst, SymbolicValue()),

     //Allocate(TcpSeq, 32),
     //Assign(TcpSeq, SymbolicValue()),

     //Allocate(TcpAck, 32),
     //Assign(TcpAck, SymbolicValue()),

     //Allocate(TcpDataOffset, 4),
     //Assign(TcpDataOffset, ConstantValue(160)),

     //Allocate(TcpReserved,3),
     //Assign(TcpReserved,SymbolicValue()),

     //Allocate(TcpFlagNS,1),
     //Assign(TcpFlagNS,ConstantValue(0)),
     //Allocate(TcpFlagCWR,1),
     //Assign(TcpFlagCWR,ConstantValue(0)),
     //Allocate(TcpFlagECE,1),
     //Assign(TcpFlagECE,ConstantValue(0)),
     //Allocate(TcpFlagURG,1),
     //Assign(TcpFlagURG,ConstantValue(0)),
     //Allocate(TcpFlagACK,1),
     //Assign(TcpFlagACK,SymbolicValue()),
     //Allocate(TcpFlagACK,1),
     //Assign(TcpFlagACK,SymbolicValue()),
     //Allocate(TcpFlagSYN,1),
     //Assign(TcpFlagSYN,SymbolicValue()),
     //Allocate(TcpFlagRST,1),
     //Assign(TcpFlagRST,ConstantValue(0)),
     //Allocate(TcpFlagPSH,1),
     //Assign(TcpFlagPSH,ConstantValue(0)),

     //CreateTag("PAYLOAD", :+:(L4Tag,:@(TcpDataOffset)),
     //Allocate(Tag("PAYLOAD"),12000),
     //Assign(Tag("PAYLOAD"),SymbolicValue()),
     CreateTag("L5", L4Tag + 64),

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
     CreateTag("END", L5Tag + 12000)
   )(bigBang, true)

   afterBigBang._1.head
 }
}
