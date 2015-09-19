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
 *
 * A state is a symbolic execution path.
 *
 * @param memory Current memory state.
 * @param history A history of what elements were explored.
 * @param errorCause If a state failed, the cause is explained.
 * @param instructionHistory A complete sequence of the instructions that led to this state.
 * @param perStateInstructions Installed whenever an invariant is to be checked.
 */
case class State(memory: MemorySpace = MemorySpace.clean,
                 history: List[LocationId] = Nil,
                 errorCause: Option[ErrorCause] = None,
                 instructionHistory: List[Instruction] = Nil,
                 perStateInstructions: Map[LocationId, Instruction] = Map.empty) {

  def location: LocationId = history.head

  def forwardTo(locationId: LocationId): State = State(memory, locationId :: history, errorCause, instructionHistory)

  def status = errorCause.getOrElse("OK")

  override def toString = s"Path ($status) {\n$memory\n} End Of Path Desc"

  def addInstructionToHistory(i: Instruction) = State(memory, history, errorCause, i :: instructionHistory)

  def eliminatePerStateInstructions(whatInstructions: Iterable[LocationId]): State = State(
    memory,
    history,
    errorCause,
    instructionHistory,
    perStateInstructions -- whatInstructions
  )

  def addPerStateInstruction(li: (LocationId, Instruction)) = State (
    memory,
    history,
    errorCause,
    instructionHistory,
    perStateInstructions + li
  )

  def executePerStateInstructions(verbose: Boolean = true): (List[State], List[State]) = {
    val (toExecute, toPostpone) = perStateInstructions.partition(_._1 == location)
    val (ok, failed) = InstructionBlock(
      toExecute.values
    )(this, verbose)

    (ok.map(_.eliminatePerStateInstructions(toExecute.keys)), failed)
  }
}

object State {
 def bigBang: State = {
   val bigBang = State(MemorySpace.clean)

   val afterBigBang = InstructionBlock (
     Assign(IPSrcString, SymbolicValue()),
     Assign(IPDstString, SymbolicValue()),
     Assign(PortSrcString, SymbolicValue()),
     Assign(PortDstString, SymbolicValue()),
     Assign(L4ProtoString, SymbolicValue()),
     Assign(IPVersionString, SymbolicValue()),

     CreateTag("START",0),
     CreateTag("L3", 0),

     Allocate(IPVersion, 4),
     Assign(IPVersion, SymbolicValue()),

     Allocate(Proto, 8),
     Assign(Proto, SymbolicValue()),

     Allocate(IPSrc, 32),
     Assign(IPSrc, SymbolicValue()),
     Allocate(IPDst, 32),
     Assign(IPDst, SymbolicValue()),

     Allocate(TTL, 8),
     Assign(TTL, ConstantValue(255)),

     Allocate(IPLength, 16),
     Assign(IPLength, SymbolicValue()),

     Allocate(IPHeaderLength, 4),
     Assign(IPHeaderLength, SymbolicValue()),

     Allocate(HeaderChecksum,16),
     Assign(HeaderChecksum, SymbolicValue()),

     Allocate(IPID, 16),
     Assign(IPID, SymbolicValue()),

     CreateTag("L4", L3Tag + 160),

     Allocate(TcpSrc, 16),
     Assign(TcpSrc, SymbolicValue()),

     Allocate(TcpDst, 16),
     Assign(TcpDst, SymbolicValue()),

     Allocate(TcpSeq, 32),
     Assign(TcpSeq, SymbolicValue()),

     Allocate(TcpAck, 32),
     Assign(TcpAck, SymbolicValue()),

     Allocate(TcpDataOffset, 4),
     Assign(TcpDataOffset, ConstantValue(160)),

     Allocate(TcpReserved,3),
     Assign(TcpReserved,SymbolicValue()),

     Allocate(TcpFlagNS,1),
     Assign(TcpFlagNS,ConstantValue(0)),
     Allocate(TcpFlagCWR,1),
     Assign(TcpFlagCWR,ConstantValue(0)),
     Allocate(TcpFlagECE,1),
     Assign(TcpFlagECE,ConstantValue(0)),
     Allocate(TcpFlagURG,1),
     Assign(TcpFlagURG,ConstantValue(0)),
     Allocate(TcpFlagACK,1),
     Assign(TcpFlagACK,SymbolicValue()),
     Allocate(TcpFlagACK,1),
     Assign(TcpFlagACK,SymbolicValue()),
     Allocate(TcpFlagSYN,1),
     Assign(TcpFlagSYN,SymbolicValue()),
     Allocate(TcpFlagRST,1),
     Assign(TcpFlagRST,ConstantValue(0)),
     Allocate(TcpFlagPSH,1),
     Assign(TcpFlagPSH,ConstantValue(0)),

     //CreateTag("PAYLOAD", :+:(L4Tag,:@(TcpDataOffset)),
     //Allocate(Tag("PAYLOAD"),12000),
     //Assign(Tag("PAYLOAD"),SymbolicValue()),
     CreateTag("END", L4Tag + 12000)
   )(bigBang, true)

   afterBigBang._1.head
 }
}
