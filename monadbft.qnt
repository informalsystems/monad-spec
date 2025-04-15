// -*- mode: Bluespec; -*-

module monadbft {
  import basicSpells.* from "./basicSpells"

//
// Data structures (page 8)
//

type QC = Set[VoteMsg]           // TODO 2f+1

pure def parent (qc1: QC, qc2: QC): bool =
  // TODO check if qc1 is a parent of qc2
  true

pure def extend (qc1: QC, qc2: QC): bool =
  // TODO 
  true

pure def conflicting_qcs (qc1: QC, qc2: QC): bool =
  // TODO check if qc1 and qc2 are conflicting
  and {
    not(extend(qc1, qc2)),
    not(extend(qc2, qc1)),
  }

type TC = {
  id: int,                  // TODO unique id to identify the TC. Otherwise recursive data type
  C: Set[TimeoutMsg]        // TODO 2f+1
}
type NEC = Set[NoEndorsementMsg] // TODO f+1

type Block = {
  seq: int,
  payload: str,
  payload_hash: str,
  qc: QC,
  block_hash: int
}

type BlockHeader = {
  seq: int,
  payload_hash: str,
  qc: QC,
  block_hash: int
}

type Signature

type Proposal = {
  view: int,
  proposal_id: int,
  block: Block,
  sigma: Signature,
  tc: TC,
  nec: NEC,
}

pure def conflicting_props (p1: Proposal, p2: Proposal): bool =
  and {
    not(extend(p1.block.qc, p2.block.qc)),
    not(extend(p2.block.qc, p1.block.qc)),

  }

// TODO Fresh proposal
// TODO Reproposal

type VoteMsg = {
  view: int,
  seq: int,
  proposal_id: int,
  state_hash: int,
  sigma: Signature,
}

type Tip = {
  view: int,
  proposal_id: int,
  block_header: BlockHeader,
  sigma: Signature,
  nec: NEC,
  tip_id: int, // added to avoid recursive data types
}

pure def high_tip (tips: Set[Tip]): Tip =
  // TODO a tip with the highest view
  tips.getOnlyElement()

type NoEndorsementMsg = {
  view: int,
  highqc_view: int,
  sigma: Signature,
}

type TimeoutMsg = {
  view: int,
  tip: int, //
  tc: int,  // timeout certificate
  sigma: Signature,
}


}