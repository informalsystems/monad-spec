// -*- mode: Bluespec; -*-

module monadbft {
  import basicSpells.* from "./basicSpells"

//
// Data structures (page 8)
//

type QC = Set[VoteMsg]           // TODO 2f+1

pure def view(qc: QC): int =
  // TODO get the view of the qc
  0

pure def proposal_id(qc: QC): int =
  // TODO get the proposal id of the qc
  0

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
  c: Set[TimeoutMsg]        // TODO 2f+1
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

type Signature = str

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

//
// Algorithm 1 (page 13)
//

pure def build_block(qc: QC, payload: str): Block =
  // TODO build a block
  {
    seq: 0,
    payload: payload,
    payload_hash: "",
    qc: qc,
    block_hash: 0,
  }

pure def build_proposal(view: int, block: Block, tc: TC, nec: NEC): Proposal =
  // TODO build a proposal
  {
    view: view,
    proposal_id: 0,
    block: block,
    sigma: "",
    tc: tc,
    nec: nec,
  }

pure def safety_check(p: Proposal): bool =
  // TODO check if the proposal is safe
  true

pure def update_tip(p: Proposal): int =
// TODO: returns tip (not tip_id) in the paper
  if (p.view == p.block.qc.view() + 1 or p.nec != Set())
    p.tc.id // TODO: spec says "tip of p"
  else
    find_high_tip(p.tc) 

pure def parent_id (p: Proposal): int =
// cut down the QC to a singleton
  p.block.qc.proposal_id()


// Algorithm 2 (page 15)

type Ledger = List[Proposal] // TODO: what is the ledger?

type Address = str

type LocalState = {
  high_tip: Tip,
  last_tc: TC,
  curView: int,
  ledger: Ledger,
  highest_voted_view: int,
  // recovering? perhaps Set[int] encoding the views where it is true
  address: Address, // TODO: this is not in the paper, but we need it to identify the process
}

pure def initialize_process (address: str): LocalState =
  {
    high_tip: {
      view: 0,
      proposal_id: 0,
      block_header: {
        seq: 0,
        payload_hash: "",
        qc: Set(),
        block_hash: 0,
      },
      sigma: "",
      nec: Set(),
      tip_id: 0,
    },
    last_tc: {
      id: 0,
      c: Set(),
    },
    curView: 0,
    ledger: [],
    highest_voted_view: -1,
    address: address,
  }

pure def is_leader(s: LocalState): bool =
  // TODO check if the node is the leader
  s.address == "v1" // TODO: this is just a placeholder

type ConsensusOutput =
  | BroadcastProp(Proposal)
  | BroadCastTimeout(TimeoutMsg)
  | Commit(Proposal)
  | EarlyConfirmation(Proposal)
  | Confirmation(Proposal)
  | SendVoteToNextLeader((Address, VoteMsg))
// Algorithm 5 has many more responses

type ConsensusResult = {
  output: Set[ConsensusOutput],
  state: LocalState,
}


// Alg 2 line 3
pure def receive_vote (s: LocalState, v: VoteMsg): ConsensusResult =
  // TODO
  { state: s, output: Set() }


// Alg 2 line 10 / line 52 (leader/ non-leader)
pure def receive_timeout (s: LocalState, t: TimeoutMsg): ConsensusResult =
  if (s.is_leader())
    // TODO line 11
    { state: s, output: Set() }
  else
    // TODO line 53
    { state: s, output: Set() }


// Alg 2 line 34
pure def receive_proposal (s: LocalState, p: Proposal): ConsensusResult =
  // TODO
  { state: s, output: Set() }


// Alg 2 line 49
pure def timeout_event (s: LocalState): ConsensusResult =
  // TODO
  { state: s, output: Set() }


// Algorithm 3 (page 16)

// Alg 3 line 1
pure def find_high_tip (tc: TC): int =
  // TODO
  tc.id


// Alg 3 line 11
pure def verify_fresh_proposal(tip: Tip): bool =
  // TODO check if the proposal is fresh
  true


// State machine

const validators: Set[str]

type Environment = {
  system: str -> LocalState,
  voteBuffer: str -> Set[VoteMsg],
  propBuffer: str -> Set[Proposal],
  timeoutBuffer: str -> Set[TimeoutMsg],
}

var s: Environment


// TODO: I think we need to preload with a vote message towards the proposer
action init = all {
  s' = {
    system: validators.mapBy(v => initialize_process(v)),
    voteBuffer: validators.mapBy(v => Set()),
    propBuffer: validators.mapBy(v => Set()),
    timeoutBuffer: validators.mapBy(v => Set()),
  }
}




pure def apply_effect (env: Environment, v: str, res: ConsensusResult): Environment =
// TODO
  val new = { ... env, system: env.system.set(v, res.state) }
  res.output.fold(new, (s, x) => {
    match x {
      | BroadcastProp(p) =>
          s
      | BroadCastTimeout(t) =>
          s
      | Commit(p) =>
          s
      | EarlyConfirmation(p) =>
          s
      | Confirmation(p) =>
          s
      | SendVoteToNextLeader(am) =>
          s
    }
  })

action apply_output(v: str, res: ConsensusResult) : bool = all {
  s' = apply_effect(s, v, res)
}

action act_receive_some_vote (v: str) : bool = all {
  s.voteBuffer.get(v) != Set(),
  nondet vote = oneOf(s.voteBuffer.get(v))
  val res = s.system.get(v).receive_vote(vote)
  apply_output(v, res)
}

action act_receive_some_timeout (v: str) : bool = all {
  s.timeoutBuffer.get(v) != Set(),
  nondet timeout = oneOf(s.timeoutBuffer.get(v))
  val res = s.system.get(v).receive_timeout(timeout)
  apply_output(v, res)
}

action act_receive_some_proposal (v: str) : bool = all {
  s.propBuffer.get(v) != Set(),
  nondet prop = oneOf(s.propBuffer.get(v))
  val res = s.system.get(v).receive_proposal(prop)
  apply_output(v, res)
}

action act_timeout (v: str) : bool = all {
  val res = s.system.get(v).timeout_event()
  apply_output(v, res)
}

action step =
  nondet v = oneOf(validators)
  any {
    act_receive_some_vote(v),
    act_receive_some_timeout(v),
    act_receive_some_proposal(v),
    act_timeout(v)
  }

} // end of module monadbft



module example {
  import monadbft(validators = Set("v1", "v2", "v3", "v4")).* 
  
} // end of module example
