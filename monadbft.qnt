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
  id: int,                 // TODO unique id to identify the TC. Otherwise recursive data type
  c: Set[TimeoutMsg]       // TODO 2f+1
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

type Signature = Address // let's just add the sender address as a signature
type ProposalID = int

type Proposal = {
  view: int,
  proposal_id: ProposalID,
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
  proposal_id: ProposalID,
  state_hash: int,
  sigma: Signature,
}

type Tip = {
  view: int,
  proposal_id: ProposalID,
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

type TCAux = {
  id: int,
  view: int, // This is to just add the data that we need in Alg 4 line 4
}

type TimeoutMsg = {
  view: int,
  tip: int, //
  tc: TCAux,  // timeout certificate use tc.view. Needed due to cyclic data type
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


//
// Algorithm 2 (page 15)
//

type Ledger = List[Proposal] // TODO: what is the ledger? Alg 2 line 45

type Address = str

type LocalState = {
  // Variables from Algorithm 2
  high_tip: Tip,
  last_tc: TC,
  curView: int,
  ledger: Ledger,
  highest_voted_view: int,
  // recovering? perhaps Set[int] encoding the views where it is true

  // Variables from Algorithm 4
  votes: ProposalID -> Set[VoteMsg], // not listed on top but used in Alg 4 line 30

  // TODO: this is not in the paper, but we need it to identify the process
  address: Address, 
}

pure def initialize_process (address: str): LocalState =
  {
    high_tip: {
      view: -1,
      proposal_id: -1,
      block_header: {
        seq: -1,
        payload_hash: "",
        qc: Set(),
        block_hash: -1,
      },
      sigma: "",
      nec: Set(),
      tip_id: -1,
    },
    last_tc: {
      id: -1,
      c: Set(),
    },
    curView: 0,
    ledger: [],
    highest_voted_view: -1,
    votes: Map(), 
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
// Algorithm 5 has more responses

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


//
// Algorithm 3 (page 16)
//

// Alg 3 line 1
pure def find_high_tip (tc: TC): int =
  // TODO
  tc.id


// Alg 3 line 11
pure def verify_fresh_proposal(tip: Tip): bool =
  // TODO check if the proposal is fresh
  true


//
// Alg 5 (page 26)
//

// TODO: shall/can we ignore block recovery for now?
// TODO: there are more upons here.

// State machine

const validators: Set[Address]




type Environment = {
  system: Address -> LocalState,
  voteBuffer: Address -> Set[VoteMsg],
  propBuffer: Address -> Set[Proposal],
  timeoutBuffer: Address -> Set[TimeoutMsg],
  activeTimeouts: Address -> Set[int], // in which views the process is waiting for a timeout
}

var s: Environment


// TODO: I think we need to preload with a vote message towards the proposer
action init = all {
  s' = {
    system: validators.mapBy(v => initialize_process(v)),
    voteBuffer: validators.mapBy(v => Set()),
    propBuffer: validators.mapBy(v => 
      Set(build_proposal( 1, 
                          build_block(Set(), "first block"),
                          { id: 1, c: Set()}, 
                          Set()))),
    timeoutBuffer: validators.mapBy(v => Set()),
    activeTimeouts: validators.mapBy(v => Set()),
  }
}




pure def apply_effect (env: Environment, v: str, res: ConsensusResult): Environment =
// TODO
  val new = { ... env, system: env.system.set(v, res.state) }
  res.output.fold(new, (s, x) => {
    match x {
      | BroadcastProp(p) =>
          s // update propBuffers
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





// Example of acceleration: function that receives all votes
// TODO: we should think about recording the consensus inputs
pure def receive_all_votes (state: Environment, v: Address): Environment =
  state.voteBuffer.get(v).fold(state, (s, vote) => 
    receive_one_vote(s, v, vote)
  )

pure def receive_one_vote (state: Environment, v: Address, vote: VoteMsg): Environment =
  val consumed_vote_state = { ... state, voteBuffer: state.voteBuffer.set(v, state.voteBuffer.get(v).exclude(Set(vote))) }
  val res = consumed_vote_state.system.get(v).receive_vote(vote)
  apply_effect(consumed_vote_state, v, res)

action act_receive_some_vote (v: str) : bool = all {
  s.voteBuffer.get(v) != Set(),
  nondet vote = oneOf(s.voteBuffer.get(v))
  s' = receive_one_vote(s, v, vote)
}

pure def receive_one_timeout (state: Environment, v: Address, timeout: TimeoutMsg): Environment =
  val consumed_timeout_state = { ... state, timeoutBuffer: state.timeoutBuffer.set(v, state.timeoutBuffer.get(v).exclude(Set(timeout))) }
  val res = consumed_timeout_state.system.get(v).receive_timeout(timeout)
  apply_effect(consumed_timeout_state, v, res)

action act_receive_some_timeout (v: str) : bool = all {
  s.timeoutBuffer.get(v) != Set(),
  nondet tomsg = oneOf(s.timeoutBuffer.get(v))
  s' = receive_one_timeout(s, v, tomsg)
}

pure def receive_one_proposal (state: Environment, v: Address, prop: Proposal): Environment =
  val consumed_prop_state = { ... state, propBuffer: state.propBuffer.set(v, state.propBuffer.get(v).exclude(Set(prop))) }
  val res = consumed_prop_state.system.get(v).receive_proposal(prop)
  apply_effect(consumed_prop_state, v, res)

action act_receive_some_proposal (v: str) : bool = all {
  s.propBuffer.get(v) != Set(),
  nondet prop = oneOf(s.propBuffer.get(v))
  s' = receive_one_proposal(s, v, prop)
}

action act_timeout (v: str) : bool = all {
  s.activeTimeouts.get(v).filter(t => t == s.system.get(v).curView) != Set(),
  val consumed_timeout_state = { ... s, activeTimeouts: s.activeTimeouts.put(v, Set()) } // TODO: check whether we want this semantics
  val res = consumed_timeout_state.system.get(v).timeout_event()
  apply_output(v, res)
}

pure def apply_acceleration (state: Environment, msgs: Address -> Set[VoteMsg]): Environment =
  msgs.keys().fold(state, (s, v) => 
    msgs.get(v).fold(s, (s2, vote) => 
      val consumed_vote_state = { ... s2, voteBuffer: s2.voteBuffer.set(v, s2.voteBuffer.get(v).exclude(Set(vote))) }
      val res = consumed_vote_state.system.get(v).receive_vote(vote)
      apply_effect(consumed_vote_state, v, res)
    ))

// TODO: this needs to be checked. 
// non-deterministically pick a set of processes. For each, non-deterministically pick a set of votes
// and deliver them to the process in one action.
action acceleration = 
  nondet vals = validators.powerset().oneOf()
  val msgs = tuples(vals, s.voteBuffer.values()).filter(t => t._2.subseteq(s.voteBuffer.get(t._1)))
  val deliver = msgs.setToMap() // I am mis-using this here. I want a non-deterministic choice
  s' =  apply_acceleration(s, deliver)

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
