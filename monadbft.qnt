// -*- mode: Bluespec; -*-

module monadbft {
  import basicSpells.* from "./basicSpells"



//
//
// Consensus / State machine interface CSMI
// functional side
//
// Here one needs to specify 
// - the types: Address, LocalState, Message, TimeoutEvent, ConsensusOutput
// - the functions: initialize_process, receive_message, fire_timeout_event (that define the
//   message handlers and timeout handlers of the algorithm)  
//

// CSMI
type Address = str

// CSMI
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

// CSMI
type Message =
  | Prop(Proposal)
  | Vote(VoteMsg)
  | Timeout(TimeoutMsg)
  | NoEndorsement(NoEndorsementMsg)

// CSMI
type TimeoutEvent = int

// CSMI
type ConsensusOutput =
  | BroadcastProp(Proposal)
  | BroadCastTimeout(TimeoutMsg)
  | Commit(Proposal)
  | EarlyConfirmation(Proposal)
  | Confirmation(Proposal)
  | SendVoteToNextLeader(VoteMsg)
// Algorithm 5 has more responses

type ConsensusResult = {
  output: Set[ConsensusOutput],
  state: LocalState,
}


// CSMI
// This initializes process that is identified by address
pure def initialize_process (address: Address): LocalState =
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

// CSMI
// This is the function that contains all the message handlers of the algorithm
pure def receive_message(state: LocalState, msg: Message): ConsensusResult =
  match msg {
    | Prop(p) =>
        state.receive_proposal(p)
    | Vote(v) =>
        state.receive_vote(v)
    | Timeout(t) =>
        state.receive_timeout(t)
    | NoEndorsement(n) =>
        // TODO
        { state: state, output: Set() }
  }

// CSMI
// This is the function that contains all the timeout handlers of the algorithm
pure def fire_timeout_event(state: LocalState, timeout: TimeoutEvent): ConsensusResult =
  if (state.curView == timeout)
    state.timeout_event()
  else
    { state: state, output: Set() }

// End of CSMI functional side




//
// Specifics of MonadBFT
//

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

pure def is_leader(s: LocalState): bool =
  // TODO check if the node is the leader
  s.address == "v1" // TODO: this is just a placeholder

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

// End of specifics of MonadBFT


//
//
// State machine
//
//


const validators: Set[Address]

type Environment = {
  system: Address -> LocalState,
  msgBuffer: Address -> Set[Message],
  activeTimeouts: Address -> Set[TimeoutEvent], 
}

var s: Environment

//
//
// Consensus / State machine interface CSMI
// effect side
//
//

// CSMI
action init = all {
  s' = {
    system: validators.mapBy(v => initialize_process(v)),
    msgBuffer: validators.mapBy(v => 
      Set(Prop(build_proposal(  1, 
                                build_block(Set(), "first block"),
                                { id: 1, c: Set()}, 
                                Set())))),
    activeTimeouts: validators.mapBy(v => Set()),
  }
}


// CSMI
/// Interface consensus algorithm / state machine: how consensus algorithm effects the environment 
pure def apply_effect (env: Environment, v: Address, res: ConsensusResult): Environment =
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

// end of CSMI effect side


//
// General state machine
//


//
// Standard interleaving semantics (one process receives one message/timeout in one action)
//

pure def receive_one_message (state: Environment, v: Address, msg: Message): Environment =
  val consumed_msg_state = { ... state, msgBuffer: state.msgBuffer.set(v, state.msgBuffer.get(v).exclude(Set(msg))) }
  val res = state.system.get(v).receive_message(msg)
  apply_effect(state, v, res)


pure def fire_one_timeout_event (state: Environment, v: Address, timeout: TimeoutEvent): Environment =
  val consumed_timeout_state = { ... state, activeTimeouts: state.activeTimeouts.set(v, state.activeTimeouts.get(v).exclude(Set(timeout))) }
  val res = consumed_timeout_state.system.get(v).fire_timeout_event(timeout)
  apply_effect(consumed_timeout_state, v, res)

action act_receive_some_msg (v: Address) : bool = all {
  s.msgBuffer.get(v) != Set(),
  nondet msg = oneOf(s.msgBuffer.get(v))
  s' = receive_one_message(s, v, msg)
}

action act_timeout (v: Address) : bool = all {
  s.activeTimeouts.get(v) != Set(),
  nondet timeout = oneOf(s.activeTimeouts.get(v))
  s' = fire_one_timeout_event(s, v, timeout)
}

action step =
  nondet v = oneOf(validators)
  any {
    act_receive_some_msg(v),
    act_timeout(v)
  }

//
// Accelerated semantics (multiple process may receive multiple messages/timeouts in one action)
//

/// Returns the new state after each message in the msgs map is delivered to the respective receiver
pure def apply_acceleration_msgs (state: Environment, msgs: Address -> Set[Message]): Environment =
  msgs.keys().fold(state, (s, v) => 
    msgs.get(v).fold(s, (s2, msg) => 
      receive_one_message(s2, v, msg) 
    ))

pure def apply_acceleration_timeout (state: Environment, timeouts: Address -> Set[TimeoutEvent]): Environment =
  timeouts.keys().fold(state, (s, v) => 
    timeouts.get(v).fold(s, (s2, timeout) => 
      fire_one_timeout_event(s2, v, timeout)
    ))

// non-deterministically pick a set of processes. For each, non-deterministically pick a set of votes
// and deliver them to the process in one action.
action acceleration_msgs = 
  nondet msgs = validators.map(v => s.msgBuffer.get(v).powerset().exclude(Set(Set())).map(m => (v, m)))
                .flatten().powerset().oneOf()
  val deliver = msgs.setToMap() 
  s' =  apply_acceleration_msgs(s, deliver)

action acceleration_timeouts = 
  nondet timeouts = validators.map(v => s.activeTimeouts.get(v).powerset().exclude(Set(Set())).map(t => (v, t)))
                .flatten().powerset().oneOf()
  val deliver = timeouts.setToMap() 
  s' =  apply_acceleration_timeout(s, deliver)


action accelerated_step = any {
  acceleration_msgs,
  acceleration_timeouts,
}  




} // end of module monadbft



module example {
  import monadbft(validators = Set("v1", "v2", "v3", "v4")).* 
  
} // end of module example
