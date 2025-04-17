// -*- mode: Bluespec; -*-

module monadbft {
  import basicSpells.* from "./basicSpells"

  val ERROR_CODE = (-256)
  val NO_HIGH_TIP = (-255)

//
// Data structures (page 8)
//

type Proposal_ID = {seq: int, view: int, byzantine: int} 
type Signature = Address // let's just add the sender address as a signature
type ProposalID = int


/// ⟨view, proposal id, block header, σ, nec⟩ (page 8)
type Tip = {
  view: int,
  proposal_id: ProposalID,
  block_header: BlockHeader,
  sigma: Signature,
  nec: Option[NEC],
  tip_id: int, // added to avoid recursive data types
}

/// ⟨view, seq, proposal id, state hash, σ⟩ (page 8)
type VoteMsg = {
  view: int,
  seq: int,
  proposal_id: ProposalID,
  state_hash: int,
  sigma: Signature,
}

/// ⟨view, highqc_view, σ⟩ (page 9)
type NoEndorsementMsg = {
  view: int,
  highqc_view: int,
  sigma: Signature,
}

/// ⟨view, tip, tc, σ⟩ (page 9)
type TimeoutMsg = {
  view: int,
  tip: Tip,
  tc: TCAux,  // timeout certificate use tc.view. Needed to avoid cyclic data type
  sigma: Signature,
}

/// ⟨view, seq, proposal id, state hash, Σ⟩ (page 8)
type QC = {
  c: Set[VoteMsg],       // TODO 2f+1
  view: int,
  seq: int,
  proposal_id: ProposalID,
  state_hash: int,
  sigma: Signature, 
} 

/// TC = ⟨view, tips, high_tip, Σ⟩ (page 9)
type TC = {
  id: int,                 // TODO unique id to identify the TC. Otherwise recursive data type
  view: int,
  high_tip: Tip,
  tips: Set[Tip],
  sigma: Signature,
  c: Set[TimeoutMsg],       // TODO 2f+1 
}

/// Auxiliary type for timeout certificate to avoid circular data types 
type TCAux = {
  id: int,
  view: int,
}

/// ⟨view, highqc view, Σ⟩ (page 9)
type NEC = {
  view: int,
  highqc_view: int,
  sigma: Signature,
  c: Set[NoEndorsementMsg]
}

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

/// ⟨view, proposal id, block, σ, tc, nec⟩ (page 8)
type Proposal = {
  view: int,
  proposal_id: Proposal_ID,
  block: Block,
  sigma: Signature,
  tc: Option[TC],
  nec: Option[NEC],
}

type Address = str
type Ledger = List[Proposal] 

type LocalState = {
  // Variables from Algorithm 2
  high_tip: Option[Tip],
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

/*
/// Extract the view out of a certificate
pure def get_view(cert) = {
  // First, check that the certificate is not empty.
  if (cert == Set()) ERROR_CODE
  else {
    // Extract the set of view numbers from each vote message in the certificate.
    val views = cert.fold(Set(), (acc, m) => acc.setAdd(m.view))
    // If the certificate is well-formed all vote messages should share the same view.
    if (views.size() != 1) ERROR_CODE
    else views.getOnlyElement()
  }
}

/// Extract the sequence number of votes out of a quorum certificate
pure def get_seq_qc(qc: QC): int = {
  if (qc == Set()) ERROR_CODE
  else {
    // Extract the set of view numbers from each vote message in the QC.
    val seqs = qc.fold(Set(), (acc, vote) => acc.setAdd(vote.seq))
    // If the QC is well-formed all vote messages should share the same view.
    if (seqs.size() != 1) ERROR_CODE
    else seqs.getOnlyElement()
    }
}

/// Extract the proposal id out of a quorum certificate
pure def get_proposal_id_qc(qc: QC): int = {
  if (qc == Set()) ERROR_CODE
  else {
    val ids = qc.fold(Set(), (acc, vote) => acc.setAdd(vote.proposal_id))
    if (ids.size() != 1) ERROR_CODE
    else ids.getOnlyElement() 
  }
}

pure def get_highqc_view_nec(nec: NEC): int = {
  if (nec == Set()) ERROR_CODE
  else {
    val views = nec.fold(Set(), (acc, m) => acc.setAdd(m.view))
    if (views.size() != 1) ERROR_CODE
    else views.getOnlyElement()
  }
}
*/

/// Hash is modeled as id
pure def hash(s: str): str = {
  s
}

/// Compare two quorum certificates
pure def compare_qc(qc1: QC, qc2: QC): bool =
  and {
    qc1.view == qc2.view,
    qc1.seq == qc2.seq,
    qc1.proposal_id == qc2.proposal_id,
    qc1.state_hash == qc2.state_hash,
    qc1.sigma == qc2.sigma,
    qc1.c.size() == qc2.c.size(),
    qc1.c.fold(true, (acc, x) => acc and qc2.c.contains(x)),
  }


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

pure def conflicting_props (p1: Proposal, p2: Proposal): bool =
  and {
    not(extend(p1.block.qc, p2.block.qc)),
    not(extend(p2.block.qc, p1.block.qc)),

  }


/// Extract the tips from a set of timeout messages
/// This function retruns a set of Tip. 
pure def get_tips_from_timeouts(msgs: Set[TimeoutMsg]): Set[Tip]= {
  msgs.fold(Set(), (acc, msg) => acc.setAdd(msg.tip))
}


//
// Algorithm 1 (page 13)
//

/// BuildBlock function
/// Pseudocode reference: Algorithm 1, page 13 (line 1)
pure def build_block(qc: QC, payload: str): Block =
  {
  seq: qc.seq + 1,
  payload: payload,
  payload_hash: hash(payload),
  qc: qc,
  block_hash: 0 //TODO: what to put here 
}

/// Form a proposal out of a block and a set of certificates
/// Pseudocode reference: Algorithm 1, page 13 (line 6)
pure def build_proposal(view: int, block: Block, tc: TC, nec: NEC): Proposal = {
  val id = {seq: block.seq, view: view, byzantine: 0}
  {
    view: view,
    proposal_id: id,
    block: block,
    sigma: "",
    tc: tc,
    nec: nec,
  }
}

/// Check if the proposal is safe
/// Pseudocode reference: Algorithm 1, page 13 (line 10)
pure def safety_check(p: Proposal): bool = {
  if (p.block.seq != p.block.qc.seq + 1) //Question: typo in the paper (line 11)
    false
  else if (p.view == p.block.qc.view + 1)
    true
  else if (p.tc != None and p.view == p.tc.unwrap().view + 1){
    if ((p.block.qc != p.tc.high_tip.block.qc) or 
        (p.tc.unwrap().high_tip != find_high_tip(p.tc)))
      false
    else if (p.nec != None)
     and {
        p.nec.unwrap().view == p.view,
        p.nec.unwrap().highqc_view == p.block.qc.view,
      }
    else (p.block.block_hash == p.tc.high_tip.block.block_hash)
  }
  else false
}
  
//
// Algorithm 3 (page 16)
//

/// Utility: find the tip with the highest view out of a set of tip
pure def highest_tip (tips: Set[Tip]): Tip = {
  // argmax (t∈tips) t.view 
  val max_tip = tips.fold(tips.getOnlyElement(), (acc, t) => {
    if (t.view > acc.view) t
    else acc
  })
  max_tip
}

// Alg 3 line 11
pure def verify_fresh_proposal(tip: Tip): bool ={
  if (tip.view == tip.block_header.qc.view + 1)
    true
  else match tip.nec {
    | None =>
        false
    | Some(nec) =>
      and {
      tip.view == nec.view,
      tip.block_header.qc.view == nec.highqc_view,
    }
  }
}


/// Alg 3 line 1
/// Question: the side effects of this function seem wrong.
pure def find_high_tip(tc: TC): Option[Tip] = {
  val tips = tc.tips
  // The first if else block (L3 -> L7) is skipped because until we figure out if
  // we need it

  if (not (tips.forall(t => t.verify_fresh_proposal()))) None
  else {
    // argmax (t∈tips) t.view
    val max_tip = highest_tip(tips)
    Some(max_tip)
  }
}



pure def update_tip(p: Proposal): int =
// TODO: returns tip (not tip_id) in the paper
  1

pure def parent_id (p: Proposal): int =
// cut down the QC to a singleton
  p.block.qc.proposal_id


//
// Algorithm 2 (page 15)
//

/*
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
*/

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
    system: validators.mapBy(v => v), //initialize_process(v)),
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
  state.voteBuffer.get(v).fold(state, (s, vote) => {
    val consumed_vote_state = { ... s, voteBuffer: s.voteBuffer.set(v, s.voteBuffer.get(v).exclude(Set(vote))) }
    val res = consumed_vote_state.system.get(v).receive_vote(vote)
    apply_effect(consumed_vote_state, v, res)
  })


action act_receive_some_vote (v: str) : bool = all {
  s.voteBuffer.get(v) != Set(),
  nondet vote = oneOf(s.voteBuffer.get(v))
  val consumed_msg_state = { ... s, voteBuffer: s.voteBuffer.set(v, s.voteBuffer.get(v).exclude(Set(vote))) }
  val res = consumed_msg_state.system.get(v).receive_vote(vote) 
  apply_output(v, res)
}

action act_receive_some_timeout (v: str) : bool = all {
  s.timeoutBuffer.get(v) != Set(),
  nondet tomsg = oneOf(s.timeoutBuffer.get(v))
  val consumed_msg_state = { ... s, timeoutBuffer: s.timeoutBuffer.set(v, s.timeoutBuffer.get(v).exclude(Set(tomsg))) }
  val res = consumed_msg_state.system.get(v).receive_timeout(tomsg)
  apply_output(v, res)
}

action act_receive_some_proposal (v: str) : bool = all {
  s.propBuffer.get(v) != Set(),
  nondet prop = oneOf(s.propBuffer.get(v))
  val consumed_msg_state = { ... s, propBuffer: s.propBuffer.set(v, s.propBuffer.get(v).exclude(Set(prop))) }
  val res = consumed_msg_state.system.get(v).receive_proposal(prop)
  apply_output(v, res)
}

action act_timeout (v: str) : bool = all {
  s.activeTimeouts.get(v).filter(t => t == s.system.get(v).curView) != Set(),
  val consumed_timeout_state = { ... s, activeTimeouts: s.activeTimeouts.put(v, Set()) } // TODO: check whether we want this semantics
  val res = consumed_timeout_state.system.get(v).timeout_event()
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
