// -*- mode: Bluespec; -*-

module monadbft {

import basicSpells.* from "./basicSpells"

val BLOCK_PAYLOAD = "VALID_TRANSACTION" //TODO: this is just a placeholder

//
// Data structures (page 8)
//

type Address = str
type Ledger = List[Proposal] 
type ProposalID = {seq: int, view: int, byzantine: int} 
type Signature = Address // let's just add the sender address as a signature
type StateHash = List[ProposalID]


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
  state_hash: StateHash,
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
  tc: TCAux,  //timeout certificate use tc.view. Needed to avoid cyclic data type. TODO: Maybe this should be an option
  sigma: Signature,
}

/// ⟨view, seq, proposal id, state hash, Σ⟩ (page 8)
type QC = {
  c: Set[VoteMsg],       
  view: int,
  seq: int,
  proposal_id: ProposalID,
  state_hash: StateHash,
  sigma: Signature, 
} 

/// TC = ⟨view, tips, high_tip, Σ⟩ (page 9)
type TC = {
  //id: int,          // TODO This might not be needed
  view: int,
  high_tip: Option[Tip],  
  tips: Set[Tip],
  sigma: Signature,
  c: Set[TimeoutMsg],       
}

/// Auxiliary type for timeout certificate to avoid circular data types 
type TCAux = {
  view: int,
  high_tip: Option[Tip],
  tips: Set[Tip],
  sigma: Signature,
}

/// ⟨view, highqc view, Σ⟩ (page 9)
type NEC = {
  view: int,
  highqc_view: int,
  sigma: Signature,
  c: Set[NoEndorsementMsg]
}

type BlockHash = {
  seq: int,
  payload_hash: str,
  qc: QC
}

/// ⟨seq, payload, payload hash, qc, block hash⟩ (page 8)
type Block = {
  seq: int,
  payload: str,
  payload_hash: str,
  qc: QC,
  block_hash: BlockHash
}

/// ⟨seq, payload hash, qc, block hash⟩ (page 8)
type BlockHeader = {
  seq: int,
  payload_hash: str,
  qc: QC,
  block_hash: BlockHash
}

/// ⟨view, proposal id, block, σ, tc, nec⟩ (page 8)
type Proposal = {
  view: int,
  proposal_id: ProposalID,
  block: Block,
  sigma: Signature,
  tc: Option[TC],
  nec: Option[NEC],
}

type LocalState = {
  // Variables from Algorithm 2
  high_tip: Option[Tip], //NB: sure this is an option
  tip: Option[Tip], //NB: sure this is an option
  last_tc: Option[TC],
  curView: int,
  ledger: Ledger,
  highest_voted_view: int,
  // recovering? perhaps Set[int] encoding the views where it is true
  // Variables from Algorithm 4
  votes: ProposalID -> Set[VoteMsg], // not listed on top but used in Alg 4 line 30
  proposals: Set[Proposal],
  timeouts : int -> Set[TimeoutMsg],
  broadcast_timeout: int -> bool,
  address: Address,   //This is not in the paper, but we need it to identify the process
}

//
// Utility functions
//

/// Hash is modeled as id for payloads
pure def hash(s: str): str = {
  s
}

/// Block hash is modeled as a tuple of the block seq, the payload hash and the qc 
pure def hash_block(seq: int, payload_hash: str, qc: QC): BlockHash = {
  {
    seq: seq,
    payload_hash: payload_hash,
    qc: qc,
  }
}

/// Cryptographic hash to the post‐execution state
/// Modeled here as the ordered list of executed proposal ids
pure def state_hash(s: LocalState) : StateHash = {
  s.ledger.foldl(List(), (acc, p) => acc.append(p.proposal_id))
}


/// Check if qc1 is a parent of qc2
pure def parent (qc1: QC, qc2: QC): bool =
  // TODO check if qc1 is a parent of qc2
  true

/// Check if qc1 extends qc2
pure def extend (qc1: QC, qc2: QC): bool =
  // TODO 
  true

/// Check if two QCs are conflicting
pure def conflicting_qcs (qc1: QC, qc2: QC): bool =
  // TODO check if qc1 and qc2 are conflicting
  and {
    not(extend(qc1, qc2)),
    not(extend(qc2, qc1)),
  }

/// Check if two proposals are conflicting
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

/// Find the tip with the highest view out of a set of tip
pure def highest_tip (tips: Set[Tip]): Tip = {
  // argmax (t∈tips) t.view 
  val max_tip = tips.fold(tips.getOnlyElement(), (acc, t) => {
    if (t.view > acc.view) t
    else acc
  })
  max_tip
}

/// Extract the block header out of a block
pure def header_from_block(b: Block): BlockHeader = {
  {
    seq: b.seq,
    payload_hash: b.payload_hash,
    qc: b.qc,
    block_hash: b.block_hash,
  }
}

/// Build a TC out of a set of timeout messages
pure def build_tc(timeouts: Set[TimeoutMsg], addr: Address): TC = {
  {
    c: timeouts,
    view: timeouts.getOnlyElement().view,
    high_tip: Some(timeouts.getOnlyElement().tip),
    tips: get_tips_from_timeouts(timeouts),
    sigma: addr,
  }
}

/// Build a QC out of a set of votes
pure def build_qc(votes: Set[VoteMsg], addr: Address, state_hash: StateHash): QC = {
  val id = {seq: votes.getOnlyElement().seq, view: votes.getOnlyElement().view, byzantine: 0}
  {
    c: votes,
    view: votes.getOnlyElement().view,
    seq: votes.getOnlyElement().seq,
    proposal_id: id,
    state_hash: state_hash,
    sigma: addr,
  }
}

/// Get a proposal based on the proposal id
pure def get_proposal(id: ProposalID, proposals: Set[Proposal] ): Proposal =
 proposals.filter(p => p.proposal_id == id).getOnlyElement()


/// Execute the proposal
/// TODO: redesign this function
pure def execute_block(ledger: Ledger, p: Proposal): Ledger = {
  append(ledger, p)
}

/// Check if a proposal conflicts with the ledger
pure def proposal_conflicts(ledger: Ledger, p: Proposal): bool = {
  ledger.select(l => conflicting_props(l, p)).length() > 0
}

/// Revert the ledger to the common ancestor
pure def revert_ledger(ledger: Ledger, p: Proposal): Ledger = {
  val common_ancestor = ledger.select(l => l.proposal_id == p.proposal_id)[0] //TODO: 1)this might throw an error if the parent is not in the ledger, handle the error. 
  // TODO, (line above) 2) Might need to recheck the logic of prop_id == ledger_element.id, should we go up the tree for the proposal too? 
  // Slice the ledger to the common ancestor
  val index = ledger.foldl((0,false), (acc, l) => { //TODO: this is just an index function, check quint spells.
    if (l == common_ancestor) (acc._1, true)
    else if (not (acc._2)) (acc._1 + 1, acc._2)
    else (acc._1 , acc._2)
  })
  ledger.slice(0, index._1)
}
  


/// Check if a block was alread received
pure def block_present(s: LocalState, bh: BlockHeader): bool = {
  s.proposals.filter(p => p.block.header_from_block() == bh).size() > 0
}

/// Fetch the block from the local state
pure def get_present_block(s: LocalState, bh: BlockHeader): Block = {
  s.proposals.filter(p => p.block.header_from_block() == bh).getOnlyElement().block
}

//
// Algorithm 1 (page 13)
//

/// Alg 1, line 1
/// Form a block out of a QC and a payload
pure def build_block(qc: QC, payload: str): Block =
{
  seq: qc.seq + 1,
  payload: payload,
  payload_hash: hash(payload),
  qc: qc,
  block_hash: hash_block(qc.seq + 1, hash(payload), qc),
}



/// Algorithm 1, page 13 (line 6)
/// Form a proposal out of a block and a set of certificates
pure def build_proposal(view: int, block: Block, tc: Option[TC], nec: Option[NEC], addr: Address): Proposal = {
  val id = {seq: block.seq, view: view, byzantine: 0}
  {
    view: view,
    proposal_id: id,
    block: block,
    sigma: addr,
    tc: tc,
    nec: nec,
  }
}

/// Alg 1, line 10
/// Confirm the proposal continues from the last certified block.
/// It then accepts proposals made under normal operation, when the leader advances without any timeouts.
/// In recovery after a timeout, it only accepts either the reproposal of the highest‐tip block or, if that block couldn’t be recovered, a new block justified by a No‑Endorsement Certificate.
pure def safety_check(p: Proposal): bool = {
  if (p.block.seq != p.block.qc.seq + 1) //Warning: typo in the paper (line 11)
    false
  else if (p.view == p.block.qc.view + 1)
    true
  else if (p.tc != None and p.view == p.tc.unwrap().view + 1){
    val p_tc = p.tc.unwrap()
    if (
      or {p.block.qc != p_tc.high_tip.unwrap().block_header.qc, // The unwrap will throw an error if the option is None, but it should not be
        p_tc.high_tip != find_high_tip(p_tc)}
      )
      false
    else if (p.nec != None)
     and {
        p.nec.unwrap().view == p.view,
        p.nec.unwrap().highqc_view == p.block.qc.view,
      }
    else (p.block.block_hash == p_tc.high_tip.unwrap().block_header.block_hash) // The unwrap will throw an error if the option is None, but it should not be
  }
  else false
}

/// Alg 1 line 22
pure def update_tip(p: Proposal): Option[Tip] = {
  if (p.view == p.block.qc.view + 1 or p.nec != None)
    val p_tip = {
        view: p.view,
        proposal_id: p.proposal_id,
        block_header: p.block.header_from_block(),
        sigma: p.sigma,
        nec: p.nec,
        tip_id: 0, // TODO: Update this 
      }
    Some(p_tip)
  else
    find_high_tip(p.tc.unwrap()) // TODO: Question? why there is no check for none TC 
}     
    
/// Alg 1 line 26
pure def parent_id (p: Proposal): ProposalID =
  p.block.qc.proposal_id


//
// Algorithm 3 (page 16)
//

/// Alg 3 line 11
/// Returns true only if a tip represents a “fresh” proposal:
///   - A happy‑path proposal where tip.view == tip.block.qc.view+1, or
///   - A NEC‑driven proposal where tip.nec.view == tip.view && tip.nec.highqc_view == tip.block.qc.view
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
/// Gathers all validator tips from a timeout certificate or timeout messages.
///   - It aborts and returns nil if any tip fails the fresh‑proposal checks.
///   - Otherwise, it picks and returns the tip with the highest view number.
/// Warning: the side effects of this function seem wrong.
pure def find_high_tip(tc: TC): Option[Tip] = {
  val tips = tc.tips
  // TODO: The first if else block (L3 -> L7) is skipped until we figure out if we need it
  if (not (tips.forall(t => t.verify_fresh_proposal()))) None
  else {
    // argmax (t∈tips) t.view
    val max_tip = highest_tip(tips)
    Some(max_tip)
  }
}



//
// Algorithm 4 (page 22)
//


/// Alg 4 line 2
/// Update the local state with a new view
/// We pass view instead of cert to simplify the code.
/// We remove the timer start/stop as we don't model them in Quint.
pure def increment_view(view: int, s: LocalState): LocalState = {
  if (view >= s.curView)
    { ... s, curView: view } // Increment view to next
  else
    s
}


/// Alg 4 line 10
/// Output = (TC, LocalState, bool: whether to trigger timeout event)
pure def handle_timeout(t: TimeoutMsg, s: LocalState): (Option[TC] ,LocalState, bool)  = {
  val v = t.view

  if (v < s.curView) (None, s, false)
  else {
    val s1 = increment_view(t.tip.block_header.qc.view, s)
    val s2 = increment_view(t.tc.view, s1)
    // Timeout is new, let's accumulate it
    val acc_timeouts = s2.timeouts.getOrElse(v, Set()).setAdd(t)
    val s3 = {...s2, timeouts: s2.timeouts.put(v, acc_timeouts)}
    // Check if we have a quorum
    if (acc_timeouts.size() >= Q) {
      val tc = build_tc(acc_timeouts, s3.address)
      (Some(tc), increment_view(tc.view, s3), false)
    }
    else if(acc_timeouts.size() >= F + 1 and not (s3.broadcast_timeout.getOrElse(v, false))) {
      //TRIGGER TIMEOUT EVENT
      (None, {...s3, broadcast_timeout: s3.broadcast_timeout.put(v, true)}, true)
    }
    else (None, s3, false)
  }
}


/// Alg 4 line 25
pure def handle_vote(vote: VoteMsg, s: LocalState): (Option[QC],LocalState) = {
  val v = vote.view
  val id = vote.proposal_id
  
  if (v < s.curView) (None,s)
  else {
    // Vote is new, let's accumulate it
    val acc_votes = s.votes.getOrElse(id, Set()).setAdd(vote)
    val new_state = {...s, votes: s.votes.put(id, acc_votes)}
    // Check if we have a quorum
    if (acc_votes.size() >= Q) {
      val qc = build_qc(acc_votes, s.address, state_hash(new_state))
      (Some(qc), increment_view(qc.view, new_state))
    }
    else (None, new_state)
    }
  }




//
// Algorithm 2 (page 15)
//

pure def initialize_process (address: str): LocalState =
  {
    high_tip: None,
    last_tc: None,
    tip: None,
    proposals: Set(),
    timeouts: Map(),
    broadcast_timeout: Map(),
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
  | InternalTimeout // TODO: @josef-widder Algorithm 2 line 49
// Algorithm 5 has more responses

type ConsensusResult = {
  output: Set[ConsensusOutput],
  state: LocalState,
}


/// Alg 2 line 3 (Only for the leader)
pure def receive_vote (s: LocalState, v: VoteMsg): ConsensusResult = {
  if (s.is_leader()){
    // Line 3
    val res = handle_vote(v, s)
    val qc = res._1
    val s1 = res._2

    if (qc != None) {
      val new_block = build_block(qc.unwrap(), BLOCK_PAYLOAD)
      val p = build_proposal(s1.curView, new_block, None, None, s1.address)
      { state: s1, output: Set(BroadcastProp(p))}
    } else 
      { state: s1, output: Set()}

  } else
    { state: s, output: Set()}
}


/// Alg 2 line 10 / line 52 (leader/ non-leader)
pure def receive_timeout (s: LocalState, t: TimeoutMsg): ConsensusResult = {
  val res = handle_timeout(t, s)
  val tc = res._1
  val s1 = res._2
  val trigger_timeout = res._3

  val effects = if (trigger_timeout) Set(InternalTimeout) else Set()

  if (s.is_leader()){
    //line 11
    if (tc != None) {
      val s2 = {... s1, last_tc: tc, high_tip: tc.unwrap().high_tip}
      val high_tip = tc.unwrap().high_tip.unwrap() //TODO: it should be safe to unwrap here, but we should check. 

      if (block_present(s2, high_tip.block_header)) { //if block and its payload (block.payload) for high tip is present
        // Line 17
        val block = get_present_block(s2, high_tip.block_header)
        val p = build_proposal(s2.curView, block, tc, None, s2.address)
        { state: s2, output: effects.setAdd(BroadcastProp(p))}
      } else
        // Line 18-19-20 recoveringcurView ← true; SendRecoveryRequest(curView, high tip, tc); 
        { state: s2, output: effects}
      
  }else
    // Not enough timeout messages to form tc
    { state: s1, output: effects}
  
  } else {
    //line 52
    { state: s1, output: effects}
  }
}


// Alg 2 line 34
pure def receive_proposal (s: LocalState, p: Proposal): ConsensusResult = {
  val s1 = match p.tc {
    | None =>
        s
    | Some(tc) =>
        increment_view(tc.view, s)
  }
  val s2 = increment_view(p.block.qc.view, s1)
  if (not(s.is_leader()) or p.view != s.curView) { //Warning: is the view being updated before the Leader check? Lines 35-36.
    { output: Set(), state: s }
  } else if (safety_check(p) and p.view > max(s.highest_voted_view, p.block.qc.view)) {  //Warning: is the view being updated before the safety check.
    // Line 38
    val parent_proposal = get_proposal(p.parent_id(), s.proposals) 
    val grandparent_proposal= get_proposal(parent_proposal.parent_id(), s.proposals)

    //Line 41
    val commit = if (p.block.qc.view == parent_proposal.block.qc.view + 1)
      Set(Commit(grandparent_proposal), Confirmation(p))
    else Set()

    //Line 43
    val early_confirmation = if (p.view == p.block.qc.view + 1) //Warning; typo in the paper
      Set(EarlyConfirmation(parent_proposal))
    else Set()


    val reverted_ledger = if (proposal_conflicts(s2.ledger, p)) { //Line 44 - If ledger conflicts with parent proposal, revert to the common ancestor. 
      revert_ledger(s2.ledger, p)
    } else s2.ledger


    val ledger = if (p.view == p.block.qc.view + 1) //Line 45 - Speculate
      execute_block(reverted_ledger, p) 
    else reverted_ledger

    val s3 = {...s2, highest_voted_view: p.view, ledger: ledger, tip: update_tip(p)}
    val vote_msg = {
      view: p.view,
      seq: p.block.seq,
      proposal_id: p.proposal_id,
      state_hash: state_hash(s3),
      sigma: s3.address,
    }
    val send_vote = Set(SendVoteToNextLeader(("v1", vote_msg))) //TODO: send to next leader
    { output: commit.union(early_confirmation).union(send_vote), state: s3 }
  } else {
    { output: Set(), state: s }
  }
}

// Alg 2 line 49
pure def timeout_event (s: LocalState): ConsensusResult = {
  if (s.is_leader())  { state: s, output: Set()} //Leader does nothing upon receival of timeout event
  else {
    val s1 = {...s, highest_voted_view: max(s.highest_voted_view, s.curView)}
    val tc_aux = {
      view: s.curView,
      high_tip: None,
      tips: Set(),
      sigma: s.address
    }
    val timeout_msg = {
      view: s.curView,
      tip: s.tip.unwrap(), //TODO: check if this is safe, it is not specified in the paper, made me think it is an option (Line 50)
      tc: tc_aux,
      sigma: s.address
    }
    { state: s1, output: Set(BroadCastTimeout(timeout_msg))}
  }
}
  
  
  



//
// Alg 5 (page 26)
//

// TODO: shall/can we ignore block recovery for now?
// TODO: there are more upons here.

// State machine

const validators: Set[Address]
const F : int // number of byzantine nodes
const Q : int // number of votes needed to reach consensus
const N : int // number of nodes 



type Environment = {
  system: Address -> LocalState,
  voteBuffer: Address -> Set[VoteMsg],
  propBuffer: Address -> Set[Proposal],
  timeoutBuffer: Address -> Set[TimeoutMsg],
  activeTimeouts: Address -> Set[int], // in which views the process is waiting for a timeout
}

var s: Environment



// TODO: what to do about the genesis block/QC?
val genesis_qc = {
    c: Set(),
    view: 0,
    seq: 0,
    proposal_id: {seq: 0, view: 0, byzantine: 0},
    state_hash: List(),
    sigma: "v1",
  }

action init = all {
  s' = {
    system: validators.mapBy(v => initialize_process(v)),
    voteBuffer: validators.mapBy(v => Set()),
    propBuffer: validators.mapBy(v => 
      Set(build_proposal( 1, 
                          build_block(genesis_qc, "first block"),
                          None, 
                          None, "v1"))),
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
      | InternalTimeout =>
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
