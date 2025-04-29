// -*- mode: Bluespec; -*-

module monadbft {

import basicSpells.* from "./basicSpells"

val BLOCK_PAYLOAD = "VALID_TRANSACTION" //TODO: this is just a placeholder



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
  ancestors:      ProposalID -> Set[ProposalID],     // NEW: for each proposal, its full set of ancestors
  timeouts : int -> Set[TimeoutMsg],
  broadcast_timeout: int -> bool,
  address: Address,   //This is not in the paper, but we need it to identify the process
}

// CSMI
type Message =
  | Prop(Proposal)
  | Vote(VoteMsg)
  | Timeout(TimeoutMsg)
  | NoEndorsement(NoEndorsementMsg)

// CSMI
type TimeoutEvent = int // view number for the timeout event



// CSMI
type ConsensusOutput =
  | BroadcastProp(Proposal)
  | BroadCastTimeout(TimeoutMsg)
  | Commit(Proposal)
  | EarlyConfirmation(Proposal)
  | Confirmation(Proposal)
  | SendVoteToNextLeader((Address, VoteMsg))
  | StartTimeout(TimeoutEvent) // TODO: @josef-widder Algorithm 2 line 49
// Algorithm 5 has more responses

type ConsensusResult = {
  output: Set[ConsensusOutput],
  state: LocalState,
}


// CSMI
// This initializes process that is identified by address
pure def initialize_process (address: Address): LocalState =
  {
    high_tip: None,
    last_tc: None,
    tip: None,
    proposals: Set(),
    ancestors: Map(),
    timeouts: Map(),
    broadcast_timeout: Map(),
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
    | _ =>
        // TODO
        { state: state, output: Set() }
  }

// CSMI
// This is the function that contains all the timeout handlers of the algorithm
pure def fire_timeout_event(state: LocalState, timeout: TimeoutEvent): ConsensusResult =
  if (state.curView == timeout)
    state.timeout_event()
  else
    { state: state, output: Set()}

// End of CSMI functional side




//
// Specifics of MonadBFT
//

//
// Data structures (page 8)
//

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



/// Check if qc1 extends qc2
pure def extend (qc1: QC, qc2: QC, ancestors: ProposalID -> Set[ProposalID]): bool =
  // TODO IMPLEMENT ME
  ancestors.getOrElse(qc1.proposal_id, Set()).contains(qc2.proposal_id)
  

/// Check if two QCs are conflicting
/*
pure def conflicting_qcs (qc1: QC, qc2: QC): bool =
  and {
    not(extend(qc1, qc2)),
    not(extend(qc2, qc1)),
  }
*/

/// Check if two proposals are conflicting
pure def conflicting_props (p1: Proposal, p2: Proposal, ancestors: ProposalID -> Set[ProposalID]): bool =
  and {
    not(extend(p1.block.qc, p2.block.qc, ancestors)),
    not(extend(p2.block.qc, p1.block.qc, ancestors)),
  }


/// Extract the tips from a set of timeout messages
/// This function retruns a set of Tip. 
pure def get_tips_from_timeouts(msgs: Set[TimeoutMsg]): Set[Tip]= {
  msgs.fold(Set(), (acc, msg) => acc.setAdd(msg.tip))
}

/// Find the tip with the highest view out of a set of tip
pure def highest_tip (tips: Set[Tip]): Tip = {
  // argmax (t∈tips) t.view
  val init = tips.fold(List(), (acc, t) => acc.append(t))[0] //TODO: this is just a placeholder, refractor after debugging.
  val max_tip = tips.fold(init, (acc, t) => {
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
  val view = timeouts.map(t => t.view).getOnlyElement() //will throw an error if we have different views
  val high_tip = find_high_tip_from_messages(timeouts) 
  {
    c: timeouts,
    view: view,
    high_tip: high_tip,
    tips: get_tips_from_timeouts(timeouts),
    sigma: addr,
  }
}

/// Build a QC out of a set of votes
pure def build_qc(votes: Set[VoteMsg], addr: Address, state_hash: StateHash): QC = {
  val seq = votes.map(v => v.seq).getOnlyElement() //will throw an error if we have different seqs
  val view = votes.map(v => v.view).getOnlyElement() //will throw an error if we have different views
  val id = {seq: seq, view: view, byzantine: 0}
  {
    c: votes,
    view: view,
    seq: seq,
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
pure def proposal_conflicts(ledger: Ledger, p: Proposal, ancestors: ProposalID -> Set[ProposalID]): bool = {
  ledger.select(l => conflicting_props(l, p, ancestors)).length() > 0
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


/// Revert the ledger back to the latest ancestor of p that is still in the ledger.
/// `ancestors` maps each proposalID to the full set of its ancestor‐IDs.
pure def revert_ledger_2(
    ledger:   Ledger,
    p:        Proposal,
    ancestors: ProposalID -> Set[ProposalID]
  ): Ledger = {
  
  // 1) Look up the set of all ancestor IDs for p
  val ancIds = ancestors.getOrElse(p.proposal_id, Set())

  // 2) Walk the ledger and remember the last index where we saw one of those IDs
  //    foldl((nextIndex, lastMatch), ( (idx, foundIdx), elem )) =>
  //      if elem.proposal_id ∈ ancIds then (idx+1, idx) else (idx+1, foundIdx)
  val result = ledger.foldl((0, -1), (acc, elem) => {
    val idx = acc._1
    val lastMatch = acc._2
    if (ancIds.contains(elem.proposal_id))
      (idx + 1, idx)
    else
      (idx + 1, lastMatch)
  })

  val lastIdx = result._2

  // 3) If we never found a match (lastIdx == -1), just return an empty ledger
  //    otherwise slice up through that index (inclusive)
  if (lastIdx < 0) {
    List()
  } else {
    ledger.slice(0, lastIdx + 1)
  }
}


/// Check if a block was alread received
pure def block_present(s: LocalState, bh: BlockHeader): bool = {
  s.proposals.filter(p => p.block.header_from_block() == bh).size() > 0
}

/// Fetch the block from the local state
pure def get_present_block(s: LocalState, bh: BlockHeader): Block = {
  s.proposals.filter(p => p.block.header_from_block() == bh).map(p => p.block).getOnlyElement()
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
  // Adding a magic case for the genesis block

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
    else p.block.block_hash == p_tc.high_tip.unwrap().block_header.block_hash // The unwrap will throw an error if the option is None, but it should not be
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

/// Alg 3 line 1
/// Does the same as find_high_tip(tc), but for a set of timeout messages.
pure def find_high_tip_from_messages(msgs: Set[TimeoutMsg]): Option[Tip] = {
  val tips = get_tips_from_timeouts(msgs)
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
pure def increment_view(view: int, s: LocalState): ConsensusResult = {
  if (view >= s.curView)
    {state :{ ... s, curView: view+1}, output: Set(StartTimeout(view+1))} // Increment view to next and reset the timer, TODO: discard old timeouts
  else
    {state:s , output: Set()} // No change
}


/// Alg 4 line 10
/// Output = (TC, LocalState, bool: whether to trigger timeout event)
pure def handle_timeout(t: TimeoutMsg, s: LocalState): (Option[TC] , ConsensusResult, bool)  = {
  val v = t.view

  if (v < s.curView) (None, {state: s , output: Set()}, false)
  else {
    val co1 = increment_view(t.tip.block_header.qc.view, s)
    val s1 = co1.state
    
    val co2 = increment_view(t.tip.view, s1)
    val s2 = co2.state
    val effects = co1.output.union(co2.output)  
    
    // Timeout is new, let's accumulate it
    val acc_timeouts = s2.timeouts.getOrElse(v, Set()).setAdd(t)
    val s3 = {...s2, timeouts: s2.timeouts.put(v, acc_timeouts)}
    // Check if we have a quorum
    if (acc_timeouts.size() >= Q) {
      val tc = build_tc(acc_timeouts, s3.address)
      val co3 = increment_view(tc.view, s3)
      (Some(tc), {state: co3.state, output: co3.output.union(effects)}, false) //TODO: do we need to carry the effects of the previous increment_view here
    }
    else if(acc_timeouts.size() >= F + 1 and not (s3.broadcast_timeout.getOrElse(v, false))) {
      //TRIGGER TIMEOUT EVENT
      val s4 = {...s3, broadcast_timeout: s3.broadcast_timeout.put(v, true)}
      val te = timeout_event(s4)
      (None, {state: te.state, output: effects.union(te.output)}, true)
    }
    else (None, {state: s3, output: effects}, false)
  }
}


/// Alg 4 line 25
pure def handle_vote(vote: VoteMsg, s: LocalState): (Option[QC], ConsensusResult) = {
  val v = vote.view
  val id = vote.proposal_id
  
  if (v < s.curView) (None,{state: s, output: Set()})
  else {
    // Vote is new, let's accumulate it
    val acc_votes = s.votes.getOrElse(id, Set()).setAdd(vote)
    val s1 = {...s, votes: s.votes.put(id, acc_votes)}
    // Check if we have a quorum
    if (acc_votes.size() >= Q) {
      val qc = build_qc(acc_votes, s.address, state_hash(s1))
      (Some(qc), increment_view(qc.view, s1))
    }
    else (None, {state: s1, output: Set()})
    }
  }


//
// Algorithm 2 (page 15)
//


/// Check if the node is the leader of the current view
pure def is_leader(s: LocalState): bool =
  // TODO check if the node is the leader
  s.address == "v1" // TODO: this is just a placeholder



/// Alg 2 line 3 (Only for the leader)
pure def receive_vote (s: LocalState, v: VoteMsg): ConsensusResult = {
  if (s.is_leader()){
    // Line 3
    val res = handle_vote(v, s)
    val qc = res._1
    val s1 = res._2.state
    val effects = res._2.output

    if (qc != None) {
      val new_block = build_block(qc.unwrap(), BLOCK_PAYLOAD)
      val p = build_proposal(s1.curView, new_block, None, None, s1.address)
      { state: s1, output: effects.setAdd(BroadcastProp(p))}
    } else 
      { state: s1, output: effects}

  } else
    { state: s, output: Set() }
}

/// Alg 2 line 49
/// As opposed to the paper, we write the timeout event as a function that is called by the timeout handler.
/// Instead of of a "upon" statement that gets triggered.
/// Warning: The upon trigger event block is not clear on its synchrony assumptions. 
pure def timeout_event (s: LocalState): ConsensusResult = {
  //if (s.is_leader())  { state: s, output: Set()} //Leader does nothing upon receival of timeout event
  //if (s.curView < 8 or s.curView % 5 != 2) { state: s, output: Set()} //TODO: debug, start timing out after view 5
  if (false)  { state: s, output: Set()}  
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

/// Alg 2 line 10 / line 52 (leader/ non-leader)
pure def receive_timeout (s: LocalState, t: TimeoutMsg): ConsensusResult = {
  val res = handle_timeout(t, s)
  val tc = res._1
  val s0 = res._2.state
  val trigger_timeout = res._3

  val co = if (trigger_timeout) timeout_event(s0) else { state: s0, output: Set()} // calling the timeout event synchronously in the timeout message handler
  val s1 = co.state
  val effects = co.output.union(res._2.output)

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
        // TODO: For the moment this is replaced by fetching the block from the DA variable for debugging purposes
        val block = get_present_block(s2, high_tip.block_header)
        val p = build_proposal(s2.curView, block, tc, None, s2.address)
        { state: s2, output: effects.setAdd(BroadcastProp(p))}
      
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
  //val s0 = {...s, proposals: s.proposals.setAdd(p)}
  val co1 = match p.tc {
    | None =>
        {state:s, output: Set()}
    | Some(tc) =>
        increment_view(tc.view, s)
  }
  val s1 = co1.state
  val co2 = increment_view(p.block.qc.view, s1)
  val s2 = co2.state
  val timers = co1.output.union(co2.output)
  
  //TODO: add leader check here -> Is this message coming form the leader of the view?
  if (p.view != s2.curView) { //Warning: is the view being updated before the Leader check? Lines 35-36. 
    { output: timers, state: s2} //TODO: should we update this now that the timer get's triggered upon entering a new view.
  } else if (safety_check(p) and p.view > max(s2.highest_voted_view, p.block.qc.view)) {  //Warning: is the view being updated before the safety check.
    // Line 38
    val parent_proposal = get_proposal(p.parent_id(), s2.proposals)
    val grandparent_proposal= get_proposal( parent_proposal.parent_id(), s2.proposals)

    //Line 41
    val commit = if (p.block.qc.view == parent_proposal.block.qc.view + 1)
      Set(Commit(grandparent_proposal), Confirmation(p))
    else Set()

    //Line 43
    val early_confirmation = if (p.view == p.block.qc.view + 1) //Warning; typo in the paper
      Set(EarlyConfirmation(parent_proposal))
    else Set()


    val reverted_ledger = if (proposal_conflicts(s2.ledger, parent_proposal, s2.ancestors)) { //Line 44 - If ledger conflicts with parent proposal, revert to the common ancestor. 
      revert_ledger_2(s2.ledger, parent_proposal, s2.ancestors)
    } else s2.ledger


    val ledger = if (p.view == p.block.qc.view + 1) //Line 45 - Speculate
      execute_block(reverted_ledger, parent_proposal) 
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
    { output: commit.union(early_confirmation).union(send_vote).union(timers), state: s3 }
  } else {
    { output: timers, state: s2 }
  }
}




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
const F : int // number of byzantine nodes
val Q = 2 * F + 1// number of votes needed to reach consensus
val N = validators.size() // number of nodes 



type Environment = {
  system: Address -> LocalState,
  msgBuffer: Address -> Set[Message],
  activeTimeouts: Address -> Set[TimeoutEvent], 
  // for analysis purposes
  committed_proposals: Address -> Set[Proposal],
  msgHistory: Set[Message],
  timeoutHistory: Set[(Address, TimeoutEvent)],
  lastMsg: Option[Message], // last message sent
}

var s: Environment

/// Safety property page 10: No two correct validators commit different blocks
/// for the same sequence number
def safety = 
  val a = s.committed_proposals.values().flatten()
  a.forall(p1 => a.forall(p2 => p1.block.seq == p2.block.seq implies p1 == p2))

/// No Tail-Forking (NTF) page 10:
/// todo: This needs to be revised. The reproposal changes the proposal id of the proposal but proposes the same block.
def ntf = 
  val a = s.msgHistory.fold(Set(), (s, x) => match x {
    | Vote(v) => s.setAdd(v)
    | _ => s
  })
  a.forall(v => 
    // If an honest leader’s proposal for sequence number s is voted upon by a majority of honest validators, 
    // TODO: filter faulty proposer's proposals
    a.filter(v2 => v2.seq == v.seq and v2.proposal_id == v.proposal_id).size() > validators.size() / 2
    implies
    //  then no other proposal can commit at sequence s.
    s.committed_proposals.values().flatten().forall(p =>
      p.block.seq == v.seq implies p.proposal_id == v.proposal_id)
  )

//
//
// Consensus / State machine interface CSMI
// effect side
//
//


// TODO: what to do about the genesis block/QC?
val genesis_qc = {
    c: Set(),
    view: 0,
    seq: 0,
    proposal_id: {seq: 0, view: 0, byzantine: 0},
    state_hash: List(),
    sigma: "v1",
  }

// CSMI
action init_0 = all {
  s' = {
    system: validators.mapBy(v => initialize_process(v)),
    msgBuffer: validators.mapBy(v => 
      Set(Prop(build_proposal( 1, 
                          build_block(genesis_qc, "first block"),
                          None, 
                          None, "v1")))),
    activeTimeouts: validators.mapBy(v => Set()),
    committed_proposals: validators.mapBy(v => Set()),
    msgHistory: Set(),
    timeoutHistory: Set(),
    lastMsg: None,
  },
}

action init = all {
    // --- build block 1 on genesis ---
    val b1     = build_block(genesis_qc, "block1")
    val p1     = build_proposal(1, b1, None, None, "v1")
    val votes1 = Set(
      { view : 1, seq : 1, proposal_id : p1.proposal_id, state_hash : List(), sigma : "v1" },
      { view : 1, seq : 1, proposal_id : p1.proposal_id, state_hash : List(), sigma : "v2" },
      { view : 1, seq : 1, proposal_id : p1.proposal_id, state_hash : List(), sigma : "v3" }
    )
    val qc1    = build_qc(votes1, "v1", List())  

    // --- build block 2 on qc1 ---
    val b2     = build_block(qc1, "block2")
    val p2     = build_proposal(2, b2, None, None, "v1")
    val votes2 = Set(
      { view : 2, seq : 2, proposal_id : p2.proposal_id, state_hash : List(p1.proposal_id), sigma : "v1" },
      { view : 2, seq : 2, proposal_id : p2.proposal_id, state_hash : List(p1.proposal_id), sigma : "v2" },
      { view : 2, seq : 2, proposal_id : p2.proposal_id, state_hash : List(p1.proposal_id), sigma : "v3" }
    )
    val qc2    = build_qc(votes2, "v1", List(p1.proposal_id))
    val p3 =  build_proposal(
            3,
            build_block(qc2, "block3"),
            None,
            None,
            "v1"
          )

    val ancestors = Map(
      p1.proposal_id -> Set(p1.proposal_id),
      p2.proposal_id -> Set(p1.proposal_id),
      p3.proposal_id -> Set(p1.proposal_id, p2.proposal_id)
    )
     
    // --- now override the environment ---
    s' = {
      system: validators.mapBy(v => {
        val base = initialize_process(v)
        { ... base,
          curView            :2,
          ledger             : List(p1, p2),
          proposals          : Set(p1, p2, p3),
          ancestors         : ancestors,
          highest_voted_view : 2,
          tip : Some({
            view         : 2,
            proposal_id  : p2.proposal_id,
            block_header : header_from_block(b2),
            sigma        : v,
            nec          : None,
            tip_id       : 0
          })
        }
      }),
      msgBuffer: validators.mapBy(v =>
        Set( Prop(p3)
      )),

      activeTimeouts:      validators.mapBy(_ => Set()),
      committed_proposals: validators.mapBy(_ => Set(p1, p2)),
      msgHistory:          Set(),
      timeoutHistory:      Set(),
      lastMsg:            None,
    },
  }






// CSMI
/// Interface consensus algorithm / state machine: how consensus algorithm effects the environment 
pure def apply_effect (env: Environment, v: Address, res: ConsensusResult): Environment =
  val new = { ... env, system: env.system.set(v, res.state) }
  res.output.fold(new, (s, x) => {
    match x {
      | BroadcastProp(p) =>
          // Warning: This was added to provide a data availability layer for the proposals to avoid having missing proposals in the proposal handler. This is to be adressed in the future.
          val s_with_prop = s.system.keys().fold(s.system, (sys_map, node) => //TODO: refractor this to be more readable
            sys_map.set(node,
              { ... sys_map.get(node),
                proposals : sys_map.get(node).proposals.setAdd(p),
                ancestors : sys_map.get(node).ancestors.put(p.proposal_id, sys_map.get(node).ancestors.getOrElse(p.parent_id(), Set()).setAdd(p.parent_id()))
              }
            )
         )
          { ... s,
            system:      s_with_prop,
            msgBuffer:   s.msgBuffer.keys().mapBy(x => s.msgBuffer.get(x).union(Set(Prop(p))))
          }
      | BroadCastTimeout(t) =>
          { ... s, msgBuffer: s.msgBuffer.keys().mapBy(x => s.msgBuffer.get(x).union(Set(Timeout(t))))} // update propBuffers
      | Commit(p) =>
          { ... s, committed_proposals: s.committed_proposals.set(v, s.committed_proposals.get(v).setAdd(p)) }
      | EarlyConfirmation(p) =>
          s
      | Confirmation(p) =>
          s
      | SendVoteToNextLeader(am) =>
          { ... s,  msgHistory: s.msgHistory.setAdd(Vote(am._2)),
                    msgBuffer: s.msgBuffer.setBy(am._1, msgs => msgs.setAdd(Vote(am._2)))}
      | StartTimeout(t) =>
          val ot = if (s.activeTimeouts.get(v).size() == 0) t else s.activeTimeouts.get(v).getOnlyElement() //ot = old timeout
          val ft  = if (ot > t) ot else t //ft = fresher timeout
          { ... s, activeTimeouts: s.activeTimeouts.setBy(v, ato => Set(ft))}
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
  val consumed_msg_state = { ... state, msgBuffer: state.msgBuffer.set(v, state.msgBuffer.get(v).exclude(Set(msg))),lastMsg: Some(msg)} //TODO: eliminate this line
  val res = consumed_msg_state.system.get(v).receive_message(msg)
  apply_effect(consumed_msg_state, v, res)


pure def fire_one_timeout_event (state: Environment, v: Address, timeout: TimeoutEvent): Environment =
  val consumed_timeout_state = { ... state, activeTimeouts: state.activeTimeouts.set(v, state.activeTimeouts.get(v).exclude(Set(timeout))), 
                                 timeoutHistory: state.timeoutHistory.setAdd((v, timeout))} //TODO: eliminate this line
  val res = consumed_timeout_state.system.get(v).fire_timeout_event(timeout)
  apply_effect(consumed_timeout_state, v, res)

action act_receive_msg (v: Address, msg: Message) : bool = all {
  s.msgBuffer.get(v).contains(msg),
  s' = receive_one_message(s, v, msg)
}

action act_receive_some_msg (v: Address) : bool = all {
  s.msgBuffer.get(v) != Set(),
  nondet msg = oneOf(s.msgBuffer.get(v))
  act_receive_msg(v, msg)
}

action act_timeout (v: Address, timeout: TimeoutEvent) : bool = all {
  s.activeTimeouts.get(v).contains(timeout),
  s' = fire_one_timeout_event(s, v, timeout)
}

action act_some_timeout (v: Address) : bool = all {
  s.activeTimeouts.get(v) != Set(),
  nondet timeout = oneOf(s.activeTimeouts.get(v))
  act_timeout(v, timeout)
}

action step =
  nondet v = oneOf(validators.filter(v => s.msgBuffer.get(v) != Set() or s.activeTimeouts.get(v) != Set())) //pick a process that has a message available in its buffer or a timeout
  any {
    act_receive_some_msg(v),
    act_some_timeout(v)
  }

action step_no_timeout =
  nondet v = oneOf(validators.filter(v => s.msgBuffer.get(v) != Set())) //pick a process that has a message available in its buffer
  act_receive_some_msg(v)

action step_timeout =
  nondet v = oneOf(validators.filter(v => s.activeTimeouts.get(v) != Set())) //pick a process that has a timeout available
  act_some_timeout(v)

action step_fine =
  nondet p = oneOf(1.to(20))
  if (p == 1) step
  else if(validators.filter(v => s.msgBuffer.get(v) != Set()).size() > 0) step_no_timeout
  else step

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
  val msgs = validators.map(v => s.msgBuffer.get(v).map(m => (v, m))).flatten()
  nondet msgsToDeliver = msgs.powerset().oneOf()
  val deliver = validators.mapBy(v => msgsToDeliver.filter(m => m._1 == v).map(m => m._2))
  s' =  apply_acceleration_msgs(s, deliver)

action acceleration_timeouts = 
  val timeouts = validators.map(v => s.activeTimeouts.get(v).map(m => (v, m))).flatten()
  nondet timeoutsToDeliver = timeouts.powerset().oneOf()
  val deliver = validators.mapBy(v => timeoutsToDeliver.filter(m => m._1 == v).map(m => m._2))
  s' =  apply_acceleration_timeout(s, deliver)

action accelerated_step = any {
  acceleration_msgs,
  acceleration_timeouts,
}  


val empty_buffer = not (s.msgBuffer.keys().filter(v => s.msgBuffer.get(v) == Set()).size() == validators.size())

} // end of module monadbft



module example {
  import monadbft(validators = Set("v1", "v2", "v3", "v4"), F = 1).*  
} // end of module example
