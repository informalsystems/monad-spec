// -*- mode: Bluespec; -*-

module types {
  import basicSpells.* from "lib/basicSpells"
  import csmi from "csmi"
  //
  // Data structures (page 8)
  //

  type Ledger = List[Proposal]
  type ProposalID = { seq: int, view: int, byzantine: int }
  // The signature is symbolic, we represent it with the sender address
  type Signature = Address
  // We don't model hashes explicitly, but under the assumption of collision-free hash functions,
  // we can use this model to test for equality.
  type StateHash = List[ProposalID]

  /// ⟨view, proposal id, block header, σ, nec⟩ (page 8)
  type Tip = {
    view: int,
    proposal_id: ProposalID,
    block_header: BlockHeader,
    sigma: Signature,
    nec: Option[NEC],
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
    // Timeout certificate use tc.view. Needed to avoid cyclic data type.
    // TODO: Maybe this should be an option
    tc: TCAux,
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

  /// We don't model hashes explicitly, but under the assumption of collision-free hash functions,
  /// we can use this model to test for equality.
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

  type Address = str

  type TimeoutData = int
  type InternalEvent = TimeoutEvent(TimeoutData)

  type Message =
    | Prop(Proposal)
    | Vote(VoteMsg)
    | Timeout(TimeoutMsg)
    | NoEndorsement(NoEndorsementMsg)

  type Effects =
    | BroadcastProp(Proposal)
    | BroadCastTimeout(TimeoutMsg)
    | Commit(Proposal)
    | EarlyConfirmation(Proposal)
    | Confirmation(Proposal)
    | SendVoteToNextLeader((Address, VoteMsg))
    | StartTimeout(TimeoutData)
    // Algorithm 5 (block recovery) has more responses

  type Logging = {
    committed_proposals: Address -> Set[Proposal],
    msgHistory: Set[Message],
    timeoutHistory: Set[TimeoutData],
  }

  type State = {
    // Variables from Algorithm 2
    high_tip: Option[Tip],
    tip: Option[Tip],
    last_tc: Option[TC],
    curView: int,
    ledger: Ledger,
    highest_voted_view: int,
    // Set of view numbers for which a timeout was broadcast (Alg 4 line 21/22)
    timeouts_already_broadcast: Set[int],
  }

  // Define types for the parameterized CSMI types with the proper type arguments
  // for MonadBFT
  type LocalState = csmi::LocalState[Address, State]
  type Transition = csmi::Transition[LocalState, Effects]
  type Transitions = csmi::Transitions[Transition]

  type EnvExtensions = {
    proposals_mock: Set[Proposal],
    ancestors_mock: ProposalID -> Set[ProposalID],
  }

  type Environment = csmi::Environment[
    Address,
    State,
    Message,
    InternalEvent,
    Logging,
    EnvExtensions
  ]

  type LocalContext = csmi::LocalContext[Message, InternalEvent, EnvExtensions]
}
