// -*- mode: Bluespec; -*-

module consensus {

//
// Consensus / State machine interface CSMI
// functional side
//
// Here one needs to specify 
// - the types: ProcessID, LocalState, Message, TimeoutEvent, ConsensusOutput
// - the functions: initialize_process, receive_message, fire_timeout_event (that define the
//   message handlers and timeout handlers of the algorithm)  
//

// CSMI-TODO
type ProcessID

// CSMI-TODO
type LocalState = {
  ProcessID: ProcessID,
  // CSMI-TODO: add more fields here, e.g., view, round, received_votes
}

// CSMI-TODO
type Message =
  // CSMI-TODO: add here the message types of the algorithm
  // e.g. Proposal, Prevote, Precommit, etc.
  | Propose // this could carry data in an argument

// CSMI-TODO
type TimeoutEvent
  // CSMI-TODO: add here the timeout types of the algorithm
  // e.g. TimeoutPrevote, TimeoutPrecommit, etc.

// CSMI-TODO
type ConsensusOutput = 
  | Broadcast(Message)
  // CSMI-TODO: add here the output types of the algorithm
  // e.g. SendMessageTo, BroadCastProposal, etc.

type ConsensusResult = {
  output: Set[ConsensusOutput],
  state: LocalState,
}


// CSMI-TODO 
// This initializes process that is identified by ProcessID
pure def initialize_process (id: ProcessID): LocalState =
  { ProcessID: id } // CSMI-TODO: add more here


// CSMI-TODO
// This is the function that contains all the message handlers of the algorithm
pure def receive_message(state: LocalState, msg: Message): ConsensusResult =
  // CSMI-TODO: add here the message handlers of the algorithm
  // e.g. if msg leads to sending another message m, then add a corresponding entry to output
  // and return the new state of the node
  match msg {
    | Propose => { state: state, output: Set() } // CSMI-TODO: add more here
  }



// CSMI-TODO
// This is the function that contains all the timeout handlers of the algorithm
pure def fire_timeout_event(state: LocalState, timeout: TimeoutEvent): ConsensusResult =
  // CSMI-TODO: add here the timeout handlers of the algorithm
  // e.g. if timeout leads to go to a new round and send a message m, then 
  // return the state of the node with the new round number and add an entry for sending the
  // message to output
  { state: state, output: Set() }

// End of CSMI functional side




//
// Add here the types and functions that are needed for the consensus algorithm
//


//
//
// State machine
//
//


const processes: Set[ProcessID]

type Environment = {
  system: ProcessID -> LocalState,
  msgBuffer: ProcessID -> Set[Message],
  activeTimeouts: ProcessID -> Set[TimeoutEvent], 
}

var s: Environment

//
//
// Consensus / State machine interface CSMI
// effect side
//
//


// CSMI-TODO 
// This specification is event triggered, so a process step can only be triggered by a message or a timeout
// as a result we need either messages or timeouts in the initial state
pure def initial_global_state: Environment =
  { system: processes.mapBy(v => initialize_process(v)),
    msgBuffer: processes.mapBy(v => Set()),
    activeTimeouts: processes.mapBy(v => Set()),
  }


// CSMI-TODO
/// Interface consensus algorithm / state machine: how consensus algorithm effects the environment 
pure def apply_effect (env: Environment, v: ProcessID, res: ConsensusResult): Environment =
  val new = { ... env, system: env.system.set(v, res.state) } // update the state of the process
  res.output.fold(new, (s, o) => 
    match o {
      | Broadcast(msg) => { ... new, msgBuffer: processes.mapBy(p => new.msgBuffer.get(p).union(Set(msg)))} // add the message to the buffer
    })
  // CSMI-TODO: add here the effect of the consensus algorithm on the environment
  // e.g. { ... new, msgBuffer: new.msgBuffer.set(v, new.msgBuffer.get(v).union(res.output)) }
  
  


// end of CSMI effect side


//
// General state machine
//

action init = all {
  s' = initial_global_state
}

//
// Standard interleaving semantics (one process receives one message/timeout in one action)
//

pure def receive_one_message (state: Environment, v: ProcessID, msg: Message): Environment =
  val consumed_msg_state = { ... state, msgBuffer: state.msgBuffer.set(v, state.msgBuffer.get(v).exclude(Set(msg))) }
  val res = state.system.get(v).receive_message(msg)
  apply_effect(state, v, res)


pure def fire_one_timeout_event (state: Environment, v: ProcessID, timeout: TimeoutEvent): Environment =
  val consumed_timeout_state = { ... state, activeTimeouts: state.activeTimeouts.set(v, state.activeTimeouts.get(v).exclude(Set(timeout))) }
  val res = consumed_timeout_state.system.get(v).fire_timeout_event(timeout)
  apply_effect(consumed_timeout_state, v, res)

action act_receive_msg (v: ProcessID, msg: Message) : bool = all {
  s.msgBuffer.get(v).contains(msg),
  s' = receive_one_message(s, v, msg)
}

action act_receive_some_msg (v: ProcessID) : bool = all {
  s.msgBuffer.get(v) != Set(),
  nondet msg = oneOf(s.msgBuffer.get(v))
  act_receive_msg(v, msg)
}

action act_timeout (v: ProcessID, timeout: TimeoutEvent) : bool = all {
  s.activeTimeouts.get(v).contains(timeout),
  s' = fire_one_timeout_event(s, v, timeout)
}

action act_some_timeout (v: ProcessID) : bool = all {
  s.activeTimeouts.get(v) != Set(),
  nondet timeout = oneOf(s.activeTimeouts.get(v))
  act_timeout(v, timeout)
}

action step =
  nondet v = oneOf(processes)
  any {
    act_receive_some_msg(v),
    act_some_timeout(v)
  }

//
// Accelerated semantics (multiple process may receive multiple messages/timeouts in one action)
//

/// Returns the new state after each message in the msgs map is delivered to the respective receiver
pure def apply_acceleration_msgs (state: Environment, msgs: ProcessID -> Set[Message]): Environment =
  msgs.keys().fold(state, (s, v) => 
    msgs.get(v).fold(s, (s2, msg) => 
      receive_one_message(s2, v, msg) 
    ))

pure def apply_acceleration_timeout (state: Environment, timeouts: ProcessID -> Set[TimeoutEvent]): Environment =
  timeouts.keys().fold(state, (s, v) => 
    timeouts.get(v).fold(s, (s2, timeout) => 
      fire_one_timeout_event(s2, v, timeout)
    ))

// non-deterministically pick a set of processes. For each, non-deterministically pick a set of votes
// and deliver them to the process in one action.
action acceleration_msgs = 
  nondet msgs = processes.map(v => s.msgBuffer.get(v).powerset().exclude(Set(Set())).map(m => (v, m)))
                .flatten().powerset().oneOf()
  val deliver = msgs.setToMap() 
  s' =  apply_acceleration_msgs(s, deliver)

action acceleration_timeouts = 
  nondet timeouts = processes.map(v => s.activeTimeouts.get(v).powerset().exclude(Set(Set())).map(t => (v, t)))
                .flatten().powerset().oneOf()
  val deliver = timeouts.setToMap() 
  s' =  apply_acceleration_timeout(s, deliver)


action accelerated_step = any {
  acceleration_msgs,
  acceleration_timeouts,
}  




} 



