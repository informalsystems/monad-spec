// -*- mode: Bluespec; -*-

/// Consensus State Machine Interface (CSMI)
module CSMI {
  // FIXME: I think this will not work with Apalache. However, I can't test this as this is resulting in a
  // huge spec once we resolve type aliases as the types are too complex. This exceeds the gRPC limit which
  // is already quite high.
  // My alternative approachh (sending processes as a param in init and step) is not working due to some
  // weird effect error on step.
  const processes: Set[p]

  type LocalState[process_id, other_fields] = { process_id: process_id | other_fields }

  type ConsensusResult[state, output] = {
    output: Set[output],
    state: state,
  }

  type TimeoutEvent[fields] = fields

  type Environment[p, s, m, te, b] = {
    system: p -> LocalState[p, s],
    msgBuffer: p -> Set[m],
    activeTimeouts: p -> Set[TimeoutEvent[te]],
    bookkeeping: b
  }

  type ReceiveMessage[p, s, m, o] = (LocalState[p, s], m) => ConsensusResult[LocalState[p, s], o]
  type ApplyEffect[p, s, m, te, o, b] =
    (Environment[p, s, m, te, b], p, ConsensusResult[LocalState[p, s], o]) => Environment[p, s, m, te, b]
  type FireTimeoutEvent[p, s, te, o] = (LocalState[p, s], TimeoutEvent[te]) => ConsensusResult[LocalState[p, s], o]

  var s: Environment

  // This specification is event triggered, so a process step can only be triggered by a message or a timeout
  // as a result we need either messages or timeouts in the initial state
  action init(
    initialize_process: (p) => LocalState[p, s],
    initial_message: m,
    initial_bookkeeping: b
  ): bool = all {
    s' = {
      system: processes.mapBy(v => initialize_process(v)),
      msgBuffer: processes.mapBy(v => Set(initial_message)),
      activeTimeouts: processes.mapBy(v => Set()),
      bookkeeping: initial_bookkeeping,
    }
  }

  //
  // Standard interleaving semantics (one process receives one message/timeout in one action)
  //

  pure def receive_one_message(
    state: Environment[p, s, m, te, b],
    v: p,
    msg: m,
    receive_message: ReceiveMessage[p, s, m, o],
    apply_effect: ApplyEffect[p, s, m, te, o, b],
  ): Environment[p, s, m, te, b] = {
    val consumed_msg_state = {
      ...state,
      msgBuffer: state.msgBuffer.setBy(v, b => b.exclude(Set(msg)))
    }
    val res = state.system.get(v).receive_message(msg)
    apply_effect(state, v, res)
  }

  pure def fire_one_timeout_event(
    state: Environment[p, s, m, te, b],
    v: p,
    timeout: TimeoutEvent[te],
    fire_timeout_event: FireTimeoutEvent[p, s, te, o],
    apply_effect: ApplyEffect[p, s, m, te, o, b]
  ): Environment[p, s, m, te, b] = {
    val consumed_timeout_state = {
      ...state,
      activeTimeouts: state.activeTimeouts.setBy(v, ts => ts.exclude(Set(timeout)))
    }
    val res = consumed_timeout_state.system.get(v).fire_timeout_event(timeout)
    apply_effect(consumed_timeout_state, v, res)
  }

  action receive_msg(
    v: p,
    msg: m,
    receive_message: ReceiveMessage[p, s, m, o],
    apply_effect: ApplyEffect[p, s, m, te, o, b]
  ): bool = all {
    s.msgBuffer.get(v).contains(msg),
    s' = receive_one_message(s, v, msg, receive_message, apply_effect)
  }

  action receive_some_msg(
    v: p,
    receive_message: ReceiveMessage[p, s, m, o],
    apply_effect: ApplyEffect[p, s, m, te, o, b]
  ): bool = all {
    s.msgBuffer.get(v) != Set(),
    nondet msg = oneOf(s.msgBuffer.get(v))
    receive_msg(v, msg, receive_message, apply_effect)
  }

  action timeout(
    v: p,
    timeout: TimeoutEvent[te],
    fire_timeout_event: FireTimeoutEvent[p, s, te, o],
    apply_effect: ApplyEffect[p, s, m, te, o, b]
  ): bool = all {
    s.activeTimeouts.get(v).contains(timeout),
    s' = fire_one_timeout_event(s, v, timeout, fire_timeout_event, apply_effect)
  }

  action some_timeout(
    v: p,
    fire_timeout_event: FireTimeoutEvent[p, s, te, o],
    apply_effect: ApplyEffect[p, s, m, te, o, b]
  ): bool = all {
    s.activeTimeouts.get(v) != Set(),
    nondet t = oneOf(s.activeTimeouts.get(v))
    timeout(v, t, fire_timeout_event, apply_effect)
  }

  action step(
    receive_message: ReceiveMessage[p, s, m, o],
    fire_timeout_event: FireTimeoutEvent[p, s, te, o],
    apply_effect: ApplyEffect[p, s, m, te, o, b]
  ): bool = {
    nondet v = oneOf(processes)
    any {
      receive_some_msg(v, receive_message, apply_effect),
      some_timeout(v, fire_timeout_event, apply_effect),
    }
  }

  //
  // Accelerated semantics (multiple process may receive multiple messages/timeouts in one action)
  //

  // Returns the new state after each message in the msgs map is delivered to the respective receiver
  // pure def apply_acceleration_msgs (state: Environment[m], msgs: ProcessID -> Set[m]): Environment[m] =
  //   msgs.keys().fold(state, (s, v) =>
  //     msgs.get(v).fold(s, (s2, msg) =>
  //       receive_one_message(s2, v, msg)
  //     ))

  // pure def apply_acceleration_timeout (state: Environment[s, m, te, b] timeouts: ProcessID -> Set[TimeoutEvent]): Environment[s, m, te, b]=
  //   timeouts.keys().fold(state, (s, v) =>
  //     timeouts.get(v).fold(s, (s2, timeout) =>
  //       fire_one_timeout_event(s2, v, timeout)
  //     ))

  // // non-deterministically pick a set of processes. For each, non-deterministically pick a set of votes
  // // and deliver them to the process in one action.
  // action acceleration_msgs =
  //   nondet msgs = processes.map(v => s.msgBuffer.get(v).powerset().exclude(Set(Set())).map(m => (v, m)))
  //                 .flatten().powerset().oneOf()
  //   val deliver = msgs.setToMap()
  //   s' =  apply_acceleration_msgs(s, deliver)

  // action acceleration_timeouts =
  //   nondet timeouts = processes.map(v => s.activeTimeouts.get(v).powerset().exclude(Set(Set())).map(t => (v, t)))
  //                 .flatten().powerset().oneOf()
  //   val deliver = timeouts.setToMap()
  //   s' =  apply_acceleration_timeout(s, deliver)


  // action accelerated_step = any {
  //   acceleration_msgs,
  //   acceleration_timeouts,
  // }

  pure def bookkeep(env: Environment[p, s, m, te, b], f: b => b): Environment[p, s, m, te, b] = {
    { ...env, bookkeeping: f(env.bookkeeping) }
  }
}
