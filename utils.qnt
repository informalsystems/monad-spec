// -*- mode: Bluespec; -*-

module utils {
  import basicSpells.* from "lib/basicSpells"
  import types.* from "types"

  pure def noop(s: LocalState): Transition = {
    { postState: s, effects: Set() }
  }

  pure def join_results(
    t1: Transition,
    t2: Transition
  ): Transition = {
    { postState: t2.postState, effects: t1.effects.union(t2.effects) }
  }

  pure def and_then(
    t: Transition,
    f: LocalState => Transition
  ): Transition =
    join_results(t, f(t.postState))

  pure def and_then_state(
    state: LocalState,
    f: LocalState => r
  ): r = f(state)
  pure def get_proposals(
    messages: Set[Message]
  ): Set[Proposal] =
    messages.filterMap(m => {
      match m {
        | Prop(p) => Some(p)
        | _ => None
      }
    })

  pure def get_votes(
    messages: Set[Message]
  ): Set[VoteMsg] =
    messages.filterMap(m => {
      match m {
        | Vote(v) => Some(v)
        | _ => None
      }
    })

  pure def get_timeouts(
    messages: Set[Message]
  ): Set[TimeoutMsg] =
    messages.filterMap(m => {
      match m {
        | Timeout(t) => Some(t)
        | _ => None
      }
    })

  pure def get_timeout_events(
    events: Set[InternalEvent]
  ): Set[TimeoutData] =
    events.filterMap(e => {
      match e {
        | TimeoutEvent(data) => Some(data)
        | _ => None
      }
    })

  pure def displayer(
    env: Environment
  ): Address -> Set[ProposalID] = {
    env.logging.committed_proposals.
      transformValues(s => s.map(p => p.proposal_id))
  }
}
