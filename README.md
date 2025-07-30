# MonadBFT Consensus specification in Quint

A Quint specification of MonadBFT consensus, based on the
[paper](https://arxiv.org/abs/2502.20692), using a consensus library that is still under development.

## Findings
The specification is a translation of the pseudo-code in the paper. The main goal of this work is to check some properties of the protocol.
The file `monadbft.qnt` contains warning annotations under `// Warning` that indicate unspecified behavior in the paper or potential issues in the protocol. Those were already flagged to the Monad team and they confirmed there is no significant problem.

## Properties

We define the following properties:
- **safety**
- **no-tail-forking** (NTF)

To check that they hold in the absence of byzantine nodes, run the simulator:

``` sh
quint run instance.qnt --invariant=safety --hide monadbft::csmi::s
```

``` sh
quint run instance.qnt --invariant=ntf --hide monadbft::csmi::s
```
to disable timeout events in the simulator, run with the option `--step=step_no_timeout`. To show the full state in the output, remove the `--hide` option, or change the definition of `displayer` in `utils.qnt` to include the data you want to see.

## Witnesses

We provide witnesses to show interesting execution scenarios. To look for a case where a block is reproposed in a subsequent view, run:

``` sh
quint run instance.qnt --invariant=reproposal_w
```

As the state is quite big, it's easier to see the reproposals in the REPL. Run this command to enter the REPL with the `test` module already loaded:

``` sh
quint -r instance.qnt::instance
```

Then, you can use `q::test` to run the simulation and find a violation inside the REPL:

```
Quint REPL 0.25.1
Type ".exit" to exit, or ".help" for more information
>>> q::test(10000, 20, 1, init, step, reproposal_w)
"violation"
>>> reproposals
// shows pairs of different proposals for the same block
>>> q::lastTrace
// shows the entire trace leading to this point
```
