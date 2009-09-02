# state machines

This is a little library for state machines. These are definitely not finite state machines, or anything really formal. It is also not optimized in any way.

# reference

Macro `define-state-machine name` creates a state machine with `name`. Any existing states will be retained. Name must be a symbol.

Macro `in-state-machine name` sets the `*current-state-machine*` variable, making it a default for defining states.

Macro `defsm* name` just does both of the above.

Function `drop-state-machine name` erases the state machine.

Function `find-state-machine name` finds a state machine named by symbol `name`

Class `state-machine-state` is a state machine instance which will be passed through all states, so it might be a good place to keep any additional state. It has to be initialized with arguments `:state-machine` and `:next-state` to be ready for execution.

Generic Function `drive-state-machine state-machine-state &rest driver-args` will execute next state of state machine of `state-machine-state`, passing `driver-args` as driver arguments (see `defstate`).

Macro `defstate name-and-options state-args driver-args &body body` defines a state. `name-and-options` is either a symbol naming a state, or a list of form `(name . option-plist)`, with option keys being `:state-machine` naming a state machine (with `*current-state-machine*` being the default) and `:state` with a value being a symbol to which `state-machine-state` object is bound. `state-args` is a destructuring lambda list for arguments passed to `next-state`, and `driver-args` is a destructuring lambda list for arguments passed to `drive-state-machine`.

Local function `next-state next-state &rest next-state-args` is active within `defstate` body. It immediately terminates the execution of the state body and prepares the `state-machine-state` object for next execution. Special values of `next-state` are `nil`, which means termination of state machine, possibly returning to a higher in chain, and `t` which means repeat current state, possibly with new arguments.

Local function `sub-machine next-state sub-machine sub-state &rest next-state-args` immediately terminates the execution of the state body and moves the `state-machine-state` object into another state machine, with the initial state `sub-state` and state arguments `next-state-args`. When that machine terminates `state-machine-state` object is moved into `next-state` state with whatever arguments the submachine terminated with.
