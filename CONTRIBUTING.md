# Contributing

### Assembly Calling Convention
The caller first pushes space for the return value onto the stack, as a stack var.
Then the caller pushes the parameters in order onto the stack, as stack vars. Then,
after the callee returns, the caller pops the stack vars off the stack, and pops
the return value stack var onto its local stack.

