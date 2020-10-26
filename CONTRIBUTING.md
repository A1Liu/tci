# Contributing

### Websocket Interface
Defined in `src/commands.rs`. Send data using JSON in this interface:

```json
{
  "command": "AddFile",
  "data": {
    path: "test/hello_world.c",
    data: "...",
  }
}
```

The available commands are in `src/commands.rs`:

```rust
#[derive(Deserialize, Serialize)]
#[serde(tag = "command", content = "data")]
pub enum Command {
    AddFile(String),
    Compile,
    RunUntilScopedPC(u32),
    RunOp,
    RunCount(u32),
    RunCountOrUntil {
        count: u32,
        stack_size: u16,
        pc: u32,
    },
}
```

The responses are defined in `src/commands.rs`, and are in this format:

```json
{
  "response": "StatusRet",
  "data": {
    "status": { ... },
    "ret": 0,
  }
}
```

The available responses are in `src/commands.rs`:

```rust
#[derive(Serialize)]
#[serde(tag = "response", content = "data")]
pub enum CommandError {
    IOError(String),
    CompileError(String),
    RuntimeError(String),
    InvalidCommand,
}

#[derive(Serialize)]
#[serde(tag = "response", content = "data")]
pub enum CommandResult {
    Confirm(Command),
    Compiled(Program<'static>),
    Status(RuntimeDiagnostic),
    StatusRet { status: RuntimeDiagnostic, ret: i32 },
}
```


### Implicit Contracts
- Never have a `TCType` with `kind = TCTypeKind::Uninit` and `pointer_count > 0`
- The `cb!()` macro relies on the length of `ops`. When you use it, make sure you've
  first added all relevant temporaries to ops, or add the length of those teporaries
  to the value of `cb!()`

### Assembly Calling Convention
The caller first pushes space for the return value onto the stack, as a stack var.
Then the caller pushes the parameters in order onto the stack, as stack vars. Then,
after the callee returns, the caller pops the stack vars off the stack, and pops
the return value stack var onto its local stack.

On variable argument functions, the caller also pushes an integer value onto the stack
as the last parameter, with a count of how many parameters there are.
