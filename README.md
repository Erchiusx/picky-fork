# picky-fork

A Haskell runtime for node-scoped LLM history and selective subagent forks.
The master can inspect histories and explicitly choose the node/attempt
segments relevant to a child's task. The child receives that selection as its
model input prefix and contributes only its own new messages back to the trace.

## Runtime

`Agent.PickyFork.Agentic` exports the runtime API:

- `initialAgentState` initializes an isolated run.
- `executeNodeTask node action` selects the node's context using
  `envHistorySelectionPolicy`, updates `envCurrentNode`, seeds `stateOwnHistory`,
  and records the history returned by the action under that node and attempt.
- `runAgentLoop config prompt` executes bounded model/tool rounds against that
  context. Its result contains only the current node's messages, including its
  prompt, model responses and tool outputs. Set `loopRunModel = runLlm backend`
  and supply an explicit `loopRunTool` handler for workspace effects.
- `withSubagent forkId runId policy target childAction continuation` starts an
  isolated asynchronous child with a selection made by the parent. The
  continuation can run other work and call `joinSubagent`. Scope exit joins an
  unjoined worker; failure or cancellation cancels it. Joined child and descendant
  histories, artifacts and events are collected into the parent.

`runWorkflow` connects context selection to each SSL logic step. Use
`workflowStepInterpreter = \step -> runAgentLoop config (promptFor step)`.
An interpreter must return **only its own delta**, rather than copying its
inherited prefix into the result. This follows SeaOfGoals' `HistoryHandoff`
separation between inherited context and node-owned history.

Policies select the current node, the target plus direct control predecessors,
the target plus transitive control predecessors, or explicit node/attempt
pointers. Control predecessors follow `SceneNext` and `LogicStepNext` edges;
these policies do not infer semantic relevance or flatten scene boundaries.
Duplicate pointers are replayed once, using the newest available segment.

## Letting the master choose

Expose `delegationTools targets` in the master's request template and install
`runDelegationTool childConfig targets` as its tool handler:

```haskell
let targets = [DelegationTarget "analysis" "Analyze relevant evidence" childTarget]
    masterConfig = AgentLoopConfig
      { loopRequestTemplate = template{requestTools = delegationTools targets}
      , loopMaxTurns = 16
      , loopRunModel = runLlm backend
      , loopRunTool = runDelegationTool childConfig targets
      }
runAgentic env initialAgentState $
  executeNodeTask masterNode $
    runAgentLoop masterConfig [masterPrompt]
```

The master can call `list_context` to inspect node histories and their stable
keys, then call:

```json
{"target":"analysis","task":"Explain the failure","history":["<key from list_context>"]}
```

`delegate_task` validates every key, forks at the named target, passes **only**
those histories plus the child config's instructions and the task, and returns
its result to the master. An empty `history` array starts without inherited
context. This model-facing tool waits for its child; the lower-level
`withSubagent` API also supports overlapping workers.

Selection granularity is a node/attempt segment, not individual messages.
A running master's own messages are available before its node finishes;
pending tool calls without outputs are excluded from that snapshot. Child
context is fixed at fork time. A child does not inherit the master's complete
trace, conversation ID or tools unless the host supplies them in its config.
Use a fresh request template for children, without a parent conversation ID.

## History and limits

Runtime histories store chronological completed turns in `historyInputs`.
`InputResponse` preserves model messages, tool calls and provider response
items for subsequent requests. `historyResponses` is an optional pending tail;
`historyInputItems` replays it once. OpenAI tool calls replay as top-level
`function_call` items with JSON-string arguments, followed by matching outputs;
Gemini replay uses function-call and function-response parts. Raw provider
items are provider-specific; use histories with their originating backend.

LLM, tool and fork budgets are per agent run, not a shared tree-wide quota.
Tool errors returned as `Left Text` become failed tool outputs so the model can
recover. Model errors, exhausted budgets and child failures fail the run.
The existing `runAgentic` transformer discards state/events on `Left`; callers
needing failure-time streaming traces should use an external event sink.

The runtime is a library; the existing executable entry point is still a stub.

## Verification

```sh
cabal build all --offline
cabal test picky-fork-runtime-test --offline --test-show-details=direct
# Existing fixture tests, explicitly skipping paid provider calls:
env -u OPENAI_API_KEY -u GEMINI_API_KEY cabal test all --offline --test-show-details=direct
```

The deterministic runtime suite exercises all 30 fixture workflow steps,
direct and transitive selection, exact attempts, selective model-driven
delegation, multi-turn tool replay, overlapping and nested workers, joins,
budgets, child errors and cancellation cleanup. Provider serialization is
checked locally; these tests do not establish live API compatibility or model
selection quality.
