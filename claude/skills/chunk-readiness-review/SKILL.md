---
name: chunk-readiness-review
description: "Use when you have a plan with trellis chunks and want to pressure-test whether a chunk represents a cohesive, buildable featureset before implementation"
user_invocable: true
---

# Chunk Readiness Review

Adversarial critique of chunk content. Dispatches subagents to actively break plans — find hidden assumptions, complexity traps, cohesion gaps, and missing edge cases.

**This is NOT a structural checklist.** This is a pressure test. The reviewer's job is to find reasons the chunk will fail during implementation.

**Announce at start:** "I'm using the chunk-readiness-review skill to pressure-test this chunk."

## Invocation

```
/chunk-readiness-review              # review all chunks
/chunk-readiness-review core-data    # review a single chunk
```

## Critical Constraint: No Scripts

**Do NOT write Python, jq, or any scripts to process data.** You can read JSON output from commands directly — extract what you need by reading it. All output is markdown. Work in your strongest modality: reading text, writing prose.

## The Process

### Phase 0: Discover Chunks

Run `trellis chunks --json` via Bash. Read the JSON output directly — you can extract chunk IDs, plan paths, edges, and line counts from the text without any scripting.

If a specific chunk name was passed as argument, focus on only that chunk.

If the command fails, stop immediately — `trellis` must be available.

### Phase 1: Parallel Adversarial Reviews

For each chunk, dispatch a `sonnet-general-purpose` subagent via the Task tool. All chunk agents run **in parallel** (multiple Task calls in one message).

Each subagent receives a prompt built from `./chunk-critique-prompt.md`. Interpolate these values by reading them from the trellis output:
- `{{chunkId}}` — the chunk's ID
- `{{planList}}` — newline-separated list of `planId: filePath` entries
- `{{internalEdges}}` — the chunk's internal dependency edges as `from -> to` lines
- `{{crossChunkEdges}}` — cross-chunk edges touching this chunk
- `{{totalLines}}` — total line count for the chunk

The subagent reads plan files and runs **4 adversarial passes** — cohesion, assumptions, edge cases, and complexity traps. Each subagent returns **structured markdown** findings (see chunk-critique-prompt.md for format).

**Error handling:** If a subagent fails, note it as a finding in the report. Don't block other chunks.

### Phase 2: Cross-Chunk Synthesis

Read all chunk review results and boundary notes.

If there are more than 3 chunks, dispatch an `opus-general-purpose` subagent. Otherwise, perform synthesis in the main context.

The synthesis uses the prompt from `./synthesis-prompt.md` and checks:
- **Boundary cohesion:** Do connected chunks agree on what crosses boundaries?
- **Workset justification:** Should any chunk be regrouped?
- **Missing chunks:** Is there a chunk-shaped gap nobody covers?

### Phase 3: Report

Compose the final report as markdown and write it to `plans/.review/readiness/latest.md`.

Auto-add `plans/.review/` to `.gitignore` if missing.

When writing the report, naturally consolidate duplicate findings (same category affecting the same plans). Keep the higher severity version.

Order findings: errors first, then warnings, then info. Within each severity, group by chunk.

### Report Format

```markdown
# Chunk Readiness Review
Generated: {timestamp}
Chunks reviewed: {N} | Plans reviewed: {N}

## Verdict: READY | NEEDS WORK | REGROUP

{1-2 sentence justification}

## Errors ({count})

### {chunkId}: {plan1} + {plan2}
- **[{category}]** {description}
  Why this matters: {impact}
  Suggestion: {suggestion}

## Warnings ({count})
...

## Cross-Chunk Findings ({count})
...
```

## Verdicts

| Verdict | Meaning | Next Step |
|---------|---------|-----------|
| **READY** | Chunk is a cohesive, buildable unit | Proceed to implementation |
| **NEEDS WORK** | Plans have gaps or flawed assumptions | Fix plans, re-review |
| **REGROUP** | Plans don't form a coherent unit | Restructure chunks, re-review |

## What This Is Not

- **Not `plan-readiness-review`** — that's a lightweight structural gate for individual plans (sections present, scope right).
- **Not `plan-review`** — that's post-implementation verification (did we build what the plan said).
- **This skill** — adversarial pre-implementation critique at the chunk level. Does this workset hold together? Will the implementer hit walls?
