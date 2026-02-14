---
name: plan-review
description: "Use after implementing a plan to verify completeness, correctness, and merge safety - the post-implementation gate"
user_invocable: true
---

# Plan Review

Orchestrate multi-agent review of a trellis plan set using chunks as the unit of work. Produces structured findings for consistency, correctness, dependency coherence, and scope/feasibility.

## Invocation

```
/plan-review              # review all chunks in current project
/plan-review core-data    # review a single chunk
/plan-review --recheck    # only re-review stale or failing chunks
```

## The Process

### Phase 0: Discover Chunks

Run `trellis chunks --json` via Bash to get the chunk structure. Parse the JSON output.

If a specific chunk name was passed as argument, filter to only that chunk.

If the command fails, stop immediately with a clear error — `trellis` must be available.

### Phase 1: Parallel Chunk Reviews

For each chunk, dispatch a `sonnet-general-purpose` subagent via the Task tool. All chunk agents run **in parallel** (multiple Task calls in one message).

Each subagent receives a prompt built from `./chunk-reviewer-prompt.md` with these interpolated values:
- `{{chunkId}}` — the chunk's ID
- `{{planList}}` — newline-separated list of `planId: filePath` entries
- `{{internalEdges}}` — the chunk's internal dependency edges as `from -> to` lines
- `{{crossChunkEdges}}` — cross-chunk edges touching this chunk, as `from (fromChunk) -> to (toChunk)` lines
- `{{totalLines}}` — total line count for the chunk

The subagent reads plan files itself using the Read tool and runs 4 review passes (contract alignment, dependency coherence, internal consistency, scope/feasibility).

Each subagent returns structured JSON findings. Parse the JSON from the agent's response.

**Error handling:** If a subagent fails (timeout, malformed output), log to stderr and record as a finding with `type: "review_error"`. Don't block other chunks.

**`--recheck` mode:** Before dispatching, read existing `plans/.review/chunks/{chunkId}.json`. Skip chunks where:
- The cached report exists AND
- All member plan files are unmodified since `generatedAt` AND
- The chunk membership (plan IDs) matches AND
- The cached report has no errors or warnings

### Phase 2: Cross-Chunk Synthesis

Read all chunk summaries and boundary notes plus `crossChunkEdges` from the chunks JSON.

If there are more than 3 chunks, dispatch an `opus-general-purpose` subagent for synthesis (3+ chunks produce enough cross-boundary context to benefit from deeper reasoning; fewer fit comfortably in the main context). Otherwise, perform synthesis in the main context.

The synthesis agent/logic receives a prompt built from `./synthesis-prompt.md` with:
- `{{chunkSummaries}}` — JSON array of chunk review summaries
- `{{crossChunkEdges}}` — the full cross-chunk edges array
- `{{boundaryNotes}}` — all boundary notes from chunk reviews

It checks:
- **Boundary compatibility:** Do boundary notes from connected chunks agree?
- **Summary consistency:** Contradictions between chunk assumptions?
- **Coverage gaps:** Consumed interfaces that no chunk exposes?

### Phase 3: Cleanup & Report

1. **Clean stale artifacts:** Delete `plans/.review/chunks/*.json` for chunks whose membership changed or plans were modified.

2. **Deduplicate findings:** Two findings are duplicates if they share the same `type`, same set of `plans` (order-independent), and same `category`. Keep the higher severity, more specific description.

3. **Write reports:**
   - `plans/.review/latest.json` — full structured report
   - `plans/.review/latest.md` — human-readable summary (format below)
   - `plans/.review/chunks/{chunkId}.json` — per-chunk cached reports

4. **Gitignore:** Auto-add `plans/.review/` to `.gitignore` if the entry is missing.

5. **Sort findings:** severity (error > warning > info), then alphabetically by first plan ID.

### Human-Readable Report Format

Write `plans/.review/latest.md`:

```markdown
# Plan Review Report
Generated: {timestamp}
Chunks reviewed: {N} | Plans reviewed: {N}

## Errors ({count})

### {plan1} + {plan2}
- **[{category}]** {description}
  Suggestion: {suggestion}

## Warnings ({count})
...

## Info ({count})
...

## Cross-Chunk Findings ({count})
...
```

## Prompt Templates

Subagent prompts are in sibling files:
- `./chunk-reviewer-prompt.md` — dispatched per chunk
- `./synthesis-prompt.md` — dispatched for cross-chunk analysis
