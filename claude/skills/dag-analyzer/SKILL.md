---
name: dag-analyzer
description: Use when you have a large set of interconnected trellis plans and need to verify the DAG is sound before implementation begins
---

# DAG Analyzer

## Overview

Review a trellis plan DAG for accuracy, completeness, and consistency. Scales to 100+ plans by decomposing the graph into parallel analysis lines.

**Core principle:** Decompose the DAG into independent analysis lines, dispatch parallel agents, then synthesize findings ordered by downstream impact.

## When to Use

**Use when:**
- You have a trellis-managed plan set and need to verify the DAG before implementation
- Plans number 20+ and manual review would miss cross-plan inconsistencies
- Strategy/architecture documents exist to check plans against
- Interface specs (OpenAPI, schema DDL, etc.) are available as ground truth

**Don't use when:**
- Plans are still being written (review after they stabilize)
- No authoritative specs exist to check against (accuracy axis is meaningless)
- Fewer than 10 plans with simple linear dependencies (just read them)

## Prerequisites

- `trellis status` showing all plans
- Strategy/architecture documents the plans were derived from
- Interface specifications (OpenAPI, schema definitions, etc.)
- Plans must have populated `implementation.md`, `inputs.md`, and `outputs.md`

## The Three Axes

Every plan is reviewed against three criteria:

### Accuracy — Does the plan match the authoritative specs?

- Field names, types, enum values match interface contracts
- API paths, HTTP methods, status codes match OpenAPI specs
- Schema columns, constraints, PKs match canonical model
- Auth flows, token formats, cookie paths match security specs
- Config values (timeouts, limits, intervals) match documented values

### Completeness — Does the plan cover everything it needs to?

- All spec-defined endpoints have an owning plan
- Testing is specified (unit, integration, e2e as appropriate)
- Error handling, edge cases, and failure modes are addressed
- Observability, analytics, and deployment concerns are covered
- Done-when criteria are concrete and verifiable

### Consistency — Are the DAG edges correct?

- Every `depends_on` edge corresponds to a real build-time dependency
- Every import/consumption in `inputs.md` has a matching `depends_on`
- Every export in `outputs.md` is consumed by at least one downstream plan
- No circular dependencies or missing transitive edges
- Layer and epic tags are consistent within logical groups

## Step 1: Extract the Graph

```
trellis graph
```

From the graph data, extract:
- **All nodes** relevant to the project (filter by repo prefix or tag)
- **All edges** between those nodes
- **Root nodes** (no incoming edges) — implementation starting points
- **Leaf nodes** (no outgoing edges) — integration/smoke test plans

Count the nodes and edges. For a project with 100+ plans, expect 300-500 edges.

## Step 2: Identify Analysis Lines

Decompose the DAG into 6-10 logical lines that trace paths from roots to leaves. Each line should:

- Follow a coherent architectural concern (types -> tools -> adapters, or scaffold -> schema -> service -> API -> smoke test)
- Stay mostly within one repo when possible
- Overlap at boundary plans (a plan can appear in multiple lines)
- Cover every node in the DAG at least once

Good decomposition patterns:
- **By repo** — one line per repo if repos are relatively independent
- **By layer** — foundation -> data -> service -> API -> frontend
- **By epic** — auth line, domain line, infrastructure line
- **By data flow** — trace the path a user request takes through the system

Example for a microservices project:
1. SDK types and shared packages
2. Database schemas and migrations
3. Each API service (domain, research, import, etc.)
4. Auth and admin
5. Agent/AI layer
6. Frontend app
7. Infrastructure (Terraform, Helm, ops)

## Step 3: Gather Reference Documents

Before launching analysis agents, identify all authoritative sources:

- **Strategy docs** — architecture, epics, critical path, vision
- **Interface specs** — OpenAPI YAML, schema DDL, SSE event catalogs, gateway configs, style guides
- **Existing implementation** — if migrating from a prior codebase, the source of truth for entity shapes, tool surfaces, etc.

Map where these live in the filesystem so agents can read them directly.

## Step 4: Launch Parallel Agents

For each analysis line, launch a `sonnet-general-purpose` agent with a prompt that includes:

1. **The line definition** — which plans to review, in dependency order
2. **Plan file locations** — absolute paths to each plan's directory
3. **Reference spec locations** — paths to the interface specs to check against
4. **The three-axis framework** — accuracy, completeness, consistency
5. **Output format** — structured report with severity ratings

### Agent Prompt Template

```
Review the following plans in dependency order: [plan list]

Plan files are at: [repo]/plans/[plan-name]/{README.md, implementation.md, inputs.md, outputs.md}

Cross-reference against these specs:
- [path to OpenAPI spec]
- [path to schema spec]
- [path to interface contract]

For each plan, evaluate:
1. ACCURACY: Does implementation.md match the spec? Check field names, types,
   paths, status codes, enum values, config values.
2. COMPLETENESS: Are all spec endpoints covered? Is testing specified? Are
   error cases handled? Are done-when criteria concrete?
3. CONSISTENCY: Do inputs.md items match upstream outputs.md? Are depends_on
   edges correct and complete? Are layer/epic tags consistent?

Report format:
- Section 1: Accuracy findings (with specific field-level evidence)
- Section 2: Completeness gaps (missing endpoints, missing tests, missing plans)
- Section 3: Consistency issues (wrong edges, missing edges, tag mismatches)
- Section 4: Gaps and recommendations (prioritized by severity)
- Summary table with severity ratings
```

### Parallelism Tips

- Launch all line agents simultaneously — they are independent
- Each agent reads 10-25 plans plus specs, which fits in a single context
- Agents should read actual plan files, not summarize from memory
- Use `sonnet-general-purpose` for the balance of depth and cost
- If a line has 25+ plans, split it into sub-lines

## Step 5: Synthesize Findings

After all agents complete, synthesize into a unified report:

1. **Critical Issues** — build failures, security vulnerabilities, runtime errors
2. **High-Severity Issues** — spec deviations with functional impact
3. **Missing DAG Edges** — table: from | to | reason
4. **Missing Plans** — table: plan name | repo | depends on | rationale
5. **Internal Inconsistencies** — where plans contradict themselves or each other
6. **Strategic Gaps** — higher-level coverage problems

Deduplicate across agents — the same issue often appears in multiple lines when it sits at a boundary plan.

Prioritize by **downstream impact**: a type mismatch in a root plan is critical because it propagates everywhere; a missing test in a leaf plan is low severity.

## Step 6: Remediation Order

Order fixes by risk of downstream rework:

1. Type/schema mismatches in root plans (propagation risk)
2. Security issues (compliance risk)
3. Build-blocking bugs (velocity risk)
4. Spec routing/contract mismatches (integration risk)
5. Missing DAG edges (build-order risk)
6. Missing plans (coverage risk)
7. Internal inconsistencies (implementer confusion risk)
8. Strategic gaps (long-term risk)

## Pitfalls

**Don't skip reading the actual plan files.** Agents must read `implementation.md`, not just `README.md`. Most issues are in the implementation details — stale test code, wrong field names, phantom table references.

**Don't trust plan descriptions over specs.** When a plan says "implements the foo endpoint" but the implementation details show a different path or schema, the spec wins. Plans are proposals; specs are contracts.

**Don't ignore outputs.md / inputs.md mismatches.** These are the most common source of integration failures. If plan A's outputs.md exports `FooOperations` but plan B's inputs.md imports `FooOps`, that's a real bug.

**Don't analyze plans in isolation.** The value of DAG review is catching cross-plan inconsistencies. An agent reviewing a single plan in isolation will miss that its outputs don't match what downstream plans expect.

**Watch for stale artifacts.** Plans that went through multiple design iterations often have implementation.md and outputs.md from different iterations. Test code is especially prone to staleness — it's often written for an earlier design and never updated when the implementation section changed.

**Check both directions on edges.** If plan A says it outputs X for plan B, verify that plan B's `depends_on` includes plan A AND that plan B's `inputs.md` references X. Missing edges in either direction are real bugs.

## Scaling

| DAG Size | Lines | Agent Type | Wall Clock |
|----------|-------|------------|------------|
| 20-40 plans | 3-4 | sonnet | ~5 min |
| 40-80 plans | 5-7 | sonnet | ~8 min |
| 80-150 plans | 7-10 | sonnet | ~12 min |
| 150+ plans | 10-15 | sonnet, split large lines | ~15 min |

The bottleneck is agent execution time, not synthesis. Launching all agents in parallel means wall clock scales with the largest line, not the total plan count.
