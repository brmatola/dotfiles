# Chunk Review Prompt Template

This template is interpolated by the plan-review skill orchestrator before being passed to a `sonnet-general-purpose` subagent via the Task tool.

## Prompt

You are reviewing a chunk of plans from a trellis project. Your job is to read each plan file and check for issues across four review passes.

**Chunk:** {{chunkId}}
**Total lines:** {{totalLines}}

**Plans to review:**
{{planList}}

**Internal dependency edges (within this chunk):**
{{internalEdges}}

**Cross-chunk edges (boundaries with other chunks):**
{{crossChunkEdges}}

## Instructions

1. Use the Read tool to read each plan file listed above. Read ALL files before starting analysis.

2. Run four review passes:

### Pass 1: Contract Alignment
- Do implementation plans match their contract specs?
- Missing fields, wrong types, contradictory behavior?
- Are API definitions consistent between contract and implementation?

### Pass 2: Dependency Coherence
- Are `depends_on` edges correct based on actual content references?
- Missing dependencies (plan references work defined in another plan but doesn't depend on it)?
- Unnecessary edges (dependency declared but nothing from that plan is actually used)?
- Implicit circular dependencies not caught by the DAG?

### Pass 3: Internal Consistency
- Do plans within this chunk agree on types, naming, APIs, assumptions?
- Are there contradictions between plans?
- Do shared concepts have consistent definitions?

### Pass 4: Scope & Feasibility
- Is each plan reasonably sized for a single unit of work?
- Is anything obviously missing that should be a plan?
- Should any plan be split or merged?

3. For each issue found, create a finding object.

4. Write boundary notes for any plan that provides to or requires from external chunks (based on the cross-chunk edges listed above).

## Output Format

Return ONLY a JSON object (no markdown fences, no preamble). Schema:

    {
      "chunkId": "{{chunkId}}",
      "generatedAt": "<ISO timestamp>",
      "plansReviewed": ["<plan-id>", ...],
      "findings": [
        {
          "type": "inconsistency | missing_dep | unnecessary_dep | contract_gap | scope_issue | missing_plan",
          "severity": "error | warning | info",
          "plans": ["<plan-id>", ...],
          "description": "Clear description of the issue",
          "suggestion": "Actionable suggestion to fix it",
          "category": "contract_alignment | dependency_coherence | internal_consistency | scope_feasibility"
        }
      ],
      "summary": "<N> plans reviewed. <errors> errors, <warnings> warnings. Main issue: <brief>.",
      "boundaryNotes": [
        {
          "planId": "<plan-id>",
          "direction": "exposes | requires",
          "description": "What this plan provides to or requires from other chunks"
        }
      ]
    }

Important:
- Return raw JSON only, no markdown code fences
- Include ALL plans in `plansReviewed` even if they have no findings
- If no issues found, return empty `findings` array
- Always include `boundaryNotes` for plans with cross-chunk edges
- Use the exact plan IDs as listed above (not file paths)
