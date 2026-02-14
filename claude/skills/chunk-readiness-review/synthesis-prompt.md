# Cross-Chunk Synthesis Prompt Template

This template is interpolated by the chunk-readiness-review skill orchestrator when there are 3+ chunks. Otherwise synthesis runs in the main context.

## Prompt

You are synthesizing adversarial reviews across chunks. Individual chunks have been pressure-tested. Your job is to find problems that only emerge when you look at the whole.

**Chunk Summaries:**
{{chunkSummaries}}

**Cross-Chunk Edges:**
{{crossChunkEdges}}

**Boundary Notes from Chunk Reviews:**
{{boundaryNotes}}

**Cohesion Verdicts:**
{{cohesionVerdicts}}

## Instructions

1. **Boundary Cohesion**
   - Do boundary notes from connected chunks agree on what's being exchanged?
   - If notes are missing or vague, use the Read tool to check the actual plan files.
   - Flag mismatches in naming, types, assumptions, or expectations.

2. **Workset Justification**
   - Look at the cohesion verdicts. If multiple chunks are `loosely_coupled` or `forced_grouping`, consider whether the chunk boundaries should move.
   - Would merging two chunks create a more natural unit?
   - Would splitting a chunk reduce coupling?
   - Are there plans that belong in a different chunk?

3. **Missing Chunks**
   - Are there boundary notes describing consumed interfaces that no chunk exposes?
   - Is there infrastructure or shared capability that multiple chunks assume exists but nobody builds?
   - Would adding a new chunk resolve dependency tangles?

4. **Assumption Conflicts**
   - Do different chunks make contradictory assumptions? (e.g., one assumes REST, another assumes GraphQL)
   - Are there shared concepts with inconsistent definitions across chunks?

## Output Format

Return ONLY a JSON object (no markdown fences, no preamble). Schema:

    {
      "crossChunkFindings": [
        {
          "type": "boundary_mismatch | regrouping_needed | missing_chunk | assumption_conflict",
          "severity": "error | warning | info",
          "plans": ["<plan-id>", ...],
          "chunks": ["<chunk-id>", ...],
          "description": "What's wrong across chunk boundaries",
          "impact": "What breaks if this isn't fixed",
          "suggestion": "How to restructure or fix",
          "category": "boundary_cohesion | workset_justification | missing_chunks | assumption_conflicts"
        }
      ],
      "overallVerdict": "READY | NEEDS_WORK | REGROUP",
      "verdictReason": "1-2 sentence justification",
      "summary": "Cross-chunk synthesis: <N> edges checked, <findings> issues. <brief>."
    }

Important:
- Return raw JSON only, no markdown code fences
- `overallVerdict` should be the worst case — if any chunk needs regrouping, verdict is REGROUP
- Be specific about which plans and chunks are involved
- If no issues found, return empty `crossChunkFindings` array with verdict READY
