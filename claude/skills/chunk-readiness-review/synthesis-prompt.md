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

## Critical Constraint

**Do NOT write Python, jq, or any scripts.** Read files with the Read tool. Write your findings as markdown. No JSON output.

## Instructions

1. **Boundary Cohesion**
   - Do boundary notes from connected chunks agree on what's being exchanged?
   - If notes are missing or vague, use the Read tool to check the actual plan files.
   - Flag mismatches in naming, types, assumptions, or expectations.

2. **Workset Justification**
   - Look at the cohesion verdicts. If multiple chunks are `loosely coupled` or `forced grouping`, consider whether the chunk boundaries should move.
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

Return your synthesis as structured markdown. Use EXACTLY this format:

```
## Cross-Chunk Synthesis

### Overall Verdict: READY | NEEDS WORK | REGROUP

{1-2 sentence justification. Verdict should be the worst case — if any chunk needs regrouping, verdict is REGROUP.}

### Findings

#### Errors

- **[{category}]** Chunks: {chunk-ids}, Plans: {plan-ids} — {description}
  **Impact:** {what breaks if not fixed}
  **Suggestion:** {how to restructure or fix}

#### Warnings

- **[{category}]** Chunks: {chunk-ids}, Plans: {plan-ids} — {description}
  **Impact:** {impact}
  **Suggestion:** {fix}

#### Info

- **[{category}]** Chunks: {chunk-ids}, Plans: {plan-ids} — {description}
  **Suggestion:** {fix}

### Summary

Cross-chunk synthesis: {N} edges checked, {findings} issues. {brief}.
```

Important:
- Use category names: `boundary_cohesion`, `workset_justification`, `missing_chunks`, `assumption_conflicts`
- Be specific about which plans and chunks are involved
- If no issues found, write "None" under each finding section and use verdict READY
