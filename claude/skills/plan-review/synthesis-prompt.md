# Cross-Chunk Synthesis Prompt Template

This template is interpolated by the plan-review skill orchestrator before being passed to an `opus-general-purpose` subagent (if >3 chunks) or used directly in the main context.

## Prompt

You are performing cross-chunk synthesis for a trellis plan review. Individual chunks have already been reviewed. Your job is to check for issues that span chunk boundaries.

**Chunk Summaries:**
{{chunkSummaries}}

**Cross-Chunk Edges:**
{{crossChunkEdges}}

**Boundary Notes from Chunk Reviews:**
{{boundaryNotes}}

## Instructions

1. For each cross-chunk edge, check **boundary compatibility**:
   - Do the boundary notes from both sides agree on what's being exchanged?
   - If notes are missing or ambiguous, use the Read tool to read the specific plan files at that boundary (the plans on either side of the edge).
   - Flag mismatches in naming, types, assumptions, or expectations.

2. Check **summary consistency**:
   - Are there contradictions between chunk summaries?
   - e.g., one chunk assumes JWT auth while another assumes session-based auth
   - e.g., one chunk defines a REST API while a consumer expects GraphQL

3. Check **coverage gaps**:
   - Are there boundary notes describing consumed interfaces that no other chunk exposes?
   - Are there plans that reference capabilities not covered by any plan?

## Output Format

Return ONLY a JSON object (no markdown fences, no preamble). Schema:

    {
      "crossChunkFindings": [
        {
          "type": "boundary_mismatch | summary_contradiction | coverage_gap | integration_risk",
          "severity": "error | warning | info",
          "plans": ["<plan-id>", ...],
          "chunks": ["<chunk-id>", ...],
          "description": "Clear description of the cross-chunk issue",
          "suggestion": "Actionable suggestion to resolve it",
          "category": "boundary_compatibility | summary_consistency | coverage_gap"
        }
      ],
      "summary": "Cross-chunk synthesis: <N> edges checked, <findings> issues found. <brief>."
    }

Important:
- Return raw JSON only, no markdown code fences
- Include the chunk IDs in each finding's `chunks` array
- Reference specific plan IDs in the `plans` array
- If no issues found, return empty `crossChunkFindings` array
- Be specific about what each side expects vs provides
