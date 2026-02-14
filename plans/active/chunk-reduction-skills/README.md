---
title: Chunk Reduction Skill Updates
status: not_started
tags: [skills, review, contracts]
depends_on: []
description: Update Claude skills to work with trellis plan contracts (inputs.md/outputs.md) for plan review, code review, and plan authoring
---

# Chunk Reduction Skill Updates

Update Claude Code skills to leverage trellis plan contracts for smarter plan development, review, and implementation verification.

## Context

The trellis `chunk-reduction` plan introduces plan contracts — `inputs.md` and `outputs.md` files that declare what a plan consumes and produces. This plan updates the skills that interact with plans to use these contracts.

## Skill Changes

### writing-plans

When creating a new plan:
- Create folder structure: `README.md`, `inputs.md`, `outputs.md`
- Pre-populate `inputs.md` "From plans" sections from `depends_on` in frontmatter
- Agent drafts `outputs.md` collaboratively with the human during planning
- A plan isn't marked ready until contracts are defined

### chunk-readiness-review (plan review)

Core question becomes: **"Assuming inputs are satisfied, can this plan deliver its stated outputs?"**

- Review subagents receive upstream `outputs.md` as context instead of full upstream plans
- New adversarial pass: contract coherence — do inputs of each plan match outputs of its dependencies?
- Cross-chunk synthesis focuses on interface width and contract mismatches
- Flag plans where inputs reference outputs that don't exist upstream

### requesting-code-review (implementation review)

Add output verification pass:
- Read the plan's `outputs.md`
- Verify each stated output exists in the implementation
- Types exported? Interfaces match? Files created where expected?
- This is the gate — downstream plans stay blocked until outputs are confirmed

### worktree-workflow

Chain the lifecycle:
- Plan review validates contracts before implementation starts
- Code review verifies outputs after implementation completes
- On code review pass, signal that downstream plans are unblocked
