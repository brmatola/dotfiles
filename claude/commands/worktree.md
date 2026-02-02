---
name: worktree
description: Start or continue a worktree workflow for a plan
argument-hint: <plan-name> [start|implement|audit|reach|status|abort]
---

Use the `gremlins:worktree-workflow` skill to orchestrate development for plan: **$ARGUMENTS**

If no plan name is provided, check for existing workflow state and continue where left off.
