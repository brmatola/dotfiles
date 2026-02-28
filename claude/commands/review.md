---
name: review
description: Run implementation review on current branch
argument-hint: [plan-name]
---

Use the `implementation-review` skill to audit the current branch.

If a plan name is provided (**$ARGUMENTS**), load it from `plans/active/` for plan compliance checking. If no plan name is provided, review the current branch's changes against the base branch.
