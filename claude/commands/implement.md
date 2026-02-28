---
name: implement
description: Execute a trellis plan by name
argument-hint: <plan-name> [--batched]
---

Use the `executing-plans` skill to implement plan: **$ARGUMENTS**

Load the plan from `plans/active/` and execute it task-by-task. If `--batched` is specified, stop every 3 tasks for a checkpoint. Otherwise run autonomously until complete or blocked.
