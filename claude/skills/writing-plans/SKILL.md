---
name: writing-plans
description: Use when you have a spec or requirements for a multi-step task, before touching code
---

# Writing Plans

## Overview

Write comprehensive implementation plans assuming the engineer has zero context for our codebase and questionable taste. Document everything they need to know: which files to touch for each task, code, testing, docs they might need to check, how to test it. Give them the whole plan as bite-sized tasks. DRY. YAGNI. TDD. Frequent commits.

Assume they are a skilled developer, but know almost nothing about our toolset or problem domain. Assume they don't know good test design very well.

**Announce at start:** "I'm using the writing-plans skill to create the implementation plan."

**Context:** This should be run in a dedicated worktree (created by brainstorming skill).

**Save plans to:** `plans/active/{plan-name}/implementation/`

Create multiple focused files that cross-reference each other (e.g., `phase-1.md`, `phase-2.md`, `setup.md`).

## Bite-Sized Task Granularity

**Each step is one action (2-5 minutes):**
- "Write the failing test" - step
- "Run it to make sure it fails" - step
- "Implement the minimal code to make the test pass" - step
- "Run the tests and make sure they pass" - step
- "Commit" - step

## Plan Document Header

**Every plan file MUST start with this header:**

```markdown
# [Phase/Component Name] Implementation

> **For Claude:** REQUIRED SUB-SKILL: Use gremlins:executing-plans to implement this plan task-by-task.

**Goal:** [One sentence describing what this builds]

**Architecture:** [2-3 sentences about approach]

**Tech Stack:** [Key technologies/libraries]

**Related:** [Links to other plan files: ../design/overview.md, ./phase-2.md, etc.]

---
```

## Task Structure

```markdown
### Task N: [Component Name]

**Files:**
- Create: `exact/path/to/file.py`
- Modify: `exact/path/to/existing.py:123-145`
- Test: `tests/exact/path/to/test.py`

**Step 1: Write the failing test**

```python
def test_specific_behavior():
    result = function(input)
    assert result == expected
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/path/test.py::test_name -v`
Expected: FAIL with "function not defined"

**Step 3: Write minimal implementation**

```python
def function(input):
    return expected
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/path/test.py::test_name -v`
Expected: PASS

**Step 5: Commit**

```bash
git add tests/path/test.py src/path/file.py
git commit -m "feat: add specific feature"
```
```

## Remember
- Exact file paths always
- Complete code in plan (not "add validation")
- Exact commands with expected output
- Reference relevant skills with gremlins: prefix
- DRY, YAGNI, TDD, frequent commits

## Trellis Plan Contracts (Conditional)

After saving the implementation plan, check if the repo uses trellis for plan tracking:

```bash
# Only emit frontmatter if trellis is configured
test -f .trellis || trellis lint &>/dev/null
```

If no `.trellis` config exists, skip this entire section silently.

If trellis is present, create the plan folder with three files:

### 1. `plans/active/{plan-name}/README.md`

```yaml
---
title: Plan Title
status: not_started
depends_on: []
tags: []
repo: <repo-name>
---
```

Populate `repo` from `basename $(git rev-parse --show-toplevel)`. Leave `depends_on` and `tags` empty unless the user specifies dependencies.

### 2. `plans/active/{plan-name}/inputs.md`

```markdown
# Inputs

## From plans

<!-- Auto-populated from depends_on entries -->
<!-- For each dependency, pull outputs via: trellis show <dep-id> --contracts -->

## From existing code

<!-- What existing code/APIs/state does this plan consume? -->
```

If `depends_on` has entries, run `trellis show <dep-id> --contracts` for each dependency and populate the "From plans" section with what the upstream plan's `outputs.md` promises. Format each as:

```markdown
### From: {dep-id}
- {output item 1}
- {output item 2}
```

### 3. `plans/active/{plan-name}/outputs.md`

```markdown
# Outputs

<!-- What does this plan produce that downstream plans or users consume? -->
<!-- Examples: files created, types/interfaces exported, CLI commands, config options, API endpoints -->
```

### Collaborative Output Drafting

After writing the README and implementation plan, guide the user through defining outputs:

> "What does this plan produce that other plans might consume? Think about: files created, interfaces exported, commands registered, config options added."

Work with the user to fill `outputs.md` with concrete output items.

### Readiness Gate

**A plan isn't ready until `outputs.md` has at least one concrete output defined.** If the user skips output definition, note it as a warning:

> "Warning: No outputs defined. Downstream plans won't be able to declare inputs from this plan."

## Execution Handoff

After saving the plan, offer execution choice:

**"Plan complete and saved to `plans/active/{plan-name}/implementation/`. Two execution options:**

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration

**2. Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

**Which approach?"**

**If Subagent-Driven chosen:**
- **REQUIRED SUB-SKILL:** Use gremlins:subagent-driven-development
- Stay in this session
- Fresh subagent per task + code review

**If Parallel Session chosen:**
- Guide them to open new session in worktree
- **REQUIRED SUB-SKILL:** New session uses gremlins:executing-plans
