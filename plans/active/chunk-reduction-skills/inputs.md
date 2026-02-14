# Inputs

## From plans
### chunk-reduction (trellis repo)
- Plan contract convention: `inputs.md`/`outputs.md` folder structure
- Extended `trellis chunks --json` output with `chunkInputs`/`chunkOutputs`
- `trellis show <id> --contracts` for inline contract display
- Lint checks for contract mismatches

## From existing code
### claude/skills/writing-plans/
- Current plan creation flow and templates

### claude/skills/chunk-readiness-review/
- Current chunk critique prompt and synthesis prompt
- Phase structure: trellis chunks → parallel subagents → synthesis → report

### claude/skills/requesting-code-review/
- Current code review flow

### claude/skills/worktree-workflow/
- Current workflow phases and trellis integration points
