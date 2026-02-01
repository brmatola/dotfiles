# Global Claude Instructions

## Principles

1. **Verify before claiming** - Run commands, see output, then claim results
2. **Test-first** - Write failing test before implementation
3. **Understand before fixing** - Find root cause, don't guess

## Skills Available

The skills in `~/.claude/skills/` are loaded automatically when relevant:

- `test-driven-development` - TDD workflow (red-green-refactor)
- `systematic-debugging` - Four-phase debugging framework
- `verification-before-completion` - Evidence before assertions

## Agents Available

Use these via the Task tool for delegation:

- `haiku-general-purpose` - Fast, cheap tasks (research, summarization)
- `sonnet-general-purpose` - Balanced tasks (analysis, implementation)
- `opus-general-purpose` - Complex reasoning tasks

## How to Expand

Add more skills: Copy SKILL.md files to `~/.claude/skills/<name>/SKILL.md`
Add more agents: Copy .md files to `~/.claude/agents/<name>.md`
Add hooks: Configure in `~/.claude/settings.json`
