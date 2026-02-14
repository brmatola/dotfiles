# Outputs

## Updated writing-plans skill
- Creates plan folder with `README.md`, `inputs.md`, `outputs.md`
- Pre-populates `inputs.md` from `depends_on`
- Guides agent to draft `outputs.md` during planning conversation
- Gates "ready" status on contract completeness

## Updated chunk-readiness-review skill
- Review subagents receive upstream `outputs.md` as context (not full plans)
- Fifth adversarial pass: contract coherence
- Cross-chunk synthesis focuses on interface width and contract alignment
- Reports flag contract gaps and mismatches

## Updated requesting-code-review skill
- Output verification pass: check each `outputs.md` item against implementation
- Clear pass/fail signal for downstream unblocking

## Updated worktree-workflow skill
- Chains plan review (contract validation) → implementation → code review (output verification)
- Integrates trellis status updates with contract verification gates
