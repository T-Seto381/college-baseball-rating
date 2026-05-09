# Codex automation files for this repository

This folder contains project-local automation helpers inspired by:

- Codex `rules/*.rules` for command prefix approvals
- Codex Stop hook + `AGENTS.md` for scoped auto-confirm

Files:

- `rules/default.rules`
  Project-local command approval rules for routine website work.
- `hooks/auto_approve_stop.ps1`
  Windows Stop hook implementation.
- `hooks/auto_approve_stop.cmd`
  Wrapper so Codex can execute the PowerShell hook as a command.
- `templates/global_AGENTS.append.md`
  Block to append to `~/.codex/AGENTS.md`.
- `templates/config.stop-hook.toml.example`
  Hook config snippet for `~/.codex/config.toml`.

Recommended setup:

```powershell
powershell -ExecutionPolicy Bypass -File .\scripts\install_codex_automation.ps1
```

After installation, restart Codex and launch it from this repository.
