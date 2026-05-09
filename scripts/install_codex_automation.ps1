$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
$userCodex = Join-Path $HOME ".codex"
$userHooks = Join-Path $userCodex "hooks"
$globalAgents = Join-Path $userCodex "AGENTS.md"
$configPath = Join-Path $userCodex "config.toml"

$sourceHookCmd = Join-Path $repoRoot ".codex\hooks\auto_approve_stop.cmd"
$sourceHookPs1 = Join-Path $repoRoot ".codex\hooks\auto_approve_stop.ps1"
$sourceAgentsAppend = Join-Path $repoRoot ".codex\templates\global_AGENTS.append.md"

New-Item -ItemType Directory -Force -Path $userCodex | Out-Null
New-Item -ItemType Directory -Force -Path $userHooks | Out-Null

Copy-Item -Force $sourceHookCmd (Join-Path $userHooks "auto_approve_stop.cmd")
Copy-Item -Force $sourceHookPs1 (Join-Path $userHooks "auto_approve_stop.ps1")

$agentsBlock = Get-Content -Raw $sourceAgentsAppend
$agentsMarker = "## Auto confirm rules"

if (Test-Path $globalAgents) {
  $agentsText = Get-Content -Raw $globalAgents
} else {
  $agentsText = ""
}

if ($agentsText -notmatch [Regex]::Escape($agentsMarker)) {
  $updatedAgents = if ([string]::IsNullOrWhiteSpace($agentsText)) {
    $agentsBlock.Trim() + "`r`n"
  } else {
    $agentsText.TrimEnd() + "`r`n`r`n" + $agentsBlock.Trim() + "`r`n"
  }
  Set-Content -Path $globalAgents -Value $updatedAgents -Encoding utf8
}

$hookCmdPath = (Join-Path $userHooks "auto_approve_stop.cmd").Replace("\", "/")
$stopBlock = @"
Stop = [
  { hooks = [
    { type = "command", command = "$hookCmdPath", timeout = 10 },
  ] },
]
"@

if (Test-Path $configPath) {
  $configText = Get-Content -Raw $configPath
  Copy-Item -Force $configPath "$configPath.bak"
} else {
  $configText = ""
}

if ($configText -match "(?m)^\s*codex_hooks\s*=") {
  $configText = [Regex]::Replace(
    $configText,
    "(?m)^(\s*)codex_hooks(\s*=)",
    '$1hooks$2'
  )
}

if ($configText -notmatch "(?m)^\[features\]\s*$") {
  $configText = ($configText.TrimEnd() + "`r`n`r`n[features]`r`nhooks = true`r`n").TrimStart("`r", "`n")
} elseif ($configText -notmatch "(?m)^\s*hooks\s*=") {
  $configText = [Regex]::Replace(
    $configText,
    "(?m)^\[features\]\s*$",
    "[features]`r`nhooks = true",
    1
  )
}

if ($configText -notmatch [Regex]::Escape($hookCmdPath)) {
  if ($configText -match "(?m)^\[hooks\]\s*$") {
    if ($configText -match "(?m)^\s*Stop\s*=") {
      $snippetPath = Join-Path $userCodex "config.stop-hook.snippet.toml"
      Set-Content -Path $snippetPath -Value $stopBlock -Encoding utf8
      Write-Warning "Existing [hooks] Stop setting detected. Review and merge: $snippetPath"
    } else {
      $configText = [Regex]::Replace(
        $configText,
        "(?m)^\[hooks\]\s*$",
        "[hooks]`r`n$stopBlock".TrimEnd(),
        1
      )
    }
  } else {
    $configText = $configText.TrimEnd() + "`r`n`r`n[hooks]`r`n$stopBlock"
  }
}

Set-Content -Path $configPath -Value ($configText.Trim() + "`r`n") -Encoding utf8

Write-Host "Codex automation files installed."
Write-Host "Restart Codex after this step."
Write-Host "Project-local rules are already in: $repoRoot\.codex\rules\default.rules"
