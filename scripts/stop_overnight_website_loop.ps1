$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$pidFile = Join-Path $repoRoot "logs\overnight_website\state\runner.pid"

if (-not (Test-Path $pidFile)) {
  Write-Host "No runner.pid found."
  exit 0
}

$pidValue = (Get-Content -Raw $pidFile).Trim()
if ($pidValue -notmatch '^\d+$') {
  Write-Host "runner.pid is invalid."
  exit 1
}

$proc = Get-Process -Id ([int]$pidValue) -ErrorAction SilentlyContinue
if ($proc) {
  Stop-Process -Id $proc.Id -Force
  Write-Host ("Stopped overnight loop PID {0}" -f $proc.Id)
} else {
  Write-Host ("Process already stopped: {0}" -f $pidValue)
}
