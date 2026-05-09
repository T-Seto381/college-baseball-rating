$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$logRoot = Join-Path $repoRoot "logs\overnight_website"
$stateDir = Join-Path $logRoot "state"
$pidFile = Join-Path $stateDir "runner.pid"
$statusFile = Join-Path $stateDir "status.json"
$lastMessageFile = Join-Path $stateDir "last_message.txt"
$runnerLog = Join-Path $logRoot "runner.log"

if (Test-Path $pidFile) {
  $pidValue = (Get-Content -Raw $pidFile).Trim()
  if ($pidValue -match '^\d+$') {
    $proc = Get-Process -Id ([int]$pidValue) -ErrorAction SilentlyContinue
    if ($proc) {
      Write-Host ("Runner PID: {0} ({1})" -f $proc.Id, $proc.ProcessName)
    } else {
      Write-Host ("Runner PID file exists, but process is not running: {0}" -f $pidValue)
    }
  }
}

if (Test-Path $statusFile) {
  Write-Host "`nStatus:"
  Get-Content -Raw $statusFile
}

if (Test-Path $lastMessageFile) {
  Write-Host "`nLast agent message:"
  Get-Content -Raw $lastMessageFile
}

if (Test-Path $runnerLog) {
  Write-Host "`nRecent runner log:"
  Get-Content $runnerLog -Tail 20
}
