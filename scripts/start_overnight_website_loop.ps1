param(
  [datetime]$Until,
  [int]$SleepSeconds = 30,
  [int]$MaxCycles = 12,
  [switch]$SkipInstall
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$runner = Join-Path $repoRoot "scripts\run_overnight_website_loop.ps1"
$logRoot = Join-Path $repoRoot "logs\overnight_website"
$stateDir = Join-Path $logRoot "state"
$pidFile = Join-Path $stateDir "runner.pid"

New-Item -ItemType Directory -Force -Path $stateDir | Out-Null

if (-not $Until) {
  $now = Get-Date
  $todaySeven = Get-Date -Hour 7 -Minute 0 -Second 0
  if ($now -lt $todaySeven) {
    $Until = $todaySeven
  } else {
    $Until = $todaySeven.AddDays(1)
  }
}

if (Test-Path $pidFile) {
  $existingPid = Get-Content -Raw $pidFile
  if ($existingPid -match '^\d+$') {
    $existing = Get-Process -Id ([int]$existingPid) -ErrorAction SilentlyContinue
    if ($existing) {
      Write-Host ("Overnight loop is already running with PID {0}" -f $existing.Id)
      exit 0
    }
  }
}

$shell = Get-Command pwsh.exe -ErrorAction SilentlyContinue
if ($null -eq $shell) {
  $shell = Get-Command powershell.exe
  $args = @(
    "-NoProfile",
    "-ExecutionPolicy", "Bypass",
    "-File", $runner,
    "-Until", $Until.ToString("yyyy-MM-ddTHH:mm:ss"),
    "-SleepSeconds", $SleepSeconds,
    "-MaxCycles", $MaxCycles
  )
} else {
  $args = @(
    "-NoProfile",
    "-File", $runner,
    "-Until", $Until.ToString("yyyy-MM-ddTHH:mm:ss"),
    "-SleepSeconds", $SleepSeconds,
    "-MaxCycles", $MaxCycles
  )
}

if ($SkipInstall) {
  $args += "-SkipInstall"
}

$proc = Start-Process -FilePath $shell.Source -ArgumentList $args -WorkingDirectory $repoRoot -WindowStyle Hidden -PassThru
Set-Content -Path $pidFile -Value $proc.Id

Write-Host ("Started overnight website loop. PID={0} Until={1}" -f $proc.Id, $Until.ToString("yyyy-MM-dd HH:mm:ss"))
Write-Host ("Check status with: powershell -ExecutionPolicy Bypass -File .\scripts\check_overnight_website_loop.ps1")
