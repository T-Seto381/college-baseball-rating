param(
  [datetime]$Until,
  [int]$SleepSeconds = 30,
  [int]$MaxCycles = 12,
  [switch]$SkipInstall
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$promptPath = Join-Path $repoRoot ".codex\prompts\overnight_website_improvement.md"
$logRoot = Join-Path $repoRoot "logs\overnight_website"
$stateDir = Join-Path $logRoot "state"
$runnerLog = Join-Path $logRoot "runner.log"
$pidFile = Join-Path $stateDir "runner.pid"
$statusFile = Join-Path $stateDir "status.json"
$lastMessageFile = Join-Path $stateDir "last_message.txt"
$cycleRoot = Join-Path $logRoot ((Get-Date).ToString("yyyyMMdd"))
$gitSafeDirectory = "safe.directory=$repoRoot"

function Write-RunnerLog {
  param([string]$Message)
  $line = "[{0}] {1}" -f (Get-Date).ToString("yyyy-MM-dd HH:mm:ss"), $Message
  $line | Tee-Object -FilePath $runnerLog -Append
}

function Save-Status {
  param(
    [string]$State,
    [int]$Cycle,
    [string]$Note
  )
  $payload = [ordered]@{
    state = $State
    cycle = $Cycle
    note = $Note
    updated_at = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss")
    until = $Until.ToString("yyyy-MM-dd HH:mm:ss")
    pid = $PID
  }
  $payload | ConvertTo-Json | Set-Content -Path $statusFile
}

if (-not $Until) {
  $now = Get-Date
  $todaySeven = Get-Date -Hour 7 -Minute 0 -Second 0
  if ($now -lt $todaySeven) {
    $Until = $todaySeven
  } else {
    $Until = $todaySeven.AddDays(1)
  }
}

New-Item -ItemType Directory -Force -Path $logRoot, $stateDir, $cycleRoot | Out-Null
Set-Content -Path $pidFile -Value $PID

if (-not $SkipInstall) {
  $installer = Join-Path $repoRoot "scripts\install_codex_automation.ps1"
  if (Test-Path $installer) {
    Write-RunnerLog "Installing Codex automation helpers."
    & powershell.exe -NoProfile -ExecutionPolicy Bypass -File $installer *> (Join-Path $logRoot "install.log")
  }
}

Write-RunnerLog ("Overnight loop started. Target end time: {0}" -f $Until.ToString("yyyy-MM-dd HH:mm:ss"))
Save-Status -State "running" -Cycle 0 -Note "Initialized"

$cycle = 0
while ((Get-Date) -lt $Until -and $cycle -lt $MaxCycles) {
  $cycle++
  $cycleStamp = (Get-Date).ToString("yyyyMMdd_HHmmss")
  $cycleDir = Join-Path $cycleRoot ("cycle_{0:D2}_{1}" -f $cycle, $cycleStamp)
  $stdoutFile = Join-Path $cycleDir "codex.stdout.log"
  $stderrFile = Join-Path $cycleDir "codex.stderr.log"
  $messageFile = Join-Path $cycleDir "codex.final.txt"
  $beforeHead = (& git -c $gitSafeDirectory -C $repoRoot rev-parse HEAD).Trim()
  $beforeStatus = (& git -c $gitSafeDirectory -C $repoRoot status --short) -join "`n"
  $prompt = Get-Content -Raw $promptPath

  New-Item -ItemType Directory -Force -Path $cycleDir | Out-Null
  Write-RunnerLog ("Starting cycle {0}" -f $cycle)
  Save-Status -State "running" -Cycle $cycle -Note "Cycle started"

  $output = $null
  $errorOutput = $null
  try {
    $output = & codex --search --dangerously-bypass-approvals-and-sandbox exec `
      --cd $repoRoot `
      --output-last-message $messageFile `
      $prompt 2>&1

    $exitCode = $LASTEXITCODE
    if ($null -eq $output) {
      $output = @()
    }
    $output | Out-File -FilePath $stdoutFile -Encoding utf8
  } catch {
    $errorOutput = $_ | Out-String
    $errorOutput | Out-File -FilePath $stderrFile -Encoding utf8
    $exitCode = 1
  }

  $afterHead = (& git -c $gitSafeDirectory -C $repoRoot rev-parse HEAD).Trim()
  $afterStatus = (& git -c $gitSafeDirectory -C $repoRoot status --short) -join "`n"
  $lastMessage = if (Test-Path $messageFile) { Get-Content -Raw $messageFile } else { "" }
  $summary = [ordered]@{
    cycle = $cycle
    started_head = $beforeHead
    ended_head = $afterHead
    exit_code = $exitCode
    before_status = $beforeStatus
    after_status = $afterStatus
    final_message_file = $messageFile
    finished_at = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss")
  }
  $summary | ConvertTo-Json | Set-Content -Path (Join-Path $cycleDir "summary.json")
  Set-Content -Path $lastMessageFile -Value $lastMessage

  if ($exitCode -eq 0) {
    Write-RunnerLog ("Cycle {0} finished successfully. HEAD {1} -> {2}" -f $cycle, $beforeHead, $afterHead)
    Save-Status -State "running" -Cycle $cycle -Note "Cycle finished successfully"
  } else {
    Write-RunnerLog ("Cycle {0} failed. See {1}" -f $cycle, $cycleDir)
    Save-Status -State "running" -Cycle $cycle -Note "Cycle failed; continuing"
  }

  if ((Get-Date) -lt $Until -and $cycle -lt $MaxCycles) {
    Start-Sleep -Seconds $SleepSeconds
  }
}

Write-RunnerLog "Overnight loop finished."
Save-Status -State "finished" -Cycle $cycle -Note "Loop completed"
