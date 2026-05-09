$ErrorActionPreference = "Stop"

function Emit-Empty {
  Write-Output "{}"
  exit 0
}

$raw = [Console]::In.ReadToEnd()
if ([string]::IsNullOrWhiteSpace($raw)) {
  $raw = ($input | Out-String)
}
if ([string]::IsNullOrWhiteSpace($raw)) {
  Emit-Empty
}

try {
  $payload = $raw | ConvertFrom-Json -Depth 20
} catch {
  Emit-Empty
}

if ($payload.stop_hook_active -eq $true) {
  Emit-Empty
}

$last = ""
if ($null -ne $payload.PSObject.Properties["last_assistant_message"]) {
  $last = [string]$payload.last_assistant_message
}
if ([string]::IsNullOrWhiteSpace($last)) {
  Emit-Empty
}

if ($last -notmatch "\[CONFIRM\]") {
  Emit-Empty
}

$denyPattern = "(delete|remove|drop|reset|--hard|force[- ]?push|--force|format|billing|payment|contract|secret|token|password|api[ -]?key|credential|personal data)"
if ($last -match $denyPattern) {
  Emit-Empty
}

$allowPattern = "(college-baseball-rating|website/|docs/|quarto render|git push origin main|GitHub Pages|t-seto381\.github\.io|T-Seto381/college-baseball-rating|baseball rating)"
if ($last -notmatch $allowPattern) {
  Emit-Empty
}

$hooksDir = Join-Path $HOME ".codex\hooks"
$counterFile = Join-Path $hooksDir "auto_approve_stop.log"
New-Item -ItemType Directory -Force -Path $hooksDir | Out-Null

$now = [DateTimeOffset]::UtcNow.ToUnixTimeSeconds()
$hourAgo = $now - 3600

if (-not (Test-Path $counterFile)) {
  New-Item -ItemType File -Path $counterFile | Out-Null
}

$recent = @()
foreach ($line in Get-Content $counterFile) {
  $trimmed = $line.Trim()
  if (-not [string]::IsNullOrWhiteSpace($trimmed)) {
    $value = 0
    if ([Int64]::TryParse($trimmed, [ref]$value) -and $value -gt $hourAgo) {
      $recent += $value
    }
  }
}

$recent | Set-Content $counterFile
if ($recent.Count -ge 6) {
  Emit-Empty
}

Add-Content -Path $counterFile -Value $now

$response = @{
  decision = "block"
  reason = "Approved. Continue within this college-baseball-rating website scope, including commit, push, and published-site verification."
}

$response | ConvertTo-Json -Compress
exit 0
