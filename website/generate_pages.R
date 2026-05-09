# ============================================================
# Quarto 個別ページ生成スクリプト
# 大学・リーグごとに _template.qmd をレンダリングして
# university/{slug}.html, league/{slug}.html を生成する
# ============================================================

# quarto R パッケージが CLI を見つけられるよう QUARTO_PATH を設定
qb <- Sys.which("quarto")
if (!nzchar(qb)) {
  for (p in c("/usr/local/bin/quarto", "/usr/bin/quarto",
              "/opt/quarto/bin/quarto")) {
    if (file.exists(p)) { qb <- p; break }
  }
}
if (nzchar(qb)) {
  Sys.setenv(QUARTO_PATH = qb)
  message("QUARTO_PATH: ", qb)
} else {
  warning("quarto binary not found on PATH or common locations")
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(quarto)
  library(readxl)
  library(fs)
})

# ---- ASCII スラグマッピング（日本語ファイル名はGitHub Pagesで機能しないため） ----
TEAM_SLUG <- c(
  "東大"   = "todai",
  "明治"   = "meiji",
  "慶應"   = "keio",
  "早稲田" = "waseda",
  "立教"   = "rikkyo",
  "法政"   = "hosei"
)

LEAGUE_SLUG <- c(
  "東京六大学" = "tokyo6"
)

# ---- チーム・リーグ情報の取得 ----
teams_info <- read_excel("../data_out/teamdata.xlsx")

all_teams   <- unique(teams_info$TeamShortName)
all_leagues <- unique(teams_info$LeagueName)

cat("=== ページ生成開始 ===\n")
cat("大学:", length(all_teams), "校  リーグ:", length(all_leagues), "\n\n")

# ---- 大学ページ生成 ----
cat("--- 大学ページ ---\n")

dir_create("university")

for (tm in all_teams) {
  slug <- TEAM_SLUG[tm]
  if (is.na(slug)) {
    message("  SKIP (no slug): ", tm); next
  }
  out_file <- paste0("university/", slug, ".html")
  cat(sprintf("  %s (%s) → %s\n", tm, slug, out_file))

  tryCatch({
    quarto_render(
      input          = "university/_template.qmd",
      output_file    = paste0(slug, ".html"),
      execute_params = list(team = tm),
      quiet          = TRUE
    )
  }, error = function(e) {
    message("  ERROR: ", e$message)
  })
}

# ---- リーグページ生成 ----
cat("\n--- リーグページ ---\n")

dir_create("league")

for (lg in all_leagues) {
  slug <- LEAGUE_SLUG[lg]
  if (is.na(slug)) {
    message("  SKIP (no slug): ", lg); next
  }
  out_file <- paste0("league/", slug, ".html")
  cat(sprintf("  %s (%s) → %s\n", lg, slug, out_file))

  tryCatch({
    quarto_render(
      input          = "league/_template.qmd",
      output_file    = paste0(slug, ".html"),
      execute_params = list(league = lg),
      quiet          = TRUE
    )
  }, error = function(e) {
    message("  ERROR: ", e$message)
  })
}

cat("\n=== ページ生成完了 ===\n")
