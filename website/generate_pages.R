# ============================================================
# Quarto 個別ページ生成スクリプト
# 大学・リーグごとに _template.qmd をレンダリングして
# university/{team}.html, league/{league}.html を生成する
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(fs)
})

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
  out_file <- paste0("university/", tm, ".html")
  cat(sprintf("  %s → %s\n", tm, out_file))

  ret <- system2("quarto", c(
    "render", "university/_template.qmd",
    "--output", paste0(tm, ".html"),
    "-P", paste0("team:", tm)
  ))
  if (ret != 0) message("  ERROR: quarto render failed (exit code ", ret, ")")
}

# ---- リーグページ生成 ----
cat("\n--- リーグページ ---\n")

dir_create("league")

for (lg in all_leagues) {
  out_file <- paste0("league/", lg, ".html")
  cat(sprintf("  %s → %s\n", lg, out_file))

  ret <- system2("quarto", c(
    "render", "league/_template.qmd",
    "--output", paste0(lg, ".html"),
    "-P", paste0("league:", lg)
  ))
  if (ret != 0) message("  ERROR: quarto render failed (exit code ", ret, ")")
}

cat("\n=== ページ生成完了 ===\n")
