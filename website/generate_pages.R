# ============================================================
# Quarto 個別ページ生成スクリプト
# 大学・リーグごとに _template.qmd をレンダリングして
# university/{team}.html, league/{league}.html を生成する
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(quarto)
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

  tryCatch({
    quarto_render(
      input          = "university/_template.qmd",
      output_file    = paste0(tm, ".html"),
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
  out_file <- paste0("league/", lg, ".html")
  cat(sprintf("  %s → %s\n", lg, out_file))

  tryCatch({
    quarto_render(
      input          = "league/_template.qmd",
      output_file    = paste0(lg, ".html"),
      execute_params = list(league = lg),
      quiet          = TRUE
    )
  }, error = function(e) {
    message("  ERROR: ", e$message)
  })
}

cat("\n=== ページ生成完了 ===\n")
