# ============================================================
# Quarto 個別ページ生成スクリプト
# university/{slug}.qmd, league/{slug}.qmd を生成し
# quarto render でまとめてレンダリングする
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(fs)
})

source("site_utils.R", local = TRUE)

# ---- チーム・リーグ情報の取得 ----
teams_info <- read_excel("../data_out/teamdata.xlsx")
game_results <- readr::read_csv("../data_out/ratings/game_results_with_ratings.csv", show_col_types = FALSE)

team_slug_table <- build_team_slug_table(teams_info)
league_slug_table <- build_league_slug_table(unique(infer_league_name(game_results$gametype)))

all_teams   <- unique(teams_info$TeamShortName)
all_leagues <- league_slug_table$LeagueName

cat("=== .qmd ファイル生成開始 ===\n")
cat("大学:", length(all_teams), "校  リーグ:", length(all_leagues), "\n\n")

# ---- テンプレート行の読み込み（YAML frontmatter を除く） ----
read_template_body_lines <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  yaml_end <- which(trimws(lines) == "---")
  if (length(yaml_end) >= 2) {
    lines[(yaml_end[2] + 1):length(lines)]
  } else {
    lines
  }
}

uni_body_lines    <- read_template_body_lines("university/_template.qmd")
league_body_lines <- read_template_body_lines("league/_template.qmd")

# ---- 大学ページ生成 ----
cat("--- 大学ページ ---\n")
dir_create("university")

for (tm in all_teams) {
  slug <- team_slug_table |>
    filter(team == tm) |>
    pull(team_slug) |>
    dplyr::first()
  if (is.na(slug)) {
    message("  SKIP (no slug): ", tm); next
  }
  out_qmd <- paste0("university/", slug, ".qmd")
  cat(sprintf("  %s (%s) -> %s\n", tm, slug, out_qmd))

  team_full_name <- teams_info |>
    filter(TeamShortName == tm) |>
    slice(1) |>
    pull(TeamName)
  if (length(team_full_name) == 0 || is.na(team_full_name)) {
    team_full_name <- tm
  }

  # params$team をチーム名に直接置換（YAML内に日本語を埋め込まない）
  body <- gsub("TEAM <- params\\$team",
               sprintf('TEAM <- "%s"', tm),
               uni_body_lines,
               fixed = FALSE)

  content <- c(
    "---",
    sprintf('title: "%s"', team_full_name),
    "format:",
    "  html:",
    "    page-layout: full",
    "---",
    body
  )

  con <- file(out_qmd, open = "w", encoding = "UTF-8")
  writeLines(content, con)
  close(con)
}

# ---- リーグページ生成 ----
cat("\n--- リーグページ ---\n")
dir_create("league")

for (lg in all_leagues) {
  slug <- league_slug_table |>
    filter(LeagueName == lg) |>
    pull(league_slug) |>
    dplyr::first()
  if (is.na(slug)) {
    message("  SKIP (no slug): ", lg); next
  }
  out_qmd <- paste0("league/", slug, ".qmd")
  cat(sprintf("  %s (%s) -> %s\n", lg, slug, out_qmd))

  # params$league をリーグ名に直接置換
  body <- gsub("LEAGUE <- params\\$league",
               sprintf('LEAGUE <- "%s"', lg),
               league_body_lines,
               fixed = FALSE)

  content <- c(
    "---",
    sprintf('title: "%s"', lg),
    "format:",
    "  html:",
    "    page-layout: full",
    "---",
    body
  )

  con <- file(out_qmd, open = "w", encoding = "UTF-8")
  writeLines(content, con)
  close(con)
}

cat("\n=== .qmd ファイル生成完了 ===\n")
