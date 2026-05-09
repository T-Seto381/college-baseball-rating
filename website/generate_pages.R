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

cat("=== .qmd ファイル生成開始 ===\n")
cat("大学:", length(all_teams), "校  リーグ:", length(all_leagues), "\n\n")

# ---- テンプレート本文の読み込み（YAML frontmatter 以降） ----
read_template_body <- function(path) {
  lines <- readLines(path, encoding = "UTF-8")
  yaml_end <- which(lines == "---")
  if (length(yaml_end) >= 2) {
    paste(lines[(yaml_end[2] + 1):length(lines)], collapse = "\n")
  } else {
    paste(lines, collapse = "\n")
  }
}

uni_body    <- read_template_body("university/_template.qmd")
league_body <- read_template_body("league/_template.qmd")

# ---- 大学ページ生成 ----
cat("--- 大学ページ ---\n")
dir_create("university")

for (tm in all_teams) {
  slug <- TEAM_SLUG[tm]
  if (is.na(slug)) {
    message("  SKIP (no slug): ", tm); next
  }
  out_qmd <- paste0("university/", slug, ".qmd")
  cat(sprintf("  %s (%s) → %s\n", tm, slug, out_qmd))

  content <- paste0(
    '---\nparams:\n  team: "', tm, '"\nformat:\n  html:\n    page-layout: full\n---\n',
    uni_body
  )
  con <- file(out_qmd, open = "w", encoding = "UTF-8")
  writeLines(content, con)
  close(con)
}

# ---- リーグページ生成 ----
cat("\n--- リーグページ ---\n")
dir_create("league")

for (lg in all_leagues) {
  slug <- LEAGUE_SLUG[lg]
  if (is.na(slug)) {
    message("  SKIP (no slug): ", lg); next
  }
  out_qmd <- paste0("league/", slug, ".qmd")
  cat(sprintf("  %s (%s) → %s\n", lg, slug, out_qmd))

  content <- paste0(
    '---\nparams:\n  league: "', lg, '"\nformat:\n  html:\n    page-layout: full\n---\n',
    league_body
  )
  con <- file(out_qmd, open = "w", encoding = "UTF-8")
  writeLines(content, con)
  close(con)
}

cat("\n=== .qmd ファイル生成完了 ===\n")
