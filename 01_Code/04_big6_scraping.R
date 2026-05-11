# ============================================================
# 東京六大学野球連盟 試合結果スクレイピング (2000〜2026春)
#
# URL構造:
#   https://www.big6.gr.jp/game/league/{year}{season}/{year}{season}_schedule.html
#   例: .../2000s/2000s_schedule.html  (2000年春)
#       .../2000a/2000a_schedule.html  (2000年秋)
#
# HTML構造パターン（年代により異なる）:
#   2000s: 月セル(4／) と 日セル(9 (土)) が別  スコア X-Y
#   2010-2012: 日付統合(4/10 (土))             スコア X-Y
#   2013以降:  日付統合(4/11 (土))             スコア X - Y  チーム名に接尾("東大東")
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(httr2)
  library(stringr)
  library(lubridate)
  library(glue)
  library(readr)
  library(fs)
})

dir_create("data_out")
dir_create("logs")

# ============================================================
# 定数・マッピング
# ============================================================

USER_AGENT  <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120 Safari/537.36"
LEAGUE_NAME <- "東京六大学"
BASE_URL    <- "https://www.big6.gr.jp/game/league"

# 略称 → 出力名称
TEAM_MAP <- c(
  "東大" = "東大",
  "明大" = "明治",
  "立大" = "立教",
  "法大" = "法政",
  "慶大" = "慶應",
  "早大" = "早稲田"
)

# セルテキストを正規化してチーム名の先頭一致で略称を抽出
# 例: "東　大" → "東大", "東大東" → "東大", "早大早" → "早大"
extract_team_abbr <- function(x) {
  x_clean <- str_replace_all(x, "[　 \t]+", "")
  for (abbr in names(TEAM_MAP)) {
    if (startsWith(x_clean, abbr)) return(abbr)
  }
  NA_character_
}

is_team_name <- function(x) {
  !is.na(extract_team_abbr(x))
}

normalize_team <- function(x) {
  abbr <- extract_team_abbr(x)
  if (is.na(abbr)) return(str_replace_all(x, "[　 \t]+", ""))
  unname(TEAM_MAP[abbr])
}

# ============================================================
# URL生成
# ============================================================

big6_url <- function(year, season) {
  glue("{BASE_URL}/{year}{season}/{year}{season}_schedule.html")
}

seasons_to_fetch <- bind_rows(
  expand_grid(year = 2000:2025, season = c("s", "a")),
  tibble(year = 2026L, season = "s")
) |>
  mutate(
    url   = map2_chr(year, season, big6_url),
    label = if_else(season == "s", paste0(year, "春"), paste0(year, "秋"))
  )

# ============================================================
# HTML取得（文字コード自動検出）
# ============================================================

safe_fetch_html <- function(url, sleep_sec = 1) {
  Sys.sleep(sleep_sec)
  tryCatch({
    raw <- request(url) |>
      req_user_agent(USER_AGENT) |>
      req_timeout(30) |>
      req_perform() |>
      resp_body_raw()

    # charset検出: 非ASCIIバイト(>127)をスペースに置換してから安全に検索
    ascii_only <- raw
    ascii_only[as.integer(raw) > 127L] <- as.raw(0x20L)
    safe_head <- rawToChar(ascii_only[seq_len(min(2000L, length(ascii_only)))])

    m <- regmatches(safe_head,
      regexpr('charset=.?([A-Za-z0-9_-]+)', safe_head, ignore.case = TRUE))

    raw_enc <- if (length(m) > 0 && !is.na(m[1])) {
      sub('charset=.?', '', m[1], ignore.case = TRUE)
    } else "UTF-8"

    enc <- toupper(trimws(raw_enc))
    enc <- switch(enc,
      "SHIFT-JIS" = , "SHIFT_JIS" = , "X-SJIS" = , "SJIS" = "Shift-JIS",
      "EUC-JP"    = , "X-EUC-JP"  = "EUC-JP",
      enc
    )

    # tempファイル経由で正しいエンコーディングとしてパース（rawToChar二重変換を回避）
    tmp <- tempfile(fileext = ".html")
    on.exit(unlink(tmp), add = TRUE)
    writeBin(raw, tmp)
    html <- read_html(tmp, encoding = enc)

    list(html = html, encoding = enc)
  }, error = function(e) {
    message("  取得失敗: ", conditionMessage(e))
    NULL
  })
}

# ============================================================
# スコアセル検出
# スコア形式: "X-Y" または "X - Y"（スペース有無を両方対応）
# ============================================================

SCORE_PAT <- "^[0-9]{1,3} *- *[0-9]{1,3}$"

is_score_cell <- function(x) grepl(SCORE_PAT, x)

parse_score_cell <- function(x) {
  m <- regmatches(x, regexpr("([0-9]+) *- *([0-9]+)", x))
  if (length(m) == 0) return(NULL)
  parts <- strsplit(m, " *- *")[[1]]
  c(as.integer(parts[1]), as.integer(parts[2]))
}

# ============================================================
# 日付検出
# 2種類のフォーマットを処理:
#   統合形式: "4/10 (土)" → month=4, day=10
#   分離形式: "9／" → month=9, "9 (土)" → day=9
# ============================================================

parse_date_from_cells <- function(cells, current_month, year) {
  month <- current_month
  day   <- NA_integer_
  has_day <- FALSE

  for (cell in cells) {
    cell_clean <- str_replace_all(cell, "[　 \t]+", " ") |> trimws()

    # 統合形式: "4/10 (土)" or "4/10(土)"
    m_combined <- regmatches(cell_clean,
      regexpr("^([0-9]{1,2})[/／]([0-9]{1,2})\\s*[(（]", cell_clean))
    if (length(m_combined) > 0) {
      parts <- strsplit(m_combined, "[/／]")[[1]]
      month <- as.integer(parts[1])
      d_raw <- regmatches(parts[2], regexpr("^[0-9]+", parts[2]))
      day   <- as.integer(d_raw)
      has_day <- TRUE
      break
    }

    # 分離・月のみ: "9／" or "9/" or "10/"
    m_month <- regmatches(cell_clean, regexpr("^([0-9]{1,2})[/／]$", cell_clean))
    if (length(m_month) > 0) {
      month <- as.integer(sub("[/／]$", "", m_month))
    }

    # 分離・日のみ: "9 (土)" or "10(月)" 、ただし統合形式ではないもの
    if (!grepl("[/／]", cell_clean)) {
      m_day <- regmatches(cell_clean, regexpr("^([0-9]{1,2})\\s*[(（]", cell_clean))
      if (length(m_day) > 0) {
        day <- as.integer(trimws(sub("\\s*[(（].*$", "", m_day)))
        has_day <- TRUE
      }
    }
  }

  list(month = month, day = day, has_day = has_day)
}

make_date <- function(year, month, day) {
  if (is.na(month) || is.na(day)) return(as.Date(NA))
  tryCatch(
    as.Date(sprintf("%04d-%02d-%02d", year, month, day)),
    error = function(e) as.Date(NA)
  )
}

# ============================================================
# スケジュールテーブル パーサー
#
# アルゴリズム:
# 1. スコアセルを多く含む（=日程テーブルらしい）テーブルを特定
# 2. 各行を走査してスコアセルの前後からチーム名を抽出
# 3. 月・日を追跡しながら試合日を組み立てる
# ============================================================

parse_schedule_table_node <- function(tbl, year) {
  rows <- tbl |> html_elements("tr")
  if (length(rows) == 0) return(NULL)

  out           <- list()
  current_month <- NA_integer_

  for (row in rows) {
    # セルテキスト取得（全角スペース除去・空破棄）
    cells_raw <- row |> html_elements("td, th") |> html_text2()
    cells <- str_replace_all(cells_raw, "[　 \t]+", " ") |>
      trimws() |>
      discard(~ .x == "")

    if (length(cells) == 0) next

    # 日付更新
    date_result   <- parse_date_from_cells(cells, current_month, year)
    current_month <- date_result$month

    # スコアセルを検索し、前後のチーム名を取得
    score_positions <- which(is_score_cell(cells))
    row_day <- date_result$day

    # 実際の日程表は1行あたり最大2試合。日付のない行や異常に多い行は除外する。
    if (!date_result$has_day || length(score_positions) == 0 || length(score_positions) > 2) {
      next
    }

    for (si in score_positions) {
      if (si < 2 || si >= length(cells)) next

      t1_raw <- cells[si - 1]
      t2_raw <- cells[si + 1]

      if (!is_team_name(t1_raw) || !is_team_name(t2_raw)) next

      sc <- parse_score_cell(cells[si])
      if (is.null(sc)) next

      game_date <- make_date(year, current_month, row_day)

      out[[length(out) + 1]] <- tibble(
        date   = game_date,
        team1  = normalize_team(t1_raw),
        score1 = sc[1],
        score2 = sc[2],
        team2  = normalize_team(t2_raw)
      )
    }
  }

  if (length(out) == 0) return(NULL)
  bind_rows(out)
}

parse_schedule_table <- function(html, year) {
  tables <- html |> html_elements("table")
  if (length(tables) == 0) return(NULL)

  parsed <- tables |>
    map(~ parse_schedule_table_node(.x, year)) |>
    compact()

  if (length(parsed) == 0) return(NULL)

  bind_rows(parsed) |>
    filter(!is.na(date), !is.na(score1), !is.na(score2)) |>
    distinct(date, team1, team2, score1, score2)
}

# ============================================================
# フォールバック: テキスト行ベースパーサー
# ============================================================

parse_schedule_text <- function(html, year) {
  lines <- html |> html_text2() |>
    str_split("\n") |> pluck(1) |>
    str_replace_all("[　\t]+", " ") |>
    trimws() |>
    discard(~ .x == "")

  out           <- list()
  current_date  <- as.Date(NA)

  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]

    # 統合日付: "4/10 (土)"
    m_comb <- regmatches(line,
      regexpr("([0-9]{1,2})[/／]([0-9]{1,2})\\s*[(（]", line))
    if (length(m_comb) > 0) {
      parts <- strsplit(m_comb, "[/／]")[[1]]
      mo <- as.integer(parts[1])
      dy <- as.integer(regmatches(parts[2], regexpr("^[0-9]+", parts[2])))
      current_date <- make_date(year, mo, dy)
      i <- i + 1; next
    }

    # 1行: "チームA X-Y チームB" または "チームA X - Y チームB"
    m1 <- regmatches(line,
      regexpr("^(.+?) ([0-9]+) *- *([0-9]+) (.+)$", line))
    if (length(m1) > 0) {
      # チームとスコアに分解
      parts <- strsplit(m1, " ([0-9]+ *- *[0-9]+) ")[[1]]
      if (length(parts) == 2) {
        t1 <- trimws(parts[1]); t2 <- trimws(parts[2])
        sc_text <- regmatches(m1, regexpr("[0-9]+ *- *[0-9]+", m1))
        sc <- parse_score_cell(sc_text)
        if (!is.null(sc) && is_team_name(t1) && is_team_name(t2)) {
          out[[length(out) + 1]] <- tibble(
            date = current_date,
            team1 = normalize_team(t1), score1 = sc[1],
            score2 = sc[2], team2 = normalize_team(t2)
          )
          i <- i + 1; next
        }
      }
    }

    # 3行セット: チームA → "X - Y" → チームB
    if (i + 2 <= length(lines)) {
      t1 <- lines[i]; sc_line <- lines[i + 1]; t2 <- lines[i + 2]
      sc <- parse_score_cell(sc_line)
      if (!is.null(sc) && is_team_name(t1) && is_team_name(t2) &&
          grepl(SCORE_PAT, trimws(sc_line))) {
        out[[length(out) + 1]] <- tibble(
          date = current_date,
          team1 = normalize_team(t1), score1 = sc[1],
          score2 = sc[2], team2 = normalize_team(t2)
        )
        i <- i + 3; next
      }
    }

    i <- i + 1
  }

  if (length(out) == 0) return(NULL)
  bind_rows(out) |>
    filter(!is.na(date), !is.na(score1), !is.na(score2)) |>
    distinct(date, team1, team2, score1, score2)
}

# ============================================================
# ページパース（テーブル戦略 → テキスト戦略の順）
# ============================================================

parse_big6_page <- function(html, year, season, url) {
  result <- tryCatch(parse_schedule_table(html, year), error = function(e) NULL)

  if (is.null(result) || nrow(result) == 0) {
    result <- tryCatch(parse_schedule_text(html, year), error = function(e) NULL)
  }

  if (is.null(result) || nrow(result) == 0) return(NULL)

  result |> mutate(試合種別 = LEAGUE_NAME, source_url = url)
}

# ============================================================
# メイン処理
# ============================================================

message("=== 東京六大学野球 スクレイピング開始 ===")
message("対象: ", nrow(seasons_to_fetch), " ページ\n")

all_results <- list()
failed_log  <- list()

for (i in seq_len(nrow(seasons_to_fetch))) {
  row   <- seasons_to_fetch[i, ]
  url   <- row$url
  yr    <- row$year
  seas  <- row$season
  label <- row$label

  message(sprintf("[%d/%d] %s", i, nrow(seasons_to_fetch), label))

  fetched <- safe_fetch_html(url)

  if (is.null(fetched)) {
    message("  → 取得失敗")
    failed_log[[length(failed_log) + 1]] <- tibble(label = label, url = url, reason = "fetch_failed")
    next
  }

  result <- parse_big6_page(fetched$html, yr, seas, url)

  if (is.null(result) || nrow(result) == 0) {
    # 診断用: 最もスコアを含むテーブルの先頭行を出力
    tables <- fetched$html |> html_elements("table")
    score_counts <- map_int(tables, function(tbl) {
      rows <- tbl |> html_elements("tr")
      sum(map_lgl(rows, function(r) {
        cells <- r |> html_elements("td,th") |> html_text2() |> trimws()
        any(is_score_cell(cells))
      }))
    })
    best_idx <- if (max(score_counts) > 0) which.max(score_counts) else NA_integer_
    message("  → パース失敗 (encoding:", fetched$encoding,
            " tables:", length(tables),
            " best_table:", best_idx,
            " score_rows:", if (!is.na(best_idx)) score_counts[best_idx] else 0, ")")
    failed_log[[length(failed_log) + 1]] <- tibble(label = label, url = url, reason = "parse_failed")
    next
  }

  message("  → ", nrow(result), " 試合取得")
  all_results[[length(all_results) + 1]] <- result
}

# ============================================================
# 出力
# ============================================================

message("\n=== 集計 ===")

if (length(all_results) == 0) {
  warning("取得できた試合データがありません。")
} else {
  final_df <- bind_rows(all_results) |>
    transmute(
      gamedate = date,
      gametype = 試合種別,
      team1    = team1,
      score1   = score1,
      score2   = score2,
      team2    = team2
    ) |>
    arrange(gamedate, team1)

  message("合計試合数: ", nrow(final_df))

  write_excel_csv(final_df, "data_out/big6_results_2000_2026.csv")
  message("出力: data_out/big6_results_2000_2026.csv")

  message("\n--- 年別試合数 ---")
  final_df |>
    mutate(year = year(gamedate)) |>
    count(year, name = "試合数") |>
    print(n = 30)
}

if (length(failed_log) > 0) {
  failed_df <- bind_rows(failed_log)
  write_excel_csv(failed_df, "logs/big6_failed_pages.csv")
  message("\n--- 失敗ページ: ", nrow(failed_df), " 件 ---")
  failed_df |> count(reason) |> print()
}
