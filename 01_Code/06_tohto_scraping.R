# ============================================================
# 東都大学野球連盟 試合結果スクレイピング (2002秋〜現在)
#
# URL構造:
#   リーグ戦: http://www.tohto-bbl.com/gameinfo/schedule.php
#             ?YEAR={url_year}&SEASONID={seasonid}&LEAGUEID={lid}
#   入替戦:   http://www.tohto-bbl.com/gameinfo/{label_year}/change_{s|a}.shtml
#
# シーズン対応（ラベル年 N）:
#   秋（9-11月 N年）: YEAR=N, SEASONID=02, LEAGUEID=01-04
#   春（4-6月 N+1年）: YEAR=N+1, SEASONID=01, LEAGUEID=01-04
#
# HTML構造:
#   リーグ戦: <!--start-->...<!--end--> ブロックを行単位で解析
#             日付セル(rowspan) + 内部テーブル[spacer,team1,sc1,"-",sc2,team2,link]
#   入替戦:   <tr>ごとに日付セル + 内部テーブル[team1," N - M ",team2,link]
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
# 定数
# ============================================================

USER_AGENT <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120 Safari/537.36"
BASE_URL   <- "http://www.tohto-bbl.com/gameinfo"

# ============================================================
# チーム名マッピング（略称 → 正式名称）
# ============================================================

TEAM_MAP <- c(
  # 1部
  "青学大"       = "青山学院大学",
  "亜大"         = "亜細亜大学",
  "亜細亜大"     = "亜細亜大学",
  "東洋大"       = "東洋大学",
  "中大"         = "中央大学",
  "中央大"       = "中央大学",
  "國學院大"     = "國學院大學",
  "国学大"       = "國學院大學",
  "日大"         = "日本大学",
  "日本大"       = "日本大学",
  "駒大"         = "駒澤大学",
  "駒澤大"       = "駒澤大学",
  "専大"         = "専修大学",
  "専修大"       = "専修大学",
  "帝京"         = "帝京大学",
  "帝京大"       = "帝京大学",
  # 2部
  "立正大"       = "立正大学",
  "拓大"         = "拓殖大学",
  "拓殖大"       = "拓殖大学",
  "東農大"       = "東京農業大学",
  "東京農大"     = "東京農業大学",
  "国士大"       = "国士舘大学",
  "国士舘大"     = "国士舘大学",
  "武蔵大"       = "武蔵大学",
  "大東大"       = "大東文化大学",
  "大東文化大"   = "大東文化大学",
  "東経大"       = "東京経済大学",
  "東京経大"     = "東京経済大学",
  "上武大"       = "上武大学",
  # 3部・4部
  "桜美林大"     = "桜美林大学",
  "東情大"       = "東京情報大学",
  "東京情報大"   = "東京情報大学",
  "流経大"       = "流通経済大学",
  "流通経済大"   = "流通経済大学",
  "山梨学院大"   = "山梨学院大学",
  "日体大"       = "日本体育大学",
  "日本体育大"   = "日本体育大学",
  "神大"         = "神奈川大学",
  "神奈川大"     = "神奈川大学",
  "関東学院大"   = "関東学院大学",
  "東国大"       = "東京国際大学",
  "東京国際大"   = "東京国際大学",
  "城西大"       = "城西大学",
  "順大"         = "順天堂大学",
  "順天堂大"     = "順天堂大学",
  "明星大"       = "明星大学",
  "東海大"       = "東海大学",
  "武蔵野大"     = "武蔵野大学",
  "千葉経大"     = "千葉経済大学",
  "江戸川大"     = "江戸川大学",
  "麗澤大"       = "麗澤大学",
  "共栄大"       = "共栄大学",
  "千葉商大"     = "千葉商科大学",
  "健大"         = "健康科学大学",
  "筑波大"       = "筑波大学",
  "関東学大"     = "関東学院大学",
  "神奈川工科大" = "神奈川工科大学",
  "湘南工科大"   = "湘南工科大学",
  "平国大"       = "平成国際大学",
  "平成国際大"   = "平成国際大学",
  "国士館大"     = "国士舘大学",
  "高崎経大"     = "高崎経済大学",
  # 略称不足・誤変換対策
  "亜細大"       = "亜細亜大学",
  "國學大"       = "國學院大學",
  "国学院大"     = "國學院大學",
  "東工大"       = "東京科学大学",
  "東科大"       = "東京科学大学",
  "科学大"       = "東京科学大学",
  "学習大"       = "学習院大学",
  "学習院大"     = "学習院大学",
  "芝工大"       = "芝浦工業大学",
  "芝浦工大"     = "芝浦工業大学",
  "順天大"       = "順天堂大学",
  "帝平大"       = "帝京平成大学",
  "都市大"       = "東京都市大学",
  "東京都市大"   = "東京都市大学",
  "武工大"       = "東京都市大学",
  "武蔵工大"     = "東京都市大学",
  "上武大"       = "上武大学",
  "明大"         = "明治大学",
  "早大"         = "早稲田大学",
  "慶大"         = "慶應義塾大学",
  "法大"         = "法政大学",
  "立大"         = "立教大学"
)

# 略称 → 正式名称への変換
# 1. 明示的マップに存在すればその値を返す
# 2. 末尾が"大"で"大学"/"大學"で終わっていなければ"学"を付与
# 3. それ以外はそのまま返す（不明チームは名前を保持してログに記録）
normalize_team <- function(x) {
  x_clean <- str_replace_all(x, "[　 \t\n\r]+", "") |> trimws()
  if (x_clean == "" || x_clean == "&nbsp;") return(NA_character_)
  # ※などのプレフィックスを除去（降格圏マーカーなど）
  x_clean <- str_remove(x_clean, "^[※★☆▲△▼▽●○◎◇◆□■×]+")
  if (x_clean == "") return(NA_character_)
  if (x_clean %in% names(TEAM_MAP)) return(unname(TEAM_MAP[x_clean]))
  # 末尾が"大"（大学/大學ではない）なら"学"を付与
  if (str_detect(x_clean, "大$") && !str_detect(x_clean, "(大学|大學)$")) {
    return(paste0(x_clean, "学"))
  }
  x_clean
}

unknown_teams_env <- new.env(parent = emptyenv())

track_unknown <- function(name) {
  clean <- str_replace_all(name, "[　 \t\n\r]+", "") |> trimws()
  if (clean == "" || clean %in% names(TEAM_MAP)) return(invisible(NULL))
  # すでにフォールバックで処理できる場合は記録不要
  if (str_detect(clean, "大$") && !str_detect(clean, "(大学|大學)$")) return(invisible(NULL))
  assign(clean, TRUE, envir = unknown_teams_env)
}

# ============================================================
# HTML取得（Shift-JIS対応）
# ============================================================

safe_fetch_html <- function(url, sleep_sec = 1.5) {
  Sys.sleep(sleep_sec)
  tryCatch({
    raw <- request(url) |>
      req_user_agent(USER_AGENT) |>
      req_timeout(30) |>
      req_perform() |>
      resp_body_raw()

    # charset 検出
    ascii_only <- raw
    ascii_only[as.integer(raw) > 127L] <- as.raw(0x20L)
    safe_head  <- rawToChar(ascii_only[seq_len(min(2000L, length(ascii_only)))])
    m          <- regmatches(safe_head,
                    regexpr('charset=["\']?([A-Za-z0-9_-]+)', safe_head, ignore.case = TRUE))
    raw_enc    <- if (length(m) > 0 && !is.na(m[1])) {
      sub('^charset=["\']?', '', m[1], ignore.case = TRUE)
    } else "UTF-8"

    enc <- toupper(trimws(raw_enc))
    # CP932 はShift-JISの上位互換（Windows拡張）で, libxml2が確実に認識する
    enc <- switch(enc,
      "SHIFT-JIS" = , "SHIFT_JIS" = , "X-SJIS" = , "SJIS" = , "CP932" = "CP932",
      "EUC-JP"    = , "X-EUC-JP"  = "EUC-JP",
      enc
    )

    tmp <- tempfile(fileext = ".html")
    on.exit(unlink(tmp), add = TRUE)
    writeBin(raw, tmp)
    html <- read_html(tmp, encoding = enc)
    # as.character(html) returns the UTF-8 serialized DOM (preserves HTML comments)
    raw_text <- as.character(html)

    list(html = html, encoding = enc, raw_text = raw_text)
  }, error = function(e) {
    message("  取得失敗: ", conditionMessage(e))
    NULL
  })
}

# ============================================================
# 日付パース
# ============================================================

make_date <- function(year, month, day) {
  if (is.na(year) || is.na(month) || is.na(day)) return(as.Date(NA))
  tryCatch(
    as.Date(sprintf("%04d-%02d-%02d", as.integer(year), as.integer(month), as.integer(day))),
    error = function(e) as.Date(NA)
  )
}

# "4/7（月）" or "9/16（土）" or "6/23(月)" → list(month, day) / NULL
parse_date_text <- function(text) {
  m <- regmatches(text, regexpr("(\\d{1,2})/(\\d{1,2})[（(]", text))
  if (length(m) == 0) return(NULL)
  parts <- strsplit(m, "[/（(]")[[1]]
  if (length(parts) < 2) return(NULL)
  list(month = as.integer(parts[1]), day = as.integer(parts[2]))
}

# ============================================================
# リーグ戦ページパーサー
# ============================================================
# <!--start-->...<!--end--> ブロック単位で解析。
# 各ブロック内の全tdを取得し、"-"セルの前後からチームとスコアを抽出。
# 日付は rowspan のため「最後に見た日付」を引き継ぐ。

parse_league_page <- function(raw_text, url_year, gametype) {
  # <!--start-->...<!--end--> ブロック抽出
  chunks <- str_split(raw_text, fixed("<!--start-->"))[[1]]
  if (length(chunks) < 2) return(NULL)

  game_blocks <- map(chunks[-1], ~ str_split(.x, fixed("<!--end-->"))[[1]][1])

  current_month <- NA_integer_
  current_day   <- NA_integer_
  out <- list()

  for (block in game_blocks) {
    # 日付更新（rowspan のある日付セルが含まれているブロックだけ）
    date_hit <- regmatches(block, regexpr("\\d{1,2}/\\d{1,2}[（(]", block))
    if (length(date_hit) > 0) {
      dp <- parse_date_text(date_hit[1])
      if (!is.null(dp)) {
        current_month <- dp$month
        current_day   <- dp$day
      }
    }

    if (is.na(current_month) || is.na(current_day)) next

    # ブロックを HTML としてパース→全td テキスト取得
    cells <- tryCatch({
      read_html(block, encoding = "UTF-8") |>
        html_elements("td") |>
        html_text2() |>
        trimws() |>
        str_replace_all("[　 \t\n\r]+", " ") |>
        trimws()
    }, error = function(e) character(0))

    if (length(cells) == 0) next

    # "-" の位置を検索（スコア区切りセル）
    dash_pos <- which(cells == "-")

    for (di in dash_pos) {
      if (di < 3 || di + 2 > length(cells)) next

      t1_raw  <- cells[di - 2]
      sc1_raw <- cells[di - 1]
      sc2_raw <- cells[di + 1]
      t2_raw  <- cells[di + 2]

      # スコアが数字のみ（延長は最後に x が付くことがある）
      if (!grepl("^\\d+$",  sc1_raw)) next
      if (!grepl("^\\d+x?$", sc2_raw)) next
      # チーム名が空・数字始まり・明らかに日付/球場でない
      if (nchar(t1_raw) == 0 || nchar(t2_raw) == 0) next
      if (grepl("^\\d", t1_raw) || grepl("^\\d", t2_raw)) next

      track_unknown(t1_raw)
      track_unknown(t2_raw)

      out[[length(out) + 1]] <- tibble(
        date     = make_date(url_year, current_month, current_day),
        team1    = normalize_team(t1_raw),
        score1   = as.integer(sc1_raw),
        score2   = as.integer(gsub("x", "", sc2_raw)),
        team2    = normalize_team(t2_raw),
        gametype = gametype
      )
    }
  }

  if (length(out) == 0) return(NULL)
  bind_rows(out) |>
    filter(!is.na(date), !is.na(team1), !is.na(team2)) |>
    distinct(date, team1, team2, score1, score2, .keep_all = TRUE)
}

# ============================================================
# 入替戦ページパーサー
# ============================================================
# 各 <tr align="center"> から:
#   - 日付セル: M/D(曜日) パターン
#   - スコアセル: " N - M " パターン（1セルに両スコア）
#   - チームセル: スコアセルの前後

parse_change_page <- function(html, cal_year, gametype) {
  rows <- html |> html_elements("tr")
  if (length(rows) == 0) return(NULL)

  out <- list()

  for (row in rows) {
    # 全td（ネスト含む）のテキストを取得
    all_cells <- row |>
      html_elements("td, th") |>
      html_text2() |>
      trimws() |>
      str_replace_all("[　 \t\n\r]+", " ") |>
      trimws() |>
      discard(~ .x == "" || .x == "&nbsp;")

    if (length(all_cells) < 3) next

    # スコアセル: 空白を含む "N - M" パターン（延長時は末尾 x）
    score_idx <- which(grepl("^\\s*\\d+\\s*-\\s*\\d+x?\\s*$", all_cells))
    if (length(score_idx) == 0) next

    # 複数ある場合は最初のものを使用
    si <- score_idx[1]
    if (si < 2 || si > length(all_cells) - 1) next

    t1_raw  <- all_cells[si - 1]
    sc_text <- all_cells[si]
    t2_raw  <- all_cells[si + 1]

    # チーム名の妥当性チェック
    if (grepl("^\\d|球場|神宮|明治|時$|分$", t1_raw)) next
    if (grepl("^\\d|球場|神宮|明治|時$|分$", t2_raw)) next
    if (nchar(t1_raw) < 2 || nchar(t2_raw) < 2) next
    if (t2_raw %in% c("詳細", "中止", "結果")) next

    # スコアパース
    sc_parts <- strsplit(trimws(sc_text), "\\s*-\\s*")[[1]]
    if (length(sc_parts) != 2) next
    sc1 <- suppressWarnings(as.integer(trimws(sc_parts[1])))
    sc2 <- suppressWarnings(as.integer(gsub("x", "", trimws(sc_parts[2]))))
    if (is.na(sc1) || is.na(sc2)) next

    # 日付: 同行に M/D(曜日) パターンを探す
    date_hit <- all_cells[grepl("^\\d{1,2}/\\d{1,2}[（(]", all_cells)]
    game_date <- as.Date(NA)
    if (length(date_hit) > 0) {
      dp <- parse_date_text(date_hit[1])
      if (!is.null(dp)) game_date <- make_date(cal_year, dp$month, dp$day)
    }

    track_unknown(t1_raw)
    track_unknown(t2_raw)

    out[[length(out) + 1]] <- tibble(
      date     = game_date,
      team1    = normalize_team(t1_raw),
      score1   = sc1,
      score2   = sc2,
      team2    = normalize_team(t2_raw),
      gametype = gametype
    )
  }

  if (length(out) == 0) return(NULL)
  bind_rows(out) |>
    filter(!is.na(date), !is.na(team1), !is.na(team2)) |>
    distinct(date, team1, team2, score1, score2, .keep_all = TRUE)
}

# ============================================================
# シーズン一覧生成
# ============================================================

LABEL_YEARS <- 2002:2025
LEAGUE_IDS  <- 1:4

# リーグ戦 URL 一覧
league_pages <- bind_rows(
  # 秋シーズン (Sept-Nov of label_year)
  expand_grid(label_year = LABEL_YEARS, league_id = LEAGUE_IDS) |>
    mutate(
      season    = "秋",
      url_year  = label_year,
      season_id = "02",
      url = glue("{BASE_URL}/schedule.php?YEAR={url_year}&SEASONID={season_id}&LEAGUEID={sprintf('%02d', league_id)}"),
      gametype  = paste0("東都", league_id, "部"),
      label     = paste0(label_year, "秋", league_id, "部")
    ),
  # 春シーズン (Apr-Jun of label_year+1)
  expand_grid(label_year = LABEL_YEARS, league_id = LEAGUE_IDS) |>
    mutate(
      season    = "春",
      url_year  = label_year + 1L,
      season_id = "01",
      url = glue("{BASE_URL}/schedule.php?YEAR={url_year}&SEASONID={season_id}&LEAGUEID={sprintf('%02d', league_id)}"),
      gametype  = paste0("東都", league_id, "部"),
      label     = paste0(label_year, "春", league_id, "部")
    )
) |>
  arrange(label_year, desc(season == "秋"), league_id)

# 入替戦 URL 一覧
change_pages <- bind_rows(
  # 秋入替戦 (Nov of label_year)
  tibble(label_year = LABEL_YEARS) |>
    mutate(
      season   = "秋",
      url      = glue("{BASE_URL}/{label_year}/change_a.shtml"),
      cal_year = label_year,
      gametype = "東都入替戦",
      label    = paste0(label_year, "秋入替戦")
    ),
  # 春入替戦 (Jun of label_year+1)
  tibble(label_year = LABEL_YEARS) |>
    mutate(
      season   = "春",
      url      = glue("{BASE_URL}/{label_year}/change_s.shtml"),
      cal_year = label_year + 1L,
      gametype = "東都入替戦",
      label    = paste0(label_year, "春入替戦")
    )
) |>
  arrange(label_year, desc(season == "秋"))

# ============================================================
# メイン処理
# ============================================================

message("=== 東都大学野球 スクレイピング開始 ===")
message("リーグ戦: ", nrow(league_pages), " ページ / 入替戦: ", nrow(change_pages), " ページ")

all_results <- list()
failed_log  <- list()

# --- リーグ戦 ---
message("\n--- リーグ戦 ---")
for (i in seq_len(nrow(league_pages))) {
  row   <- league_pages[i, ]
  url   <- row$url
  label <- row$label

  message(sprintf("[%d/%d] %s", i, nrow(league_pages), label))

  fetched <- safe_fetch_html(url)

  if (is.null(fetched)) {
    failed_log[[length(failed_log) + 1]] <- tibble(label = label, url = url, reason = "fetch_failed")
    next
  }

  result <- tryCatch(
    parse_league_page(fetched$raw_text, row$url_year, row$gametype),
    error = function(e) { message("  パースエラー: ", e$message); NULL }
  )

  if (is.null(result) || nrow(result) == 0) {
    message("  → データなし/パース失敗")
    failed_log[[length(failed_log) + 1]] <- tibble(label = label, url = url, reason = "no_data")
    next
  }

  message("  → ", nrow(result), " 試合")
  all_results[[length(all_results) + 1]] <- result |> mutate(source_url = url)
}

# --- 入替戦 ---
message("\n--- 入替戦 ---")
for (i in seq_len(nrow(change_pages))) {
  row   <- change_pages[i, ]
  url   <- row$url
  label <- row$label

  message(sprintf("[%d/%d] %s", i, nrow(change_pages), label))

  fetched <- safe_fetch_html(url)

  if (is.null(fetched)) {
    failed_log[[length(failed_log) + 1]] <- tibble(label = label, url = url, reason = "fetch_failed")
    next
  }

  result <- tryCatch(
    parse_change_page(fetched$html, row$cal_year, row$gametype),
    error = function(e) { message("  パースエラー: ", e$message); NULL }
  )

  if (is.null(result) || nrow(result) == 0) {
    message("  → データなし/パース失敗")
    failed_log[[length(failed_log) + 1]] <- tibble(label = label, url = url, reason = "no_data")
    next
  }

  message("  → ", nrow(result), " 試合")
  all_results[[length(all_results) + 1]] <- result |> mutate(source_url = url)
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
      gametype = gametype,
      team1    = team1,
      score1   = score1,
      score2   = score2,
      team2    = team2
    ) |>
    arrange(gamedate, gametype, team1)

  message("合計試合数: ", nrow(final_df))

  write_excel_csv(final_df, "data_out/tohto_results.csv")
  message("出力: data_out/tohto_results.csv")

  message("\n--- 年別試合数 ---")
  final_df |>
    mutate(year = year(gamedate)) |>
    count(year, name = "試合数") |>
    print(n = 30)

  message("\n--- 部別試合数 ---")
  final_df |>
    count(gametype, name = "試合数") |>
    print()
}

# 未知チーム名を出力
unknown_names <- ls(envir = unknown_teams_env)
if (length(unknown_names) > 0) {
  message("\n--- 未マッピングのチーム名 (", length(unknown_names), " 件) ---")
  message("  ", paste(sort(unknown_names), collapse = ", "))
  message("  ※ これらは名前が短く正式名が推定できなかったものです")
  message("    TEAM_MAP への追加を検討してください")
}

if (length(failed_log) > 0) {
  failed_df <- bind_rows(failed_log)
  write_excel_csv(failed_df, "logs/tohto_failed_pages.csv")
  message("\n--- 失敗ページ: ", nrow(failed_df), " 件 ---")
  failed_df |> count(reason) |> print()
}
