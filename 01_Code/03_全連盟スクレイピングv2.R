# ============================================================
# 大学野球 全連盟リーグ戦スクレイピング v2
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

dir_create("data_raw")
dir_create("data_out")
dir_create("logs")

user_agent_text <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120 Safari/537.36"

safe_read_html <- function(url, sleep_sec = 1, encoding = "UTF-8") {
  Sys.sleep(sleep_sec)
  tryCatch({
    resp <- request(url) |>
      req_user_agent(user_agent_text) |>
      req_timeout(30) |>
      req_perform()
    resp_body_html(resp, encoding = encoding)
  }, error = function(e) {
    message("  読み込み失敗: ", url, "\n  理由: ", e$message)
    NULL
  })
}

# ============================================================
# OmyuTech 関連関数
# ============================================================
omyutech_base    <- "https://baseball.omyutech.com"
omyutech_json_url <- "https://baseball.omyutech.com/json/omyuinningscore.action"

fetch_omyutech_json <- function(cup_id, game_date = "", sleep_sec = 0.5) {
  Sys.sleep(sleep_sec)
  tryCatch({
    request(omyutech_json_url) |>
      req_url_query(cup_id=cup_id, team_id="", game_date=game_date,
                    game_id="", from="omyutech") |>
      req_user_agent(user_agent_text) |>
      req_headers(
        Referer = paste0(omyutech_base, "/CupHomePageMain.action?cupId=", cup_id),
        Accept  = "application/json"
      ) |>
      req_timeout(30) |>
      req_perform() |>
      resp_body_json()
  }, error = function(e) {
    message("  JSON API失敗: cup_id=", cup_id, " / ", e$message)
    NULL
  })
}

parse_omyutech_games <- function(game_list, cup_title, league_name) {
  if (is.null(game_list) || length(game_list) == 0) return(tibble())
  map_dfr(game_list, function(g) {
    gs     <- if (!is.null(g$game_status)) g$game_status else NA_character_
    note   <- if (!is.na(gs) && grepl("中止|不戦|没収", gs)) gs else NA_character_
    score1 <- if (!is.null(g$team1_score)) as.integer(g$team1_score) else NA_integer_
    score2 <- if (!is.null(g$team2_score)) as.integer(g$team2_score) else NA_integer_
    if (!is.na(gs) && grepl("中止|不戦|没収", gs)) { score1 <- NA_integer_; score2 <- NA_integer_ }
    tibble(
      source    = "OmyuTech",
      league    = league_name,
      cup_title = cup_title,
      date      = tryCatch(as.Date(as.character(g$game_date), format="%Y%m%d"), error=function(e) as.Date(NA)),
      team1     = if (!is.null(g$team1_name)) g$team1_name else NA_character_,
      team2     = if (!is.null(g$team2_name)) g$team2_name else NA_character_,
      score1    = score1,
      score2    = score2,
      game_status = gs,
      source_url  = paste0(omyutech_base, "/CupHomePageMain.action?cupId=", g$cup_id),
      note        = note
    )
  })
}

scrape_omyutech_cup <- function(cup_id, cup_title, league_name) {
  resp <- fetch_omyutech_json(cup_id)
  if (is.null(resp)) return(tibble())
  day_list <- if (!is.null(resp$day_list)) resp$day_list else character(0)
  if (length(day_list) == 0) {
    return(parse_omyutech_games(resp$game_list, cup_title, league_name))
  }
  map_dfr(day_list, function(d) {
    r <- fetch_omyutech_json(cup_id, game_date = d)
    if (is.null(r)) return(tibble())
    parse_omyutech_games(r$game_list, cup_title, league_name)
  })
}

scrape_omyutech_league <- function(league_id, league_name, years = 2019:2021) {
  message("  OmyuTech leagueId=", league_id, " (", league_name, ")")
  html <- safe_read_html(
    paste0(omyutech_base, "/leagueCup.action?leagueId=", league_id)
  )
  if (is.null(html)) return(tibble())

  cup_links <- html |>
    html_elements("a") |>
    (\(n) tibble(text=html_text2(n), href=html_attr(n,"href")))() |>
    filter(!is.na(href), str_detect(href, "CupHomePageMain")) |>
    mutate(
      cup_id = str_match(href, "cupId=(\\d+)")[,2],
      year   = as.integer(str_sub(cup_id, 1, 4))
    ) |>
    filter(!is.na(cup_id), year %in% years) |>
    distinct(cup_id, .keep_all = TRUE)

  if (nrow(cup_links) == 0) {
    message("  カップなし")
    return(tibble())
  }
  message("  カップ数: ", nrow(cup_links))

  map_dfr(seq_len(nrow(cup_links)), function(i) {
    scrape_omyutech_cup(cup_links$cup_id[i], cup_links$text[i], league_name)
  }) |>
    filter(!is.na(team1)) |>
    filter(is.na(date) | between(date, as.Date("2019-01-01"), as.Date("2021-12-31"))) |>
    distinct(date, team1, team2, .keep_all = TRUE)
}

# OmyuTechでleagueIdが不明な連盟のためにleagueCup付近をスキャン
discover_omyutech_league_id <- function(search_range = 320:380) {
  found <- map_dfr(search_range, function(lid) {
    Sys.sleep(0.3)
    html <- tryCatch(
      request(paste0(omyutech_base, "/leagueCup.action?leagueId=", lid)) |>
        req_user_agent(user_agent_text) |>
        req_timeout(15) |>
        req_perform() |>
        resp_body_html(encoding = "UTF-8"),
      error = function(e) NULL
    )
    if (is.null(html)) return(tibble(league_id=lid, title=NA_character_))
    title <- html |> html_element("title") |> html_text2()
    # CupHomePageMainへのリンク数（大会リストが実際にあるか確認）
    n_cups <- html |> html_elements("a") |>
      html_attr("href") |>
      keep(~!is.na(.x) && str_detect(.x, "CupHomePageMain")) |>
      length()
    tibble(league_id=lid, title=str_squish(title), n_cups=n_cups)
  }) |>
    filter(!is.na(title), !str_detect(title, "一球速報\\.com$|^$"), n_cups > 0)
  found
}

# ============================================================
# Step 1: OmyuTech leagueId スキャン（leagueId不明の連盟探索）
# ============================================================
message("=== Step 1: OmyuTech leagueId スキャン (320-380) ===")
discovered_leagues <- discover_omyutech_league_id(320:380)
write_excel_csv(discovered_leagues, "logs/omyutech_discovered_leagues.csv")
message("発見リーグ数: ", nrow(discovered_leagues))
print(discovered_leagues)

# ============================================================
# Step 2: 全OmyuTech連盟の試合データ取得
# ============================================================
message("\n=== Step 2: OmyuTech 連盟スクレイピング ===")

# 既知 + 発見分のleagueIdマッピング（手動確認済みを優先）
known_league_ids <- tribble(
  ~league_id, ~league_name,
  332L, "北東北大学野球連盟",
  333L, "仙台六大学野球連盟",
  334L, "南東北大学野球連盟",
  336L, "関甲新学生野球連盟",
  345L, "関西学生野球連盟",
  347L, "阪神大学野球連盟"
)

# 発見された新リーグIDを追加（既知のものを除く）
all_omyu_league_ids <- bind_rows(
  known_league_ids,
  discovered_leagues |>
    filter(!league_id %in% known_league_ids$league_id) |>
    transmute(league_id = as.integer(league_id), league_name = title)
) |>
  distinct(league_id, .keep_all = TRUE)

message("スクレイピング対象leagueId数: ", nrow(all_omyu_league_ids))

omyutech_results <- map_dfr(seq_len(nrow(all_omyu_league_ids)), function(i) {
  scrape_omyutech_league(
    all_omyu_league_ids$league_id[i],
    all_omyu_league_ids$league_name[i]
  )
})

# 既知cupId分も追加（関西学生2021秋・関西六大学2021秋）
extra_cup_results <- bind_rows(
  scrape_omyutech_cup("20210039058", "2021関西学生秋", "関西学生野球連盟"),
  scrape_omyutech_cup("20210038548", "2021関西六大学秋", "関西六大学野球連盟")
) |>
  filter(!is.na(team1)) |>
  filter(is.na(date) | between(date, as.Date("2019-01-01"), as.Date("2021-12-31")))

# 合算・重複除去
omyutech_all <- bind_rows(omyutech_results, extra_cup_results) |>
  distinct(league, date, team1, team2, .keep_all = TRUE)

message("OmyuTech合計取得試合数: ", nrow(omyutech_all))
print(omyutech_all |> count(league, sort=TRUE))

write_excel_csv(omyutech_all, "data_out/league_results_omyutech.csv")

# ============================================================
# Step 3: OmyuTech非対応連盟の独自サイトスクレイピング
# ============================================================
message("\n=== Step 3: 独自サイトスクレイピング ===")

# 独自サイト連盟（OmyuTechなし）
non_omyu_leagues <- tribble(
  ~league,                ~base_url,                              ~encoding,
  "北海道学生野球連盟",   "http://www.do6.jp/",                   "UTF-8",
  "札幌学生野球連盟",     "http://satsu6.com/",                   "UTF-8",
  "千葉県大学野球連盟",   "http://www.cub-channel.net/",          "UTF-8",
  "東京新大学野球連盟",   "http://new-tokyo-bbl.com/",            "UTF-8",
  "東京六大学野球連盟",   "http://www.big6.gr.jp/",               "UTF-8",
  "東都大学野球連盟",     "http://www.tohto-bbl.com/",            "CP932",
  "首都大学野球連盟",     "http://tmubl.jp/",                     "UTF-8",
  "神奈川大学野球連盟",   "http://www.kubl.jp/",                  "UTF-8",
  "愛知大学野球連盟",     "http://aubl.jp/",                      "UTF-8",
  "東海地区大学野球連盟", "http://tokaibbl.jp/",                  "UTF-8",
  "北陸大学野球連盟",     "http://hu-bl.com/",                    "UTF-8",
  "関西六大学野球連盟",   "http://www.kan6bb.jp/",                "UTF-8",
  "近畿学生野球連盟",     "http://www.kinkigakusei.org/",         "UTF-8",
  "京滋大学野球連盟",     "http://www.keijidaigaku.com/",         "UTF-8",
  "広島六大学野球連盟",   "http://hiroshima-big6.com/",           "UTF-8",
  "中国地区大学野球連盟", "http://www.cubf5589.com/",             "UTF-8",
  "四国地区大学野球連盟", "http://shikokubaseball.seesaa.net/",   "UTF-8",
  "九州六大学野球連盟",   "http://96bbl.com/",                    "UTF-8",
  "福岡六大学野球連盟",   "https://fukuokabig6league.wixsite.com/my-site-5", "UTF-8",
  "九州地区大学野球連盟", "http://www.kubu.jp/",                  "UTF-8"
)

# 汎用スクレイパー: トップページ→テーブル抽出
generic_scrape_site <- function(league_name, base_url, encoding, years = 2019:2021) {
  message("  ", league_name, " (", base_url, ")")

  # 試みるURLパターン
  candidate_urls <- c(
    base_url,
    paste0(base_url, "result/"),
    paste0(base_url, "schedule/"),
    paste0(base_url, "game/"),
    # 年別
    unlist(map(years, ~ c(
      paste0(base_url, .x, "/"),
      paste0(base_url, "result/", .x, "/"),
      paste0(base_url, .x, "/result/")
    )))
  )

  collected <- list()
  seen_urls <- character(0)

  for (url in candidate_urls) {
    if (url %in% seen_urls) next
    seen_urls <- c(seen_urls, url)
    html <- safe_read_html(url, sleep_sec = 1, encoding = encoding)
    if (is.null(html)) next

    # テーブルを試みる
    tables <- html |> html_elements("table")
    if (length(tables) > 0) {
      tbls <- map(tables, function(tbl) {
        tryCatch({
          d <- tbl |> html_table(fill = TRUE)
          if (nrow(d) > 1 && ncol(d) >= 2) {
            d |> mutate(across(everything(), as.character),
                        source_url = url)
          } else tibble()
        }, error = function(e) tibble())
      }) |>
        keep(~ nrow(.x) > 0)
      if (length(tbls) > 0) {
        collected <- c(collected, tbls)
        break  # 最初にテーブルが見つかったURLで取得終了
      }
    }
  }

  if (length(collected) == 0) {
    message("    → 試合テーブル取得不可")
    return(tibble())
  }

  result <- bind_rows(collected) |>
    mutate(source = "独自サイト", league = league_name, note = NA_character_)

  message("    → ", nrow(result), "行取得")
  result
}

non_omyu_results <- map_dfr(seq_len(nrow(non_omyu_leagues)), function(i) {
  tryCatch(
    generic_scrape_site(
      non_omyu_leagues$league[i],
      non_omyu_leagues$base_url[i],
      non_omyu_leagues$encoding[i]
    ),
    error = function(e) {
      message("  エラー: ", non_omyu_leagues$league[i], " / ", e$message)
      tibble()
    }
  )
})

if (nrow(non_omyu_results) > 0) {
  write_excel_csv(non_omyu_results, "data_out/league_results_non_omyu_raw.csv")
}

# ============================================================
# Step 4: サマリーレポート
# ============================================================
message("\n=== Step 4: 結果サマリー ===")

all_leagues <- c(
  "北海道学生野球連盟", "札幌学生野球連盟", "北東北大学野球連盟",
  "仙台六大学野球連盟", "南東北大学野球連盟", "千葉県大学野球連盟",
  "関甲新学生野球連盟", "東京新大学野球連盟", "東京六大学野球連盟",
  "東都大学野球連盟", "首都大学野球連盟", "神奈川大学野球連盟",
  "愛知大学野球連盟", "東海地区大学野球連盟", "北陸大学野球連盟",
  "関西学生野球連盟", "関西六大学野球連盟", "阪神大学野球連盟",
  "近畿学生野球連盟", "京滋大学野球連盟", "広島六大学野球連盟",
  "中国地区大学野球連盟", "四国地区大学野球連盟", "九州六大学野球連盟",
  "福岡六大学野球連盟", "九州地区大学野球連盟"
)

# 各連盟の取得試合数
count_omyu <- omyutech_all |>
  count(league, name = "n_games") |>
  mutate(method = "OmyuTech")

count_non_omyu <- if (nrow(non_omyu_results) > 0) {
  non_omyu_results |>
    count(league, name = "n_games") |>
    mutate(method = "独自サイト(要確認)")
} else tibble(league=character(), n_games=integer(), method=character())

summary_tbl <- tibble(league = all_leagues) |>
  left_join(bind_rows(count_omyu, count_non_omyu), by = "league") |>
  mutate(
    n_games = replace_na(n_games, 0L),
    method  = replace_na(method, "未取得"),
    status  = if_else(n_games > 0, "成功", "失敗")
  )

write_excel_csv(summary_tbl, "logs/league_scraping_summary_v2.csv")

message("\n--- 全連盟スクレイピング結果 ---")
print(summary_tbl |> arrange(status, league), n = 30)

failed <- summary_tbl |> filter(status == "失敗")
message("\n=== 取得できなかった連盟 (", nrow(failed), "件) ===")
print(failed |> select(league, method))

succeeded <- summary_tbl |> filter(status == "成功")
message("\n=== 取得できた連盟 (", nrow(succeeded), "件, 合計", sum(succeeded$n_games), "試合) ===")
print(succeeded |> select(league, method, n_games) |> arrange(desc(n_games)))
