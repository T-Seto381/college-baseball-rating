# ============================================================
# 大学野球 全連盟リーグ戦試合結果スクレイピング
# ============================================================

library(tidyverse)
library(rvest)
library(httr2)
library(stringr)
library(lubridate)
library(glue)
library(readr)
library(fs)

dir_create("data_raw")
dir_create("data_out")
dir_create("logs")

user_agent_text <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120 Safari/537.36"

safe_read_html <- function(url, sleep_sec = 1, encoding = "UTF-8") {
  Sys.sleep(sleep_sec)
  tryCatch({
    request(url) |>
      req_user_agent(user_agent_text) |>
      req_timeout(30) |>
      req_perform() |>
      resp_body_html(encoding = encoding)
  }, error = function(e) {
    message("読み込み失敗: ", url, " / ", e$message)
    NULL
  })
}

clean_lines <- function(x) {
  x |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("\u3000", " ") |>
    str_squish() |>
    discard(~ .x == "")
}

# ============================================================
# 全JUBF加盟連盟定義
# ============================================================

all_leagues <- tribble(
  ~league,                ~official_url,
  "北海道学生野球連盟",   "http://www.do6.jp/",
  "札幌学生野球連盟",     "http://satsu6.com/",
  "北東北大学野球連盟",   "http://kitatohoku-u.umineco.jp/",
  "仙台六大学野球連盟",   "http://www.sen6.jp/",
  "南東北大学野球連盟",   "http://www.mtu-bbl.jp/",
  "千葉県大学野球連盟",   "http://www.cub-channel.net/",
  "関甲新学生野球連盟",   "http://kankoushin.jp",
  "東京新大学野球連盟",   "http://new-tokyo-bbl.com/",
  "東京六大学野球連盟",   "http://www.big6.gr.jp/",
  "東都大学野球連盟",     "http://www.tohto-bbl.com/",
  "首都大学野球連盟",     "http://tmubl.jp/",
  "神奈川大学野球連盟",   "http://www.kubl.jp/",
  "愛知大学野球連盟",     "http://aubl.jp/",
  "東海地区大学野球連盟", "http://tokaibbl.jp",
  "北陸大学野球連盟",     "http://hu-bl.com",
  "関西学生野球連盟",     "http://kansaibig6.jp/",
  "関西六大学野球連盟",   "http://www.kan6bb.jp/",
  "阪神大学野球連盟",     "http://www.hanshin-bbl.com/",
  "近畿学生野球連盟",     "http://www.kinkigakusei.org/top",
  "京滋大学野球連盟",     "http://www.keijidaigaku.com/",
  "広島六大学野球連盟",   "http://hiroshima-big6.com",
  "中国地区大学野球連盟", "http://www.cubf5589.com/",
  "四国地区大学野球連盟", "http://shikokubaseball.seesaa.net/",
  "九州六大学野球連盟",   "http://96bbl.com/",
  "福岡六大学野球連盟",   "http://fukuokabig6league.wixsite.com/my-site-5",
  "九州地区大学野球連盟", "http://www.kubu.jp/"
)

# ============================================================
# Step 1: 各公式サイトのOmyuTechリンク確認
# ============================================================

message("=== Step 1: 各公式サイトのOmyuTechリンク確認 ===")

check_omyutech_link <- function(league, url) {
  message("確認中: ", league, " (", url, ")")
  html <- safe_read_html(url, sleep_sec = 1)
  if (is.null(html)) {
    return(tibble(league = league, official_url = url,
                  has_omyutech = FALSE, league_id = NA_character_,
                  omyutech_url = NA_character_, note = "fetch_failed"))
  }

  links <- html |>
    html_elements("a") |>
    (\(n) tibble(text = html_text2(n), href = html_attr(n, "href")))() |>
    filter(!is.na(href))

  omyu_links <- links |>
    filter(str_detect(href, "omyutech\\.com")) |>
    mutate(
      league_id = str_match(href, "leagueId=(\\d+)")[, 2],
      cup_id    = str_match(href, "cupId=(\\d+)")[, 2]
    )

  if (nrow(omyu_links) > 0) {
    return(tibble(
      league = league,
      official_url = url,
      has_omyutech = TRUE,
      league_id = first(na.omit(omyu_links$league_id)),
      omyutech_url = first(omyu_links$href),
      note = paste(nrow(omyu_links), "件のOmyuTechリンク")
    ))
  }

  tibble(league = league, official_url = url,
         has_omyutech = FALSE, league_id = NA_character_,
         omyutech_url = NA_character_, note = "OmyuTechリンクなし")
}

league_omyutech_check <- map2_dfr(
  all_leagues$league,
  all_leagues$official_url,
  check_omyutech_link
)

write_excel_csv(league_omyutech_check, "logs/league_omyutech_check.csv")
message("OmyuTechリンクあり: ", sum(league_omyutech_check$has_omyutech))
print(league_omyutech_check)

# ============================================================
# Step 2: OmyuTech利用連盟のleagueId補完（既知）
# ============================================================

# 既知のleagueId / cupIdマッピング（公式サイト調査・既存コードより）
# 関甲新: leagueId=336
# 関西学生: leagueId不明、cupId既知
# 関西六大学: leagueId不明、cupId既知

omyutech_base <- "https://baseball.omyutech.com"
omyutech_json_url <- "https://baseball.omyutech.com/json/omyuinningscore.action"

fetch_omyutech_json <- function(cup_id, game_date = "", sleep_sec = 0.5) {
  Sys.sleep(sleep_sec)
  tryCatch({
    request(omyutech_json_url) |>
      req_url_query(
        cup_id    = cup_id,
        team_id   = "",
        game_date = game_date,
        game_id   = "",
        from      = "omyutech"
      ) |>
      req_user_agent(user_agent_text) |>
      req_headers(
        Referer = paste0(omyutech_base, "/CupHomePageMain.action?cupId=", cup_id),
        Accept  = "application/json"
      ) |>
      req_timeout(30) |>
      req_perform() |>
      resp_body_json()
  }, error = function(e) {
    message("JSON API失敗: cup_id=", cup_id, " / ", e$message)
    NULL
  })
}

parse_omyutech_game_list <- function(game_list, cup_title, league_name) {
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
      date      = as.Date(as.character(g$game_date), format = "%Y%m%d"),
      team1     = if (!is.null(g$team1_name)) g$team1_name else NA_character_,
      team2     = if (!is.null(g$team2_name)) g$team2_name else NA_character_,
      score1    = score1,
      score2    = score2,
      stadium   = if (!is.null(g$stadium)) g$stadium else NA_character_,
      game_status = gs,
      source_url  = paste0(omyutech_base, "/CupHomePageMain.action?cupId=",
                           g$cup_id, "&date=", g$game_date),
      note        = note
    )
  })
}

scrape_omyutech_league <- function(league_id, league_name, years = 2019:2021) {
  message("OmyuTech leagueId=", league_id, " (", league_name, ")")
  html <- safe_read_html(
    paste0(omyutech_base, "/leagueCup.action?leagueId=", league_id),
    sleep_sec = 1
  )
  if (is.null(html)) return(tibble())

  cup_links <- html |>
    html_elements("a") |>
    (\(n) tibble(text = html_text2(n), href = html_attr(n, "href")))() |>
    filter(!is.na(href), str_detect(href, "CupHomePageMain")) |>
    mutate(
      cup_id = str_match(href, "cupId=(\\d+)")[, 2],
      year   = as.integer(str_match(cup_id, "^(\\d{4})")[, 2])
    ) |>
    filter(!is.na(cup_id), year %in% years) |>
    distinct(cup_id, .keep_all = TRUE)

  if (nrow(cup_links) == 0) return(tibble())

  map_dfr(seq_len(nrow(cup_links)), function(i) {
    cid   <- cup_links$cup_id[i]
    title <- cup_links$text[i]
    resp  <- fetch_omyutech_json(cid)
    if (is.null(resp)) return(tibble())
    day_list <- if (!is.null(resp$day_list)) resp$day_list else character(0)
    if (length(day_list) == 0) {
      parse_omyutech_game_list(resp$game_list, title, league_name)
    } else {
      map_dfr(day_list, function(d) {
        r <- fetch_omyutech_json(cid, game_date = d)
        if (is.null(r)) return(tibble())
        parse_omyutech_game_list(r$game_list, title, league_name)
      })
    }
  })
}

scrape_omyutech_cup_ids <- function(cup_ids, league_name, cup_titles = NULL) {
  map_dfr(seq_along(cup_ids), function(i) {
    cid   <- cup_ids[i]
    title <- if (!is.null(cup_titles)) cup_titles[i] else cid
    message("OmyuTech cupId=", cid, " (", title, ")")
    resp  <- fetch_omyutech_json(cid)
    if (is.null(resp)) return(tibble())
    day_list <- if (!is.null(resp$day_list)) resp$day_list else character(0)
    if (length(day_list) == 0) {
      parse_omyutech_game_list(resp$game_list, title, league_name)
    } else {
      map_dfr(day_list, function(d) {
        r <- fetch_omyutech_json(cid, game_date = d)
        if (is.null(r)) return(tibble())
        parse_omyutech_game_list(r$game_list, title, league_name)
      })
    }
  })
}

# ============================================================
# Step 3: 各連盟個別スクレイパー
# ============================================================

# ---- 3-A. 関甲新学生野球連盟 (OmyuTech leagueId=336) ----
message("=== 関甲新学生野球連盟 ===")
results_kankoushin <- scrape_omyutech_league(336, "関甲新学生野球連盟")

# ---- 3-B. 関西学生野球連盟 / 関西六大学野球連盟 (OmyuTech cupId既知) ----
message("=== 関西学生野球連盟 ===")
# 2021秋 cupId=20210039058 のほか、2019・2020分を探索
kansai_gakusei_cup_ids <- c("20210039058")  # 追加分は後でleagueId探索で補完
results_kansai_gakusei_known <- scrape_omyutech_cup_ids(
  kansai_gakusei_cup_ids, "関西学生野球連盟"
)

message("=== 関西六大学野球連盟 ===")
kansai_roku_cup_ids <- c("20210038548")
results_kansai_roku_known <- scrape_omyutech_cup_ids(
  kansai_roku_cup_ids, "関西六大学野球連盟"
)

# ---- OmyuTechリンク確認後のleagueId追加 ----
# league_omyutech_check から追加のleagueIdが見つかれば使用
extra_omyu <- league_omyutech_check |>
  filter(has_omyutech, !is.na(league_id)) |>
  filter(!league %in% c("関甲新学生野球連盟"))  # 既に処理済みを除外

results_extra_omyu <- map_dfr(seq_len(nrow(extra_omyu)), function(i) {
  scrape_omyutech_league(extra_omyu$league_id[i], extra_omyu$league[i])
})

# OmyuTechで取得した全リーグ結果をまとめる
results_omyutech_all <- bind_rows(
  results_kankoushin,
  results_kansai_gakusei_known,
  results_kansai_roku_known,
  results_extra_omyu
) |>
  filter(!is.na(team1)) |>
  filter(is.na(date) | between(date, as.Date("2019-01-01"), as.Date("2021-12-31"))) |>
  distinct(league, date, team1, team2, .keep_all = TRUE)

message("OmyuTech取得試合数: ", nrow(results_omyutech_all))

# ============================================================
# Step 4: 独自サイト連盟のスクレイピング
# ============================================================

# 試合結果を格納するリストと失敗ログ
all_results   <- list(omyutech = results_omyutech_all)
failed_leagues <- character(0)

scrape_result <- function(league_name, result_df) {
  if (nrow(result_df) > 0) {
    all_results[[league_name]] <<- result_df
    message("  OK: ", nrow(result_df), "試合取得")
  } else {
    failed_leagues <<- c(failed_leagues, league_name)
    message("  FAIL: 試合取得できず")
  }
}

# ---- 3-C. 東京六大学野球連盟 ----
# http://www.big6.gr.jp/ → 独自システム。スケジュールページを試みる
message("=== 東京六大学野球連盟 ===")
scrape_big6_tokyo <- function(year, season) {
  # 試合日程: http://www.big6.gr.jp/games/schedule/YYYY/spring/ or /fall/
  season_code <- if (season == "spring") "spring" else "fall"
  url <- glue("http://www.big6.gr.jp/games/schedule/{year}/{season_code}/")
  html <- safe_read_html(url, sleep_sec = 1)
  if (is.null(html)) return(tibble())

  # テーブルを探す
  tables <- html |> html_elements("table")
  if (length(tables) == 0) return(tibble())

  map_dfr(tables, function(tbl) {
    rows <- tbl |> html_elements("tr")
    map_dfr(rows, function(r) {
      cells <- r |> html_elements("td, th") |> html_text2() |> str_squish()
      cells <- cells[cells != ""]
      if (length(cells) < 3) return(tibble())
      tibble(raw = paste(cells, collapse = " | "))
    })
  }) |> mutate(source = "東京六大学野球連盟", year = year, season = season)
}

big6_tokyo_raw <- map_dfr(
  cross2(c(2019, 2020, 2021), c("spring", "fall")),
  function(x) scrape_big6_tokyo(x[[1]], x[[2]])
)

# パース試み
parse_big6_tokyo <- function(df) {
  if (nrow(df) == 0) return(tibble())
  df |>
    filter(str_detect(raw, "\\d+\\s*[-－]\\s*\\d+")) |>
    mutate(
      date_str  = str_extract(raw, "\\d{1,2}/\\d{1,2}"),
      score_str = str_extract(raw, "(\\d+)\\s*[-－]\\s*(\\d+)"),
      score1    = as.integer(str_match(score_str, "(\\d+)\\s*[-－]")[, 2]),
      score2    = as.integer(str_match(score_str, "[-－]\\s*(\\d+)")[, 2]),
      league    = "東京六大学野球連盟",
      note      = NA_character_
    ) |>
    select(source, league, year, season, date_str, score1, score2, raw, note)
}

results_big6_tokyo <- parse_big6_tokyo(big6_tokyo_raw)
scrape_result("東京六大学野球連盟", results_big6_tokyo)

# ---- 3-D. 東都大学野球連盟 ----
message("=== 東都大学野球連盟 ===")
scrape_tohto <- function(year, season) {
  season_code <- if (season == "spring") "haru" else "aki"
  urls <- c(
    glue("http://www.tohto-bbl.com/result/{year}/{season_code}/"),
    glue("http://www.tohto-bbl.com/schedule/{year}/{season_code}/"),
    glue("http://www.tohto-bbl.com/{year}/{season_code}/result/")
  )
  for (url in urls) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        raw <- map_dfr(tables, function(tbl) {
          tbl |> html_table(fill = TRUE) |>
            mutate(across(everything(), as.character),
                   source = "東都大学野球連盟", year = year, season = season,
                   source_url = url)
        })
        if (nrow(raw) > 0) return(raw)
      }
    }
  }
  tibble()
}

results_tohto_raw <- map_dfr(
  cross2(c(2019, 2020, 2021), c("spring", "fall")),
  function(x) scrape_tohto(x[[1]], x[[2]])
)

results_tohto <- if (nrow(results_tohto_raw) > 0) {
  results_tohto_raw |> mutate(league = "東都大学野球連盟", note = NA_character_)
} else tibble()

scrape_result("東都大学野球連盟", results_tohto)

# ---- 3-E. 首都大学野球連盟 ----
message("=== 首都大学野球連盟 ===")
scrape_shuto <- function(year, season) {
  season_code <- if (season == "spring") "spring" else "autumn"
  urls <- c(
    glue("http://tmubl.jp/result/{year}/{season_code}/"),
    glue("http://tmubl.jp/schedule/{year}/{season_code}/"),
    glue("http://tmubl.jp/{year}/result/")
  )
  for (url in urls) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "首都大学野球連盟", year = year,
                       season = season, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_shuto_raw <- map_dfr(
  cross2(c(2019, 2020, 2021), c("spring", "fall")),
  function(x) scrape_shuto(x[[1]], x[[2]])
)

results_shuto <- if (nrow(results_shuto_raw) > 0) {
  results_shuto_raw |> mutate(league = "首都大学野球連盟", note = NA_character_)
} else tibble()

scrape_result("首都大学野球連盟", results_shuto)

# ---- 3-F. 北海道学生野球連盟 ----
message("=== 北海道学生野球連盟 ===")
scrape_hokkaido <- function(year) {
  url <- glue("http://www.do6.jp/result/{year}/")
  urls_try <- c(
    glue("http://www.do6.jp/result/{year}/"),
    glue("http://www.do6.jp/{year}/result/"),
    glue("http://www.do6.jp/")
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "北海道学生野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_hokkaido_raw <- map_dfr(2019:2021, scrape_hokkaido)
results_hokkaido <- if (nrow(results_hokkaido_raw) > 0) {
  results_hokkaido_raw |> mutate(league = "北海道学生野球連盟", note = NA_character_)
} else tibble()
scrape_result("北海道学生野球連盟", results_hokkaido)

# ---- 3-G. 札幌学生野球連盟 ----
message("=== 札幌学生野球連盟 ===")
scrape_sapporo <- function(year) {
  urls_try <- c(
    glue("http://satsu6.com/result/{year}/"),
    glue("http://satsu6.com/{year}/"),
    "http://satsu6.com/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "札幌学生野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_sapporo_raw <- map_dfr(2019:2021, scrape_sapporo)
results_sapporo <- if (nrow(results_sapporo_raw) > 0) {
  results_sapporo_raw |> mutate(league = "札幌学生野球連盟", note = NA_character_)
} else tibble()
scrape_result("札幌学生野球連盟", results_sapporo)

# ---- 3-H. 北東北大学野球連盟 ----
message("=== 北東北大学野球連盟 ===")
scrape_kitaotohoku <- function(year) {
  urls_try <- c(
    glue("http://kitatohoku-u.umineco.jp/result/{year}/"),
    "http://kitatohoku-u.umineco.jp/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "北東北大学野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_kitaotohoku_raw <- map_dfr(2019:2021, scrape_kitaotohoku)
results_kitaotohoku <- if (nrow(results_kitaotohoku_raw) > 0) {
  results_kitaotohoku_raw |> mutate(league = "北東北大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("北東北大学野球連盟", results_kitaotohoku)

# ---- 3-I. 仙台六大学野球連盟 ----
message("=== 仙台六大学野球連盟 ===")
scrape_sendai <- function(year, season) {
  urls_try <- c(
    glue("http://www.sen6.jp/result/{year}/{season}/"),
    glue("http://www.sen6.jp/{year}/"),
    "http://www.sen6.jp/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "仙台六大学野球連盟", year = year,
                       season = season, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_sendai_raw <- map_dfr(
  cross2(c(2019, 2020, 2021), c("spring", "fall")),
  function(x) scrape_sendai(x[[1]], x[[2]])
)
results_sendai <- if (nrow(results_sendai_raw) > 0) {
  results_sendai_raw |> mutate(league = "仙台六大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("仙台六大学野球連盟", results_sendai)

# ---- 3-J. 南東北大学野球連盟 ----
message("=== 南東北大学野球連盟 ===")
scrape_minamitohoku <- function(year) {
  urls_try <- c(
    glue("http://www.mtu-bbl.jp/result/{year}/"),
    "http://www.mtu-bbl.jp/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "南東北大学野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_minamitohoku_raw <- map_dfr(2019:2021, scrape_minamitohoku)
results_minamitohoku <- if (nrow(results_minamitohoku_raw) > 0) {
  results_minamitohoku_raw |> mutate(league = "南東北大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("南東北大学野球連盟", results_minamitohoku)

# ---- 3-K. 千葉県大学野球連盟 ----
message("=== 千葉県大学野球連盟 ===")
scrape_chiba <- function(year) {
  urls_try <- c(
    glue("http://www.cub-channel.net/result/{year}/"),
    "http://www.cub-channel.net/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "千葉県大学野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_chiba_raw <- map_dfr(2019:2021, scrape_chiba)
results_chiba <- if (nrow(results_chiba_raw) > 0) {
  results_chiba_raw |> mutate(league = "千葉県大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("千葉県大学野球連盟", results_chiba)

# ---- 3-L. 東京新大学野球連盟 ----
message("=== 東京新大学野球連盟 ===")
scrape_tokyoshin <- function(year) {
  urls_try <- c(
    glue("http://new-tokyo-bbl.com/result/{year}/"),
    "http://new-tokyo-bbl.com/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "東京新大学野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_tokyoshin_raw <- map_dfr(2019:2021, scrape_tokyoshin)
results_tokyoshin <- if (nrow(results_tokyoshin_raw) > 0) {
  results_tokyoshin_raw |> mutate(league = "東京新大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("東京新大学野球連盟", results_tokyoshin)

# ---- 3-M. 神奈川大学野球連盟 ----
message("=== 神奈川大学野球連盟 ===")
scrape_kanagawa <- function(year) {
  urls_try <- c(
    glue("http://www.kubl.jp/result/{year}/"),
    "http://www.kubl.jp/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "神奈川大学野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_kanagawa_raw <- map_dfr(2019:2021, scrape_kanagawa)
results_kanagawa <- if (nrow(results_kanagawa_raw) > 0) {
  results_kanagawa_raw |> mutate(league = "神奈川大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("神奈川大学野球連盟", results_kanagawa)

# ---- 3-N. 愛知大学野球連盟 ----
message("=== 愛知大学野球連盟 ===")
scrape_aichi <- function(year, season) {
  urls_try <- c(
    glue("http://aubl.jp/result/{year}/{season}/"),
    glue("http://aubl.jp/schedule/{year}/{season}/"),
    "http://aubl.jp/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "愛知大学野球連盟", year = year,
                       season = season, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_aichi_raw <- map_dfr(
  cross2(c(2019, 2020, 2021), c("spring", "fall")),
  function(x) scrape_aichi(x[[1]], x[[2]])
)
results_aichi <- if (nrow(results_aichi_raw) > 0) {
  results_aichi_raw |> mutate(league = "愛知大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("愛知大学野球連盟", results_aichi)

# ---- 3-O. 東海地区大学野球連盟 ----
message("=== 東海地区大学野球連盟 ===")
scrape_tokai <- function(year) {
  urls_try <- c(
    glue("http://tokaibbl.jp/result/{year}/"),
    "http://tokaibbl.jp/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "東海地区大学野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_tokai_raw <- map_dfr(2019:2021, scrape_tokai)
results_tokai <- if (nrow(results_tokai_raw) > 0) {
  results_tokai_raw |> mutate(league = "東海地区大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("東海地区大学野球連盟", results_tokai)

# ---- 3-P. 北陸大学野球連盟 ----
message("=== 北陸大学野球連盟 ===")
scrape_hokuriku <- function(year, season) {
  urls_try <- c(
    glue("http://hu-bl.com/result/{year}/{season}/"),
    glue("http://hu-bl.com/{year}/result/"),
    "http://hu-bl.com/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "北陸大学野球連盟", year = year,
                       season = season, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_hokuriku_raw <- map_dfr(
  cross2(c(2019, 2020, 2021), c("spring", "fall")),
  function(x) scrape_hokuriku(x[[1]], x[[2]])
)
results_hokuriku <- if (nrow(results_hokuriku_raw) > 0) {
  results_hokuriku_raw |> mutate(league = "北陸大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("北陸大学野球連盟", results_hokuriku)

# ---- 3-Q. 関西学生野球連盟（公式サイト直接） ----
message("=== 関西学生野球連盟（公式サイト） ===")
scrape_kansai_gakusei_official <- function(year, season) {
  urls_try <- c(
    glue("http://kansaibig6.jp/result/{year}/{season}/"),
    glue("http://kansaibig6.jp/schedule/{year}/{season}/"),
    "http://kansaibig6.jp/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "関西学生野球連盟", year = year,
                       season = season, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_kansai_gakusei_official_raw <- map_dfr(
  cross2(c(2019, 2020, 2021), c("spring", "fall")),
  function(x) scrape_kansai_gakusei_official(x[[1]], x[[2]])
)
# OmyuTechで既取得分と合算
results_kansai_gakusei_combined <- bind_rows(
  results_kansai_gakusei_known,
  if (nrow(results_kansai_gakusei_official_raw) > 0)
    results_kansai_gakusei_official_raw |> mutate(league = "関西学生野球連盟", note = NA_character_)
  else tibble()
)
scrape_result("関西学生野球連盟", results_kansai_gakusei_combined)

# ---- 3-R. 関西六大学野球連盟（公式サイト直接） ----
message("=== 関西六大学野球連盟（公式サイト） ===")
scrape_kansai_roku_official <- function(year, season) {
  urls_try <- c(
    glue("http://www.kan6bb.jp/result/{year}/{season}/"),
    glue("http://www.kan6bb.jp/schedule/{year}/{season}/"),
    "http://www.kan6bb.jp/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "関西六大学野球連盟", year = year,
                       season = season, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_kansai_roku_official_raw <- map_dfr(
  cross2(c(2019, 2020, 2021), c("spring", "fall")),
  function(x) scrape_kansai_roku_official(x[[1]], x[[2]])
)
results_kansai_roku_combined <- bind_rows(
  results_kansai_roku_known,
  if (nrow(results_kansai_roku_official_raw) > 0)
    results_kansai_roku_official_raw |> mutate(league = "関西六大学野球連盟", note = NA_character_)
  else tibble()
)
scrape_result("関西六大学野球連盟", results_kansai_roku_combined)

# ---- 3-S. 阪神大学野球連盟 ----
message("=== 阪神大学野球連盟 ===")
scrape_hanshin <- function(year) {
  urls_try <- c(
    glue("http://www.hanshin-bbl.com/result/{year}/"),
    "http://www.hanshin-bbl.com/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "阪神大学野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_hanshin_raw <- map_dfr(2019:2021, scrape_hanshin)
results_hanshin <- if (nrow(results_hanshin_raw) > 0) {
  results_hanshin_raw |> mutate(league = "阪神大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("阪神大学野球連盟", results_hanshin)

# ---- 3-T. 近畿学生野球連盟 ----
message("=== 近畿学生野球連盟 ===")
scrape_kinki <- function(year) {
  urls_try <- c(
    glue("http://www.kinkigakusei.org/result/{year}/"),
    "http://www.kinkigakusei.org/top"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "近畿学生野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_kinki_raw <- map_dfr(2019:2021, scrape_kinki)
results_kinki <- if (nrow(results_kinki_raw) > 0) {
  results_kinki_raw |> mutate(league = "近畿学生野球連盟", note = NA_character_)
} else tibble()
scrape_result("近畿学生野球連盟", results_kinki)

# ---- 3-U. 京滋大学野球連盟 ----
message("=== 京滋大学野球連盟 ===")
scrape_keiji <- function(year) {
  urls_try <- c(
    glue("http://www.keijidaigaku.com/result/{year}/"),
    "http://www.keijidaigaku.com/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "京滋大学野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_keiji_raw <- map_dfr(2019:2021, scrape_keiji)
results_keiji <- if (nrow(results_keiji_raw) > 0) {
  results_keiji_raw |> mutate(league = "京滋大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("京滋大学野球連盟", results_keiji)

# ---- 3-V. 広島六大学野球連盟 ----
message("=== 広島六大学野球連盟 ===")
scrape_hiroshima <- function(year) {
  urls_try <- c(
    glue("http://hiroshima-big6.com/result/{year}/"),
    "http://hiroshima-big6.com/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "広島六大学野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_hiroshima_raw <- map_dfr(2019:2021, scrape_hiroshima)
results_hiroshima <- if (nrow(results_hiroshima_raw) > 0) {
  results_hiroshima_raw |> mutate(league = "広島六大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("広島六大学野球連盟", results_hiroshima)

# ---- 3-W. 中国地区大学野球連盟 ----
message("=== 中国地区大学野球連盟 ===")
scrape_chugoku <- function(year) {
  urls_try <- c(
    glue("http://www.cubf5589.com/result/{year}/"),
    "http://www.cubf5589.com/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "中国地区大学野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_chugoku_raw <- map_dfr(2019:2021, scrape_chugoku)
results_chugoku <- if (nrow(results_chugoku_raw) > 0) {
  results_chugoku_raw |> mutate(league = "中国地区大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("中国地区大学野球連盟", results_chugoku)

# ---- 3-X. 四国地区大学野球連盟 (Seesaaブログ) ----
message("=== 四国地区大学野球連盟 ===")
# Seesaaブログ: カテゴリ別またはタグ別でリーグ戦結果を投稿
scrape_shikoku <- function(year) {
  urls_try <- c(
    glue("http://shikokubaseball.seesaa.net/archives/{year}-01-01.html"),
    "http://shikokubaseball.seesaa.net/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      # Seesaaブログは記事一覧形式
      articles <- html |> html_elements("article, .entry, .article")
      if (length(articles) > 0) {
        return(tibble(
          source = "四国地区大学野球連盟",
          year = year,
          source_url = url,
          raw = html |> html_text2() |> str_trunc(500)
        ))
      }
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "四国地区大学野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_shikoku_raw <- map_dfr(2019:2021, scrape_shikoku)
results_shikoku <- if (nrow(results_shikoku_raw) > 0) {
  results_shikoku_raw |> mutate(league = "四国地区大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("四国地区大学野球連盟", results_shikoku)

# ---- 3-Y. 九州六大学野球連盟 ----
message("=== 九州六大学野球連盟 ===")
scrape_kyushu_roku <- function(year) {
  urls_try <- c(
    glue("http://96bbl.com/result/{year}/"),
    "http://96bbl.com/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "九州六大学野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_kyushu_roku_raw <- map_dfr(2019:2021, scrape_kyushu_roku)
results_kyushu_roku <- if (nrow(results_kyushu_roku_raw) > 0) {
  results_kyushu_roku_raw |> mutate(league = "九州六大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("九州六大学野球連盟", results_kyushu_roku)

# ---- 3-Z. 福岡六大学野球連盟 (Wix) ----
message("=== 福岡六大学野球連盟 ===")
# Wixサイトは動的レンダリング。トップページのみ試行
scrape_fukuoka_roku <- function() {
  urls_try <- c(
    "https://fukuokabig6league.wixsite.com/my-site-5",
    "http://fukuokabig6league.wixsite.com/my-site-5"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 2)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "福岡六大学野球連盟", source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_fukuoka_roku_raw <- scrape_fukuoka_roku()
results_fukuoka_roku <- if (nrow(results_fukuoka_roku_raw) > 0) {
  results_fukuoka_roku_raw |> mutate(league = "福岡六大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("福岡六大学野球連盟", results_fukuoka_roku)

# ---- 3-AA. 九州地区大学野球連盟 ----
message("=== 九州地区大学野球連盟 ===")
scrape_kyushu_chiku <- function(year) {
  urls_try <- c(
    glue("http://www.kubu.jp/result/{year}/"),
    "http://www.kubu.jp/"
  )
  for (url in urls_try) {
    html <- safe_read_html(url, sleep_sec = 1)
    if (!is.null(html)) {
      tables <- html |> html_elements("table")
      if (length(tables) > 0) {
        return(
          map_dfr(tables, function(tbl) {
            tryCatch(
              tbl |> html_table(fill = TRUE) |>
                mutate(across(everything(), as.character),
                       source = "九州地区大学野球連盟", year = year, source_url = url),
              error = function(e) tibble()
            )
          })
        )
      }
    }
  }
  tibble()
}

results_kyushu_chiku_raw <- map_dfr(2019:2021, scrape_kyushu_chiku)
results_kyushu_chiku <- if (nrow(results_kyushu_chiku_raw) > 0) {
  results_kyushu_chiku_raw |> mutate(league = "九州地区大学野球連盟", note = NA_character_)
} else tibble()
scrape_result("九州地区大学野球連盟", results_kyushu_chiku)

# ============================================================
# Step 5: 結果まとめとレポート出力
# ============================================================

message("\n=== Step 5: 結果集計 ===")

# 全連盟リスト
all_league_names <- all_leagues$league

# 成功連盟
succeeded_leagues <- names(all_results)
# "omyutech" キーに含まれる連盟を別途展開
omyu_leagues_got <- if (!is.null(all_results$omyutech) && nrow(all_results$omyutech) > 0) {
  all_results$omyutech |> distinct(league) |> pull(league)
} else character(0)

succeeded_named <- c(omyu_leagues_got, setdiff(succeeded_leagues, "omyutech"))

# 失敗連盟（試合0件）
truly_failed <- all_league_names[
  !all_league_names %in% succeeded_named &
  all_league_names %in% failed_leagues
]

# 結果サマリー
summary_tbl <- all_leagues |>
  mutate(
    status = case_when(
      league %in% omyu_leagues_got ~ "成功(OmyuTech)",
      league %in% setdiff(succeeded_named, omyu_leagues_got) ~ "成功(公式サイト)",
      TRUE ~ "失敗"
    ),
    game_count = map_int(league, function(lg) {
      # OmyuTech結果を確認
      n_omyu <- if (!is.null(all_results$omyutech) && nrow(all_results$omyutech) > 0) {
        all_results$omyutech |> filter(league == lg) |> nrow()
      } else 0L
      # 個別結果を確認
      n_ind <- if (!is.null(all_results[[lg]])) nrow(all_results[[lg]]) else 0L
      n_omyu + n_ind
    })
  )

write_excel_csv(summary_tbl, "logs/league_scraping_summary.csv")

message("\n=== 連盟別スクレイピング結果 ===")
print(summary_tbl |> select(league, status, game_count, official_url))

message("\n=== 取得失敗連盟 ===")
failed_summary <- summary_tbl |> filter(status == "失敗")
print(failed_summary |> select(league, official_url))

message("\n失敗連盟数: ", nrow(failed_summary), " / 全", nrow(all_leagues), "連盟")
