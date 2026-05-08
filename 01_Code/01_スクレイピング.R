# ============================================================
# 大学野球 試合結果スクレイピング 共通設定
# ============================================================

library(tidyverse)
library(rvest)
library(httr2)
library(stringr)
library(lubridate)
library(janitor)
library(glue)
library(readr)
library(fs)

dir_create("data_raw")
dir_create("data_out")
dir_create("logs")

user_agent_text <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120 Safari/537.36"

safe_read_html <- function(url, sleep_sec = 1) {
  Sys.sleep(sleep_sec)
  
  tryCatch({
    request(url) |>
      req_user_agent(user_agent_text) |>
      req_timeout(30) |>
      req_perform() |>
      resp_body_html(encoding = "UTF-8")
  }, error = function(e) {
    message("読み込み失敗: ", url)
    message("理由: ", e$message)
    return(NULL)
  })
}

clean_lines <- function(x) {
  x |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("　", " ") |>
    str_squish() |>
    discard(~ .x == "")
}

parse_score <- function(score_text) {
  # "1 - 6", "10 - 9", "3-2", "8 - 9" などを想定
  m <- str_match(score_text, "^(\\d+)\\s*-\\s*(\\d+)$")
  
  tibble(
    score1 = suppressWarnings(as.integer(m[, 2])),
    score2 = suppressWarnings(as.integer(m[, 3]))
  )
}

parse_mmdd_to_date <- function(mmdd, year) {
  # "6/10" → Date
  md <- str_match(mmdd, "^(\\d{1,2})/(\\d{1,2})$")
  as.Date(sprintf("%d-%02d-%02d", year, as.integer(md[, 2]), as.integer(md[, 3])))
}





# ============================================================
# 1-1. OmyuTech cupId URL収集
# ============================================================

omyutech_base <- "https://baseball.omyutech.com"

# ここにOmyuTechで見つけたリーグ一覧ページや大会ページを追加していく
# leagueCup.action?leagueId=... が分かる場合はそれを入れる
seed_urls <- c(
  # 関甲新学生野球連盟
  "https://baseball.omyutech.com/leagueCup.action?leagueId=336",
  
  # 直接cupIdが分かっている大会例
  "https://baseball.omyutech.com/CupHomePageMain.action?cupId=20210038386", # 2021 関甲新秋 1部
  "https://baseball.omyutech.com/CupHomePageMain.action?cupId=20210039058", # 2021 関西学生秋
  "https://baseball.omyutech.com/CupHomePageMain.action?cupId=20210038548", # 2021 関西六大学秋
  "https://baseball.omyutech.com/CupHomePageMain.action?cupId=20190015465", # 2019 全日本大学選手権
  "https://baseball.omyutech.com/CupHomePageMain.action?cupId=20210001622"  # 2021 神宮大会 大学の部
)

collect_omyutech_cup_urls <- function(seed_urls) {
  
  map_dfr(seed_urls, function(url) {
    
    html <- safe_read_html(url)
    if (is.null(html)) {
      return(tibble(seed_url = url, cup_url = NA_character_, cup_id = NA_character_, title = NA_character_))
    }
    
    links <- html |>
      html_elements("a") |>
      (\(nodes) tibble(
        text = html_text2(nodes),
        href = html_attr(nodes, "href")
      ))() |>
      filter(!is.na(href)) |>
      mutate(
        cup_url = url_absolute(href, omyutech_base)
      ) |>
      filter(str_detect(cup_url, "CupHomePageMain\\.action\\?cupId=")) |>
      mutate(
        cup_id = str_match(cup_url, "cupId=([0-9]+)")[, 2],
        seed_url = url
      ) |>
      distinct(seed_url, cup_url, cup_id, text)
    
    # seed自体がcupページの場合も追加
    seed_cup <- tibble(
      seed_url = url,
      cup_url = url,
      cup_id = str_match(url, "cupId=([0-9]+)")[, 2],
      text = NA_character_
    ) |>
      filter(!is.na(cup_id))
    
    bind_rows(links, seed_cup) |>
      distinct(cup_url, .keep_all = TRUE)
  }) |>
    mutate(
      title = map_chr(cup_url, function(u) {
        html <- safe_read_html(u, sleep_sec = 0.5)
        if (is.null(html)) return(NA_character_)
        html |>
          html_element("title") |>
          html_text2() |>
          str_squish()
      })
    ) |>
    distinct(cup_url, .keep_all = TRUE)
}

omyutech_cups <- collect_omyutech_cup_urls(seed_urls)

write_excel_csv(omyutech_cups, "data_out/omyutech_cup_urls.csv")

omyutech_cups









# ============================================================
# 1-2. OmyuTech 試合結果パーサー（JSON API版）
# ============================================================
# OmyuTechのゲームデータはJSで動的レンダリングされるため、
# 内部JSON API (https://baseball.omyutech.com/json/omyuinningscore.action) を利用する。
# パラメータ: cup_id, team_id, game_date(YYYYMMDD), game_id, from

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
    message("JSON API失敗: cup_id=", cup_id, " date=", game_date, " / ", e$message)
    NULL
  })
}

parse_omyutech_game_list <- function(game_list, cup_title) {
  if (is.null(game_list) || length(game_list) == 0) return(tibble())

  map_dfr(game_list, function(g) {
    gs     <- if (!is.null(g$game_status)) g$game_status else NA_character_
    note   <- if (!is.na(gs) && grepl("中止|不戦|没収", gs)) gs else NA_character_
    score1 <- if (!is.null(g$team1_score)) as.integer(g$team1_score) else NA_integer_
    score2 <- if (!is.null(g$team2_score)) as.integer(g$team2_score) else NA_integer_
    if (!is.na(note)) { score1 <- NA_integer_; score2 <- NA_integer_ }

    tibble(
      source      = "OmyuTech",
      cup_title   = cup_title,
      date        = as.Date(g$game_date, format = "%Y%m%d"),
      game_no     = as.integer(g$game_number),
      team1       = if (!is.null(g$team1_name)) g$team1_name else NA_character_,
      team2       = if (!is.null(g$team2_name)) g$team2_name else NA_character_,
      score1      = score1,
      score2      = score2,
      stadium     = if (!is.null(g$stadium)) g$stadium else NA_character_,
      game_status = gs,
      source_url  = paste0(omyutech_base, "/CupHomePageMain.action?cupId=",
                           g$cup_id, "&date=", g$game_date),
      note        = note
    )
  })
}

scrape_omyutech_cup_json <- function(cup_id, cup_title = NA_character_) {

  empty_row <- tibble(
    source = "OmyuTech", cup_title = cup_title,
    date = as.Date(NA), game_no = NA_integer_,
    team1 = NA_character_, team2 = NA_character_,
    score1 = NA_integer_, score2 = NA_integer_,
    stadium = NA_character_, game_status = NA_character_,
    source_url = NA_character_, note = NA_character_
  )

  first_resp <- fetch_omyutech_json(cup_id)
  if (is.null(first_resp)) return(mutate(empty_row, note = "api_failed"))

  day_list <- if (!is.null(first_resp$day_list)) first_resp$day_list else character(0)

  if (length(day_list) == 0) {
    result <- parse_omyutech_game_list(first_resp$game_list, cup_title)
    if (nrow(result) == 0) return(mutate(empty_row, note = "no_game_parsed"))
    return(result)
  }

  all_games <- map_dfr(day_list, function(d) {
    resp <- fetch_omyutech_json(cup_id, game_date = d)
    if (is.null(resp)) return(tibble())
    parse_omyutech_game_list(resp$game_list, cup_title)
  })

  if (nrow(all_games) == 0) return(mutate(empty_row, note = "no_game_parsed"))

  distinct(all_games, date, team1, team2, .keep_all = TRUE)
}

omyutech_results <- omyutech_cups |>
  mutate(
    result = map2(cup_id, title, scrape_omyutech_cup_json)
  ) |>
  select(cup_url, cup_id, title, result) |>
  unnest(result) |>
  mutate(date = as.Date(date)) |>
  filter(
    is.na(date) | between(date, as.Date("2019-06-01"), as.Date("2021-11-30"))
  )

write_excel_csv(omyutech_results, "data_out/omyutech_game_results_201906_202111.csv")

# 取れた試合
omyutech_results |>
  filter(!is.na(team1), is.na(note)) |>
  arrange(date, cup_title) |>
  print(n = 50)

# 取れなかったページ
omyutech_results |>
  filter(!is.na(note)) |>
  distinct(cup_title, source_url, note) |>
  write_excel_csv("logs/omyutech_failed_pages.csv")


# ============================================================
# 2-1. JUBF 全日本大学野球選手権
# ============================================================

scrape_jubf_alljapan <- function(year) {
  
  url <- glue("https://www.jubf.net/system/prog/schedule.php?k=all&m=pc&s={year}")
  html <- safe_read_html(url)
  
  if (is.null(html)) {
    return(tibble(
      source = "JUBF",
      tournament = "全日本大学野球選手権大会",
      year = year,
      source_url = url,
      date = as.Date(NA),
      time = NA_character_,
      round = NA_character_,
      team1 = NA_character_,
      team2 = NA_character_,
      score1 = NA_integer_,
      score2 = NA_integer_,
      note = "read_html_failed"
    ))
  }
  
  lines <- html |>
    html_text2() |>
    str_split("\n") |>
    pluck(1) |>
    clean_lines()
  
  if (any(str_detect(lines, "中止"))) {
    return(tibble(
      source = "JUBF",
      tournament = "全日本大学野球選手権大会",
      year = year,
      source_url = url,
      date = as.Date(NA),
      time = NA_character_,
      round = NA_character_,
      team1 = NA_character_,
      team2 = NA_character_,
      score1 = NA_integer_,
      score2 = NA_integer_,
      note = "中止の可能性あり"
    ))
  }
  
  current_date <- as.Date(NA)
  current_venue <- NA_character_
  out <- list()
  
  for (line in lines) {
    
    # "■ 6/10 (月) の試合"
    if (str_detect(line, "■\\s*\\d{1,2}/\\d{1,2}")) {
      mmdd <- str_match(line, "■\\s*(\\d{1,2}/\\d{1,2})")[, 2]
      current_date <- parse_mmdd_to_date(mmdd, year)
      next
    }
    
    # 球場名
    if (line %in% c("神宮球場", "東京ドーム")) {
      current_venue <- line
      next
    }
    
    # "9:00 [1回戦] 大阪工業大 1 - 6 創価大 [詳細]"
    m <- str_match(
      line,
      "^(\\d{1,2}:\\d{2})\\s*\\[(.*?)\\]\\s*(.*?)\\s+(\\d+)\\s*-\\s*(\\d+)\\s+(.*?)\\s*\\["
    )
    
    if (!is.na(m[, 1])) {
      out[[length(out) + 1]] <- tibble(
        source = "JUBF",
        tournament = "全日本大学野球選手権大会",
        year = year,
        source_url = url,
        date = current_date,
        venue = current_venue,
        time = m[, 2],
        round = m[, 3],
        team1 = str_squish(m[, 4]),
        team2 = str_squish(m[, 7]),
        score1 = as.integer(m[, 5]),
        score2 = as.integer(m[, 6]),
        note = NA_character_
      )
    }
  }
  
  if (length(out) == 0) {
    return(tibble(
      source = "JUBF",
      tournament = "全日本大学野球選手権大会",
      year = year,
      source_url = url,
      date = as.Date(NA),
      venue = NA_character_,
      time = NA_character_,
      round = NA_character_,
      team1 = NA_character_,
      team2 = NA_character_,
      score1 = NA_integer_,
      score2 = NA_integer_,
      note = "no_game_parsed"
    ))
  }
  
  bind_rows(out)
}

jubf_alljapan_results <- map_dfr(c(2019, 2020, 2021), scrape_jubf_alljapan)

write_excel_csv(jubf_alljapan_results, "data_out/jubf_alljapan_results_2019_2021.csv")

jubf_alljapan_results

# ============================================================
# 2-2. 日本学生野球協会 明治神宮大会 大学の部
# ============================================================

scrape_jsba_jingu_college <- function(year) {
  
  url <- glue("https://www.student-baseball.or.jp/system/prog/schedule.php?e=jingu&k=all&m=pc&s={year}")
  html <- safe_read_html(url)
  
  if (is.null(html)) {
    return(tibble(
      source = "日本学生野球協会",
      tournament = "明治神宮野球大会 大学の部",
      year = year,
      source_url = url,
      date = as.Date(NA),
      game_no = NA_character_,
      time = NA_character_,
      round = NA_character_,
      team1 = NA_character_,
      team2 = NA_character_,
      score1 = NA_integer_,
      score2 = NA_integer_,
      note = "read_html_failed"
    ))
  }
  
  lines <- html |>
    html_text2() |>
    str_split("\n") |>
    pluck(1) |>
    clean_lines()
  
  if (any(str_detect(lines, "中止"))) {
    # 2020年など
    # ただしページ全体に「中止」があるだけで試合も載っているケースを避けたいので、
    # 後続で試合が取れなかった場合だけ中止扱いにする
  }
  
  current_date <- as.Date(NA)
  current_section <- NA_character_
  out <- list()
  
  i <- 1
  while (i <= length(lines)) {
    
    # "11/20(土)の試合"
    if (str_detect(lines[i], "^\\d{1,2}/\\d{1,2}\\(.+\\)の試合$")) {
      mmdd <- str_match(lines[i], "^(\\d{1,2}/\\d{1,2})")[, 2]
      current_date <- parse_mmdd_to_date(mmdd, year)
      current_section <- NA_character_
      i <- i + 1
      next
    }
    
    if (lines[i] %in% c("大学", "高校")) {
      current_section <- lines[i]
      i <- i + 1
      next
    }
    
    # 大学ブロックだけ読む
    # 構造例:
    # 第3試合
    # 13:30
    # [1回戦]
    # 神奈川大
    # 2 - 1
    # 龍谷大
    if (
      identical(current_section, "大学") &&
      str_detect(lines[i], "^第\\d+試合$") &&
      i + 5 <= length(lines)
    ) {
      game_no <- lines[i]
      time <- lines[i + 1]
      round <- str_remove_all(lines[i + 2], "\\[|\\]")
      team1 <- lines[i + 3]
      score_text <- lines[i + 4]
      team2 <- lines[i + 5]
      
      if (str_detect(score_text, "^\\d+\\s*-\\s*\\d+$")) {
        sc <- parse_score(score_text)
        
        out[[length(out) + 1]] <- tibble(
          source = "日本学生野球協会",
          tournament = "明治神宮野球大会 大学の部",
          year = year,
          source_url = url,
          date = current_date,
          game_no = game_no,
          time = time,
          round = round,
          team1 = team1,
          team2 = team2,
          score1 = sc$score1,
          score2 = sc$score2,
          note = NA_character_
        )
        
        i <- i + 6
        next
      }
      
      if (str_detect(score_text, "^-$|中止")) {
        out[[length(out) + 1]] <- tibble(
          source = "日本学生野球協会",
          tournament = "明治神宮野球大会 大学の部",
          year = year,
          source_url = url,
          date = current_date,
          game_no = game_no,
          time = time,
          round = round,
          team1 = team1,
          team2 = team2,
          score1 = NA_integer_,
          score2 = NA_integer_,
          note = "中止または未実施"
        )
        
        i <- i + 6
        next
      }
    }
    
    i <- i + 1
  }
  
  if (length(out) == 0) {
    return(tibble(
      source = "日本学生野球協会",
      tournament = "明治神宮野球大会 大学の部",
      year = year,
      source_url = url,
      date = as.Date(NA),
      game_no = NA_character_,
      time = NA_character_,
      round = NA_character_,
      team1 = NA_character_,
      team2 = NA_character_,
      score1 = NA_integer_,
      score2 = NA_integer_,
      note = "no_game_parsed_or_cancelled"
    ))
  }
  
  bind_rows(out)
}

jsba_jingu_results <- map_dfr(c(2019, 2020, 2021), scrape_jsba_jingu_college)

write_excel_csv(jsba_jingu_results, "data_out/jsba_jingu_college_results_2019_2021.csv")

jsba_jingu_results

# ============================================================
# 2-3. 全国大会結果 結合
# ============================================================

national_tournament_results <- bind_rows(
  jubf_alljapan_results,
  jsba_jingu_results
) |>
  arrange(date, tournament, game_no)

write_excel_csv(national_tournament_results, "data_out/national_tournament_results_2019_2021.csv")

national_tournament_results


# ============================================================
# 3-1. JUBF 加盟連盟公式サイト一覧を取得
# ============================================================

scrape_jubf_league_links <- function(season_code) {
  
  # season_code例:
  # 2021s = 2021春
  # 2021a = 2021秋
  
  url <- glue("https://www.jubf.net/system/prog/league_match_index.php?s={season_code}")
  html <- safe_read_html(url)
  
  if (is.null(html)) {
    return(tibble(
      season_code = season_code,
      source_url = url,
      league = NA_character_,
      official_url = NA_character_
    ))
  }
  
  link_tbl <- html |>
    html_elements("a") |>
    (\(nodes) tibble(
      text = html_text2(nodes),
      href = html_attr(nodes, "href")
    ))() |>
    filter(!is.na(href)) |>
    mutate(
      official_url = url_absolute(href, url)
    ) |>
    filter(text == "公式サイト") |>
    mutate(
      season_code = season_code,
      source_url = url
    )
  
  # HTMLの表構造が崩れている場合に備え、本文から連盟名を別途作る
  lines <- html |>
    html_text2() |>
    str_split("\n") |>
    pluck(1) |>
    clean_lines()
  
  league_names <- lines |>
    keep(~ str_detect(.x, "野球連盟|大学野球連盟|学生野球連盟|六大学野球連盟")) |>
    str_remove("\\s+\\d{1,2}/\\d{1,2}.*$") |>
    str_remove("\\s+1部.*$") |>
    str_squish() |>
    unique()
  
  # リンク数と連盟数が一致しないことがあるので、最低限URL一覧として保持
  link_tbl |>
    mutate(
      league_guess = league_names[row_number()]
    ) |>
    transmute(
      season_code,
      source_url,
      league = league_guess,
      official_url
    )
}

jubf_league_links <- map_dfr(
  c("2019a", "2020s", "2020a", "2021s", "2021a"),
  scrape_jubf_league_links
) |>
  distinct(league, official_url, .keep_all = TRUE)

write_excel_csv(jubf_league_links, "data_out/jubf_league_official_links.csv")

jubf_league_links

# ============================================================
# 3-2. OmyuTech収集済みリーグ名を推定
# ============================================================

omyutech_league_guess <- omyutech_cups |>
  mutate(
    title_clean = title |>
      str_remove(" : 一球速報.com.*$") |>
      str_remove("〖大学野球〗日程・結果-") |>
      str_squish(),
    
    league_guess = title_clean |>
      str_remove("20\\d{2}年度?") |>
      str_remove("令和\\d+年度") |>
      str_remove("春季.*$") |>
      str_remove("秋季.*$") |>
      str_remove("春.*$") |>
      str_remove("秋.*$") |>
      str_remove("リーグ戦.*$") |>
      str_squish()
  ) |>
  filter(!is.na(league_guess), league_guess != "") |>
  distinct(league_guess, cup_url, title)

write_excel_csv(omyutech_league_guess, "data_out/omyutech_league_guess.csv")

omyutech_league_guess

# install.packages("stringdist")
library(stringdist)

# ============================================================
# 3-3. JUBF公式一覧とOmyuTech収集済みを突合
# ============================================================

match_omyutech_league <- function(league_name, omyu_names) {
  
  if (is.na(league_name) || length(omyu_names) == 0) return(NA_character_)
  
  d <- stringdist::stringdist(league_name, omyu_names, method = "jw")
  best_i <- which.min(d)
  
  if (length(best_i) == 0 || is.na(d[best_i])) return(NA_character_)
  
  # 閾値は緩め。必要なら調整
  if (d[best_i] <= 0.35) {
    omyu_names[best_i]
  } else {
    NA_character_
  }
}

omyutech_names <- omyutech_league_guess$league_guess |> unique()

missing_omyutech_leagues <- jubf_league_links |>
  mutate(
    matched_omyutech_name = map_chr(league, match_omyutech_league, omyu_names = omyutech_names),
    has_omyutech = !is.na(matched_omyutech_name)
  ) |>
  filter(!has_omyutech) |>
  arrange(league)

write_excel_csv(missing_omyutech_leagues, "data_out/missing_omyutech_leagues.csv")

missing_omyutech_leagues


# ============================================================
# 3-4. OmyuTechにない連盟公式サイトを開く
# ============================================================

open_missing_league_sites <- function(missing_tbl, n = 5, start = 1) {
  
  urls <- missing_tbl |>
    filter(!is.na(official_url)) |>
    distinct(official_url) |>
    slice(start:(start + n - 1)) |>
    pull(official_url)
  
  walk(urls, browseURL)
  
  tibble(
    opened_url = urls
  )
}

# 最初の5件を開く
open_missing_league_sites(missing_omyutech_leagues, n = 5, start = 1)

# 次の5件
# open_missing_league_sites(missing_omyutech_leagues, n = 5, start = 6)

# 1. OmyuTech cupId収集
omyutech_cups <- collect_omyutech_cup_urls(seed_urls)

# 2. OmyuTech試合結果収集
omyutech_results <- omyutech_cups |>
  mutate(result = map2(cup_id, title, scrape_omyutech_cup_json)) |>
  select(cup_url, cup_id, title, result) |>
  unnest(result)

# 3. 全国大会
jubf_alljapan_results <- map_dfr(c(2019, 2020, 2021), scrape_jubf_alljapan)
jsba_jingu_results <- map_dfr(c(2019, 2020, 2021), scrape_jsba_jingu_college)

# 4. 公式サイト一覧
jubf_league_links <- map_dfr(
  c("2021s", "2021a"),
  scrape_jubf_league_links
)

# 5. OmyuTechなし候補を出す
missing_omyutech_leagues <- jubf_league_links |>
  mutate(
    matched_omyutech_name = map_chr(league, match_omyutech_league, omyu_names = omyutech_names),
    has_omyutech = !is.na(matched_omyutech_name)
  ) |>
  filter(!has_omyutech)