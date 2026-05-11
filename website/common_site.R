suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(reactable)
  library(plotly)
  library(htmltools)
  library(lubridate)
  library(jsonlite)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    y
  } else {
    x
  }
}

common_site_file <- normalizePath(
  c("common_site.R", file.path("website", "common_site.R"), "../common_site.R")[c(
    file.exists("common_site.R"),
    file.exists(file.path("website", "common_site.R")),
    file.exists("../common_site.R")
  )][1],
  winslash = "/",
  mustWork = TRUE
)
source(file.path(dirname(common_site_file), "site_utils.R"), local = TRUE)

resolve_site_root <- function() {
  candidates <- c(".", "website", "..")
  for (candidate in candidates) {
    candidate_path <- tryCatch(
      normalizePath(candidate, winslash = "/", mustWork = TRUE),
      error = function(e) NULL
    )
    if (is.null(candidate_path)) {
      next
    }
    if (file.exists(file.path(candidate_path, "_quarto.yml")) &&
        file.exists(file.path(candidate_path, "..", "data_out"))) {
      return(candidate_path)
    }
  }
  stop("website root not found from current working directory")
}

site_root <- resolve_site_root()
data_root <- normalizePath(file.path(site_root, "..", "data_out"), winslash = "/", mustWork = TRUE)
logs_root <- normalizePath(file.path(site_root, "..", "logs"), winslash = "/", mustWork = TRUE)
nav_prefix <- if (grepl("/(university|league)$", getwd())) "../" else ""

explore_url <- function(prefix = nav_prefix, anchor = NULL) {
  path <- paste0(prefix, "explore.html")
  if (is.null(anchor) || anchor == "") {
    return(path)
  }
  paste0(path, "#", anchor)
}

games_url <- function(prefix = nav_prefix) {
  paste0(prefix, "games.html")
}

final_ratings <- read_csv(file.path(data_root, "ratings", "final_ratings.csv"), show_col_types = FALSE) |>
  mutate(date = as.Date(date))

rating_hist <- read_csv(file.path(data_root, "ratings", "rating_history.csv"), show_col_types = FALSE) |>
  mutate(date = as.Date(date))

game_results <- read_csv(file.path(data_root, "ratings", "game_results_with_ratings.csv"), show_col_types = FALSE) |>
  mutate(gamedate = as.Date(gamedate))

teams_info <- read_excel(file.path(data_root, "teamdata.xlsx")) |>
  mutate(
    c1 = paste0("#", ColorCode1),
    c2 = paste0("#", if_else(is.na(ColorCode2), ColorCode1, ColorCode2)),
    c3 = paste0(
      "#",
      if_else(
        is.na(ColorCode3),
        if_else(is.na(ColorCode2), ColorCode1, ColorCode2),
        ColorCode3
      )
    )
  )

team_slug_table <- build_team_slug_table(teams_info)
league_slug_table <- build_league_slug_table(unique(infer_league_name(game_results$gametype)))
team_slug_lookup <- setNames(team_slug_table$team_slug, team_slug_table$team)
league_slug_lookup <- setNames(league_slug_table$league_slug, league_slug_table$LeagueName)

team_url <- function(team, prefix = nav_prefix) {
  slug <- unname(team_slug_lookup[team])

  ifelse(
    is.na(slug),
    "#",
    paste0(prefix, "university/", slug, ".html")
  )
}

league_url <- function(league, prefix = nav_prefix) {
  slug <- unname(league_slug_lookup[league])

  ifelse(
    is.na(slug),
    paste0(prefix, "explore.html#league-browser"),
    paste0(prefix, "league/", slug, ".html")
  )
}

team_meta <- teams_info |>
  transmute(
    team = TeamShortName,
    TeamName,
    c1,
    c2,
    c3
  )

fmt_date <- function(x) {
  ifelse(is.na(x), "", format(as.Date(x), "%Y/%m/%d"))
}

form_html <- function(form_str) {
  if (is.na(form_str) || form_str == "") {
    return(tags$span(class = "change-none", "\u2014"))
  }

  symbols <- strsplit(form_str, "")[[1]]
  tagList(lapply(symbols, function(symbol) {
    cls <- switch(symbol, W = "form-w", L = "form-l", D = "form-d", "change-none")
    label <- switch(symbol, W = "W", L = "L", D = "D", symbol)
    tags$span(class = cls, label)
  }))
}

change_html <- function(v) {
  if (is.na(v)) {
    return(tags$span(class = "change-none", "\u2014"))
  }
  if (v == 0) {
    return(tags$span(class = "change-none", "\u2014"))
  }
  if (v > 0) {
    return(tags$span(class = "change-up", paste0("\u25b2", v)))
  }
  tags$span(class = "change-down", paste0("\u25bc", abs(v)))
}

rank_cell <- function(v) {
  tags$span(class = "rank-number", v)
}

team_name_cell <- function(team, label, color, prefix = nav_prefix) {
  tags$div(
    class = "team-name-cell",
    tags$div(class = "team-color-bar", style = paste0("background:", color)),
    tags$a(
      href = team_url(team, prefix = prefix),
      class = "team-name-link",
      style = paste0("color:", color),
      label
    )
  )
}

league_name_cell <- function(league, prefix = nav_prefix) {
  tags$a(
    href = league_url(league, prefix = prefix),
    class = "team-link-inline",
    league
  )
}

team_link <- function(team, label = NULL, prefix = nav_prefix) {
  team_id <- team
  team_row <- team_meta |> filter(.data$team == team_id) |> slice_head(n = 1)
  if (nrow(team_row) == 0) {
    return(label %||% team_id)
  }

  tags$a(
    href = team_url(team, prefix = prefix),
    class = "team-link-inline",
    style = paste0("color:", team_row$c1[[1]]),
    label %||% team_row$TeamName[[1]]
  )
}

snapshot_dates <- sort(unique(rating_hist$date[rating_hist$date <= Sys.Date()]))
latest_snapshot_date <- max(snapshot_dates)
prev_snapshot_date <- max(snapshot_dates[snapshot_dates < latest_snapshot_date])
latest_game_date <- max(game_results$gamedate[game_results$gamedate <= Sys.Date()])
current_year <- max(year(game_results$gamedate[game_results$gamedate <= Sys.Date()]))
available_years <- sort(unique(year(game_results$gamedate[game_results$gamedate <= Sys.Date()])))

rank_history <- rating_hist |>
  filter(date <= Sys.Date()) |>
  arrange(date, desc(display_rating), team) |>
  group_by(date) |>
  mutate(rank = row_number()) |>
  ungroup()

team_league_events <- bind_rows(
  game_results |>
    transmute(gamedate, team = team1, LeagueName = infer_league_name(gametype)),
  game_results |>
    transmute(gamedate, team = team2, LeagueName = infer_league_name(gametype))
) |>
  filter(!is.na(LeagueName)) |>
  arrange(team, gamedate, LeagueName) |>
  group_by(team, gamedate) |>
  slice_tail(n = 1) |>
  ungroup()

team_league_history <- expand_grid(
  date = sort(unique(rank_history$date)),
  team = sort(unique(rank_history$team))
) |>
  left_join(
    team_league_events |>
      rename(date = gamedate),
    by = c("date", "team")
  ) |>
  arrange(team, date) |>
  group_by(team) |>
  fill(LeagueName, .direction = "downup") |>
  ungroup()

current_prev_ranks <- rank_history |>
  filter(date == prev_snapshot_date) |>
  select(team, prev_rank = rank)

team_game_log <- bind_rows(
  game_results |>
    transmute(
      gamedate,
      year,
      season,
      gametype,
      team = team1,
      opponent = team2,
      score_for = score1,
      score_against = score2,
      rating_before = round(display_r1_before, 1),
      rating_after = round(display_r1_after, 1),
      rating_delta = round(display_delta1, 1)
    ),
  game_results |>
    transmute(
      gamedate,
      year,
      season,
      gametype,
      team = team2,
      opponent = team1,
      score_for = score2,
      score_against = score1,
      rating_before = round(display_r2_before, 1),
      rating_after = round(display_r2_after, 1),
      rating_delta = round(display_delta2, 1)
    )
) |>
  left_join(
    team_meta |>
      select(team, TeamName, c1),
    by = "team"
  ) |>
  rename(team_name = TeamName, team_color = c1) |>
  left_join(
    team_meta |>
      select(team, TeamName, c1),
    by = c("opponent" = "team")
  ) |>
  rename(opponent_name = TeamName, opponent_color = c1) |>
  left_join(
    rank_history |>
      select(gamedate = date, team, rank_after = rank),
    by = c("gamedate", "team")
  ) |>
  mutate(
    result = case_when(
      score_for > score_against ~ "W",
      score_for < score_against ~ "L",
      TRUE ~ "D"
    ),
    score_label = paste0(score_for, "-", score_against),
    matchup_label = paste(team_name, opponent_name, sep = " vs "),
    hover_result = paste0(team_name, " ", score_for, "-", score_against, " ", opponent_name)
  )

form_lookup <- team_game_log |>
  arrange(team, desc(gamedate)) |>
  group_by(team) |>
  slice_head(n = 3) |>
  summarise(last3 = paste(result, collapse = ""), .groups = "drop")

last_game_lookup <- team_game_log |>
  group_by(team) |>
  summarise(last_gamedate = max(gamedate), .groups = "drop")

current_team_leagues <- team_league_history |>
  filter(date == latest_snapshot_date) |>
  select(team, LeagueName)

prev_team_leagues <- team_league_history |>
  filter(date == prev_snapshot_date) |>
  select(team, LeagueName)

current_team_table <- final_ratings |>
  left_join(team_meta, by = "team") |>
  left_join(current_team_leagues, by = "team") |>
  left_join(form_lookup, by = "team") |>
  left_join(last_game_lookup, by = "team") |>
  left_join(current_prev_ranks, by = "team") |>
  mutate(
    display_rating = round(display_rating, 1),
    rank_change = if_else(is.na(prev_rank), NA_integer_, prev_rank - rank)
  ) |>
  arrange(rank)

prev_league_table <- rank_history |>
  filter(date == prev_snapshot_date) |>
  left_join(prev_team_leagues, by = "team") |>
  filter(!is.na(LeagueName)) |>
  group_by(LeagueName) |>
  summarise(prev_rating = mean(display_rating), .groups = "drop") |>
  arrange(desc(prev_rating), LeagueName) |>
  mutate(prev_rank = row_number()) |>
  select(LeagueName, prev_rank)

current_league_table <- current_team_table |>
  filter(!is.na(LeagueName)) |>
  group_by(LeagueName) |>
  summarise(
    rating = round(mean(display_rating), 1),
    top_team = team[which.max(display_rating)],
    top_team_name = TeamName[which.max(display_rating)],
    top_team_rating = display_rating[which.max(display_rating)],
    .groups = "drop"
  ) |>
  arrange(desc(rating), LeagueName) |>
  mutate(rank = row_number()) |>
  left_join(prev_league_table, by = "LeagueName") |>
  mutate(rank_change = if_else(is.na(prev_rank), NA_integer_, prev_rank - rank))

league_history <- rank_history |>
  left_join(team_league_history, by = c("date", "team")) |>
  filter(!is.na(LeagueName)) |>
  group_by(date, LeagueName) |>
  summarise(display_rating = round(mean(display_rating), 1), .groups = "drop") |>
  arrange(LeagueName, date) |>
  mutate(year = year(date))

games_display <- game_results |>
  left_join(
    team_meta |> select(team, team1_name = TeamName, team1_color = c1),
    by = c("team1" = "team")
  ) |>
  left_join(
    team_meta |> select(team, team2_name = TeamName, team2_color = c1),
    by = c("team2" = "team")
  ) |>
  mutate(
    gamedate = as.Date(gamedate),
    date_label = fmt_date(gamedate),
    score_label = paste0(score1, " - ", score2),
    rating_move_team1 = round(display_delta1, 1),
    rating_move_team2 = round(display_delta2, 1),
    rating_move_label = paste0(
      team1_name, " ", sprintf("%+.1f", rating_move_team1),
      " / ",
      team2_name, " ", sprintf("%+.1f", rating_move_team2)
    ),
    year_label = as.integer(year),
    team1_url = team_url(team1),
    team2_url = team_url(team2)
  ) |>
  arrange(desc(gamedate), team1_name, team2_name)

highlight_games <- games_display |>
  mutate(
    total_level = round(display_r1_before + display_r2_before, 1),
    total_movement = round(abs(rating_move_team1), 1)
  )

highest_level_game <- highlight_games |>
  arrange(desc(total_level), desc(gamedate)) |>
  slice_head(n = 1)

largest_movement_game <- highlight_games |>
  arrange(desc(total_movement), desc(gamedate)) |>
  slice_head(n = 1)

build_university_table <- function(tbl, page_size = nrow(tbl), searchable = FALSE, show_pagination = FALSE, prefix = nav_prefix) {
  reactable(
    tbl,
    searchable = searchable,
    highlight = TRUE,
    pagination = show_pagination,
    defaultPageSize = page_size,
    paginationType = "jump",
    defaultSorted = list(rank = "asc"),
    columns = list(
      rank = colDef(
        name = "RANK",
        width = 74,
        align = "center",
        cell = function(v) rank_cell(v)
      ),
      display_rating = colDef(
        name = "RATE",
        width = 84,
        align = "right",
        format = colFormat(digits = 1)
      ),
      TeamName = colDef(
        name = "UNIVERSITY",
        minWidth = 180,
        cell = function(v, index) {
          row <- tbl[index, ]
          team_name_cell(row$team, v, row$c1, prefix = prefix)
        }
      ),
      LeagueName = colDef(
        name = "LEAGUE",
        minWidth = 130,
        cell = function(v) league_name_cell(v, prefix = prefix)
      ),
      last3 = colDef(
        name = "LAST 3",
        width = 110,
        align = "center",
        cell = function(v) form_html(v)
      ),
      last_gamedate = colDef(
        name = "LAST GAME",
        minWidth = 118,
        align = "right",
        format = colFormat(date = TRUE, locales = "ja-JP")
      ),
      rank_change = colDef(
        name = "CHANGE",
        width = 92,
        align = "center",
        cell = function(v) change_html(v)
      ),
      team = colDef(show = FALSE),
      c1 = colDef(show = FALSE),
      c2 = colDef(show = FALSE),
      c3 = colDef(show = FALSE),
      prev_rank = colDef(show = FALSE)
    ),
    class = "compact-reactable"
  )
}

build_league_table <- function(tbl, page_size = nrow(tbl), searchable = FALSE, show_pagination = FALSE, prefix = nav_prefix) {
  reactable(
    tbl,
    searchable = searchable,
    highlight = TRUE,
    pagination = show_pagination,
    defaultPageSize = page_size,
    paginationType = "jump",
    defaultSorted = list(rank = "asc"),
    columns = list(
      rank = colDef(
        name = "RANK",
        width = 74,
        align = "center",
        cell = function(v) rank_cell(v)
      ),
      rating = colDef(
        name = "RATE",
        width = 84,
        align = "right",
        format = colFormat(digits = 1)
      ),
      LeagueName = colDef(
        name = "LEAGUE",
        minWidth = 180,
        cell = function(v) league_name_cell(v, prefix = prefix)
      ),
      top_team_name = colDef(
        name = "TOP TEAM",
        minWidth = 180,
        cell = function(v, index) {
          row <- tbl[index, ]
          team_link(row$top_team, label = v, prefix = prefix)
        }
      ),
      top_team_rating = colDef(
        name = "TOP RATE",
        width = 96,
        align = "right",
        format = colFormat(digits = 1)
      ),
      rank_change = colDef(
        name = "CHANGE",
        width = 92,
        align = "center",
        cell = function(v) change_html(v)
      ),
      top_team = colDef(show = FALSE),
      prev_rank = colDef(show = FALSE)
    ),
    class = "compact-reactable"
  )
}

build_highlight_card <- function(row, kicker, prefix = nav_prefix) {
  if (nrow(row) == 0) {
    return(NULL)
  }

  tags$article(
    class = "feature-card",
    tags$div(class = "feature-kicker", kicker),
    tags$div(
      class = "feature-matchup",
      team_link(row$team1[[1]], label = row$team1_name[[1]], prefix = prefix),
      tags$span(class = "feature-score", row$score_label[[1]]),
      team_link(row$team2[[1]], label = row$team2_name[[1]], prefix = prefix)
    ),
    tags$div(
      class = "feature-meta-grid",
      tags$div(
        class = "feature-meta",
        tags$span(class = "feature-meta-label", "DATE"),
        tags$span(class = "feature-meta-value", row$date_label[[1]])
      ),
      tags$div(
        class = "feature-meta",
        tags$span(class = "feature-meta-label", "LEVEL"),
        tags$span(class = "feature-meta-value", sprintf("%.1f", row$total_level[[1]]))
      ),
      tags$div(
        class = "feature-meta",
        tags$span(class = "feature-meta-label", "MOVE"),
        tags$span(class = "feature-meta-value", row$rating_move_label[[1]])
      )
    )
  )
}
