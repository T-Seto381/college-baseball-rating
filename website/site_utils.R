suppressPackageStartupMessages({
  library(tidyverse)
})

TEAM_SLUG_OVERRIDE <- c(
  "東大" = "todai",
  "明治" = "meiji",
  "慶應" = "keio",
  "早稲田" = "waseda",
  "立教" = "rikkyo",
  "法政" = "hosei",
  "亜細亜" = "asia",
  "一橋" = "hitotsubashi",
  "学習院" = "gakushuin",
  "駒澤" = "komazawa",
  "国士舘" = "kokushikan",
  "芝浦工大" = "shibaura-it",
  "順天堂" = "juntendo",
  "上智" = "sophia",
  "成蹊" = "seikei",
  "青山学院" = "aoyama-gakuin",
  "専修" = "senshu",
  "大正" = "taisho",
  "拓殖" = "takushoku",
  "中央" = "chuo",
  "帝京平成" = "teikyo-heisei",
  "東京科学" = "science-tokyo",
  "東京都市" = "tokyo-city",
  "東農大" = "tokyo-nodai",
  "東洋" = "toyo",
  "日本大学" = "nihon",
  "立正" = "rissho",
  "國學院" = "kokugakuin"
)

LEAGUE_SLUG_OVERRIDE <- c(
  "東京六大学" = "tokyo6",
  "東都1部" = "tohto-1",
  "東都2部" = "tohto-2",
  "東都3部" = "tohto-3",
  "東都4部" = "tohto-4"
)

slugify_label <- function(x, prefix = "item") {
  if (is.na(x) || !nzchar(trimws(x))) {
    return(prefix)
  }

  ascii <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  ascii <- tolower(gsub("[^a-z0-9]+", "-", ascii))
  ascii <- gsub("(^-+|-+$)", "", ascii)

  if (!is.na(ascii) && nzchar(ascii)) {
    return(ascii)
  }

  codepoints <- utf8ToInt(enc2utf8(x))
  paste0(prefix, "-", paste(sprintf("%x", codepoints), collapse = "-"))
}

build_team_slug_table <- function(teams_info) {
  teams_info |>
    distinct(TeamShortName, .keep_all = TRUE) |>
    transmute(
      team = TeamShortName,
      team_slug = coalesce(
        unname(TEAM_SLUG_OVERRIDE[TeamShortName]),
        vapply(TeamShortName, slugify_label, character(1), prefix = "team")
      )
    )
}

build_league_slug_table <- function(league_names) {
  tibble(LeagueName = sort(unique(league_names))) |>
    filter(!is.na(LeagueName), LeagueName != "") |>
    transmute(
      LeagueName,
      league_slug = coalesce(
        unname(LEAGUE_SLUG_OVERRIDE[LeagueName]),
        vapply(LeagueName, slugify_label, character(1), prefix = "league")
      )
    )
}

standardize_team_names <- function(team_vec, teams_info) {
  lookup <- bind_rows(
    teams_info |>
      transmute(raw_team = str_squish(TeamShortName), team = TeamShortName),
    teams_info |>
      transmute(raw_team = str_squish(TeamName), team = TeamShortName)
  ) |>
    filter(!is.na(raw_team), raw_team != "") |>
    distinct(raw_team, .keep_all = TRUE)

  mapped <- unname(setNames(lookup$team, lookup$raw_team)[str_squish(team_vec)])
  ifelse(is.na(mapped), team_vec, mapped)
}

infer_league_name <- function(gametype) {
  gametype <- str_squish(gametype)
  case_when(
    gametype %in% c("東京六大学", "東京六大学野球", "東京六大学野球連盟") ~ "東京六大学",
    str_detect(gametype, "^東都[1-4]部$") ~ gametype,
    TRUE ~ NA_character_
  )
}
