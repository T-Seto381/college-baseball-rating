# ============================================================
# 大学野球レーティング - モデル実装・比較・出力
# ============================================================
#
# 比較モデル:
#   (1) Elo             - K=50固定、スコア考慮なし
#   (2) Elo+Score       - K=50固定、スコア調整あり (α,c 最適化)
#   (3) Glicko2         - 動的RD、shrink_year/shrink_season最適化
#   (4) Glicko2+Score   - 動的RD + スコア調整 (α,c,shrink_year,shrink_season 最適化)
#
# スコア調整式:
#   S_adj = (1-α)*W_binary + α*(tanh(diff/c)+1)/2
#
# Glicko2 設計:
#   - RDは固定せず動的に管理（試合をするたびに減少、休止期間に増加）
#   - 年度越え（卒業による入れ替え）と春秋間（休止期間）でshrinkを分離
#   - K_eff=50固定（更新速度を規定、Eloと比較可能に保つ）
#
# 表示スケール:
#   内部計算は Elo 標準スケール (init=1500, 400点差=10:1)
#   display_rating は出力段階で一度だけ偏差値スケール (mean=50, sd=10) に変換し、
#   Web 側ではこの列をそのまま利用する
#
# 検証: ウォークフォワードCV (学習: 2000-2019, 評価: 2020-2026)
#   指標: Log-loss, Accuracy, Brier Score
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(readr)
  library(lubridate)
  library(glue)
  library(fs)
})

dir_create("data_out/ratings")
dir_create("logs")

source("website/site_utils.R", local = TRUE)

INIT_RATING <- 1500   # 初期レーティング（内部計算用）
K_FIXED     <- 50     # Elo K-factor (固定)
SCALE       <- 173.7178  # Glicko2スケール定数

# Glicko2 動的RDパラメータ（固定値 — グリッドサーチ対象外）
# RD は「そのチームのレート推定値がどれだけ不確か」を表す
RD_INIT        <- 150  # 初期RD（新シーズン開始時）
RD_MIN         <-  20  # RD下限（どんなに試合を積んでも残る不確実性）
RD_MAX         <- 300  # RD上限
RD_DECAY       <- 0.97 # 試合ごとのRD減衰率（1試合で約3%減 → 不確実性が低下）
RD_BUMP_SEASON <-  20  # 春秋間のRD増加（活動休止による不確実性増）
RD_BUMP_YEAR   <-  60  # 年度越えのRD増加（卒業で約半数が入れ替わる影響）

TRAIN_CUTOFF <- as.Date("2019-12-31")  # ウォークフォワードCV 分割点

# ============================================================
# 1. データ読み込み
# ============================================================

teams_raw <- read_excel("data_out/teamdata.xlsx")

games_big6 <- read_csv("data_out/big6_results_2000_2026.csv",
                       show_col_types = FALSE) |>
  mutate(source = "東京六大学")

games_tohto <- read_csv("data_out/tohto_results.csv",
                        show_col_types = FALSE) |>
  mutate(source = "東都大学野球")

games_raw <- bind_rows(games_big6, games_tohto) |>
  mutate(
    gamedate = as.Date(gamedate),
    team1 = standardize_team_names(team1, teams_raw),
    team2 = standardize_team_names(team2, teams_raw),
    score1 = as.numeric(score1),
    score2 = as.numeric(score2)
  ) |>
  filter(
    !is.na(gamedate),
    !is.na(team1), team1 != "",
    !is.na(team2), team2 != "",
    !is.na(score1),
    !is.na(score2)
  ) |>
  distinct(gamedate, gametype, team1, score1, score2, team2, .keep_all = TRUE) |>
  select(gamedate, gametype, team1, score1, score2, team2) |>
  arrange(gamedate, gametype, team1, team2)

unknown_teams <- setdiff(
  sort(unique(c(games_raw$team1, games_raw$team2))),
  sort(unique(teams_raw$TeamShortName))
)

if (length(unknown_teams) > 0) {
  warning("Unmapped team names found: ", paste(unknown_teams, collapse = ", "))
}

all_team_names <- sort(unique(c(games_raw$team1, games_raw$team2)))
cat("チーム名一覧:", paste(all_team_names, collapse = ", "), "\n")

# ============================================================
# 2. 共通ユーティリティ関数
# ============================================================

score_adjusted_outcome <- function(score_i, score_j, alpha, c_scale) {
  diff     <- score_i - score_j
  W_binary <- dplyr::case_when(diff > 0 ~ 1, diff == 0 ~ 0.5, TRUE ~ 0)
  W_margin <- (tanh(diff / c_scale) + 1) / 2
  (1 - alpha) * W_binary + alpha * W_margin
}

log_loss <- function(y, p) {
  p <- pmin(pmax(p, 1e-7), 1 - 1e-7)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

brier_score <- function(y, p) mean((y - p)^2)

accuracy <- function(y, p) {
  pred <- dplyr::case_when(p > 0.5 ~ 1, p < 0.5 ~ 0, TRUE ~ 0.5)
  mean(pred == y)
}

# 内部レーティングを偏差値スケール (mean=50, sd=10) に変換
# sd=0のとき（全チーム同レート）は50を返す
to_hensachi <- function(r) {
  m <- mean(r); s <- sd(r)
  if (s < 1e-9) return(rep(50, length(r)))
  round(50 + 10 * (r - m) / s, 1)
}

# ============================================================
# 3. Elo レーティング
# ============================================================

elo_expected <- function(r_i, r_j) {
  1 / (1 + 10^((r_j - r_i) / 400))
}

compute_elo <- function(games, K = K_FIXED, alpha = 0, c_scale = 5,
                        init_r = INIT_RATING) {

  teams    <- sort(unique(c(games$team1, games$team2)))
  n_teams  <- length(teams)
  team_idx <- setNames(seq_len(n_teams), teams)
  ratings  <- rep(init_r, n_teams)
  n        <- nrow(games)

  t1 <- team_idx[games$team1]; t2 <- team_idx[games$team2]
  s1 <- games$score1;           s2 <- games$score2

  r1_before_v <- numeric(n); r2_before_v <- numeric(n)
  E1_v <- numeric(n); S1_v <- numeric(n); delta_v <- numeric(n)
  r1_after_v  <- numeric(n); r2_after_v  <- numeric(n)

  for (i in seq_len(n)) {
    i1 <- t1[[i]]; i2 <- t2[[i]]
    r_i <- ratings[[i1]]; r_j <- ratings[[i2]]

    E_i <- elo_expected(r_i, r_j)
    S_i <- score_adjusted_outcome(s1[[i]], s2[[i]], alpha, c_scale)
    d   <- K * (S_i - E_i)

    ratings[[i1]] <- r_i + d; ratings[[i2]] <- r_j - d

    r1_before_v[[i]] <- r_i; r2_before_v[[i]] <- r_j
    E1_v[[i]] <- E_i; S1_v[[i]] <- S_i; delta_v[[i]] <- d
    r1_after_v[[i]] <- ratings[[i1]]; r2_after_v[[i]] <- ratings[[i2]]
  }

  history <- tibble(
    gamedate  = games$gamedate, gametype = games$gametype,
    team1 = games$team1, score1 = s1, score2 = s2, team2 = games$team2,
    r1_before = r1_before_v, r2_before = r2_before_v,
    E1 = E1_v, S1 = S1_v, delta = delta_v,
    r1_after = r1_after_v, r2_after = r2_after_v
  )

  list(history = history, final_ratings = setNames(ratings, teams))
}

# ============================================================
# 4. Glicko2 レーティング（動的RD・分離shrink）
# ============================================================
#
# RDの役割:
#   RD（Rating Deviation）は各チームの「レート推定値の不確かさ」。
#   試合をこなすほど情報が蓄積されRDが下がる（確信が高まる）。
#   長期間試合がないと（特に年度越え）不確かさが増すためRDが上がる。
#   RDはg(φ)を通じて期待勝率の「敏感さ」に影響する。
#     g(φ) < 1 → 同じレート差でも期待勝率が0.5に引き寄せられる
#     （不確かなチーム同士の試合は結果が読みにくい）
#
# shrinkの役割:
#   シーズン変わり目にレートを全体平均方向に引き戻す。
#   年度越え: 卒業で約半数が入れ替わるため強めに引き戻す (shrink_year)
#   春秋間:   メンバー変化なし・活動休止だけのため弱めに引き戻す (shrink_season)
# ============================================================

glicko2_g <- function(phi) {
  1 / sqrt(1 + 3 * phi^2 / pi^2)
}

#' Glicko2 レーティング計算（動的RD、分離shrink）
#'
#' @param games          試合データ
#' @param K_eff          実効K-factor (固定 50)
#' @param alpha          スコア重み
#' @param c_scale        スコアスケール
#' @param shrink_year    年度越えの平均回帰率（卒業効果）
#' @param shrink_season  春秋間の平均回帰率（休止期間効果）
#' @param init_r         初期レーティング
#' @param rd_init        初期RD
#' @param rd_min         RD下限
#' @param rd_max         RD上限
#' @param rd_decay       試合ごとのRD減衰率
#' @param rd_bump_season 春秋間のRD増加量
#' @param rd_bump_year   年度越えのRD増加量
compute_glicko2 <- function(games, K_eff = K_FIXED, alpha = 0, c_scale = 5,
                             shrink_year = 0, shrink_season = 0,
                             init_r = INIT_RATING,
                             rd_init        = RD_INIT,
                             rd_min         = RD_MIN,
                             rd_max         = RD_MAX,
                             rd_decay       = RD_DECAY,
                             rd_bump_season = RD_BUMP_SEASON,
                             rd_bump_year   = RD_BUMP_YEAR) {

  teams    <- sort(unique(c(games$team1, games$team2)))
  n_teams  <- length(teams)
  team_idx <- setNames(seq_len(n_teams), teams)
  ratings  <- rep(init_r, n_teams)
  rds      <- rep(rd_init, n_teams)   # チームごとの動的RD
  n        <- nrow(games)

  t1      <- team_idx[games$team1]; t2 <- team_idx[games$team2]
  s1      <- games$score1;          s2 <- games$score2
  yr_v    <- year(games$gamedate);  mo_v <- month(games$gamedate)
  # シーズンキー: YYYY*10 + 1(春) or 2(秋)
  season_keys <- yr_v * 10L + if_else(mo_v <= 8L, 1L, 2L)

  r1_before_v <- numeric(n); r2_before_v <- numeric(n)
  E1_v <- numeric(n); S1_v <- numeric(n); delta_v <- numeric(n)
  r1_after_v  <- numeric(n); r2_after_v  <- numeric(n)

  current_season <- -1L

  for (i in seq_len(n)) {
    sk <- season_keys[[i]]

    if (current_season != -1L && sk != current_season) {
      grand_mean <- mean(ratings)
      curr_yr    <- sk %/% 10L
      prev_yr    <- current_season %/% 10L

      if (curr_yr != prev_yr) {
        # ── 年度越え（秋→春）: 卒業で約半数が入れ替わる ──
        ratings <- ratings + shrink_year * (grand_mean - ratings)
        rds     <- pmin(rds + rd_bump_year, rd_max)
      } else {
        # ── 春秋間: 活動休止期間のみ ──
        ratings <- ratings + shrink_season * (grand_mean - ratings)
        rds     <- pmin(rds + rd_bump_season, rd_max)
      }
    }
    current_season <- sk

    i1 <- t1[[i]]; i2 <- t2[[i]]
    r_i <- ratings[[i1]]; r_j <- ratings[[i2]]
    rd_i <- rds[[i1]];    rd_j <- rds[[i2]]

    # 両チームのRDを合成してg(φ)を計算
    # → 不確かなチームとの試合は期待勝率が0.5寄りになる
    phi_combined <- sqrt((rd_i / SCALE)^2 + (rd_j / SCALE)^2)
    g_phi        <- glicko2_g(phi_combined)

    E_i <- 1 / (1 + 10^((-g_phi * (r_i - r_j)) / 400))
    S_i <- score_adjusted_outcome(s1[[i]], s2[[i]], alpha, c_scale)
    d   <- K_eff * (S_i - E_i)

    ratings[[i1]] <- r_i + d; ratings[[i2]] <- r_j - d

    # 試合をするごとにRDが減少（情報蓄積）
    rds[[i1]] <- max(rds[[i1]] * rd_decay, rd_min)
    rds[[i2]] <- max(rds[[i2]] * rd_decay, rd_min)

    r1_before_v[[i]] <- r_i; r2_before_v[[i]] <- r_j
    E1_v[[i]] <- E_i; S1_v[[i]] <- S_i; delta_v[[i]] <- d
    r1_after_v[[i]] <- ratings[[i1]]; r2_after_v[[i]] <- ratings[[i2]]
  }

  history <- tibble(
    gamedate  = games$gamedate, gametype = games$gametype,
    team1 = games$team1, score1 = s1, score2 = s2, team2 = games$team2,
    r1_before = r1_before_v, r2_before = r2_before_v,
    E1 = E1_v, S1 = S1_v, delta = delta_v,
    r1_after = r1_after_v, r2_after = r2_after_v
  )

  list(
    history       = history,
    final_ratings = setNames(ratings, teams),
    final_rds     = setNames(rds, teams)
  )
}

# ============================================================
# 5. 評価関数 (ウォークフォワード)
# ============================================================

evaluate_metrics <- function(history, test_from = TRAIN_CUTOFF + 1) {
  test <- history |> filter(gamedate >= test_from)
  if (nrow(test) == 0) return(tibble(log_loss = NA, accuracy = NA, brier = NA, n = 0))

  y <- dplyr::case_when(
    test$score1 > test$score2 ~ 1,
    test$score1 < test$score2 ~ 0,
    TRUE                       ~ 0.5
  )

  tibble(
    log_loss = log_loss(y, test$E1),
    accuracy = accuracy(y, test$E1),
    brier    = brier_score(y, test$E1),
    n        = nrow(test)
  )
}

# ============================================================
# 6. パラメータ最適化
# ============================================================

optimize_elo <- function(games) {
  message("  [Elo] グリッドサーチ中...")

  grid <- expand_grid(
    alpha   = seq(0, 1, by = 0.1),
    c_scale = c(1, 2, 3, 5, 7, 10, 15)
  )
  grid$logloss <- map2_dbl(grid$alpha, grid$c_scale, function(a, c) {
    evaluate_metrics(compute_elo(games, alpha = a, c_scale = c)$history)$log_loss
  })

  best_grid <- grid |> arrange(logloss) |> slice(1)
  message(sprintf("  グリッド最良: alpha=%.2f c=%.1f logloss=%.4f",
                  best_grid$alpha, best_grid$c_scale, best_grid$logloss))

  opt <- optim(
    par    = c(best_grid$alpha, best_grid$c_scale),
    fn     = function(p) {
      evaluate_metrics(
        compute_elo(games, alpha = pmax(pmin(p[1],1),0), c_scale = pmax(p[2],0.5))$history
      )$log_loss
    },
    method = "L-BFGS-B", lower = c(0, 0.5), upper = c(1, 20)
  )

  list(
    alpha   = pmax(pmin(opt$par[1], 1), 0),
    c_scale = pmax(opt$par[2], 0.5),
    logloss = opt$value,
    grid    = grid
  )
}

optimize_glicko2 <- function(games) {
  message("  [Glicko2] グリッドサーチ中...")
  message("  (alpha×c_scale×shrink_year×shrink_season = 4×3×4×3 = 144通り)")

  grid <- expand_grid(
    alpha         = c(0, 0.3, 0.6, 1.0),
    c_scale       = c(2, 5, 10),
    shrink_year   = c(0, 0.10, 0.20, 0.30),  # 年度越え（卒業効果）
    shrink_season = c(0, 0.05, 0.10)          # 春秋間（休止効果）
  )

  grid$logloss <- pmap_dbl(grid, function(alpha, c_scale, shrink_year, shrink_season) {
    evaluate_metrics(
      compute_glicko2(games, alpha = alpha, c_scale = c_scale,
                      shrink_year = shrink_year, shrink_season = shrink_season)$history
    )$log_loss
  })

  best_grid <- grid |> arrange(logloss) |> slice(1)
  message(sprintf(
    "  グリッド最良: alpha=%.2f c=%.1f shrink_year=%.2f shrink_season=%.2f logloss=%.4f",
    best_grid$alpha, best_grid$c_scale,
    best_grid$shrink_year, best_grid$shrink_season, best_grid$logloss
  ))

  opt <- optim(
    par    = c(best_grid$alpha, best_grid$c_scale,
               best_grid$shrink_year, best_grid$shrink_season),
    fn     = function(p) {
      evaluate_metrics(
        compute_glicko2(games,
          alpha         = pmax(pmin(p[1], 1), 0),
          c_scale       = pmax(p[2], 0.5),
          shrink_year   = pmax(pmin(p[3], 0.5), 0),
          shrink_season = pmax(pmin(p[4], 0.3), 0))$history
      )$log_loss
    },
    method = "L-BFGS-B",
    lower  = c(0, 0.5, 0, 0),
    upper  = c(1, 20, 0.5, 0.3)
  )

  list(
    alpha         = pmax(pmin(opt$par[1], 1), 0),
    c_scale       = pmax(opt$par[2], 0.5),
    shrink_year   = pmax(pmin(opt$par[3], 0.5), 0),
    shrink_season = pmax(pmin(opt$par[4], 0.3), 0),
    logloss       = opt$value,
    grid          = grid
  )
}

# ============================================================
# 7. 4モデル比較
# ============================================================

message("=== モデル比較開始 ===\n")
message("試合数合計: ", nrow(games_raw))
message("学習: 〜", TRAIN_CUTOFF, "  評価: ", TRAIN_CUTOFF + 1, "〜\n")

message("--- Model 1: Elo (binary) ---")
res_elo_bin <- compute_elo(games_raw, alpha = 0)
met_elo_bin <- evaluate_metrics(res_elo_bin$history)
message(sprintf("  log-loss=%.4f  acc=%.3f  brier=%.4f  n=%d",
                met_elo_bin$log_loss, met_elo_bin$accuracy,
                met_elo_bin$brier, met_elo_bin$n))

message("\n--- Model 2: Elo + Score (最適化中...) ---")
opt_elo    <- optimize_elo(games_raw)
res_elo_sc <- compute_elo(games_raw, alpha = opt_elo$alpha, c_scale = opt_elo$c_scale)
met_elo_sc <- evaluate_metrics(res_elo_sc$history)
message(sprintf("  最適: alpha=%.3f c=%.2f", opt_elo$alpha, opt_elo$c_scale))
message(sprintf("  log-loss=%.4f  acc=%.3f  brier=%.4f  n=%d",
                met_elo_sc$log_loss, met_elo_sc$accuracy,
                met_elo_sc$brier, met_elo_sc$n))

message("\n--- Model 3: Glicko2 (binary) ---")
res_g2_bin <- compute_glicko2(games_raw, alpha = 0, shrink_year = 0, shrink_season = 0)
met_g2_bin <- evaluate_metrics(res_g2_bin$history)
message(sprintf("  log-loss=%.4f  acc=%.3f  brier=%.4f  n=%d",
                met_g2_bin$log_loss, met_g2_bin$accuracy,
                met_g2_bin$brier, met_g2_bin$n))

message("\n--- Model 4: Glicko2 + Score (最適化中...) ---")
opt_g2    <- optimize_glicko2(games_raw)
res_g2_sc <- compute_glicko2(games_raw,
                              alpha         = opt_g2$alpha,
                              c_scale       = opt_g2$c_scale,
                              shrink_year   = opt_g2$shrink_year,
                              shrink_season = opt_g2$shrink_season)
met_g2_sc <- evaluate_metrics(res_g2_sc$history)
message(sprintf("  最適: alpha=%.3f c=%.2f shrink_year=%.3f shrink_season=%.3f",
                opt_g2$alpha, opt_g2$c_scale,
                opt_g2$shrink_year, opt_g2$shrink_season))
message(sprintf("  log-loss=%.4f  acc=%.3f  brier=%.4f  n=%d",
                met_g2_sc$log_loss, met_g2_sc$accuracy,
                met_g2_sc$brier, met_g2_sc$n))

# ============================================================
# 8. 比較サマリー
# ============================================================

comparison <- tibble(
  model         = c("Elo (binary)", "Elo + Score", "Glicko2 (binary)", "Glicko2 + Score"),
  log_loss      = c(met_elo_bin$log_loss, met_elo_sc$log_loss,
                    met_g2_bin$log_loss,  met_g2_sc$log_loss),
  accuracy      = c(met_elo_bin$accuracy, met_elo_sc$accuracy,
                    met_g2_bin$accuracy,  met_g2_sc$accuracy),
  brier         = c(met_elo_bin$brier, met_elo_sc$brier,
                    met_g2_bin$brier,  met_g2_sc$brier),
  alpha         = c(0, opt_elo$alpha, 0, opt_g2$alpha),
  c_scale       = c(NA, opt_elo$c_scale, NA, opt_g2$c_scale),
  shrink_year   = c(NA, NA, 0, opt_g2$shrink_year),
  shrink_season = c(NA, NA, 0, opt_g2$shrink_season)
)

message("\n=== モデル比較結果 ===")
print(comparison |> arrange(log_loss))

write_excel_csv(comparison, "logs/model_comparison.csv")

# ============================================================
# 9. 最良モデルの採用とレーティング履歴出力
# ============================================================

best_model_name <- comparison |> arrange(log_loss) |> slice(1) |> pull(model)
message("\n採用モデル: ", best_model_name)

best_history <- switch(best_model_name,
  "Elo (binary)"     = res_elo_bin$history,
  "Elo + Score"      = res_elo_sc$history,
  "Glicko2 (binary)" = res_g2_bin$history,
  "Glicko2 + Score"  = res_g2_sc$history
)

attach_display_history <- function(history, init_r = INIT_RATING) {
  teams <- sort(unique(c(history$team1, history$team2)))
  current_ratings <- setNames(rep(init_r, length(teams)), teams)
  n <- nrow(history)

  display_r1_before <- numeric(n)
  display_r2_before <- numeric(n)
  display_r1_after  <- numeric(n)
  display_r2_after  <- numeric(n)
  display_delta1    <- numeric(n)
  display_delta2    <- numeric(n)

  for (i in seq_len(n)) {
    team1 <- history$team1[[i]]
    team2 <- history$team2[[i]]

    before_display <- setNames(to_hensachi(unname(current_ratings)), names(current_ratings))
    display_r1_before[[i]] <- before_display[[team1]]
    display_r2_before[[i]] <- before_display[[team2]]

    current_ratings[[team1]] <- history$r1_after[[i]]
    current_ratings[[team2]] <- history$r2_after[[i]]

    after_display <- setNames(to_hensachi(unname(current_ratings)), names(current_ratings))
    display_r1_after[[i]] <- after_display[[team1]]
    display_r2_after[[i]] <- after_display[[team2]]
    display_delta1[[i]] <- round(display_r1_after[[i]] - display_r1_before[[i]], 1)
    display_delta2[[i]] <- round(display_r2_after[[i]] - display_r2_before[[i]], 1)
  }

  history |>
    mutate(
      display_r1_before = display_r1_before,
      display_r2_before = display_r2_before,
      display_r1_after = display_r1_after,
      display_r2_after = display_r2_after,
      display_delta1 = display_delta1,
      display_delta2 = display_delta2
    )
}

add_event_metadata <- function(history) {
  history |>
    arrange(gamedate, gametype, team1, team2) |>
    mutate(event_id = row_number()) |>
    group_by(gamedate) |>
    mutate(
      event_index_on_date = row_number(),
      events_on_date = n(),
      event_seconds = if_else(
        events_on_date <= 1L,
        12L * 3600L,
        6L * 3600L + round((event_index_on_date - 1L) * (12L * 3600L) / (events_on_date - 1L))
      )
    ) |>
    ungroup() |>
    mutate(
      event_datetime = as.POSIXct(gamedate, tz = "Asia/Tokyo") + event_seconds
    ) |>
    select(-event_index_on_date, -events_on_date, -event_seconds)
}

build_rating_snapshots <- function(history, init_r = INIT_RATING) {
  teams <- sort(unique(c(history$team1, history$team2)))
  current_ratings <- setNames(rep(init_r, length(teams)), teams)
  snapshots <- vector("list", nrow(history))

  for (i in seq_len(nrow(history))) {
    team1 <- history$team1[[i]]
    team2 <- history$team2[[i]]

    current_ratings[[team1]] <- history$r1_after[[i]]
    current_ratings[[team2]] <- history$r2_after[[i]]

    display_ratings <- setNames(to_hensachi(unname(current_ratings)), names(current_ratings))

    snapshots[[i]] <- tibble(
      event_id = history$event_id[[i]],
      date = history$gamedate[[i]],
      event_datetime = history$event_datetime[[i]],
      team = teams,
      rating = unname(current_ratings),
      display_rating = unname(display_ratings)
    )
  }

  bind_rows(snapshots)
}

best_history <- best_history |>
  add_event_metadata() |>
  attach_display_history()

snapshot_with_hensachi <- build_rating_snapshots(best_history)

# ---- 試合履歴（Web表示用）----
game_results_with_ratings <- best_history |>
  mutate(
    season = if_else(month(gamedate) <= 8, "春", "秋"),
    year   = year(gamedate),
    event_datetime = format(event_datetime, "%Y-%m-%dT%H:%M:%S")
  ) |>
  select(
    event_id, event_datetime,
    gamedate, year, season, gametype,
    team1, r1_before, display_r1_before,
    score1, score2,
    team2, r2_before, display_r2_before,
    E1, delta, display_delta1, display_delta2,
    r1_after, display_r1_after,
    r2_after, display_r2_after
  )

write_excel_csv(game_results_with_ratings,
                "data_out/ratings/game_results_with_ratings.csv")

# ---- 最終レーティング（偏差値スケール付き）----
# 最新イベントの全チームスナップショットをそのまま採用する
final_ratings <- snapshot_with_hensachi |>
  filter(event_id == max(event_id)) |>
  arrange(desc(display_rating), team) |>
  mutate(
    event_datetime = format(event_datetime, "%Y-%m-%dT%H:%M:%S"),
    rank = row_number()
  )

write_excel_csv(final_ratings, "data_out/ratings/final_ratings.csv")

# ---- レーティング推移（偏差値スケール付き）----
write_excel_csv(
  snapshot_with_hensachi |>
    mutate(event_datetime = format(event_datetime, "%Y-%m-%dT%H:%M:%S")),
  "data_out/ratings/rating_history.csv"
)

# グリッドサーチ結果保存
write_excel_csv(opt_elo$grid, "logs/grid_elo.csv")
write_excel_csv(opt_g2$grid,  "logs/grid_glicko2.csv")

message("\n=== 出力完了 ===")
message("  data_out/ratings/game_results_with_ratings.csv")
message("  data_out/ratings/final_ratings.csv  (rating + display_rating)")
message("  data_out/ratings/rating_history.csv (rating + display_rating)")
message("  logs/model_comparison.csv")

# ============================================================
# 10. 簡易プレビュー
# ============================================================

message("\n--- 現在のレーティング ---")
print(final_ratings |> select(rank, team, rating, display_rating))

message("\n--- 番狂わせTOP5 ---")
upsets <- best_history |>
  mutate(
    y        = if_else(score1 > score2, 1, if_else(score1 < score2, 0, 0.5)),
    surprise = abs(y - E1),
    winner   = if_else(score1 > score2, team1, team2),
    loser    = if_else(score1 > score2, team2, team1)
  ) |>
  arrange(desc(surprise)) |>
  select(gamedate, gametype, winner, loser, score1, score2, E1, surprise) |>
  slice_head(n = 5)

print(upsets)
