#' RM2C2: Export trial-level data for reliability analysis
#'
#' Writes long-format CSVs with the three-level structure
#' subject (id) / session / trial, so variance components can be estimated
#' outside this repo.
#'
#' Cleaning matches clean_sample_data.qmd exactly: interrupted attempts are
#' resolved to the last complete file and unfinished visits are held out, so
#' these files cover the same 155 visits as phonedata.csv.
#'
#' Run from the project root:
#'   Rscript export_reliability_data.R [raw_dir] [out_dir]

suppressMessages({
  library(dplyr); library(tidyr); library(purrr)
  library(readr); library(stringr); library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
raw_dir <- if (length(args) >= 1) args[1] else "data_sample"
out_dir <- if (length(args) >= 2) args[2] else "output_sample"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

rm2c2_version <- numeric_version(read.dcf("DESCRIPTION", fields = "Version")[[1]])
packageVersion <- function(pkg) {
  if (identical(pkg, "RM2C2")) return(rm2c2_version)
  utils::packageVersion(pkg)
}

for (f in c("add_to", "mult_by", "distance", "SDT_adj",
            "score_symbol_search", "score_dot_memory", "score_color_shapes",
            "sample_data_prep")) {
  source(file.path("R", paste0(f, ".R")))
}

prepared <- prepare_sample_data(raw_dir)

# `session` is the app's own counter and is offset for participants 351 and 353
# (their sessions run 2,3,4). `session_seq` renumbers to 1..3 in collection
# order, which is the numbering the ALH analysis file uses. Join on whichever
# one matches the rest of your data.
session_key <- prepared$survey_clean |>
  transmute(user_id, visit_id,
            session = as.numeric(session),
            visit_date, start_time) |>
  arrange(user_id, session) |>
  group_by(user_id) |>
  mutate(session_seq = row_number()) |>
  ungroup()

add_keys <- function(df) {
  df |>
    left_join(session_key, by = c("user_id", "visit_id")) |>
    transmute(id = as.numeric(user_id), session, session_seq, visit_id,
              across(-any_of(c("user_id", "session", "session_seq", "visit_id",
                               "visit_date", "start_time")))) |>
    arrange(id, session_seq)
}

symbol_scored <- prepared$symbol_raw |> score_symbol_search()
dot_scored    <- prepared$dot_raw |> score_dot_memory() |> ungroup()
color_scored  <- prepared$color_raw |> score_color_shapes()

# ---- Symbol Search: 18 trials per session -----------------------------------
# `accuracy` is kept rather than pre-filtered: the published DV is median RT of
# *correct* trials, so filter to accuracy == 1 to reproduce it.
ss_trials <- symbol_scored |>
  transmute(user_id, visit_id,
            trial = as.integer(trial_num),
            trial_type,
            response_time = as.numeric(response_time),
            accuracy = as.integer(accuracy)) |>
  add_keys()

# ---- Dot Memory: 3 trials per session ---------------------------------------
dm_trials <- dot_scored |>
  transmute(user_id, visit_id,
            trial = as.integer(trial_num),
            response_time = as.numeric(response_time),
            error_distance = (r1_distance + r2_distance + r3_distance) / 3,
            sum_error_distance = r1_distance + r2_distance + r3_distance,
            median_error_distance = median_error_distance,
            perfect_dots = as.integer(sum_perfect_dots)) |>
  add_keys()

# ---- Dot Memory: 9 dots per session -----------------------------------------
# The trial facet can be taken as 9 dots rather than 3 trials, which uses the
# same data more efficiently.
dm_dots <- dot_scored |>
  select(user_id, visit_id, trial_num, r1_distance, r2_distance, r3_distance) |>
  pivot_longer(starts_with("r"), names_to = "dot", values_to = "distance") |>
  transmute(user_id, visit_id,
            trial = as.integer(trial_num),
            dot = as.integer(str_extract(dot, "\\d")),
            distance) |>
  add_keys()

# ---- Color Shapes: 10 trials per session ------------------------------------
# CorRec (HIT rate - FA rate) is NOT a per-trial quantity: it needs both a
# change and a no-change trial. Use this file for trial-level accuracy, and the
# blocks/halves files below for CorRec.
cs_trials <- color_scored |>
  transmute(user_id, visit_id,
            trial = as.integer(trial_num),
            trial_type = as.integer(trial_type),
            button_pressed = as.integer(button_pressed),
            HIT = as.integer(HIT), MISS = as.integer(MISS),
            FA = as.integer(FA), CR = as.integer(CR),
            correct = as.integer(HIT + CR),
            response_time = as.numeric(response_time)) |>
  add_keys()

# ---- Color Shapes: 5 CorRec blocks per session ------------------------------
# Each block pairs the k-th change trial with the k-th no-change trial, giving
# the finest split that still yields a real CorRec. i = 5.
cs_blocks <- color_scored |>
  group_by(user_id, visit_id, trial_type) |>
  mutate(rk = row_number()) |>
  ungroup() |>
  group_by(user_id, visit_id, rk) |>
  summarise(n_change = sum(trial_type == 1),
            n_no_change = sum(trial_type == 0),
            hit_rate = sum(HIT) / sum(trial_type == 1),
            fa_rate = sum(FA) / sum(trial_type == 0),
            .groups = "drop") |>
  filter(n_change > 0, n_no_change > 0) |>
  mutate(correc = hit_rate - fa_rate) |>
  rename(block = rk) |>
  add_keys()

# ---- Color Shapes: 2 CorRec halves per session ------------------------------
# Odd/even split, the procedure Sliwinski et al. (2018) used for n-back. i = 2.
cs_halves <- color_scored |>
  mutate(half = if_else(trial_num %% 2 == 1, "odd", "even")) |>
  group_by(user_id, visit_id, half) |>
  summarise(n_change = sum(trial_type == 1),
            n_no_change = sum(trial_type == 0),
            hit_rate = sum(HIT) / sum(trial_type == 1),
            fa_rate = sum(FA) / sum(trial_type == 0),
            .groups = "drop") |>
  mutate(correc = hit_rate - fa_rate) |>
  add_keys()

# ---- Occasion-level scores --------------------------------------------------
# ss / dm / cs reproduce the outcome variables in the ALH analysis file.
occasion <- reduce(
  list(
    symbol_scored |> filter(accuracy == 1, !is.na(response_time)) |>
      group_by(user_id, visit_id) |>
      summarise(ss_median_rt_accurate = median(as.numeric(response_time)),
                ss_n_accurate = n(), .groups = "drop"),
    # summary_dot_memory() defines median.error.distance.overall as the median
    # ACROSS trials of the per-trial SUM of the three dot distances. This is the
    # `dm` outcome in the ALH analysis file; the mean version is kept alongside
    # because it is the better-behaved statistic over only 3 trials.
    dot_scored |> group_by(user_id, visit_id) |>
      summarise(dm_median_error_distance =
                  median(r1_distance + r2_distance + r3_distance),
                dm_mean_error_distance =
                  mean(r1_distance + r2_distance + r3_distance),
                .groups = "drop"),
    color_scored |> group_by(user_id, visit_id) |>
      summarise(cs_hit_rate = sum(HIT) / sum(trial_type == 1),
                cs_fa_rate = sum(FA) / sum(trial_type == 0),
                cs_prop_correct = mean(HIT + CR), .groups = "drop") |>
      mutate(cs_correc = cs_hit_rate - cs_fa_rate)
  ),
  left_join, by = c("user_id", "visit_id")
) |>
  mutate(ss_seconds = ss_median_rt_accurate / 1000) |>
  add_keys()

files <- list(
  reliability_symbol_search_trials = ss_trials,
  reliability_dot_memory_trials    = dm_trials,
  reliability_dot_memory_dots      = dm_dots,
  reliability_color_shapes_trials  = cs_trials,
  reliability_color_shapes_blocks  = cs_blocks,
  reliability_color_shapes_halves  = cs_halves,
  reliability_occasion_scores      = occasion
)

iwalk(files, function(d, nm) {
  p <- file.path(out_dir, paste0(nm, ".csv"))
  write_csv(d, p)
  cat(sprintf("%-46s %5d rows x %2d cols  ->  %s\n", nm, nrow(d), ncol(d), p))
})

cat("\nparticipants:", n_distinct(occasion$id),
    " sessions:", nrow(occasion),
    " sessions per participant:",
    paste(sort(unique(table(occasion$id))), collapse = "/"), "\n")
