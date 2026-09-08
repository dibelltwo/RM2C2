#' RM2C2: Sample data preparation

#' @name prepare_sample_data
#' @description
#' Shared parsing and cleaning for the sample-data reports. Reads the raw `.txt`
#' exports, resolves interrupted attempts, and returns the visit-level survey
#' table alongside the trial-level task tables.
#'
#' Sourced by both `clean_sample_data.qmd` and `reliability_analysis.qmd` so the
#' two reports can never drift apart on how a visit is defined or cleaned.
#' @param raw_dir class: string; folder holding the `Survey`, `Symbol Search`,
#'   `Dot Memory`, and `Color Shapes` subfolders
#' @keywords m2c2, cognition
#' @import tidyverse

# Number of trials each task is expected to write per completed visit.
expected_trials <- c(
  "Symbol Search" = 18L,
  "Dot Memory" = 3L,
  "Color Shapes" = 10L
)

task_dirs <- c("Survey", "Symbol Search", "Dot Memory", "Color Shapes")

# An `exit_status` outside this set means the app never finished writing the
# visit (backgrounded, force-closed, or bounced back to the start menu). Those
# records are reported as red flags but held out of the cleaned dataset.
complete_exit_status <- c("NORMAL", "TIMED_OUT")

clean_name <- function(x) {
  x |>
    str_trim() |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

extract_filename_meta <- function(path) {
  stem <- basename(path) |>
    str_remove("\\.txt$")
  pieces <- str_split(stem, "_", simplify = TRUE)

  if (str_starts(stem, "game_")) {
    task_name <- str_c(pieces[4:(ncol(pieces) - 4)], collapse = "_") |>
      str_replace_all("_", " ")
    stamp <- pieces[, ncol(pieces) - 3]
  } else {
    task_name <- "Survey"
    stamp <- pieces[, ncol(pieces) - 3] |>
      str_replace_all("x", "")
  }

  visit_date <- str_c(
    pieces[, ncol(pieces) - 2],
    pieces[, ncol(pieces) - 1],
    pieces[, ncol(pieces)],
    sep = "-"
  )

  tibble(
    source_file = path,
    raw_file_name = basename(path),
    task_name = task_name,
    user_id = pieces[, 2],
    session = pieces[, 3],
    visit_date = visit_date,
    file_stamp = stamp,
    visit_id = str_c(pieces[, 2], pieces[, 3], visit_date, sep = "_")
  )
}

parse_kv_string <- function(x, item_sep = ",", kv_sep = ":") {
  parts <- str_split(x, item_sep, simplify = FALSE)[[1]]
  parts <- parts[nzchar(str_trim(parts))]
  keys <- str_extract(parts, paste0("^[^", kv_sep, "]+"))
  values <- str_replace(parts, paste0("^[^", kv_sep, "]+", kv_sep), "")
  tibble(key = clean_name(keys), value = str_trim(values))
}

coerce_columns <- function(df) {
  readr::type_convert(df, na = c("", "NA", "NULL", "NOT SET"))
}

normalize_meta_types <- function(df) {
  for (nm in c(
    "source_file",
    "raw_file_name",
    "task_name",
    "user_id",
    "session",
    "visit_date",
    "file_stamp",
    "visit_id"
  )) {
    if (nm %in% names(df)) {
      df[[nm]] <- as.character(df[[nm]])
    }
  }
  df
}

parse_task_file <- function(path) {
  meta <- extract_filename_meta(path)
  lines <- readLines(path, warn = FALSE)

  rows <- map(
    lines,
    ~ parse_kv_string(.x, item_sep = ",", kv_sep = ":") |>
      tidyr::pivot_wider(names_from = key, values_from = value)
  )

  bind_rows(rows) |>
    mutate(across(everything(), as.character)) |>
    bind_cols(meta[rep(1, length(rows)), c("source_file", "raw_file_name", "visit_date", "file_stamp", "visit_id")]) |>
    coerce_columns() |>
    normalize_meta_types() |>
    mutate(
      user_id = as.character(user_id),
      session = as.character(session),
      task_name = as.character(game_name)
    )
}

parse_survey_file <- function(path) {
  meta <- extract_filename_meta(path)
  lines <- readLines(path, warn = FALSE)

  survey_row <- map(
    lines,
    ~ parse_kv_string(.x, item_sep = "\n", kv_sep = ":")
  ) |>
    bind_rows() |>
    distinct(key, .keep_all = TRUE) |>
    tidyr::pivot_wider(names_from = key, values_from = value)

  if ("id" %in% names(survey_row)) {
    survey_row <- rename(survey_row, survey_user_id = id)
  }
  if ("session" %in% names(survey_row)) {
    survey_row <- rename(survey_row, survey_session = session)
  }

  bind_cols(survey_row, meta) |>
    mutate(
      across(everything(), as.character),
      user_id = coalesce(as.character(survey_user_id), as.character(user_id)),
      session = coalesce(as.character(survey_session), as.character(session))
    ) |>
    select(-any_of(c("survey_user_id", "survey_session"))) |>
    normalize_meta_types()
}

# --- Field formatting -------------------------------------------------------
# The raw app files write dates as YYYY/MM/DD, the build stamp with a two-digit
# year, and self-report clock answers in 12-hour form. Normalize each to the
# canonical shape used by the reference dataset.

drop_leading_zeros <- function(x) {
  x |>
    str_replace("^0", "") |>
    str_replace_all("(?<=/)0", "")
}

format_visit_date <- function(x) {
  parsed <- as.Date(x, format = "%Y/%m/%d")
  out <- format(parsed, "%m/%d/%Y")
  if_else(is.na(parsed), NA_character_, drop_leading_zeros(out))
}

format_build_date <- function(x) {
  parsed <- as.POSIXct(x, format = "%m/%d/%y %I:%M %p", tz = "UTC")
  out <- format(parsed, "%m/%d/%Y %H:%M")
  if_else(is.na(parsed), NA_character_, drop_leading_zeros(out))
}

format_clock_time <- function(x) {
  parsed <- as.POSIXct(x, format = "%I:%M %p", tz = "UTC")
  out <- format(parsed, "%H:%M:%S")
  if_else(is.na(parsed), NA_character_, out)
}

# Keep only the latest attempt per visit. When a session is interrupted the app
# writes a partial file and then a second, complete file with a later stamp for
# the same user/session/date. Without this, both files feed the same summary
# group and every count, sum, and SD is computed over doubled trials.
keep_latest_attempt <- function(df) {
  df |>
    group_by(visit_id) |>
    filter(file_stamp == max(file_stamp)) |>
    ungroup()
}

#' @export
prepare_sample_data <- function(raw_dir) {
  task_files <- purrr::map(
    rlang::set_names(task_dirs),
    ~ list.files(file.path(raw_dir, .x), pattern = "txt$", full.names = TRUE)
  )

  survey_all <- map_dfr(task_files[["Survey"]], parse_survey_file)
  symbol_all <- map_dfr(task_files[["Symbol Search"]], parse_task_file)
  dot_all <- map_dfr(task_files[["Dot Memory"]], parse_task_file)
  color_all <- map_dfr(task_files[["Color Shapes"]], parse_task_file)

  # A visit can leave more than one file behind: the app writes what it has when
  # a session is backgrounded or force-closed, then writes a complete file when
  # the participant returns. Keep the last attempt for every visit, and hold back
  # visits that never reached a finished state. Both sets are returned.
  survey_raw <- survey_all |>
    keep_latest_attempt() |>
    filter(exit_status %in% complete_exit_status)

  incomplete_surveys <- survey_all |>
    filter(!exit_status %in% complete_exit_status)

  complete_visit_ids <- survey_raw$visit_id

  symbol_raw <- symbol_all |> keep_latest_attempt() |> filter(visit_id %in% complete_visit_ids)
  dot_raw <- dot_all |> keep_latest_attempt() |> filter(visit_id %in% complete_visit_ids)
  color_raw <- color_all |> keep_latest_attempt() |> filter(visit_id %in% complete_visit_ids)

  survey_clean <- survey_raw |>
    transmute(
      user_id = as.character(user_id),
      session = as.character(session),
      visit_id,
      visit_date,
      survey_file = raw_file_name,
      survey_file_stamp = file_stamp,
      pack = pack,
      start_date = start_date,
      start_time = start_time,
      end_date = end_date,
      end_time = end_time,
      exit_status = exit_status,
      exit_screen = exit_screen,
      build_date = build_date,
      application_version = application_version,
      android_version = android_version,
      device_id = device_id,
      device_manufacturer = device_manufacturer,
      device_model = device_model,
      launch_type = launch_type,
      beep_file = beep_file,
      target_beep_time = target_beep_time,
      actual_beep_time = actual_beep_time,
      screen_trail = screen_trail,
      bed_time = format_clock_time(bed_time),
      getup_time = format_clock_time(getup_time),
      sleep_quality = as.numeric(sleep_quality),
      mindsharp = as.numeric(mindsharp),
      concentrate = as.numeric(concentrate),
      # Left as text: the app writes sentinels such as "NO_VALUE" that as.numeric()
      # would silently turn into NA, erasing the difference between "not answered"
      # and "screen never shown".
      game_distract = as.character(game_distract)
    ) |>
    mutate(
      start_date = format_visit_date(start_date),
      end_date = format_visit_date(end_date),
      build_date = format_build_date(build_date)
    )




  # `moment_surveys` numbers the survey moments completed on a device within a
  # single day, so a visit can be placed in the order it was actually collected
  # regardless of the app's own `session` counter.
  survey_clean <- survey_clean |>
    arrange(device_id, visit_date, as.integer(user_id), as.integer(session)) |>
    group_by(device_id, visit_date) |>
    mutate(moment_surveys = row_number()) |>
    ungroup()

  list(
    task_files = task_files,
    survey_all = survey_all,
    incomplete_surveys = incomplete_surveys,
    survey_raw = survey_raw,
    survey_clean = survey_clean,
    symbol_raw = symbol_raw,
    dot_raw = dot_raw,
    color_raw = color_raw
  )
}
